# Tema visual y funciones de tabla y gráfico
# -----------------------------------------------------------------------------
# Todo lo reutilizable del análisis descriptivo: el tema de las figuras, la
# paleta y las funciones que construyen las tablas de frecuencia, los gráficos
# univariados y las medidas de asociación bivariadas.
# Lo carga reports/Entrega 2.qmd.
#
# Depende de: R/exportacion.R (num, celda, tex_escape, tex_longtable) y de que
# `dataset` ya exista, porque varias funciones lo toman como df por defecto.

library(ggplot2)
library(scales)
library(patchwork)
library(purrr)

# La tipografía de las figuras es la misma del documento LaTeX (Times New Roman,
# XeLaTeX) para que no se note el cambio de medio al insertarlas.
theme_set(
  theme_minimal(base_size = 10, base_family = "Times New Roman") +
    theme(
      plot.title    = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8.5, colour = "grey35"),
      plot.caption  = element_text(size = 7, colour = "grey45"),
      panel.grid.minor = element_blank()
    )
)

AZUL    <- "#2C6E9B"   # color principal
NARANJA <- "#D1622B"   # acento: medias y curvas de densidad

# --- Cualitativas -----------------------------------------------------------

# Frecuencias absolutas y relativas. Si una variable tiene NA se muestran como
# categoría explícita en lugar de eliminarse en silencio.
tabla_frec <- function(var, df = dataset) {
  x  <- df[[var]]
  tb <- as.data.frame(table(x, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tb) <- c("nivel", "n")
  tb$nivel[is.na(tb$nivel)] <- "(Sin dato)"

  tb <- tibble::as_tibble(tb) |>
    dplyr::mutate(pct = 100 * n / sum(n))

  # La frecuencia acumulada sólo tiene sentido si el factor está ordenado
  if (is.ordered(x)) tb <- dplyr::mutate(tb, pct_acum = cumsum(pct))
  tb
}

# Bloque de frecuencias ya formateado para tex_tabla_doble(). `etiquetas` es
# un vector con nombre que reemplaza los niveles originales en la tabla.
bloque_frec <- function(vars, etiquetas = NULL, df = dataset) {
  map_dfr(vars, function(v) {
    tb  <- tabla_frec(v, df)
    niv <- tb$nivel
    if (!is.null(etiquetas)) niv <- dplyr::coalesce(unname(etiquetas[niv]), niv)
    tibble::tibble(
      grupo = paste0("\\texttt{", tex_escape(v), "}"),
      nivel = tex_escape(niv),
      n     = celda(tb$n, 0),
      pct   = celda(tb$pct, 1)
    )
  })
}

# Barra horizontal apilada al 100 % para una variable ordinal. Los niveles
# van de claro a oscuro según su posición en la escala; los niveles fuera de
# la escala (por ejemplo "No sabe") se pintan en gris.
graf_ord_apilado <- function(x, titulo, gris = character(0)) {
  tb <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(tb) <- c("nivel", "n")
  tb$pct   <- 100 * tb$n / sum(tb$n)
  tb$nivel <- factor(tb$nivel, levels = levels(x))

  escala <- setdiff(levels(x), gris)
  cols   <- setNames(grDevices::colorRampPalette(c("#DCE6EE", "#123D5C"))(length(escala)),
                     escala)
  cols   <- c(cols, setNames(rep("#B8BEC4", length(gris)), gris))
  oscuro <- escala[seq_along(escala) > length(escala) / 2]
  tb$txt <- ifelse(tb$nivel %in% oscuro, "white", "grey15")

  ggplot(tb, aes(x = pct, y = "", fill = nivel)) +
    geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct >= 5, paste0(num(pct, 1), "%"), ""), colour = txt),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = 3.2, family = "Times New Roman") +
    scale_fill_manual(values = cols) +
    scale_colour_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = titulo, x = NULL, y = NULL, fill = NULL) +
    theme(legend.position = "bottom", legend.justification = "left",
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(1, "mm"),
          legend.key.size = unit(3.5, "mm"), legend.text = element_text(size = 9),
          axis.text = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          plot.margin = margin(2, 4, 8, 4))
}

# --- Cuantitativas ----------------------------------------------------------

# La moda sólo se reporta en las variables discretas. EDAD e INSE son continuas
# y su moda muestral no es informativa: es el "cuando aplique" del enunciado.
CONTINUAS <- c("EDAD", "ESTU_INSE_INDIVIDUAL")

moda_num <- function(x, var) {
  if (var %in% CONTINUAS) return(NA_real_)
  x  <- x[!is.na(x)]
  tb <- table(x)
  as.numeric(names(tb)[which.max(tb)])
}

# Coeficiente de asimetría muestral, para documentar la forma de cada distribución
asimetria <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

resumen_cuant <- function(vars, df = dataset) {
  map_dfr(vars, function(v) {
    x  <- df[[v]]
    xs <- x[!is.na(x)]
    q  <- unname(quantile(xs, c(0.25, 0.50, 0.75)))
    tibble::tibble(
      variable = v,
      n        = length(xs),
      n_na     = sum(is.na(x)),
      media    = mean(xs),
      mediana  = q[2],
      moda     = moda_num(x, v),
      minimo   = min(xs),
      maximo   = max(xs),
      rango    = max(xs) - min(xs),
      varianza = var(xs),
      desv_est = sd(xs),
      cv       = 100 * sd(xs) / mean(xs),
      Q1       = q[1],
      Q3       = q[3],
      RIC      = q[3] - q[1],
      asimetria = asimetria(xs)
    )
  })
}

# Histograma con densidad superpuesta + diagrama de caja compartiendo el eje X
graf_dist <- function(var, df = dataset, titulo = var, bins = 45, unidad = "") {
  d  <- tibble::tibble(x = df[[var]][!is.na(df[[var]])])
  m  <- mean(d$x); md <- median(d$x)
  rango <- range(d$x)

  p1 <- ggplot(d, aes(x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = AZUL, colour = "white", linewidth = 0.15) +
    geom_density(colour = NARANJA, linewidth = 0.9) +
    geom_vline(xintercept = m, colour = "grey20", linetype = "dashed") +
    coord_cartesian(xlim = rango) +
    scale_y_continuous(labels = label_number(decimal.mark = ",")) +
    labs(title = titulo,
         subtitle = paste0("Media = ", num(m), "  |  Mediana = ", num(md),
                           "  |  DE = ", num(sd(d$x))),
         x = NULL, y = "Densidad")

  # Mismo rango en ambos paneles para que el histograma y la caja queden alineados
  p2 <- ggplot(d, aes(x)) +
    geom_boxplot(fill = AZUL, alpha = 0.5, outlier.alpha = 0.03,
                 outlier.size = 0.4, width = 0.6) +
    coord_cartesian(xlim = rango) +
    labs(x = unidad, y = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  p1 / p2 + plot_layout(heights = c(3.2, 1))
}

# --- Bivariado --------------------------------------------------------------

# V de Cramér. Calculamos el estadístico ji-cuadrado a mano en lugar de usar
# chisq.test().
cramer_v <- function(x, y) {
  tab <- table(x, y)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (any(dim(tab) < 2)) return(NA_real_)
  esp <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  chi <- sum((tab - esp)^2 / esp)
  sqrt(chi / (sum(tab) * (min(dim(tab)) - 1)))
}

# Eta cuadrado: proporción de la varianza de una cuantitativa explicada por
# una cualitativa. Se calcula como SS_entre / SS_total
eta2 <- function(num_v, cat_v) {
  ok <- !is.na(num_v) & !is.na(cat_v)
  y  <- num_v[ok]
  g  <- droplevels(as.factor(cat_v[ok]))
  if (nlevels(g) < 2) return(NA_real_)
  mg <- tapply(y, g, mean)
  ng <- tapply(y, g, length)
  sum(ng * (mg - mean(y))^2) / sum((y - mean(y))^2)
}

# Diagrama de caja vertical de una cuantitativa por grupos, con la media
# marcada en naranja y el eta cuadrado en el subtítulo. Recibe los vectores ya
# recodificados para poder usar etiquetas cortas en paneles estrechos.
graf_box <- function(cat, y, titulo, ylab = "Puntaje global") {
  d <- tibble::tibble(cat = cat, y = y) |>
    dplyr::filter(!is.na(cat), !is.na(y))

  ggplot(d, aes(x = cat, y = y)) +
    geom_boxplot(fill = AZUL, alpha = 0.55, outlier.alpha = 0.02,
                 outlier.size = 0.25, width = 0.62) +
    stat_summary(fun = mean, geom = "point", colour = NARANJA, size = 1.4) +
    labs(title = titulo,
         subtitle = paste0("\u03b7\u00b2 = ", num(eta2(d$y, d$cat), 3)),
         x = NULL, y = ylab) +
    theme(axis.text.x = element_text(size = 8.5), axis.text.y = element_text(size = 9))
}

# Matriz triangular inferior (correlaciones o V de Cramér) con etiquetas en
# convención española. `min_etiqueta` oculta los valores pequeños en matrices
# densas para que las etiquetas no se encimen; el color los sigue mostrando.
graf_matriz <- function(M, titulo, limites = c(-1, 1),
                        colores = c(NARANJA, "white", AZUL),
                        min_etiqueta = 0, tam = 3.1, dig = 2) {
  niv <- rownames(M)
  d <- as.data.frame(as.table(M), stringsAsFactors = FALSE) |>
    stats::setNames(c("fila", "col", "valor")) |>
    dplyr::mutate(i = match(fila, niv), j = match(col, niv)) |>
    dplyr::filter(i > j) |>
    dplyr::mutate(fila = factor(fila, levels = rev(niv)),
                  col  = factor(col, levels = niv),
                  etiqueta = ifelse(abs(valor) >= min_etiqueta, num(valor, dig), ""),
                  txt = ifelse(abs(valor) > 0.6 * max(abs(limites)), "white", "grey15"))

  ggplot(d, aes(x = col, y = fila, fill = valor)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = etiqueta, colour = txt), size = tam,
              family = "Times New Roman") +
    scale_fill_gradientn(colours = colores, limits = limites, guide = "none") +
    scale_colour_identity() +
    scale_x_discrete(drop = TRUE) +
    scale_y_discrete(drop = TRUE) +
    coord_fixed() +
    labs(title = titulo, x = NULL, y = NULL) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9))
}

# Barras apiladas al 100 %: la forma adecuada de comparar composición cuando
# los grupos tienen tamaños muy dispares. `a` define las barras y `b` el relleno.
graf_apilado <- function(a, b, titulo, xlab = NULL) {
  d <- tibble::tibble(a = a, b = b) |>
    dplyr::filter(!is.na(a), !is.na(b)) |>
    dplyr::count(a, b) |>
    dplyr::group_by(a) |>
    dplyr::mutate(pct = 100 * n / sum(n)) |>
    dplyr::ungroup()

  k <- nlevels(factor(d$b))
  cols <- grDevices::colorRampPalette(c("#DCE6EE", "#123D5C"))(k)
  d$txt <- ifelse(as.integer(factor(d$b)) > k / 2, "white", "grey15")

  ggplot(d, aes(x = pct, y = a, fill = b)) +
    geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct >= 9, paste0(num(pct, 1), "%"), ""), colour = txt),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = 3.1, family = "Times New Roman") +
    scale_fill_manual(values = cols) +
    scale_colour_identity() +
    scale_x_continuous(expand = c(0, 0), labels = \(x) paste0(x, "%")) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = titulo, x = xlab, y = NULL, fill = NULL) +
    theme(legend.position = "bottom", legend.key.size = unit(3.5, "mm"),
          legend.text = element_text(size = 9), axis.text = element_text(size = 9),
          panel.grid = element_blank())
}
