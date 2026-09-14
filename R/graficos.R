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

# Frecuencias absolutas y relativas. Los NA se muestran de forma explícita:
# el 16,3 % de faltantes de COLE_BILINGUE se conservó a propósito y ocultarlo
# daría una lectura equivocada de la variable.
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

# Tabla de frecuencias consolidada de un bloque temático: cada variable
# encabeza su propio grupo de filas dentro de una única longtable.
exportar_frec <- function(vars, archivo, caption, df = dataset) {
  d <- map_dfr(vars, function(v) {
    tb <- tabla_frec(v, df)
    tibble::tibble(
      grupo = v, nivel = tb$nivel, n = tb$n, pct = tb$pct,
      acum  = if ("pct_acum" %in% names(tb)) tb$pct_acum else NA_real_
    )
  })

  tex_longtable(
    data.frame(nivel = tex_escape(d$nivel),
               n     = celda(d$n, 0),
               pct   = celda(d$pct, 2, pct = TRUE),
               acum  = celda(d$acum, 2, pct = TRUE)),
    archivo, caption, paste0("tab:", sub("^tab-", "", archivo)),
    align     = "P{6.2cm} r r r",
    cabeceras = c("Categoría", "Frecuencia", "Frec. relativa", "Frec. acumulada"),
    grupos    = paste0("\\texttt{", tex_escape(d$grupo), "}"),
    nota      = "La frecuencia acumulada sólo se reporta en las variables ordinales. Los datos faltantes se muestran como categoría explícita."
  )
  d
}

# Diagrama de barras. Los factores ordenados conservan su orden natural; los
# nominales se ordenan por frecuencia. Se pasa a barras horizontales cuando
# hay muchos niveles o las etiquetas son largas.
graf_barras <- function(var, df = dataset, titulo = var) {
  x  <- df[[var]]
  tb <- tabla_frec(var, df)
  ord   <- is.ordered(x)
  horiz <- nrow(tb) > 6 || max(nchar(tb$nivel)) > 14

  niveles <- if (ord) {
    if (horiz) rev(tb$nivel) else tb$nivel
  } else {
    tb$nivel[order(tb$n, decreasing = !horiz)]
  }
  tb$nivel <- factor(tb$nivel, levels = niveles)

  p <- ggplot(tb, aes(x = nivel, y = pct)) +
    geom_col(fill = AZUL, width = 0.72) +
    geom_text(aes(label = paste0(num(pct), "%")), size = 2.5, colour = "grey25",
              family = "Times New Roman",
              hjust = if (horiz) -0.12 else 0.5,
              vjust = if (horiz) 0.5 else -0.45) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = titulo, x = NULL, y = "% de estudiantes")

  if (horiz) p + coord_flip() else p
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
      RIC      = q[3] - q[1]
    )
  })
}

# Coeficiente de asimetría muestral, para documentar la forma de cada distribución
asimetria <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

# Histograma con densidad superpuesta + diagrama de caja compartiendo el eje X
graf_dist <- function(var, df = dataset, titulo = var, bins = 45, unidad = "") {
  d  <- tibble::tibble(x = df[[var]][!is.na(df[[var]])])
  m  <- mean(d$x); md <- median(d$x)

  p1 <- ggplot(d, aes(x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = AZUL, colour = "white", linewidth = 0.15) +
    geom_density(colour = NARANJA, linewidth = 0.9) +
    geom_vline(xintercept = m, colour = "grey20", linetype = "dashed") +
    labs(title = titulo,
         subtitle = paste0("Media = ", num(m), "  |  Mediana = ", num(md),
                           "  |  DE = ", num(sd(d$x))),
         x = NULL, y = "Densidad")

  p2 <- ggplot(d, aes(x)) +
    geom_boxplot(fill = AZUL, alpha = 0.5, outlier.alpha = 0.03,
                 outlier.size = 0.4, width = 0.6) +
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

# Diagrama de caja por grupos, con la media marcada en naranja
graf_box_grupo <- function(var_cat, var_num = "PUNT_GLOBAL", df = dataset,
                           titulo = var_cat) {
  d <- df |>
    dplyr::select(cat = tidyselect::all_of(var_cat),
                  y   = tidyselect::all_of(var_num)) |>
    dplyr::filter(!is.na(cat), !is.na(y))

  d$cat <- if (is.ordered(d$cat)) forcats::fct_rev(factor(d$cat, ordered = FALSE))
           else forcats::fct_reorder(d$cat, d$y, .fun = median)

  # Subtítulo corto: estos gráficos se combinan en paneles estrechos.
  ggplot(d, aes(x = cat, y = y)) +
    geom_boxplot(fill = AZUL, alpha = 0.55, outlier.alpha = 0.02,
                 outlier.size = 0.3, width = 0.65) +
    stat_summary(fun = mean, geom = "point", colour = NARANJA, size = 1.6) +
    coord_flip() +
    labs(title = titulo, subtitle = paste0("eta² = ", num(eta2(d$y, d$cat), 3)),
         x = NULL, y = var_num)
}

# Tabla de contingencia con porcentajes por fila
exportar_conting <- function(v1, v2, archivo, caption, df = dataset) {
  tb <- table(df[[v1]], df[[v2]])
  pr <- 100 * prop.table(tb, margin = 1)
  d  <- as.data.frame.matrix(pr)
  out <- data.frame(fila = tex_escape(rownames(d)))
  for (cl in names(d)) out[[cl]] <- celda(d[[cl]], 1, pct = TRUE)

  tex_longtable(
    out, archivo, caption, paste0("tab:", sub("^tbl-", "", archivo)),
    align     = paste(c("P{5.2cm}", rep("r", ncol(d))), collapse = " "),
    cabeceras = c(paste0("\\texttt{", tex_escape(v1), "}"), tex_escape(names(d))),
    nota      = sprintf("Porcentajes calculados por fila. $V$ de Cramér = %s.",
                        num(cramer_v(df[[v1]], df[[v2]]), 3))
  )
  d
}

# Barras apiladas al 100 %: la forma correcta de comparar composición cuando
# los grupos tienen tamaños muy dispares
graf_apilado <- function(v1, v2, df = dataset, titulo = NULL) {
  d <- df |>
    dplyr::select(a = tidyselect::all_of(v1), b = tidyselect::all_of(v2)) |>
    dplyr::filter(!is.na(a), !is.na(b))
  if (is.ordered(d$a)) d$a <- forcats::fct_rev(factor(d$a, ordered = FALSE))

  ggplot(d, aes(y = a, fill = b)) +
    geom_bar(position = "fill", width = 0.72) +
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    labs(title = titulo %||% paste0(v1, " x ", v2),
         x = "Composición (%)", y = NULL, fill = NULL) +
    theme(legend.position = "bottom", legend.key.size = unit(3.5, "mm"),
          legend.text = element_text(size = 7))
}
