# Exportación de artefactos al documento LaTeX
# -----------------------------------------------------------------------------
# Rutas de salida, formato numérico y emisión de fragmentos .tex que consume
# reports/latex_entrega_2/main.tex. Lo carga reports/Entrega 2.qmd.
#
# Depende de: here. No depende de `dataset`.
#
# Esta parte fue hecha con IA. La idea es automatizar un flujo de tablas y
# figuras para que el manejo de las tablas y figuras en el .tex fuera mucho mas
# sencillo, evitando tomar screenshots que le bajan la calidad de entrega al
# documento, y unificar el formato.

library(here)

DIR_TEX <- here::here("reports", "latex_entrega_2")
DIR_FIG <- file.path(DIR_TEX, "figuras")
DIR_TAB <- file.path(DIR_TEX, "tablas")
for (d in c(DIR_FIG, DIR_TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Ancho de texto real del documento: carta con márgenes de 1 pulgada.
# Exportar a esta medida hace que la letra de las figuras salga al tamaño previsto.
ANCHO <- 6.5

FIGURAS_DOC <- c("fig-ordinales", "fig-global", "fig-componentes-inse",
                 "fig-corr-inse", "fig-box-clave", "fig-cramer-nse")

TABLAS_DOC <- c("tab-frec-clave", "tbl-resumen-cuant", "tab-asociacion")

options(scipen = 999)

exportar_fig <- function(p, nombre, alto = 3.2, ancho = ANCHO, png = FALSE) {
  if (!nombre %in% FIGURAS_DOC) return(p)   # sólo se muestra, no se exporta
  if (png) {
    ggsave(file.path(DIR_FIG, paste0(nombre, ".png")), p,
           width = ancho, height = alto, dpi = 300)
  } else {
    ggsave(file.path(DIR_FIG, paste0(nombre, ".pdf")), p,
           width = ancho, height = alto, device = cairo_pdf)
  }
  p
}

escribir_utf8 <- function(lineas, ruta) {
  con <- file(ruta, open = "wb")
  writeLines(enc2utf8(lineas), con, useBytes = TRUE)
  close(con)
}

# --- Formato numérico -------------------------------------------------------

# Formato numérico en convención española (coma decimal, punto de miles)
num <- function(x, dig = 1) {
  format(round(x, dig), decimal.mark = ",", big.mark = ".",
         nsmall = dig, trim = TRUE)
}

# --- Emisión de tablas ------------------------------------------------------
#
# Las tablas se emiten como fragmentos `longtable` con `booktabs`, replicando el
# idioma de la Tabla 1 del documento metodológico. Se evita a propósito
# `kableExtra`: su salida arrastra una decena de paquetes LaTeX que chocarían
# con la configuración de `titlesec` y `caption` ya fijada en el documento.

tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

# Celda numérica en convención española; los faltantes se marcan con una raya.
celda <- function(x, dig = 2, pct = FALSE) {
  s <- ifelse(is.na(x), "---", num(x, dig))
  if (pct) s <- ifelse(is.na(x), "---", paste0(s, "\\,\\%"))
  s
}

# Emite un fragmento de tabla. `grupos` inserta una fila de cabecera
# \multicolumn antes de cada bloque de filas, igual que la Tabla 1.
# \setstretch{1} anula dentro de la tabla el interlineado de 1,15 del documento:
# con 265 filas repartidas en once tablas, eso solo ahorra algo más de una página.
#
# Sólo se usa longtable cuando la tabla realmente necesita partirse entre
# páginas. Una longtable calcula dónde cortar antes de que la rutina de salida
# haya colocado los flotantes de la página, y en una página que además recibe
# una figura el resultado se desborda por el pie. Un tabular centrado no tiene
# ese problema: es una caja indivisible que el salto de página respeta.
FILAS_LONGTABLE <- 20

tex_longtable <- function(df, archivo, caption, label, align, cabeceras,
                          grupos = NULL, tamano = "\\tiny\\setstretch{1}",
                          nota = NULL) {
  if (!archivo %in% TABLAS_DOC) return(invisible(df))
  k    <- ncol(df)
  cab  <- paste0(paste0("\\textbf{", cabeceras, "}", collapse = " & "), " \\\\")
  cont <- sprintf("\\multicolumn{%d}{r}{\\textit{Continúa en la página siguiente...}} \\\\", k)
  larga <- nrow(df) > FILAS_LONGTABLE

  filas    <- character(0)
  anterior <- NULL
  for (i in seq_len(nrow(df))) {
    if (!is.null(grupos) && (is.null(anterior) || grupos[i] != anterior)) {
      filas <- c(filas,
                 sprintf("  \\multicolumn{%d}{l}{\\textit{%s}} \\\\", k, grupos[i]))
      anterior <- grupos[i]
    }
    filas <- c(filas, paste0("  ", paste(unlist(df[i, ]), collapse = " & "), " \\\\"))
  }

  # El contenido de un flotante se compone más tarde, así que los ajustes de
  # tamaño tienen que ir dentro del entorno y no envolviéndolo.
  L <- "% Generado por R/exportacion.R desde reports/Entrega 2.qmd"

  if (larga) {
    L <- c(L,
      sprintf("\\begingroup%s", tamano),
      sprintf("\\begin{longtable}{%s}", align),
      sprintf("  \\caption{%s}\\label{%s}\\\\", caption, label),
      "  \\toprule", paste0("  ", cab), "  \\midrule", "  \\endfirsthead",
      "", "  \\midrule", paste0("  ", cont), "  \\midrule",
      paste0("  ", cab), "  \\midrule", "  \\endhead",
      "", "  \\midrule", paste0("  ", cont), "  \\endfoot",
      "", "  \\bottomrule", "  \\endlastfoot", "",
      filas, "\\end{longtable}", "\\endgroup")
    if (!is.null(nota)) {
      L <- c(L, sprintf("\\noindent{\\footnotesize %s\\par}", nota))
    }
  } else {
    # Caja indivisible en el flujo del texto. No es un flotante: un flotante se
    # desplaza y puede acabar por encima del título de la sección que lo
    # introduce. El minipage mantiene juntas leyenda, tabla y nota.
    L <- c(L,
      sprintf("\\begingroup%s", tamano),
      "\\par",
      "\\noindent\\begin{minipage}{\\textwidth}",
      "\\centering",
      sprintf("\\captionof{table}{%s}\\label{%s}", caption, label),
      sprintf("\\begin{tabular}{%s}", align),
      "  \\toprule", paste0("  ", cab), "  \\midrule", "",
      filas,
      "  \\bottomrule", "\\end{tabular}")
    if (!is.null(nota)) {
      L <- c(L, sprintf("\\par\\raggedright{\\footnotesize %s\\par}", nota))
    }
    L <- c(L, "\\end{minipage}", "\\par", "\\endgroup")
  }
  escribir_utf8(L, file.path(DIR_TAB, paste0(archivo, ".tex")))
  invisible(df)
}

# Tabla de frecuencias en dos bloques lado a lado. `izq` y `der` son data
# frames con las columnas grupo, nivel, n y pct ya formateadas; cada grupo
# abre con su propia fila de cabecera. El bloque más corto se rellena con
# filas vacías para que ambos terminen a la misma altura.
tex_tabla_doble <- function(izq, der, archivo, caption, label,
                            titulos = c("Bloque 1", "Bloque 2"),
                            tamano = "\\tiny\\setstretch{1}",
                            nota = NULL) {
  if (!archivo %in% TABLAS_DOC) return(invisible(NULL))

  lado <- function(d) {
    out <- character(0)
    for (g in unique(d$grupo)) {
      out <- c(out, sprintf("\\multicolumn{3}{l}{\\textit{%s}}", g))
      s <- d[d$grupo == g, ]
      out <- c(out, paste(s$nivel, s$n, s$pct, sep = " & "))
    }
    out
  }
  a <- lado(izq)
  b <- lado(der)
  k <- max(length(a), length(b))
  a <- c(a, rep(" & & ", k - length(a)))
  b <- c(b, rep(" & & ", k - length(b)))
  filas <- paste0("  ", a, " & ", b, " \\\\")

  cab <- "\\textbf{Categoría} & \\textbf{n} & \\textbf{\\%}"
  L <- c("% Generado por R/exportacion.R desde reports/Entrega 2.qmd",
    sprintf("\\begingroup%s", tamano),
    "\\par",
    "\\noindent\\begin{minipage}{\\textwidth}",
    "\\centering",
    sprintf("\\captionof{table}{%s}\\label{%s}", caption, label),
    "\\begin{tabular}{l r r @{\\hspace{2.5em}} l r r}",
    "  \\toprule",
    sprintf("  \\multicolumn{3}{c}{\\textbf{%s}} & \\multicolumn{3}{c}{\\textbf{%s}} \\\\",
            titulos[1], titulos[2]),
    "  \\cmidrule(r{2.5em}){1-3}\\cmidrule{4-6}",
    sprintf("  %s & %s \\\\", cab, cab),
    "  \\midrule",
    filas,
    "  \\bottomrule",
    "\\end{tabular}")
  if (!is.null(nota)) {
    L <- c(L, sprintf("\\par\\raggedright{\\footnotesize %s\\par}", nota))
  }
  L <- c(L, "\\end{minipage}", "\\par", "\\endgroup")
  escribir_utf8(L, file.path(DIR_TAB, paste0(archivo, ".tex")))
  invisible(NULL)
}
