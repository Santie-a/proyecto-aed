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

FIGURAS_DOC <- c("fig-bienes", "fig-capital-educ", "fig-global", "fig-corr",
                 "fig-hexbin", "fig-box-clave", "fig-apilado-estrato",
                 "fig-ranking")

TABLAS_DOC <- c("tab-perfil", "tab-hogar", "tab-capital", "tab-colegio",
                "tbl-resumen-cuant", "tbl-conting-1", "tbl-cramer")

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
                          grupos = NULL, tamano = "\\scriptsize\\setstretch{1}",
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
      if (!is.null(anterior)) filas <- c(filas, "  \\addlinespace")
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
      L <- c(L, sprintf("\\vspace{-0.8em}\\noindent{\\footnotesize %s\\par}", nota))
    }
  } else {
    # Caja indivisible en el flujo del texto. No es un flotante: un flotante se
    # desplaza y puede acabar por encima del título de la sección que lo
    # introduce. El minipage mantiene juntas leyenda, tabla y nota.
    L <- c(L,
      sprintf("\\begingroup%s", tamano),
      "\\par\\medskip",
      "\\noindent\\begin{minipage}{\\textwidth}",
      "\\centering",
      sprintf("\\captionof{table}{%s}\\label{%s}", caption, label),
      sprintf("\\begin{tabular}{%s}", align),
      "  \\toprule", paste0("  ", cab), "  \\midrule", "",
      filas,
      "  \\bottomrule", "\\end{tabular}")
    if (!is.null(nota)) {
      L <- c(L, sprintf("\\par\\vspace{0.4em}\\raggedright{\\footnotesize %s\\par}", nota))
    }
    L <- c(L, "\\end{minipage}", "\\par\\medskip", "\\endgroup")
  }
  escribir_utf8(L, file.path(DIR_TAB, paste0(archivo, ".tex")))
  invisible(df)
}
