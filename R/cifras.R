# Hoja de cifras
# -----------------------------------------------------------------------------
# Construye el texto de reports/latex_entrega_2/cifras.txt: todos los números
# que se citan en el documento, centralizados y con acceso rápido, para no
# teclear ningún porcentaje a ciegas.
# Lo carga reports/Entrega 2.qmd.
#
# Hecho con IA. La idea es centralizar y tener acceso rápido a los datos.
#
# Depende de: R/exportacion.R (num) y R/graficos.R (tabla_frec, asimetria).
# Los seis insumos que recibe se calculan antes en el .qmd:
#   res     resumen_cuant(vars_cuanti)          chunk tbl-resumen-cuant
#   M       matriz de correlaciones de Pearson  chunk fig-corr
#   ps      Pearson y Spearman con PUNT_GLOBAL  chunk tbl-pearson-spearman
#   pares   V de Cramér por pares               chunk tbl-cramer
#   ranking tamaños de efecto ordenados         chunk ranking
#   comp    los cinco componentes de la prueba  chunk fig-componentes
#
# Devuelve el vector de líneas; escribirlo en disco queda a la vista en el .qmd.

hoja_cifras <- function(res, M, ps, pares, ranking, comp, df = dataset) {

  # --- Consultas puntuales sobre la base ------------------------------------

  ef <- function(v) unname(100 * ranking$efecto[match(v, ranking$variable)])
  q_ <- function(v, p) unname(quantile(df[[v]], p, na.rm = TRUE))
  m_ <- function(v) mean(df[[v]], na.rm = TRUE)
  med_gr <- function(v, niv) {
    mean(df$PUNT_GLOBAL[!is.na(df[[v]]) & df[[v]] == niv])
  }
  pct_en <- function(v, niveles) {
    num(100 * mean(df[[v]] %in% niveles, na.rm = TRUE), 1)
  }

  # Porcentaje de una categoría sobre el total de registros. El denominador es
  # el mismo de las tablas de frecuencia, que cuentan los NA como categoría.
  pct <- function(var, nivel, dig = 1) {
    num(100 * sum(df[[var]] == nivel, na.rm = TRUE) / nrow(df), dig)
  }

  # Variante con denominador restringido a los registros con dato. Sólo importa
  # en COLE_BILINGUE, con un 16,3 % de faltantes.
  pct_valido <- function(var, nivel, dig = 1) {
    num(100 * mean(df[[var]] == nivel, na.rm = TRUE), dig)
  }

  # --- Cifras por sección del documento -------------------------------------

  CIFRAS <- list(
    "1.1.1 Demograficas" = c(
      "Genero femenino"               = pct("ESTU_GENERO", "F"),
      "Reconoce pertenencia etnica"   = pct("ESTU_TIENEETNIA", "Si"),
      "Sin dato de etnia (registros)" = num(sum(is.na(df$ESTU_TIENEETNIA)), 0)),

    "1.1.3 Hogar" = c(
      "Estratos 1 a 3" = pct_en("FAMI_ESTRATOVIVIENDA",
                                c("Estrato 1", "Estrato 2", "Estrato 3")),
      "Estratos 5 y 6" = pct_en("FAMI_ESTRATOVIVIENDA", c("Estrato 5", "Estrato 6"))),

    "1.1.7 Establecimiento" = c(
      "Colegio oficial"        = pct("COLE_NATURALEZA", "OFICIAL"),
      "Zona urbana"            = pct("COLE_AREA_UBICACION", "URBANO"),
      "Calendario B"           = pct("COLE_CALENDARIO", "B", 3),
      "Colegio masculino"      = pct("COLE_GENERO", "MASCULINO", 2),
      "Colegio bilingue (sobre el total)"     = pct("COLE_BILINGUE", "Si", 2),
      "Colegio bilingue (con dato)"           = pct_valido("COLE_BILINGUE", "Si", 2),
      "COLE_BILINGUE sin dato"                = num(100 * mean(is.na(df$COLE_BILINGUE)), 2)),

    "1.1.8 NSE del establecimiento" = c(
      "NSE 2 y NSE 3" = pct_en("ESTU_NSE_ESTABLECIMIENTO", c("NSE 2", "NSE 3")),
      "NSE 1"         = pct("ESTU_NSE_ESTABLECIMIENTO", "NSE 1"),
      "NSE 4"         = pct("ESTU_NSE_ESTABLECIMIENTO", "NSE 4")),

    "1.2 Cuantitativas" = c(
      "PUNT_GLOBAL media"          = num(m_("PUNT_GLOBAL"), 1),
      "PUNT_GLOBAL mediana"        = num(median(df$PUNT_GLOBAL), 1),
      "PUNT_GLOBAL Q1"             = num(q_("PUNT_GLOBAL", .25), 0),
      "PUNT_GLOBAL Q3"             = num(q_("PUNT_GLOBAL", .75), 0),
      "PUNT_GLOBAL RIC"            = num(IQR(df$PUNT_GLOBAL), 0),
      "PUNT_GLOBAL asimetria"      = num(asimetria(df$PUNT_GLOBAL), 2),
      "PUNT_INGLES asimetria"      = num(asimetria(df$PUNT_INGLES), 2),
      "CV mas alto (Sociales)"     = num(res$cv[res$variable == "PUNT_SOCIALES_CIUDADANAS"], 1),
      "CV mas bajo (Lectura)"      = num(res$cv[res$variable == "PUNT_LECTURA_CRITICA"], 1),
      "EDAD asimetria"             = num(asimetria(df$EDAD), 2),
      "EDAD media"                 = num(m_("EDAD"), 1),
      "EDAD mediana"               = num(median(df$EDAD, na.rm = TRUE), 1),
      "EDAD Q1"                    = num(q_("EDAD", .25), 1),
      "EDAD Q3"                    = num(q_("EDAD", .75), 1),
      "EDAD RIC"                   = num(IQR(df$EDAD, na.rm = TRUE), 1),
      "EDAD percentil 99"          = num(q_("EDAD", .99), 1),
      "EDAD maximo"                = num(max(df$EDAD, na.rm = TRUE), 1),
      "EDAD faltantes (registros)" = num(sum(is.na(df$EDAD)), 0),
      "INSE media"                 = num(m_("ESTU_INSE_INDIVIDUAL"), 2),
      "INSE asimetria"             = num(asimetria(df$ESTU_INSE_INDIVIDUAL), 2),
      "INSE CV"                    = num(res$cv[res$variable == "ESTU_INSE_INDIVIDUAL"], 1),
      "INSE valores distintos"     = num(dplyr::n_distinct(df$ESTU_INSE_INDIVIDUAL), 0)),

    "2.1 Cuanti-cuanti" = c(
      "r INSE - PUNT_GLOBAL"              = num(cor(df$ESTU_INSE_INDIVIDUAL, df$PUNT_GLOBAL), 3),
      "r2 INSE - PUNT_GLOBAL (%)"         = num(100 * cor(df$ESTU_INSE_INDIVIDUAL, df$PUNT_GLOBAL)^2, 1),
      "r INSE - LECTURA"                  = num(M["ESTU_INSE_INDIVIDUAL", "PUNT_LECTURA_CRITICA"], 2),
      "r INSE - MATEMATICAS"              = num(M["ESTU_INSE_INDIVIDUAL", "PUNT_MATEMATICAS"], 2),
      "r INSE - C_NATURALES"              = num(M["ESTU_INSE_INDIVIDUAL", "PUNT_C_NATURALES"], 2),
      "r INSE - SOCIALES"                 = num(M["ESTU_INSE_INDIVIDUAL", "PUNT_SOCIALES_CIUDADANAS"], 2),
      "r INSE - INGLES"                   = num(M["ESTU_INSE_INDIVIDUAL", "PUNT_INGLES"], 2),
      "r LECTURA - SOCIALES"              = num(M["PUNT_LECTURA_CRITICA", "PUNT_SOCIALES_CIUDADANAS"], 2),
      "r minimo entre componentes"        = num(min(M[comp, comp][upper.tri(M[comp, comp])]), 2),
      "r maximo entre componentes"        = num(max(M[comp, comp][upper.tri(M[comp, comp])]), 2),
      "r EDAD - PUNT_GLOBAL (Pearson)"    = num(ps$pearson[ps$variable == "EDAD"], 2),
      "rho EDAD - PUNT_GLOBAL (Spearman)" = num(ps$spearman[ps$variable == "EDAD"], 2)),

    "2.2 Cuali-cuanti (media de PUNT_GLOBAL)" = c(
      "Colegio NO OFICIAL" = num(med_gr("COLE_NATURALEZA", "NO OFICIAL"), 1),
      "Colegio OFICIAL"    = num(med_gr("COLE_NATURALEZA", "OFICIAL"), 1),
      "Zona URBANO"        = num(med_gr("COLE_AREA_UBICACION", "URBANO"), 1),
      "Zona RURAL"         = num(med_gr("COLE_AREA_UBICACION", "RURAL"), 1),
      "CON internet"       = num(med_gr("FAMI_TIENEINTERNET", "Si"), 1),
      "SIN internet"       = num(med_gr("FAMI_TIENEINTERNET", "No"), 1),
      "CON computador"     = num(med_gr("FAMI_TIENECOMPUTADOR", "Si"), 1),
      "SIN computador"     = num(med_gr("FAMI_TIENECOMPUTADOR", "No"), 1),
      "Genero F"           = num(med_gr("ESTU_GENERO", "F"), 1),
      "Genero M"           = num(med_gr("ESTU_GENERO", "M"), 1)),

    "2.2 Varianza explicada, eta cuadrado (%)" = c(
      "ESTU_NSE_ESTABLECIMIENTO"  = num(ef("ESTU_NSE_ESTABLECIMIENTO"), 2),
      "FAMI_EDUCACIONMADRE"       = num(ef("FAMI_EDUCACIONMADRE"), 2),
      "FAMI_EDUCACIONPADRE"       = num(ef("FAMI_EDUCACIONPADRE"), 2),
      "FAMI_NUMLIBROS"            = num(ef("FAMI_NUMLIBROS"), 2),
      "FAMI_ESTRATOVIVIENDA"      = num(ef("FAMI_ESTRATOVIVIENDA"), 2),
      "COLE_NATURALEZA"           = num(ef("COLE_NATURALEZA"), 2),
      "COLE_AREA_UBICACION"       = num(ef("COLE_AREA_UBICACION"), 2),
      "COLE_JORNADA"              = num(ef("COLE_JORNADA"), 2),
      "FAMI_TIENECOMPUTADOR"      = num(ef("FAMI_TIENECOMPUTADOR"), 2),
      "FAMI_TIENEINTERNET"        = num(ef("FAMI_TIENEINTERNET"), 2),
      "ESTU_GENERO"               = num(ef("ESTU_GENERO"), 2),
      "ESTU_DEDICACIONLECTURADIARIA" = num(ef("ESTU_DEDICACIONLECTURADIARIA"), 2),
      "ESTU_HORASSEMANATRABAJA"   = num(ef("ESTU_HORASSEMANATRABAJA"), 2),
      "COLE_DEPTO_UBICACION"      = num(ef("COLE_DEPTO_UBICACION"), 2),
      "ESTU_INSE_INDIVIDUAL (r2)" = num(ef("ESTU_INSE_INDIVIDUAL"), 2),
      "EDAD (r2)"                 = num(ef("EDAD"), 2)),

    "2.3 Cuali-cuali (V de Cramer)" = setNames(
      num(pares$V, 3), paste(pares$v1, "-", pares$v2)),

    "2.4 Bilinguismo" = c(
      "COLE_BILINGUE varianza explicada (%)" = num(ef("COLE_BILINGUE"), 4),
      "Bilingues: n"                         = num(sum(df$COLE_BILINGUE == "Si", na.rm = TRUE), 0),
      "Bilingues: media"                     = num(med_gr("COLE_BILINGUE", "Si"), 1),
      "No bilingues: media"                  = num(med_gr("COLE_BILINGUE", "No"), 1))
  )

  # Todos los niveles de las variables que se citan por categoría en el texto,
  # para no teclear ningún porcentaje a ciegas.
  detalle <- c("ESTU_DEDICACIONLECTURADIARIA", "ESTU_DEDICACIONINTERNET",
               "ESTU_HORASSEMANATRABAJA", "FAMI_ESTRATOVIVIENDA",
               "FAMI_PERSONASHOGAR", "FAMI_CUARTOSHOGAR", "FAMI_SITUACIONECONOMICA",
               "FAMI_EDUCACIONMADRE", "FAMI_EDUCACIONPADRE", "FAMI_TRABAJOLABORMADRE",
               "FAMI_TRABAJOLABORPADRE", "FAMI_NUMLIBROS", "FAMI_COMELECHEDERIVADOS",
               "COLE_JORNADA", "COLE_DEPTO_UBICACION")

  for (v in detalle) {
    tb <- tabla_frec(v, df)
    CIFRAS[[paste("Niveles de", v, "(%)")]] <- setNames(num(tb$pct, 2), tb$nivel)
  }

  bienes_ord <- sort(
    vapply(grep("^FAMI_TIENE", names(df), value = TRUE),
           \(v) 100 * mean(df[[v]] == "Si", na.rm = TRUE), numeric(1)),
    decreasing = TRUE)
  CIFRAS[["Tenencia de bienes (% que SI lo tiene)"]] <-
    setNames(num(bienes_ord, 1), names(bienes_ord))

  CIFRAS[["Media de PUNT_GLOBAL por estrato"]] <- setNames(
    num(vapply(levels(df$FAMI_ESTRATOVIVIENDA),
               \(l) med_gr("FAMI_ESTRATOVIVIENDA", l), numeric(1)), 1),
    levels(df$FAMI_ESTRATOVIVIENDA))

  CIFRAS[["Media por componente y genero"]] <- setNames(
    num(unlist(lapply(comp, \(p) c(
      mean(df[[p]][df$ESTU_GENERO == "F"], na.rm = TRUE),
      mean(df[[p]][df$ESTU_GENERO == "M"], na.rm = TRUE)))), 1),
    as.vector(t(outer(comp, c("F", "M"), paste))))

  # --- Composición del archivo ----------------------------------------------

  lineas <- c(
    "HOJA DE CIFRAS - Entrega 2, analisis descriptivo",
    "Generado por R/cifras.R desde reports/Entrega 2.qmd. No editar a mano.",
    paste("Base:", nrow(df), "registros x", ncol(df), "columnas."),
    paste("Generado el", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    "Convencion espanola: coma decimal, punto de miles.",
    "Los porcentajes se reportan SIN el simbolo %.",
    strrep("=", 78))

  for (sec in names(CIFRAS)) {
    lineas <- c(lineas, "", sec, strrep("-", nchar(sec)))
    v <- CIFRAS[[sec]]
    lineas <- c(lineas, sprintf("  %-46s %s", names(v), unname(v)))
  }

  lineas
}
