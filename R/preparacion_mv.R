# Preparación adicional para el análisis multivariado
# -----------------------------------------------------------------------------
# Agrupa las categorías que el análisis exploratorio de la Entrega 2 señaló como
# problemáticas para las técnicas factoriales y deja definidos los bloques de
# variables que usan el ACM, el ACP y el modelo de dependencia.
# Lo carga reports/Entrega 3.qmd después de R/preparacion.R.
#
# Depende de que `dataset` ya exista. No modifica las columnas originales: añade
# las recodificadas, de modo que la Entrega 2 sigue reproduciéndose igual.

library(dplyr)
library(forcats)

# Los 33 departamentos dejan categorías de hasta el 0,05 % de los registros, que
# distorsionan el plano factorial y gastan 32 grados de libertad en el modelo.
# Se agrupan por región natural, siguiendo la clasificación del DANE, con Bogotá
# aparte porque por sí sola reúne el 15,4 % de los evaluados.
REGIONES <- c(
  "BOGOTÁ" = "Bogotá",

  "ANTIOQUIA" = "Andina", "CUNDINAMARCA" = "Andina", "SANTANDER" = "Andina",
  "NORTE SANTANDER" = "Andina", "BOYACA" = "Andina", "TOLIMA" = "Andina",
  "HUILA" = "Andina", "CALDAS" = "Andina", "RISARALDA" = "Andina",
  "QUINDIO" = "Andina",

  "ATLANTICO" = "Caribe", "BOLIVAR" = "Caribe", "CORDOBA" = "Caribe",
  "MAGDALENA" = "Caribe", "CESAR" = "Caribe", "SUCRE" = "Caribe",
  "LA GUAJIRA" = "Caribe", "SAN ANDRES" = "Caribe",

  "VALLE" = "Pacífica", "CAUCA" = "Pacífica", "NARIÑO" = "Pacífica",
  "CHOCO" = "Pacífica",

  "META" = "Orinoquía", "CASANARE" = "Orinoquía", "ARAUCA" = "Orinoquía",
  "VICHADA" = "Orinoquía",

  "CAQUETA" = "Amazonía", "PUTUMAYO" = "Amazonía", "GUAVIARE" = "Amazonía",
  "AMAZONAS" = "Amazonía", "VAUPES" = "Amazonía", "GUAINIA" = "Amazonía"
)

NIVELES_REGION <- c("Bogotá", "Andina", "Caribe", "Pacífica",
                    "Orinoquía", "Amazonía")

# Agrupación de la educación materna en cuatro niveles más la no respuesta. Es
# la misma que usa la Figura 2 del informe, de modo que los dos coinciden.
EDUC_MADRE_GRUPOS <- c(
  "Ninguno"                              = "Primaria o menos",
  "Primaria incompleta"                  = "Primaria o menos",
  "Primaria completa"                    = "Primaria o menos",
  "Secundaria (Bachillerato) incompleta" = "Secundaria",
  "Secundaria (Bachillerato) completa"   = "Secundaria",
  "Técnica o tecnológica incompleta"     = "Técnica o tecnológica",
  "Técnica o tecnológica completa"       = "Técnica o tecnológica",
  "Educación profesional incompleta"     = "Profesional o posgrado",
  "Educación profesional completa"       = "Profesional o posgrado",
  "Postgrado"                            = "Profesional o posgrado",
  "No sabe"                              = "No sabe o no aplica",
  "No Aplica"                            = "No sabe o no aplica"
)

NIVELES_EDUC_MADRE <- c("Primaria o menos", "Secundaria",
                        "Técnica o tecnológica", "Profesional o posgrado",
                        "No sabe o no aplica")

dataset <- dataset |>
  mutate(
    COLE_REGION = factor(unname(REGIONES[as.character(COLE_DEPTO_UBICACION)]),
                         levels = NIVELES_REGION),

    FAMI_EDUCMADRE_G = factor(
      unname(EDUC_MADRE_GRUPOS[as.character(FAMI_EDUCACIONMADRE)]),
      levels = NIVELES_EDUC_MADRE
    ),

    # El estrato 6 reúne el 0,62 % de los registros. Se funde con el 5, que es
    # su vecino en la escala y el único otro estrato por debajo del 2 %.
    FAMI_ESTRATO_G = fct_recode(
      factor(FAMI_ESTRATOVIVIENDA, ordered = FALSE),
      "Estrato 5 o 6" = "Estrato 5",
      "Estrato 5 o 6" = "Estrato 6"
    ),

    # La edad es muy asimétrica (8,26) y su relación con el puntaje no tiene por
    # qué ser lineal, así que entra por tramos en lugar de como término lineal.
    EDAD_TRAMO = cut(
      EDAD, c(-Inf, 16.99, 17.99, 18.99, 20.99, Inf), ordered_result = TRUE,
      labels = c("Hasta 16", "17 años", "18 años", "19 a 20", "21 o más")
    )
  )

# Bloques de variables del análisis multivariado ------------------------------

# Activas del ACM: las que describen la posición socioeconómica e institucional,
# que es la estructura que el análisis exploratorio mostró como dominante.
VARS_ACM <- c("FAMI_ESTRATO_G", "FAMI_EDUCMADRE_G", "FAMI_NUMLIBROS",
              "FAMI_TIENEINTERNET", "COLE_NATURALEZA", "COLE_JORNADA",
              "COLE_AREA_UBICACION", "COLE_REGION", "ESTU_NSE_ESTABLECIMIENTO")

# Suplementarias: no superan V = 0,20 con ninguna otra cualitativa, así que no
# definen esa estructura, pero sí se proyectan sobre ella para situarlas.
VARS_ACM_SUP <- c("ESTU_GENERO", "ESTU_DEDICACIONLECTURADIARIA",
                  "ESTU_HORASSEMANATRABAJA", "EDAD_TRAMO")

# Respuesta del ACP y del modelo de dependencia. PUNT_GLOBAL queda fuera: es un
# promedio ponderado de los cinco y no puede acompañarlos como respuesta.
VARS_PUNTAJES <- c("PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS",
                   "PUNT_C_NATURALES", "PUNT_SOCIALES_CIUDADANAS",
                   "PUNT_INGLES")

# Bloques explicativos del modelo de dependencia, para separar el aporte propio
# de cada dimensión del que comparte con las demás.
BLOQUES <- list(
  individual = c("ESTU_GENERO", "EDAD_TRAMO", "ESTU_DEDICACIONLECTURADIARIA",
                 "ESTU_HORASSEMANATRABAJA"),
  hogar      = c("FAMI_ESTRATO_G", "FAMI_EDUCMADRE_G", "FAMI_NUMLIBROS",
                 "FAMI_TIENEINTERNET"),
  colegio    = c("COLE_NATURALEZA", "COLE_JORNADA", "COLE_AREA_UBICACION"),
  geografia  = "COLE_REGION"
)

# Los dos índices del ICFES resumen lo que ya miden las variables del hogar y
# del colegio, así que van en un modelo aparte y no junto a ellas.
VARS_INDICES <- c("ESTU_INSE_INDIVIDUAL", "ESTU_NSE_ESTABLECIMIENTO")

rm(REGIONES, NIVELES_REGION, EDUC_MADRE_GRUPOS, NIVELES_EDUC_MADRE)
