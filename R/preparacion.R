library(here)
library(arrow)
library(dplyr)
library(tidyr)
library(forcats)

ds <- arrow::open_dataset(here::here("data", "processed", "saber11_2020_2.parquet"))

dataset <- ds |>
  select(
    # Demográficas
    ESTU_GENERO,
    ESTU_FECHANACIMIENTO,

    # Hábitos y dedicación
    ESTU_DEDICACIONLECTURADIARIA,
    ESTU_HORASSEMANATRABAJA,

    # Condiciones del hogar
    FAMI_ESTRATOVIVIENDA,

    # Capital educativo y cultural del hogar
    FAMI_EDUCACIONMADRE,
    FAMI_NUMLIBROS,

    # Acceso tecnológico
    FAMI_TIENEINTERNET,

    # Establecimiento
    COLE_NATURALEZA,
    COLE_JORNADA,
    COLE_AREA_UBICACION,
    COLE_DEPTO_UBICACION,

    # Índices de referencia
    ESTU_INSE_INDIVIDUAL,
    ESTU_NSE_ESTABLECIMIENTO,

    # Desempeño
    PUNT_LECTURA_CRITICA,
    PUNT_MATEMATICAS,
    PUNT_C_NATURALES,
    PUNT_SOCIALES_CIUDADANAS,
    PUNT_INGLES,
    PUNT_GLOBAL,

    # Criterio de filtro
    ESTU_ESTADOINVESTIGACION
  ) |>
  collect()

dataset <- dataset |>
  filter(ESTU_ESTADOINVESTIGACION == "PUBLICAR") |>
  select(-ESTU_ESTADOINVESTIGACION)

vars_encuesta <- dataset |>
  select(starts_with("FAMI_"), starts_with("ESTU_DEDICACION"),
         ESTU_HORASSEMANATRABAJA) |>
  names()

dataset <- dataset |>
  filter(rowSums(is.na(pick(all_of(vars_encuesta)))) == 0)

dataset <- dataset |>
  mutate(
    FAMI_TIENEINTERNET = factor(FAMI_TIENEINTERNET, levels = c("No", "Si")),

    ESTU_GENERO      = factor(ESTU_GENERO, levels = c("F", "M")),
    COLE_NATURALEZA  = factor(COLE_NATURALEZA, levels = c("OFICIAL", "NO OFICIAL")),
    COLE_AREA_UBICACION = factor(COLE_AREA_UBICACION, levels = c("URBANO", "RURAL")),
    COLE_JORNADA     = factor(COLE_JORNADA,
                              levels = c("MAÑANA", "TARDE", "NOCHE",
                                         "UNICA", "COMPLETA", "SABATINA")),
    COLE_DEPTO_UBICACION = factor(COLE_DEPTO_UBICACION),

    ESTU_DEDICACIONLECTURADIARIA = factor(
      ESTU_DEDICACIONLECTURADIARIA, ordered = TRUE,
      levels = c("No leo por entretenimiento", "30 minutos o menos",
                 "Entre 30 y 60 minutos", "Entre 1 y 2 horas",
                 "Más de 2 horas")
    ),
    ESTU_HORASSEMANATRABAJA = factor(
      ESTU_HORASSEMANATRABAJA, ordered = TRUE,
      levels = c("0", "Menos de 10 horas", "Entre 11 y 20 horas",
                 "Entre 21 y 30 horas", "Más de 30 horas")
    ),

    FAMI_ESTRATOVIVIENDA = factor(
      FAMI_ESTRATOVIVIENDA, ordered = TRUE,
      levels = c("Sin Estrato", paste("Estrato", 1:6))
    ),
    FAMI_NUMLIBROS = factor(
      FAMI_NUMLIBROS, ordered = TRUE,
      levels = c("0 A 10 LIBROS", "11 A 25 LIBROS",
                 "26 A 100 LIBROS", "MÁS DE 100 LIBROS")
    ),

    FAMI_EDUCACIONMADRE = factor(
      FAMI_EDUCACIONMADRE, levels = c(
          "Ninguno",
          "Primaria incompleta", "Primaria completa",
          "Secundaria (Bachillerato) incompleta",
          "Secundaria (Bachillerato) completa",
          "Técnica o tecnológica incompleta", "Técnica o tecnológica completa",
          "Educación profesional incompleta", "Educación profesional completa",
          "Postgrado",
          "No sabe", "No Aplica")
    ),

    ESTU_NSE_ESTABLECIMIENTO = factor(
      ESTU_NSE_ESTABLECIMIENTO, ordered = TRUE,
      levels = 1:4, labels = paste("NSE", 1:4)
    )
  )

FECHA_PRUEBA <- as.Date("2020-11-07")

dataset <- dataset |>
  mutate(
    EDAD = as.numeric(
      difftime(FECHA_PRUEBA,
               as.Date(ESTU_FECHANACIMIENTO, tz = "America/Bogota"),
               units = "days")
    ) / 365.25,
    EDAD = if_else(EDAD < 10 | EDAD > 100, NA_real_, EDAD)
  ) |>
  relocate(EDAD, .after = ESTU_FECHANACIMIENTO)

vars_cuanti <- c("EDAD", "ESTU_INSE_INDIVIDUAL",
                 "PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS", "PUNT_C_NATURALES",
                 "PUNT_SOCIALES_CIUDADANAS", "PUNT_INGLES", "PUNT_GLOBAL")

vars_cuali <- setdiff(names(dataset), c(vars_cuanti, "ESTU_FECHANACIMIENTO"))

rm(ds, vars_encuesta, FECHA_PRUEBA)
