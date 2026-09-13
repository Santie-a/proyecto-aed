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
    ESTU_TIENEETNIA,

    # Hábitos y dedicación
    ESTU_DEDICACIONLECTURADIARIA,
    ESTU_DEDICACIONINTERNET,
    ESTU_HORASSEMANATRABAJA,

    # Condiciones del hogar
    FAMI_ESTRATOVIVIENDA,
    FAMI_PERSONASHOGAR,
    FAMI_CUARTOSHOGAR,
    FAMI_SITUACIONECONOMICA,

    # Capital educativo y ocupacional
    FAMI_EDUCACIONPADRE,
    FAMI_EDUCACIONMADRE,
    FAMI_TRABAJOLABORPADRE,
    FAMI_TRABAJOLABORMADRE,
    FAMI_NUMLIBROS,

    # Bienes y acceso tecnológico
    FAMI_TIENEINTERNET,
    FAMI_TIENECOMPUTADOR,
    FAMI_TIENEAUTOMOVIL,
    FAMI_TIENEMOTOCICLETA,
    FAMI_TIENELAVADORA,
    FAMI_TIENEHORNOMICROOGAS,
    FAMI_TIENECONSOLAVIDEOJUEGOS,

    # Seguridad alimentaria
    FAMI_COMELECHEDERIVADOS,
    FAMI_COMECARNEPESCADOHUEVO,
    FAMI_COMECEREALFRUTOSLEGUMBRE,

    # Establecimiento
    COLE_NATURALEZA,
    COLE_CALENDARIO,
    COLE_GENERO,
    COLE_BILINGUE,
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
    across(
      c(ESTU_TIENEETNIA, FAMI_TIENEINTERNET, FAMI_TIENECOMPUTADOR,
        FAMI_TIENEAUTOMOVIL, FAMI_TIENEMOTOCICLETA, FAMI_TIENELAVADORA,
        FAMI_TIENEHORNOMICROOGAS, FAMI_TIENECONSOLAVIDEOJUEGOS),
      ~ factor(.x, levels = c("No", "Si"))
    ),
    COLE_BILINGUE = factor(COLE_BILINGUE, levels = c("N", "S"),
                           labels = c("No", "Si")),

    ESTU_GENERO      = factor(ESTU_GENERO, levels = c("F", "M")),
    COLE_NATURALEZA  = factor(COLE_NATURALEZA, levels = c("OFICIAL", "NO OFICIAL")),
    COLE_AREA_UBICACION = factor(COLE_AREA_UBICACION, levels = c("URBANO", "RURAL")),
    COLE_CALENDARIO  = factor(COLE_CALENDARIO, levels = c("A", "B", "OTRO")),
    COLE_GENERO      = factor(COLE_GENERO, levels = c("MIXTO", "FEMENINO", "MASCULINO")),
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
    ESTU_DEDICACIONINTERNET = factor(
      ESTU_DEDICACIONINTERNET, ordered = TRUE,
      levels = c("No Navega Internet", "30 minutos o menos",
                 "Entre 30 y 60 minutos", "Entre 1 y 3 horas",
                 "Más de 3 horas")
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
    FAMI_PERSONASHOGAR = factor(
      FAMI_PERSONASHOGAR, ordered = TRUE,
      levels = c("1 a 2", "3 a 4", "5 a 6", "7 a 8", "9 o más")
    ),
    FAMI_CUARTOSHOGAR = factor(
      FAMI_CUARTOSHOGAR, ordered = TRUE,
      levels = c("Uno", "Dos", "Tres", "Cuatro", "Cinco", "Seis o mas")
    ),
    FAMI_SITUACIONECONOMICA = factor(
      FAMI_SITUACIONECONOMICA, ordered = TRUE,
      levels = c("Peor", "Igual", "Mejor")
    ),
    FAMI_NUMLIBROS = factor(
      FAMI_NUMLIBROS, ordered = TRUE,
      levels = c("0 A 10 LIBROS", "11 A 25 LIBROS",
                 "26 A 100 LIBROS", "MÁS DE 100 LIBROS")
    ),

    across(
      c(FAMI_COMELECHEDERIVADOS, FAMI_COMECARNEPESCADOHUEVO,
        FAMI_COMECEREALFRUTOSLEGUMBRE),
      ~ factor(.x, ordered = TRUE,
               levels = c("Nunca o rara vez comemos eso",
                          "1 o 2 veces por semana",
                          "3 a 5 veces por semana",
                          "Todos o casi todos los días"))
    ),

    across(
      c(FAMI_EDUCACIONPADRE, FAMI_EDUCACIONMADRE),
      ~ factor(.x, levels = c(
          "Ninguno",
          "Primaria incompleta", "Primaria completa",
          "Secundaria (Bachillerato) incompleta",
          "Secundaria (Bachillerato) completa",
          "Técnica o tecnológica incompleta", "Técnica o tecnológica completa",
          "Educación profesional incompleta", "Educación profesional completa",
          "Postgrado",
          "No sabe", "No Aplica"))
    ),

    across(
      c(FAMI_TRABAJOLABORPADRE, FAMI_TRABAJOLABORMADRE),
      ~ fct_recode(factor(.x),
          "Negocio pequeño"   = "Es dueño de un negocio pequeño (tiene pocos empleados o no tiene, por ejemplo tienda, papelería, etc",
          "Agrícola"          = "Es agricultor, pesquero o jornalero",
          "Cuenta propia"     = "Trabaja por cuenta propia (por ejemplo plomero, electricista)",
          "Operario"          = "Es operario de máquinas o conduce vehículos (taxita, chofer)",
          "Profesional"       = "Trabaja como profesional (por ejemplo médico, abogado, ingeniero)",
          "Servicios"         = "Trabaja como personal de limpieza, mantenimiento, seguridad o construcción",
          "Hogar / no trabaja"= "Trabaja en el hogar, no trabaja o estudia",
          "Ventas"            = "Es vendedor o trabaja en atención al público",
          "Auxiliar admin."   = "Tiene un trabajo de tipo auxiliar administrativo (por ejemplo, secretario o asistente)",
          "Directivo"         = "Es dueño de un negocio grande, tiene un cargo de nivel directivo o gerencial",
          "Pensionado"        = "Pensionado",
          "No sabe"           = "No sabe")
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
    EDAD = if_else(EDAD < 7 | EDAD > 100, NA_real_, EDAD)
  ) |>
  relocate(EDAD, .after = ESTU_FECHANACIMIENTO)

vars_cuanti <- c("EDAD", "ESTU_INSE_INDIVIDUAL",
                 "PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS", "PUNT_C_NATURALES",
                 "PUNT_SOCIALES_CIUDADANAS", "PUNT_INGLES", "PUNT_GLOBAL")

vars_cuali <- setdiff(names(dataset), c(vars_cuanti, "ESTU_FECHANACIMIENTO"))

rm(ds, vars_encuesta, FECHA_PRUEBA)
