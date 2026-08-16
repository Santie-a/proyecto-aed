# ==============================================================================
# explorar_dataset.R
#
# Ejemplo mínimo de lectura y consulta del dataset Saber 11 2020-2.
#
# Requisitos previos (ejecutar una sola vez, en orden):
#   source(here::here("R", "00_download.R"))    # baja el CSV (~500 MB)
#   source(here::here("R", "01_to_parquet.R"))  # convierte a Parquet
#
# Punto clave: NUNCA cargamos el dataset completo a memoria. Trabajamos sobre
# el Parquet de forma perezosa (lazy) y solo traemos a R el resultado final.
# ==============================================================================

library(here)
library(arrow)
library(dplyr)

ruta_pq <- here::here("data", "processed", "saber11_2020_2.parquet")

if (!file.exists(ruta_pq)) {
  stop(
    "No se encontro el Parquet en:\n  ", ruta_pq,
    "\nESe debe descargar la base de datos primero con download.R y luego procesarla con to_parquet.R",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 1. Abrir el dataset
# ------------------------------------------------------------------------------
# open_dataset() NO lee los datos: solo lee los metadatos del archivo.
# Es instantaneo, sin importar si el archivo pesa 50 MB o 50 GB.

ds <- arrow::open_dataset(ruta_pq)

cat("\n===== DIMENSIONES =====\n")
cat("Filas:    ", format(nrow(ds), big.mark = "."), "\n")
cat("Columnas: ", length(names(ds)), "\n")
cat("Tamano en disco:", round(file.size(ruta_pq) / 1e6), "MB\n")


# ------------------------------------------------------------------------------
# 2. Esquema: nombres y tipos de columna
# ------------------------------------------------------------------------------
# Esto es lo primero que hay que mirar. Si una columna de puntaje aparece como
# <string> en vez de <int32>/<double>, la ingesta se hizo mal y hay que volver
# a 01_to_parquet.R antes de seguir.

cat("\n===== ESQUEMA (primeras 20 columnas) =====\n")
print(head(ds$schema, 20))

cat("\n===== COLUMNAS DE PUNTAJE =====\n")
cols_punt <- grep("^PUNT_", names(ds), value = TRUE)
print(ds$schema[cols_punt])


# ------------------------------------------------------------------------------
# 3. Head del dataset
# ------------------------------------------------------------------------------
# Con 81 columnas, un head() completo es ilegible en consola. Seleccionamos
# un subconjunto representativo. Ojo: collect() es lo que dispara la lectura.

cat("\n===== HEAD (10 filas, columnas seleccionadas) =====\n")

vista <- ds |>
  select(
    ESTU_CONSECUTIVO,
    ESTU_DEPTO_RESIDE,
    ESTU_MCPIO_RESIDE,
    FAMI_ESTRATOVIVIENDA,
    COLE_NATURALEZA,
    COLE_AREA_UBICACION,
    PUNT_MATEMATICAS,
    PUNT_LECTURA_CRITICA,
    PUNT_INGLES,
    PUNT_GLOBAL
  ) |>
  head(10) |>
  collect()

print(as.data.frame(vista))

# Alternativa mas compacta para ver TODAS las columnas de un golpe:
# glimpse() sobre un Dataset solo trae unas pocas filas de muestra.
cat("\n===== GLIMPSE (todas las columnas) =====\n")
glimpse(ds)


# ------------------------------------------------------------------------------
# 4. Consulta agregada: puntaje global promedio por departamento
# ------------------------------------------------------------------------------
# Toda la cadena select/filter/group_by/summarise se traduce a operaciones de
# Arrow y se ejecuta sobre el archivo. A R solo llegan ~33 filas (una por
# departamento), no las 500.000.

cat("\n===== PROMEDIO DE PUNTAJE GLOBAL POR DEPARTAMENTO =====\n")

t0 <- Sys.time()

resumen <- ds |>
  select(ESTU_DEPTO_RESIDE, PUNT_GLOBAL) |>
  filter(!is.na(PUNT_GLOBAL), !is.na(ESTU_DEPTO_RESIDE)) |>
  group_by(ESTU_DEPTO_RESIDE) |>
  summarise(
    n     = n(),
    media = mean(PUNT_GLOBAL),
    sd    = sd(PUNT_GLOBAL)
  ) |>
  collect() |>                       # <- aqui se materializa en R
  arrange(desc(media))               # <- ordenamos ya en memoria

print(as.data.frame(resumen), digits = 4)

cat("\nTiempo de la consulta:",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "segundos\n")


# ------------------------------------------------------------------------------
# 5. Ejemplo con mas de un agrupador
# ------------------------------------------------------------------------------
# Mismo patron: filtrar y agregar antes de collect().

cat("\n===== BRECHA OFICIAL / NO OFICIAL POR ZONA =====\n")

brecha <- ds |>
  select(COLE_NATURALEZA, COLE_AREA_UBICACION, PUNT_GLOBAL) |>
  filter(!is.na(PUNT_GLOBAL), !is.na(COLE_NATURALEZA)) |>
  group_by(COLE_NATURALEZA, COLE_AREA_UBICACION) |>
  summarise(n = n(), media = mean(PUNT_GLOBAL)) |>
  collect() |>
  arrange(COLE_NATURALEZA, COLE_AREA_UBICACION)

print(as.data.frame(brecha), digits = 4)


# ------------------------------------------------------------------------------
# 6. Lo que NO hay que hacer
# ------------------------------------------------------------------------------
# El siguiente patron trae las 500.000 filas x 81 columnas a memoria (2-3 GB)
# antes de filtrar. Funciona, pero desperdicia RAM y tiempo. Dejado aqui como
# contraejemplo, deliberadamente comentado:
#
#   datos <- collect(ds)                    # <- MAL: materializa todo
#   datos |> group_by(ESTU_DEPTO_RESIDE) |> summarise(media = mean(PUNT_GLOBAL))
#
# Regla practica: collect() va SIEMPRE al final de la cadena, nunca al inicio.


# ------------------------------------------------------------------------------
# 7. Nota sobre nombres de columna irregulares
# ------------------------------------------------------------------------------
# La columna ESTU_GENERACION-E tiene un guion, asi que necesita backticks:
#
#   ds |> count(`ESTU_GENERACION-E`) |> collect()
#
# En el script de limpieza normalizaremos los nombres con janitor::clean_names().

cat("\nListo.\n")