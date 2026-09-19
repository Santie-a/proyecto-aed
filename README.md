# Saber 11 - 2020-2

Análisis de los resultados de las pruebas Saber 11 del segundo semestre de 2020
(ICFES). Fuente: [datos.gov.co](https://www.datos.gov.co/Educaci-n/Saber-11-2020-2/rnvb-vnyh).

## Requisitos

- R (versión reciente) y [RStudio](https://posit.co/download/rstudio-desktop/)
- Git

## Instalación

```bash
git clone https://github.com/Santie-a/proyecto-aed.git
cd proyecto-aed
```

Se puede inicializar abriendo el archivo `.Rproj`. Las rutas dependen de que la sesión arranque en la raíz.

En la consola de RStudio:

```r
renv::restore()
```

Esto reconstruye el entorno exacto de paquetes a partir de `renv.lock`.

## Preparación de los datos

Los datos (Parquet) están en el repositorio. Sin embargo es posible obtenerlos de la siguiente manera:

```r
source(here::here("R", "download.R"))    # descarga el CSV (~500 MB)
source(here::here("R", "to_parquet.R"))  # convierte a Parquet (~25 MB)
```

## Documentos

En la carpeta de [reportes](reports) están los .qmd y HTML generados correspondientes a las entregas donde se encuentra
la parte de `R`. En este mismo folder están los LaTeX separados por carpeta:

- [Entrega 1](reports/latex_entrega_1/)
- [Entrega 2](reports/latex_entrega_2/)

## Scripts de R

El código que no hace parte de la exposición de las entregas vive en [`R/`](R), y los
`.qmd` lo cargan con `source()`:

- `preparacion.R` - selección de columnas, filtros de validez, recodificación a factores
  y creación de `EDAD`. Deja en el entorno `dataset`, `vars_cuali` y `vars_cuanti`.
- `exportacion.R` - rutas de salida, formato numérico y emisión de los fragmentos `.tex`
  (tablas) y `.pdf`/`.png` (figuras) que consume el LaTeX.
- `graficos.R` - tema visual y funciones reutilizables de tabla y gráfico, univariadas y
  bivariadas. Depende de `exportacion.R`.
- `download.R` y `to_parquet.R` — obtención y conversión de la base.
