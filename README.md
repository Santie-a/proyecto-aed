# Saber 11 — 2020-2

Análisis de los resultados de las pruebas Saber 11 del segundo semestre de 2020
(ICFES). Fuente: [datos.gov.co](https://www.datos.gov.co/Educaci-n/Saber-11-2020-2/rnvb-vnyh).

## Requisitos

- R (versión reciente) y [RStudio](https://posit.co/download/rstudio-desktop/)
- Git
- ~1.5 GB libres en disco y conexión estable para la descarga inicial

## Instalación

```bash
git clone https://github.com/USUARIO/REPO.git
cd REPO
```

Abre el archivo `.Rproj` haciendo doble clic. Las rutas dependen de que la sesión arranque en la raíz.

En la consola de RStudio:

```r
renv::restore()
```

Esto reconstruye el entorno exacto de paquetes a partir de `renv.lock`. Puede
tardar varios minutos la primera vez.

## Preparación de los datos

Solo se ejecuta una vez, en caso de que sea necesario. Los datos (Parquet) están en el repositorio.

```r
source(here::here("R", "download.R"))    # descarga el CSV (~500 MB)
source(here::here("R", "to_parquet.R"))  # convierte a Parquet (~25 MB)
```