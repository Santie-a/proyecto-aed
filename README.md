# Saber 11 — 2020-2

Análisis de los resultados de las pruebas Saber 11 del segundo semestre de 2020
(ICFES). Fuente: [datos.gov.co](https://www.datos.gov.co/Educaci-n/Saber-11-2020-2/rnvb-vnyh).

Aproximadamente 504.000 registros y 81 variables.

## Requisitos

- R (versión reciente) y [RStudio](https://posit.co/download/rstudio-desktop/)
- Git
- ~1.5 GB libres en disco y conexión estable para la descarga inicial

## Instalación

```bash
git clone https://github.com/USUARIO/REPO.git
cd REPO
```

Abre el archivo `.Rproj` haciendo doble clic. **No abras R por fuera del proyecto**:
las rutas dependen de que la sesión arranque en la raíz.

En la consola de RStudio:

```r
renv::restore()
```

Esto reconstruye el entorno exacto de paquetes a partir de `renv.lock`. Puede
tardar varios minutos la primera vez.

## Preparación de los datos

Solo se ejecuta una vez. Los datos **no** están en el repositorio.

```r
source(here::here("R", "download.R"))    # descarga el CSV (~500 MB)
source(here::here("R", "to_parquet.R"))  # convierte a Parquet (~50-80 MB)
```

Ambos scripts son idempotentes: si el archivo ya existe, no lo vuelven a generar.

## Ejecución

```r
source(here::here("R", "example.r"))
```

Imprime en consola las dimensiones del dataset, el esquema de columnas, un
`head()` y dos consultas agregadas de ejemplo.

## Convenciones

- **Nunca commitees archivos de `data/`.** Verifica con `git status` antes de
  hacer push. GitHub rechaza archivos de más de 100 MB.
- **Al instalar un paquete nuevo**, ejecuta `renv::snapshot()` y commitea
  `renv.lock` en el mismo commit que el código que lo usa.
- **Trabaja de forma perezosa sobre el Parquet.** `collect()` va al final de la
  cadena, nunca al inicio. Cargar el dataset completo consume 2-3 GB de RAM sin
  necesidad.
- Ramas por feature y pull request. `main` siempre debe poder ejecutarse.

## Problemas frecuentes

| Síntoma | Causa | Solución |
|---|---|---|
| `No se encontró el Parquet` | Falta la preparación de datos | Ejecuta `download.R` y `to_parquet.R` |
| `there is no package called ...` | Entorno no restaurado | `renv::restore()` |
| Rutas rotas o `file not found` | Sesión abierta fuera del proyecto | Cierra R y abre el `.Rproj` |
| Descarga interrumpida | Conexión inestable | Vuelve a ejecutar `download.R`; reanuda desde donde quedó |
| Tildes mal mostradas | Encoding | Tools → Global Options → Code → Saving → UTF-8 |