# Descarga el dataset Saber 11 2020-2 desde datos.gov.co
# Fuente: https://www.datos.gov.co/Educaci-n/Saber-11-2020-2/rnvb-vnyh

library(here)

url_csv  <- "https://www.datos.gov.co/api/views/rnvb-vnyh/rows.csv?accessType=DOWNLOAD"
destino  <- here::here("data", "raw", "saber11_2020_2.csv")
manifest <- here::here("data", "raw", "MANIFEST.txt")

dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(destino)) {
  message("Descargando (~500 MB)...")
  res <- curl::multi_download(
    urls      = url_csv,
    destfiles = destino,
    resume    = TRUE,
    progress  = TRUE
  )
  stopifnot(res$success, res$status_code == 200)
} else {
  message("Archivo ya existe, se omite la descarga.")
}

# Registro de procedencia: esto SI se versiona en git
if (!file.exists(manifest)) {
  writeLines(c(
    paste("archivo:", basename(destino)),
    paste("url:", url_csv),
    paste("descargado:", format(Sys.time(), tz = "America/Bogota")),
    paste("bytes:", file.size(destino)),
    paste("md5:", unname(tools::md5sum(destino)))
  ), manifest)
}

cat(readLines(manifest), sep = "\n")