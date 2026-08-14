library(here)
library(DBI)
library(duckdb)

csv_path <- here::here("data", "raw", "saber11_2020_2.csv")
pq_path  <- here::here("data", "processed", "saber11_2020_2.parquet")

dir.create(dirname(pq_path), recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE))

# Limitar RAM para no ahogar el equipo
dbExecute(con, "SET memory_limit = '4GB'")
dbExecute(con, "SET preserve_insertion_order = false")

sql <- sprintf("
  COPY (
    SELECT * FROM read_csv(
      '%s',
      header      = true,
      sample_size = -1,
      all_varchar = false,
      ignore_errors = false
    )
  ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)
", csv_path, pq_path)

system.time(dbExecute(con, sql))

message(sprintf(
  "CSV: %.0f MB  ->  Parquet: %.0f MB  (%.1fx)",
  file.size(csv_path) / 1e6,
  file.size(pq_path)  / 1e6,
  file.size(csv_path) / file.size(pq_path)
))