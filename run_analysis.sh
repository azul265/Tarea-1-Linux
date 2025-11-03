#!/bin/bash
# ================================================
# Script completo para tarea de análisis sísmico
# ================================================
set -e  # Salir si hay error

WORKDIR="$(pwd)"
mkdir -p data
mkdir -p figures

echo " Descargando datos desde Dropbox..."
# Descargar CSV si no existe
[ ! -f data/data.csv ] && wget -O data/data.csv "https://www.dropbox.com/scl/fi/acm84xjyrj5xlz77ffdpp/data.csv?dl=1"

echo " Revisando estructura y generando columns.txt..."
head -1 data/data.csv | tr ',' '\n' > data/columns.txt

echo " Generando muestra aleatoria de 1000 registros..."
# Extraer 1000 registros aleatorios sin perder la cabecera
head -1 data/data.csv > data/sample_earthquakes.csv
tail -n +2 data/data.csv | shuf -n 1000 >> data/sample_earthquakes.csv

echo " Limpieza de datos y creación de CSV limpio..."
# Eliminar filas incompletas, estandarizar texto a minúsculas y seleccionar columnas relevantes
Rscript -e "
library(readr)
library(dplyr)
library(stringr)
eq <- read_csv('data/data.csv', quote='\"', na=c('', 'NA'))
eq <- eq %>% mutate(across(where(is.character), ~ str_to_lower(.)))
eq_clean <- eq %>% select(time, latitude, longitude, depth, mag, magType, nst, gap, dmin, place, type, status) %>% filter(complete.cases(.))
write_csv(eq_clean, 'data/earthquakes_clean.csv')
"

echo " Generando estadísticas descriptivas básicas..."
Rscript -e "
library(readr)
library(dplyr)
eq_clean <- read_csv('data/earthquakes_clean.csv')
stats <- eq_clean %>% group_by(magType) %>% summarise(
  count = n(),
  mean_mag = mean(mag, na.rm=TRUE),
  median_mag = median(mag, na.rm=TRUE),
  max_mag = max(mag, na.rm=TRUE),
  min_mag = min(mag, na.rm=TRUE)
)
write_csv(stats, 'data/magtype_summary.csv')
"

echo " Análisis y visualización en R..."
Rscript limpieza_y_visualizacion.R

echo "Todo completado. Revisar carpetas 'data' y 'figures' para resultados."

