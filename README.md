# Importación de Online Retail II

## Requisitos

- R 4.x
- Los paquetes `readxl` y `dplyr`. Si faltan, el script los instala solo.

## Ejecución

1. Pon `importacion.R` y `online_retail_II.xlsx` en la misma carpeta.
2. Desde esa carpeta, ejecuta:

```bash
Rscript importacion.R
```

En RStudio también sirve: abre la carpeta con *Session → Set Working Directory → To Source File Location* y ejecuta `source("importacion.R")`.

## Resultado

El script crea `online_retail_ii.rds`, con 1,044,848 filas y 8 variables. Para usarlo en los siguientes pasos:

```r
retail <- readRDS("online_retail_ii.rds")
```
