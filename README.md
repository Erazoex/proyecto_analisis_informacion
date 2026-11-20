# Importación de Online Retail II


## Ejecución

1. Desde esa carpeta, ejecuta:

```bash
Rscript importacion.R
```

En RStudio también sirve: abre la carpeta con *Session → Set Working Directory → To Source File Location* y ejecuta `source("importacion.R")` luego ejecuta `source("limpieza_datos.R")`.

## Resultado

```r
retail <- readRDS("online_retail_ii.rds")
saveRDS(datos_clientes, "datos_clientes.rds")
write_csv(datos_clientes, "datos_clientes.csv")
```
