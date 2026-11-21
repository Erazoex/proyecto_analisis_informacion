# MTIC410 - Hito 3, Grupo 4
# Validacion reproducible del conjunto analitico para modelado
# Ejecutar desde la raiz del repositorio: Rscript validacion_modelado.R
# No modifica datos_clientes.rds ni los scripts originales.

options(stringsAsFactors = FALSE)
archivo <- 'datos_clientes.rds'
if (!file.exists(archivo)) stop('No se encontro ', archivo, '. Ejecute primero limpieza_datos.R.')
d <- readRDS(archivo)
if (!is.data.frame(d)) stop('El archivo no contiene un data.frame.')

columnas <- c('customer_id', 'recencia_dias', 'frecuencia',
              'valor_monetario_gbp', 'ticket_promedio_gbp', 'unidades',
              'productos_distintos', 'antiguedad_dias', 'meses_activos',
              'tasa_cancelacion', 'pais', 'region', 'gasto_futuro_gbp')
faltantes_columnas <- setdiff(columnas, names(d))
if (length(faltantes_columnas)) stop('Faltan columnas: ', paste(faltantes_columnas, collapse=', '))

cat('\n=== 1. ESTRUCTURA Y CALIDAD ===\n')
cat('Clientes:', nrow(d), '\n')
cat('Variables:', ncol(d), '\n')
cat('IDs duplicados:', sum(duplicated(d$customer_id)), '\n')
cat('IDs ausentes:', sum(is.na(d$customer_id)), '\n')
cat('Valores faltantes totales:', sum(is.na(d)), '\n')
num <- vapply(d, is.numeric, logical(1))
cat('Valores infinitos:', sum(vapply(d[num], function(x) sum(is.infinite(x)), numeric(1))), '\n')
cat('Clientes con gasto historico negativo (< -0.01 GBP):',
    sum(d$valor_monetario_gbp < -0.01, na.rm=TRUE), '\n')
cat('Clientes con gasto futuro cero:', sum(d$gasto_futuro_gbp == 0, na.rm=TRUE), '\n')
cat('Porcentaje gasto futuro cero:',
    sprintf('%.2f%%', 100 * mean(d$gasto_futuro_gbp == 0, na.rm=TRUE)), '\n')
cat('Gasto futuro negativo:', sum(d$gasto_futuro_gbp < 0, na.rm=TRUE), '\n')
cat('Recencia negativa:', sum(d$recencia_dias < 0, na.rm=TRUE), '\n')
cat('Antiguedad negativa:', sum(d$antiguedad_dias < 0, na.rm=TRUE), '\n')

cat('\n=== 2. DISTRIBUCION GEOGRAFICA ===\n')
print(table(d$region, useNA='ifany'))
cat('Categorias de pais:', length(unique(d$pais)), '\n')
cat('Paises con multiples regiones:',
    sum(vapply(split(d$region, d$pais), function(x) length(unique(x)) > 1, logical(1))), '\n')
# Alerta de clasificacion conocida; no cambia el dataset.
if (any(d$pais == 'Czech Republic' & d$region == 'Fuera Europa', na.rm=TRUE)) {
  cat('ADVERTENCIA: Czech Republic aparece como Fuera Europa; revisar regla geografica.\n')
}

cat('\n=== 3. PARTICION REPRODUCIBLE 80/20 ===\n')
set.seed(2026)
idx <- sample.int(nrow(d), size=floor(0.8 * nrow(d)))
train <- d[idx, , drop=FALSE]
test <- d[-idx, , drop=FALSE]
cat('Entrenamiento:', nrow(train), '\n')
cat('Prueba:', nrow(test), '\n')
cat('Clientes compartidos:', length(intersect(train$customer_id, test$customer_id)), '\n')
cat('Total recuperado:', nrow(train) + nrow(test), '\n')
resumen <- function(x, nombre) {
  cat(nombre, '- gasto futuro cero:', sum(x$gasto_futuro_gbp == 0, na.rm=TRUE),
      '(', sprintf('%.2f%%', 100 * mean(x$gasto_futuro_gbp == 0, na.rm=TRUE)), ')',
      '- media GBP:', round(mean(x$gasto_futuro_gbp, na.rm=TRUE), 2),
      '- mediana GBP:', round(median(x$gasto_futuro_gbp, na.rm=TRUE), 2), '\n')
}
resumen(train, 'Entrenamiento')
resumen(test, 'Prueba')

# Guardar solamente si se solicita expresamente; evita sobreescribir archivos existentes.
if ('--guardar-particion' %in% commandArgs(trailingOnly=TRUE)) {
  saveRDS(train, 'clientes_entrenamiento.rds')
  saveRDS(test, 'clientes_prueba.rds')
  cat('Particiones guardadas en clientes_entrenamiento.rds y clientes_prueba.rds\n')
}

cat('\n=== 4. CONTROLES ===\n')
controles <- c(
  ids_unicos = !anyDuplicated(d$customer_id),
  sin_nulos = !anyNA(d),
  sin_infinitos = !any(vapply(d[num], function(x) any(is.infinite(x)), logical(1))),
  sin_ids_compartidos = length(intersect(train$customer_id, test$customer_id)) == 0,
  filas_conservadas = nrow(train) + nrow(test) == nrow(d),
  objetivo_no_negativo = all(d$gasto_futuro_gbp >= 0)
)
print(controles)
if (!all(controles)) stop('Hay controles que requieren revision.')
cat('\nValidacion estructural y particion completadas.\n')
cat('NOTA: este script no recalcula independientemente los nueve indicadores ni el gasto futuro desde las transacciones originales.\n')
cat('Las reglas de cancelaciones y la definicion de la variable objetivo deben documentarse en el informe.\n')
