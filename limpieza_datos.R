source("importacion.R")
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

#1. Cargar datos importados
retail <- readRDS("online_retail_ii.rds")
  
n_inicial <- nrow(retail)
cat("Registros importados:", n_inicial, "\n")

#-------------------------------------------------------------------
#2. Limpieza de datos, Tratamiento de valores faltantes y duplicados 
#-------------------------------------------------------------------

#2.1 Duplicados
n_dup <- sum(duplicated(retail))
retail_sin_dup <- distinct(retail)

# 2.2 ID faltantes
n_sin_cid <- sum(is.na(retail_sin_dup$customer_id) | str_trim(retail_sin_dup$customer_id) == "")
retail_cid <- retail_sin_dup %>%
  filter(!is.na(customer_id) & str_trim(customer_id) != "")

# 2.3 Identificar cancelaciones, precios/cantidades anómalos y fechas
retail_clean <- retail_cid %>%
  mutate(
    invoice        = str_trim(as.character(invoice)),
    stock_code     = str_trim(as.character(stock_code)),
    es_cancelacion = str_starts(invoice, "C"),
    fecha          = as.Date(invoice_date),
    importe_linea  = quantity * price
  )

n_cancelaciones <- sum(retail_clean$es_cancelacion, na.rm = TRUE)
n_precio_cero_neg <- sum(retail_clean$price <= 0, na.rm = TRUE)
n_cant_neg_no_canc <- sum(retail_clean$quantity <= 0 & !retail_clean$es_cancelacion, na.rm = TRUE)

# Excluir registros con precio <= 0 o cantidades <= 0 que NO son facturas de cancelación 
retail_validado <- retail_clean %>%
  filter(price > 0, (quantity > 0 | es_cancelacion))

# 2.4 Delimitación temporal (Ventana total del estudio: 2009-12-01 a 2011-11-30)
inicio_obs  <- as.Date("2009-12-01")
cierre_obs  <- as.Date("2011-05-31")
inicio_resp <- as.Date("2011-06-01")
cierre_resp <- as.Date("2011-11-30")

n_fuera_rango <- sum(retail_validado$fecha < inicio_obs | retail_validado$fecha > cierre_resp, na.rm = TRUE)

retail_ventana <- retail_validado %>%
  filter(fecha >= inicio_obs & fecha <= cierre_resp)

# 2.5 Detección de valores extremos (IQR en compras válidas) para documentar sin borrar
compras_validas_tot <- retail_ventana %>% filter(!es_cancelacion)
q1 <- quantile(compras_validas_tot$importe_linea, 0.25, na.rm = TRUE)
q3 <- quantile(compras_validas_tot$importe_linea, 0.75, na.rm = TRUE)
iqr_val <- q3 - q1
n_outliers <- sum(compras_validas_tot$importe_linea > (q3 + 1.5 * iqr_val), na.rm = TRUE)

# Tabla resumen de auditoría
auditoria <- tibble(
  Control = c(
    "Registros iniciales importados",
    "Duplicados exactos eliminados",
    "ID faltantes (excluidos)",
    "Cancelaciones identificadas (prefijo C)",
    "Precios <= 0 (excluidos)",
    "Cantidades <= 0 sin prefijo C (excluidos)",
    "Registros fuera de ventana (dic 2009 - nov 2011)",
    "Valores extremos en importe de línea (> Q3 + 1.5*IQR, conservados)"
  ),
  n = c(n_inicial, n_dup, n_sin_cid, n_cancelaciones, n_precio_cero_neg,
        n_cant_neg_no_canc, n_fuera_rango, n_outliers),
  porcentaje_sobre_inicial = round((n / n_inicial) * 100, 2)
)

print(auditoria)
write_csv(auditoria, "auditoria_limpieza.csv")

#------------------------------------------------
#3. Separacion de ventanas y creacion de variables
#------------------------------------------------

# 3.1. Ventana de Observación (01/12/2009 - 31/05/2011) 
obs <- retail_ventana %>%
  filter(fecha >= inicio_obs & fecha <= cierre_obs)

# 3.2. Compras válidas (sin cancelaciones con prefijo 'C') en observación
obs_compras <- obs %>% filter(!es_cancelacion)

# 3.3. Agrupación regional propuesta en Sección 4
clasificar_region <- function(pais) {
  europa <- c("Germany", "France", "EIRE", "Spain", "Netherlands", "Belgium",
              "Switzerland", "Portugal", "Italy", "Finland", "Austria", "Norway",
              "Denmark", "Sweden", "Cyprus", "Greece", "Poland", "Lithuania",
              "Iceland", "Malta", "Channel Islands", "European Community")
  case_when(
    pais == "United Kingdom" ~ "Reino Unido",
    pais %in% europa         ~ "Resto Europa",
    TRUE                     ~ "Fuera Europa"
  )
}

# 3.4. Predictores por cliente (al menos 1 compra válida en ventana de observación)
predictores_compras <- obs_compras %>%
  group_by(customer_id) %>%
  summarise(
    recencia_dias       = as.numeric(cierre_obs - max(fecha)),
    frecuencia          = n_distinct(invoice),
    valor_bruto_gbp     = sum(importe_linea, na.rm = TRUE),
    unidades            = sum(quantity, na.rm = TRUE),
    productos_distintos = n_distinct(stock_code),
    antiguedad_dias     = as.numeric(cierre_obs - min(fecha)),
    meses_activos       = n_distinct(floor_date(fecha, "month")),
    pais                = names(sort(table(country), decreasing = TRUE))[1],
    .groups = "drop"
  )

# 3.5. Cancelaciones en observación para calcular gasto neto y tasa de cancelación
obs_cancelaciones <- obs %>%
  filter(es_cancelacion) %>%
  group_by(customer_id) %>%
  summarise(
    facturas_canceladas = n_distinct(invoice),
    monto_cancelado_gbp = sum(importe_linea, na.rm = TRUE),
    .groups = "drop"
  )

# 3.6. Ventana de Respuesta (01/06/2011 - 30/11/2011) 
resp <- retail_ventana %>%
  filter(fecha >= inicio_resp & fecha <= cierre_resp) %>%
  group_by(customer_id) %>%
  summarise(
    gasto_futuro_gbp = sum(importe_linea, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. TABLA ANALÍTICA FINAL POR CLIENTE
# ------------------------------------------------------------------------------
datos_clientes <- predictores_compras %>%
  left_join(obs_cancelaciones, by = "customer_id") %>%
  left_join(resp, by = "customer_id") %>%
  mutate(
    facturas_canceladas = coalesce(facturas_canceladas, 0L),
    monto_cancelado_gbp = coalesce(monto_cancelado_gbp, 0),
    valor_monetario_gbp = valor_bruto_gbp + monto_cancelado_gbp,
    ticket_promedio_gbp = valor_monetario_gbp / frecuencia,
    tasa_cancelacion    = facturas_canceladas / (frecuencia + facturas_canceladas),
    region              = clasificar_region(pais),
    gasto_futuro_gbp    = pmax(coalesce(gasto_futuro_gbp, 0), 0)
  ) %>%
  select(
    customer_id,
    recencia_dias,
    frecuencia,
    valor_monetario_gbp,
    ticket_promedio_gbp,
    unidades,
    productos_distintos,
    antiguedad_dias,
    meses_activos,
    tasa_cancelacion,
    pais,
    region,
    gasto_futuro_gbp
  )

saveRDS(datos_clientes, "datos_clientes.rds")
write_csv(datos_clientes, "datos_clientes.csv")
