
# ============================================================
# PROYECTO: SEGMENTACION DE CLIENTES Y PREDICCION DE COMPRAS
# FASE 3: ANALISIS ESTADISTICO DESCRIPTIVO
# ============================================================

# 1. INSTALACION Y CARGA DE PAQUETES

paquetes <- c("dplyr", "ggplot2", "tidyr", "readr")

faltantes <- paquetes[
  !sapply(paquetes, requireNamespace, quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes,
                   repos = "https://cloud.r-project.org")
}

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# ============================================================
# 2. CARGAR DATOS
# ============================================================

datos_clientes <- readRDS("datos_clientes.rds")

cat("========================================\n")
cat("ANALISIS ESTADISTICO DESCRIPTIVO\n")
cat("========================================\n")

cat("Numero de clientes:", nrow(datos_clientes), "\n")
cat("Numero de variables:", ncol(datos_clientes), "\n")

# ============================================================
# 3. SELECCIONAR VARIABLES NUMERICAS
# ============================================================

variables_numericas <- c(
  "recencia_dias",
  "frecuencia",
  "valor_monetario_gbp",
  "ticket_promedio_gbp",
  "unidades",
  "productos_distintos",
  "antiguedad_dias",
  "meses_activos",
  "tasa_cancelacion"
)

# Variable de respuesta: se analiza por separado
variable_respuesta <- "gasto_futuro_gbp"

# ============================================================
# 4. FUNCION PARA CALCULAR ESTADISTICAS DESCRIPTIVAS
# ============================================================

calcular_estadisticas <- function(x, nombre) {
  
  x <- x[is.finite(x)]
  
  desviacion <- sd(x)
  
  asimetria <- if (length(x) >= 3 &&
                   is.finite(desviacion) &&
                   desviacion > 0) {
    mean((x - mean(x))^3) / desviacion^3
  } else {
    NA_real_
  }
  
  data.frame(
    Variable = nombre,
    N = length(x),
    Media = mean(x),
    Mediana = median(x),
    Desviacion_Estandar = desviacion,
    Minimo = min(x),
    Q1 = unname(quantile(x, 0.25)),
    Q3 = unname(quantile(x, 0.75)),
    Maximo = max(x),
    Asimetria = asimetria
  )
}

# ============================================================
# 5. ESTADISTICAS DESCRIPTIVAS DE LOS PREDICTORES
# ============================================================

resumen_descriptivo <- bind_rows(
  lapply(variables_numericas, function(v) {
    calcular_estadisticas(datos_clientes[[v]], v)
  })
)

print(resumen_descriptivo)

write_csv(
  resumen_descriptivo,
  "estadisticas_descriptivas.csv"
)

# ============================================================
# 6. ESTADISTICAS DE LA VARIABLE DE RESPUESTA
# ============================================================

resumen_respuesta <- calcular_estadisticas(
  datos_clientes[[variable_respuesta]],
  variable_respuesta
)

cat("\nESTADISTICAS DE LA VARIABLE DE RESPUESTA\n")
print(resumen_respuesta)

write_csv(
  resumen_respuesta,
  "estadisticas_gasto_futuro.csv"
)

# ============================================================
# 7. PREPARAR DATOS PARA LAS DISTRIBUCIONES
# ============================================================

datos_graficas <- datos_clientes %>%
  select(all_of(variables_numericas)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  filter(is.finite(Valor))

# ============================================================
# 8. HISTOGRAMAS
# ============================================================

histogramas <- ggplot(
  datos_graficas,
  aes(x = Valor)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Distribuciones de las variables numericas",
    subtitle = "Comportamiento de los clientes durante la observacion",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(histogramas)

ggsave(
  "histogramas_variables.png",
  histogramas,
  width = 12,
  height = 10,
  dpi = 300
)

# ============================================================
# 9. DIAGRAMAS DE CAJA (BOXPLOTS)
# ============================================================

boxplots <- ggplot(
  datos_graficas,
  aes(x = "", y = Valor)
) +
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue",
    outlier.alpha = 0.3
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Diagramas de caja de las variables numericas",
    x = "",
    y = "Valor"
  ) +
  theme_minimal()

print(boxplots)

ggsave(
  "boxplots_variables.png",
  boxplots,
  width = 12,
  height = 10,
  dpi = 300
)

# ============================================================
# 10. DISTRIBUCION DEL GASTO FUTURO
# ============================================================

grafica_gasto_futuro <- ggplot(
  datos_clientes,
  aes(x = gasto_futuro_gbp)
) +
  geom_histogram(
    bins = 30,
    fill = "darkseagreen",
    color = "white"
  ) +
  labs(
    title = "Distribucion del gasto futuro de los clientes",
    x = "Gasto futuro (GBP)",
    y = "Numero de clientes"
  ) +
  theme_minimal()

print(grafica_gasto_futuro)

ggsave(
  "distribucion_gasto_futuro.png",
  grafica_gasto_futuro,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# 11. FINALIZACION
# ============================================================

cat("\n========================================\n")
cat("ANALISIS DESCRIPTIVO COMPLETADO\n")
cat("Archivos generados:\n")
cat("- estadisticas_descriptivas.csv\n")
cat("- estadisticas_gasto_futuro.csv\n")
cat("- histogramas_variables.png\n")
cat("- boxplots_variables.png\n")
cat("- distribucion_gasto_futuro.png\n")
cat("========================================\n")


# Comprobaciones adicionales

cat("Clientes con gasto futuro igual a cero:",
    sum(datos_clientes$gasto_futuro_gbp == 0), "\n")

cat("Porcentaje de clientes con gasto futuro igual a cero:",
    round(mean(datos_clientes$gasto_futuro_gbp == 0) * 100, 2),
    "%\n")

cat("Clientes con valor monetario negativo:",
    sum(datos_clientes$valor_monetario_gbp < 0), "\n")

cat("Porcentaje con valor monetario negativo:",
    round(mean(datos_clientes$valor_monetario_gbp < 0) * 100, 2),
    "%\n")


# Verificar el comportamiento de las transacciones
# durante el periodo de respuesta

resp_detalle <- retail_ventana %>%
  filter(
    fecha >= as.Date("2011-06-01"),
    fecha <= as.Date("2011-11-30")
  ) %>%
  group_by(customer_id) %>%
  summarise(
    total_registros = n(),
    facturas = n_distinct(invoice),
    compras_validas = sum(!es_cancelacion),
    cancelaciones = sum(es_cancelacion),
    gasto_neto = sum(importe_linea, na.rm = TRUE),
    .groups = "drop"
  )

# Revisar clientes sin gasto futuro positivo
clientes_cero <- datos_clientes %>%
  filter(gasto_futuro_gbp == 0) %>%
  left_join(resp_detalle, by = "customer_id")

cat("Clientes con gasto cero:", nrow(clientes_cero), "\n")

cat("Clientes sin registros en respuesta:",
    sum(is.na(clientes_cero$total_registros)), "\n")

cat("Clientes con transacciones en respuesta:",
    sum(!is.na(clientes_cero$total_registros)), "\n")

cat("Clientes con compras válidas:",
    sum(clientes_cero$compras_validas > 0, na.rm = TRUE), "\n")

cat("Clientes con cancelaciones:",
    sum(clientes_cero$cancelaciones > 0, na.rm = TRUE), "\n")


# Analizar los 37 clientes con transacciones
# que presentan gasto futuro igual a cero

detalle_ceros <- resp_detalle %>%
  filter(
    customer_id %in%
      datos_clientes$customer_id[
        datos_clientes$gasto_futuro_gbp == 0
      ],
    total_registros > 0
  ) %>%
  mutate(
    tipo_comportamiento = case_when(
      compras_validas > 0 & cancelaciones > 0 ~
        "Compras y cancelaciones",
      
      compras_validas > 0 & cancelaciones == 0 ~
        "Solo compras",
      
      compras_validas == 0 & cancelaciones > 0 ~
        "Solo cancelaciones",
      
      TRUE ~ "Otro"
    )
  )

# Mostrar distribución
print(
  detalle_ceros %>%
    count(tipo_comportamiento)
)

# Mostrar los importes netos
print(
  detalle_ceros %>%
    select(
      customer_id,
      compras_validas,
      cancelaciones,
      gasto_neto,
      tipo_comportamiento
    )
)
