# ---- 1. Paquetes ----
paquetes <- c("dplyr", "tidyr", "readr", "ggplot2")
faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

# ---- 2. Datos ----
archivo <- "datos_clientes.rds"
if (!file.exists(archivo)) stop("No se encontro ", archivo, ". Ejecute primero source('limpieza_datos.R').")
d <- readRDS(archivo)

salida <- "resultados_relaciones"
dir.create(salida, showWarnings = FALSE)

predictores <- c("recencia_dias", "frecuencia", "valor_monetario_gbp",
                 "ticket_promedio_gbp", "unidades", "productos_distintos",
                 "antiguedad_dias", "meses_activos", "tasa_cancelacion")
objetivo <- "gasto_futuro_gbp"

# Variable binaria auxiliar: el cliente compro (gasto neto > 0) en la ventana de respuesta
d <- d %>%
  mutate(compra_futura = factor(ifelse(gasto_futuro_gbp > 0, "Si", "No"), levels = c("No", "Si")),
         region = factor(region, levels = c("Reino Unido", "Resto Europa", "Fuera Europa")))

cat("========================================\n")
cat("ANALISIS DE RELACIONES ENTRE VARIABLES\n")
cat("========================================\n")
cat("Clientes:", nrow(d), "| Con compra futura:", sum(d$compra_futura == "Si"),
    sprintf("(%.2f%%)", 100 * mean(d$compra_futura == "Si")), "\n")

# Interpretacion de la magnitud de una correlacion (criterio convencional)
fuerza <- function(r) {
  a <- abs(r)
  cut(a, breaks = c(-Inf, 0.1, 0.3, 0.5, 0.7, Inf),
      labels = c("Nula", "Debil", "Moderada", "Fuerte", "Muy fuerte"), right = FALSE) |>
    as.character()
}
formato_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.4f", p))

# ============================================================
# 3. SUPUESTO DE NORMALIDAD 
# ============================================================
# Shapiro-Wilk admite hasta 5,000 observaciones (n = 4,933).
normalidad <- bind_rows(lapply(c(predictores, objetivo), function(v) {
  x <- d[[v]]
  sw <- shapiro.test(x)
  data.frame(Variable = v,
             W = round(unname(sw$statistic), 4),
             p_valor = formato_p(sw$p.value),
             Asimetria = round(mean((x - mean(x))^3) / sd(x)^3, 2),
             Normal_al_5pct = ifelse(sw$p.value >= 0.05, "Si", "No"))
}))
cat("\n=== 3. Normalidad (Shapiro-Wilk) ===\n")
print(normalidad, row.names = FALSE)
write_csv(normalidad, file.path(salida, "01_normalidad_shapiro.csv"))

# ============================================================
# 4. CORRELACION DE CADA PREDICTOR CON EL GASTO FUTURO
# ============================================================
# Metodo principal: Spearman. Pearson se reporta solo como referencia.
cor_objetivo <- bind_rows(lapply(predictores, function(v) {
  sp <- suppressWarnings(cor.test(d[[v]], d[[objetivo]], method = "spearman", exact = FALSE))
  pe <- cor.test(d[[v]], d[[objetivo]], method = "pearson")
  data.frame(Predictor = v,
             n = nrow(d),
             rho_spearman = round(unname(sp$estimate), 3),
             p_spearman = formato_p(sp$p.value),
             r_pearson = round(unname(pe$estimate), 3),
             p_pearson = formato_p(pe$p.value),
             Direccion = ifelse(sp$estimate > 0, "Positiva", "Negativa"),
             Fuerza = fuerza(sp$estimate))
})) %>% arrange(desc(abs(rho_spearman)))

cat("\n=== 4. Correlacion con gasto_futuro_gbp ===\n")
print(cor_objetivo, row.names = FALSE)
write_csv(cor_objetivo, file.path(salida, "02_correlaciones_gasto_futuro.csv"))

# ============================================================
# 5. MATRIZ DE CORRELACION ENTRE PREDICTORES
# ============================================================
m <- cor(d[, c(predictores, objetivo)], method = "spearman")
write_csv(as.data.frame(round(m, 3)) %>% mutate(Variable = rownames(m), .before = 1),
          file.path(salida, "03_matriz_spearman.csv"))

pares_altos <- as.data.frame(as.table(m[predictores, predictores])) %>%
  rename(Var1 = Var1, Var2 = Var2, rho = Freq) %>%
  filter(as.integer(Var1) < as.integer(Var2), abs(rho) >= 0.7) %>%
  mutate(rho = round(rho, 3)) %>%
  arrange(desc(abs(rho)))
cat("\n=== 5. Pares de predictores con |rho| >= 0.7 (posible multicolinealidad) ===\n")
print(pares_altos, row.names = FALSE)
write_csv(pares_altos, file.path(salida, "04_pares_multicolinealidad.csv"))

mapa <- as.data.frame(as.table(m)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1), name = "rho") +
  labs(title = "Matriz de correlacion de Spearman",
       subtitle = "Predictores por cliente y gasto futuro (n = 4,933)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(salida, "fig_matriz_correlacion.png"), mapa, width = 9, height = 7.5, dpi = 300)

# ============================================================
# 6. REGION VS GASTO FUTURO (Kruskal-Wallis, alternativa no parametrica al ANOVA)
# ============================================================
por_region <- d %>%
  group_by(region) %>%
  summarise(n = n(),
            media_gbp = round(mean(gasto_futuro_gbp), 2),
            mediana_gbp = round(median(gasto_futuro_gbp), 2),
            desv_est_gbp = round(sd(gasto_futuro_gbp), 2),
            pct_compra_futura = round(100 * mean(compra_futura == "Si"), 2),
            .groups = "drop")
cat("\n=== 6. Gasto futuro por region ===\n")
print(as.data.frame(por_region), row.names = FALSE)
write_csv(por_region, file.path(salida, "05_gasto_por_region.csv"))

# ANOVA de referencia y verificacion de supuestos
anova_mod <- aov(gasto_futuro_gbp ~ region, data = d)
anova_p <- summary(anova_mod)[[1]][["Pr(>F)"]][1]
bartlett <- bartlett.test(gasto_futuro_gbp ~ region, data = d)  # homogeneidad de varianzas

kw <- kruskal.test(gasto_futuro_gbp ~ region, data = d)
k <- nlevels(d$region)
epsilon2 <- (unname(kw$statistic) - k + 1) / (nrow(d) - k)   # tamano del efecto
cat("ANOVA (referencia): p =", formato_p(anova_p),
    "| Bartlett (igualdad de varianzas): p =", formato_p(bartlett$p.value), "\n")
cat("Kruskal-Wallis: H =", round(kw$statistic, 3), "| gl =", kw$parameter,
    "| p =", formato_p(kw$p.value), "| epsilon^2 =", round(epsilon2, 4), "\n")

posthoc <- pairwise.wilcox.test(d$gasto_futuro_gbp, d$region, p.adjust.method = "holm", exact = FALSE)
cat("Comparaciones por pares (Wilcoxon, ajuste Holm):\n")
print(round(posthoc$p.value, 4))

pruebas_region <- data.frame(
  Prueba = c("ANOVA de un factor (referencia)", "Bartlett (homogeneidad)", "Kruskal-Wallis"),
  Estadistico = c(round(summary(anova_mod)[[1]][["F value"]][1], 3),
                  round(unname(bartlett$statistic), 3),
                  round(unname(kw$statistic), 3)),
  gl = c(paste(k - 1, nrow(d) - k, sep = ", "), k - 1, k - 1),
  p_valor = formato_p(c(anova_p, bartlett$p.value, kw$p.value)),
  Efecto = c(NA, NA, paste0("epsilon^2 = ", round(epsilon2, 4))))
write_csv(pruebas_region, file.path(salida, "06_pruebas_region.csv"))
write_csv(as.data.frame(round(posthoc$p.value, 4)) %>% mutate(Region = rownames(posthoc$p.value), .before = 1),
          file.path(salida, "07_posthoc_region_holm.csv"))

# ============================================================
# 7. REGION VS COMPRA FUTURA (Chi-cuadrado de independencia)
# ============================================================
tabla <- table(Region = d$region, Compra_futura = d$compra_futura)
chi <- chisq.test(tabla)
esperadas_min <- min(chi$expected)
if (esperadas_min < 5) chi <- chisq.test(tabla, simulate.p.value = TRUE, B = 10000)
v_cramer <- sqrt(unname(chi$statistic) / (sum(tabla) * (min(dim(tabla)) - 1)))

cat("\n=== 7. Tabla cruzada region x compra futura ===\n")
print(addmargins(tabla))
print(round(100 * prop.table(tabla, 1), 2))
cat("Chi-cuadrado =", round(chi$statistic, 3), "| gl =", chi$parameter,
    "| p =", formato_p(chi$p.value), "| V de Cramer =", round(v_cramer, 4),
    "| frecuencia esperada minima =", round(esperadas_min, 2), "\n")

write_csv(as.data.frame.matrix(tabla) %>% mutate(Region = rownames(tabla), .before = 1),
          file.path(salida, "08_tabla_cruzada_region.csv"))
write_csv(data.frame(Chi_cuadrado = round(unname(chi$statistic), 3), gl = unname(chi$parameter),
                     p_valor = formato_p(chi$p.value), V_Cramer = round(v_cramer, 4),
                     Esperada_minima = round(esperadas_min, 2)),
          file.path(salida, "09_chi_cuadrado_region.csv"))

# ============================================================
# 8. COMPRADORES VS NO COMPRADORES FUTUROS (Mann-Whitney; t de Welch como referencia)
# ============================================================
comparacion <- bind_rows(lapply(predictores, function(v) {
  si <- d[[v]][d$compra_futura == "Si"]
  no <- d[[v]][d$compra_futura == "No"]
  mw <- wilcox.test(si, no, exact = FALSE)
  tt <- t.test(si, no)  # Welch
  # Tamano del efecto: correlacion biserial por rangos (-1 a 1); positiva = mayor en compradores
  r_rb <- 2 * unname(mw$statistic) / (length(si) * length(no)) - 1
  data.frame(Predictor = v,
             mediana_compra_si = round(median(si), 2),
             mediana_compra_no = round(median(no), 2),
             media_compra_si = round(mean(si), 2),
             media_compra_no = round(mean(no), 2),
             U_Mann_Whitney = unname(mw$statistic),
             p_Mann_Whitney = formato_p(mw$p.value),
             r_biserial_rangos = round(r_rb, 3),
             Efecto = fuerza(r_rb),
             p_t_Welch = formato_p(tt$p.value))
}))
cat("\n=== 8. Compradores futuros (Si) vs no compradores (No) ===\n")
print(comparacion, row.names = FALSE)
write_csv(comparacion, file.path(salida, "10_comparacion_compradores.csv"))

# ============================================================
# 9. DIAGRAMAS DE DISPERSION (escala log(1 + x) por la fuerte asimetria)
# ============================================================
vars_disp <- c("recencia_dias", "frecuencia", "valor_monetario_gbp", "ticket_promedio_gbp",
               "meses_activos", "tasa_cancelacion")
etiquetas <- setNames(paste0(vars_disp, "  (rho = ",
                             sprintf("%.2f", cor_objetivo$rho_spearman[match(vars_disp, cor_objetivo$Predictor)]), ")"),
                      vars_disp)

vars_log <- c("frecuencia", "valor_monetario_gbp", "ticket_promedio_gbp")
# Solo para la grafica: se omiten los pocos clientes con valor/ticket <= 0 (gasto neto
# historico negativo por cancelaciones), que distorsionan la curva suavizada en escala log.
n_omitidos <- sum(d$valor_monetario_gbp <= 0)
cat("\nClientes con valor monetario <= 0 omitidos solo en la figura de dispersion:", n_omitidos, "\n")

disp <- d %>%
  select(all_of(vars_disp), gasto_futuro_gbp) %>%
  pivot_longer(-gasto_futuro_gbp, names_to = "Predictor", values_to = "Valor") %>%
  filter(!(Predictor %in% vars_log & Valor <= 0)) %>%
  mutate(Valor_t = ifelse(Predictor %in% vars_log, log10(Valor), Valor),
         Predictor = factor(etiquetas[Predictor], levels = etiquetas)) %>%
  ggplot(aes(Valor_t, log1p(gasto_futuro_gbp))) +
  geom_point(alpha = 0.15, size = 0.7, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 0.8, formula = y ~ x) +
  facet_wrap(~ Predictor, scales = "free_x", ncol = 3) +
  labs(title = "Relacion de los predictores con el gasto futuro",
       subtitle = paste0("Eje Y: log(1 + gasto futuro GBP). Frecuencia, valor y ticket en log10. ",
                         "Se omiten ", n_omitidos, " clientes con valor monetario <= 0."),
       x = "Valor del predictor", y = "log(1 + gasto futuro)") +
  theme_minimal()
ggsave(file.path(salida, "fig_dispersion_gasto_futuro.png"), disp, width = 12, height = 7.5, dpi = 300)

caja_region <- ggplot(d, aes(region, log1p(gasto_futuro_gbp), fill = region)) +
  geom_boxplot(outlier.alpha = 0.2, show.legend = FALSE) +
  labs(title = "Gasto futuro por region",
       subtitle = sprintf("Kruskal-Wallis: H = %.2f, p %s", kw$statistic,
                          ifelse(kw$p.value < 0.001, "< 0.001", paste("=", round(kw$p.value, 4)))),
       x = NULL, y = "log(1 + gasto futuro GBP)") +
  theme_minimal()
ggsave(file.path(salida, "fig_gasto_por_region.png"), caja_region, width = 8, height = 5.5, dpi = 300)

# ============================================================
# 10. ANALISIS DE SENSIBILIDAD: regla geografica
# ============================================================
# clasificar_region() deja "Czech Republic" en "Fuera Europa" y no separa "Unspecified".
# Se repiten las pruebas por region corrigiendo esto, sin alterar datos_clientes.rds.
d_sens <- d %>%
  filter(pais != "Unspecified") %>%
  mutate(region = droplevels(factor(ifelse(pais == "Czech Republic", "Resto Europa", as.character(region)),
                                    levels = levels(d$region))))
kw_s <- kruskal.test(gasto_futuro_gbp ~ region, data = d_sens)
chi_s <- suppressWarnings(chisq.test(table(d_sens$region, d_sens$compra_futura)))
cat("\n=== 10. Sensibilidad (Czech Republic -> Resto Europa; sin 'Unspecified') ===\n")
cat("Clientes:", nrow(d_sens), "| Kruskal-Wallis p =", formato_p(kw_s$p.value),
    "| Chi-cuadrado p =", formato_p(chi_s$p.value), "\n")

cat("\n========================================\n")
cat("ANALISIS DE RELACIONES COMPLETADO\n")
cat("Resultados en la carpeta:", salida, "\n")
cat("========================================\n")
