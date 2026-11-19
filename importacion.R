
# ---- 1. Paquetes ----
paquetes <- c("readxl", "dplyr")
faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")
library(readxl)
library(dplyr)

# ---- 2. Archivo local ----
archivo <- "online_retail_II.xlsx"
if (!file.exists(archivo)) stop("No se encontró ", archivo, " en la carpeta de trabajo.")

# ---- 3. Importación de las dos hojas ----
tipos <- c("text", "text", "text", "numeric", "date", "numeric", "numeric", "text")
hojas <- excel_sheets(archivo)   # "Year 2009-2010" y "Year 2010-2011"

hoja1 <- read_excel(archivo, sheet = hojas[1], col_types = tipos)
hoja2 <- read_excel(archivo, sheet = hojas[2], col_types = tipos)

# ---- 4. Unión de las hojas ----
hoja2 <- hoja2 |> filter(!Invoice %in% hoja1$Invoice)

retail <- bind_rows(hoja1, hoja2) |>
  rename(invoice = Invoice, stock_code = StockCode, description = Description,
         quantity = Quantity, invoice_date = InvoiceDate, price = Price,
         customer_id = `Customer ID`, country = Country) |>
  mutate(quantity    = as.integer(quantity),
         customer_id = as.character(customer_id))

# ---- 5. Verificación rápida ----
cat("Filas:", nrow(retail), "| Variables:", ncol(retail), "\n")
cat("Periodo:", format(min(retail$invoice_date)), "a", format(max(retail$invoice_date)), "\n")
glimpse(retail)
print(colSums(is.na(retail)))
print(retail)

# ---- 6. Guardar para los siguientes pasos del proyecto ----
saveRDS(retail, "online_retail_ii.rds")
# Para cargarlo: retail <- readRDS("online_retail_ii.rds")
