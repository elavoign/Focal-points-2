# Gasoline Spreads México — Pipeline de Datos (targets)

Pipeline reproducible en R/targets para analizar precios de gasolina minorista vs mayorista en México (2017–2025), elasticidades de demanda municipal y sus relaciones con pobreza.

---

## Contenido

1. [Qué produce el pipeline](#1-qué-produce-el-pipeline)
2. [Estructura del repo](#2-estructura-del-repo)
3. [Fuentes de datos crudos](#3-fuentes-de-datos-crudos)
4. [Cómo correr el pipeline](#4-cómo-correr-el-pipeline)
5. [Capas del pipeline](#5-capas-del-pipeline)
6. [Joins y llaves principales](#6-joins-y-llaves-principales)
7. [Flags de calidad](#7-flags-de-calidad)
8. [Parámetros globales del pipeline](#8-parámetros-globales-del-pipeline)
9. [Checklist para máquina nueva](#9-checklist-para-máquina-nueva)

---

## 1. Qué produce el pipeline

| Producto final | Ruta | Granularidad |
|---|---|---|
| Panel de spreads estación × día | `data/analysis/spreads_station_day/year=YYYY/` | station_id × date |
| Panel balanceado estación × día | `data/merged/balanced_panel/year=YYYY/` | station_id × date (con LOCF) |
| Precios municipio × mes | `data/analysis/mun_month_prices/mun_month_prices.parquet` | CVEGEO × year × month |
| Precios municipio × mes + pobreza | `data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet` | CVEGEO × year × month |
| Elasticidades municipales (FE año) | `data/analysis/elasticity/mun_elasticities.parquet` | CVEGEO |
| Elasticidades municipales (FE año-mes) | `data/analysis/elasticity/mun_elasticities_yr_month.parquet` | CVEGEO |
| Regresiones pooled (muestra completa) | `outputs/shaun/pooled_regression/` | — |
| Regresiones pooled (muestra restringida) | `outputs/shaun/pooled_regression_restricted/` | — |
| Brecha de importaciones EUA→México | `data/processed/imports_gap/gasoline_imports_gap.parquet` | mensual |
| Gráficos y mapas | `outputs/` | — |

---

## 2. Estructura del repo

```
_targets.R                  # orquesta el pipeline completo
targets/                    # factories (grupos de targets) por capa
R/
  Raw_to_Processed/         # limpieza y estandarización de fuentes crudas
  Processed_to_Merged/      # construcción de paneles
  Merged_to_Analysis/       # cómputo de spreads
  Analysis/                 # agregaciones, elasticidades, regresiones
  Graphs/                   # funciones de gráficos
  Map/                      # Marco Geoestadístico y mapas de calor
  utils/                    # station_id(), terminal_id(), utilidades
data/
  raw_public/               # fuentes públicas (no modificar)
  raw_private/              # fuentes privadas (no modificar)
  processed/                # salidas limpias por fuente
  merged/                   # paneles consolidados por año
  analysis/                 # spreads, agregaciones, elasticidades
  map/                      # shapefiles descomprimidos
outputs/
  shaun/                    # Excel, gráficos y tablas de regresión
  graphs/                   # series de tiempo, distribuciones, cuantiles
  imports_gap/              # gráficos de brecha de importaciones
```

---

## 3. Fuentes de datos crudos

### Públicas (`data/raw_public/`)

| Archivo / Carpeta | Contenido | Formato |
|---|---|---|
| `prices_retail/Retail_YYYY.csv` (2017–2025) | Precios diarios por gasolinera (regular, premium, diesel) | CSV anual |
| `terminal_prices/Terminal.csv` | Precios de terminales PEMEX 2017–2025 | CSV único |
| `international_prices/Regular_Dolars_per_Galon.xls` | Precios spot internacionales gasolina regular (USD/gal) | XLS |
| `international_prices/Disel_Dolars_per_Galon.xls` | Precios spot internacionales diesel (USD/gal) | XLS |
| `international_prices/Tipo_de_Cambio.xls` | Tipo de cambio USD→MXN diario | XLS |
| `IEPS_Combustibles_Mexico.xlsx` | IEPS mensual nacional por combustible | XLSX |
| `GASOLINE.xlsx` | Precios Bloomberg Gulf Coast (Regular 87, Premium 93) | XLSX |
| `04_volumenes_venta_expendio_petroliferos.csv` | Volúmenes de venta CRE/SENER por municipio y mes | CSV |
| `Indicadores_pobreza_grupos_municipal.xlsx` | CONEVAL 2020: índices de pobreza municipal | XLSX |
| `Inegi Vehiculos/` | INEGI: ingresos de propietarios de vehículos por municipio | DIR |
| `inegi_mg_2024/` | Marco Geoestadístico INEGI 2024 (municipios + estados) | ZIP+SHP |
| `U.S._Exports_to_Mexico_of_Finished_Motor_Gasoline.csv` | EIA: exportaciones de gasolina EUA→México | CSV |

### Privadas (`data/raw_private/`)

| Archivo | Contenido | Formato |
|---|---|---|
| `stations/Stations.rda` | Catálogo CRE de gasolineras: ubicación, CVEGEO, región PEMEX | RDA |

---

## 4. Cómo correr el pipeline

```r
library(targets)

# Pipeline completo
tar_make()

# Solo una capa o target específico
tar_make(mun_month_prices_parquet)
tar_make(mun_elasticities_flag)
tar_make(pooled_regression_outputs)

# Diagnóstico
tar_manifest()          # lista de todos los targets
tar_visnetwork()        # grafo de dependencias en el navegador
tar_meta(fields = warnings, complete_only = TRUE)  # warnings activos
```

---

## 5. Capas del pipeline

### Capa 1A — Precios minoristas (retail)

**Factory:** `raw_to_processed()` | **Script:** `R/Raw_to_Processed/process_retail_year.R`

**Inputs:** `data/raw_public/prices_retail/Retail_YYYY.csv` (2017–2025)

**Transformaciones:**
- Parse robusto de fechas (múltiples formatos: YYYY-MM-DD, DD/MM/YYYY, YYYY/MM/DD)
- Selección automática de columna diesel (`diesel_automotriz` tiene prioridad)
- Coerción numérica de precios; flags por dato faltante, no positivo o fecha mala
- Detección de duplicados `station_id × date`
- Normalización de `station_id` via `station_id()` (elimina espacios, caracteres especiales)

**Outputs:** `data/processed/retail/year=YYYY/retail.parquet`

**Targets:** `retail_2017_parquet`, …, `retail_2025_parquet`

---

### Capa 1B — Precios de terminales PEMEX

**Factory:** `raw_to_processed()` | **Script:** `R/Raw_to_Processed/process_terminal_year.R`

**Input:** `data/raw_public/terminal_prices/Terminal.csv` (un solo archivo con todos los años)

**Transformaciones:**
- Canonización de nombre de terminal via `terminal_id()` (elimina prefijos, normaliza alias)
- Parse de fechas y coerción numérica con flags
- Filtro por año y escritura particionada

**Outputs:** `data/processed/terminal/year=YYYY/terminal.parquet`

**Targets:** `terminal_parquet` (vector de rutas)

---

### Capa 1C — Catálogo de estaciones

**Factory:** `raw_to_processed()` | **Script:** `R/Raw_to_Processed/process_stations.R`

**Input:** `data/raw_private/stations/Stations.rda`

**Transformaciones:**
- Extrae objeto `stations` del RDA en environment aislado
- Mapea `region_wholesale_pemex` → `terminal_id` via `terminal_id()`
- Construye `CVEGEO` (5 dígitos, zero-padded) desde `municode_map`
- Valida que `station_id` sea no-vacío y único

**Output:** `data/processed/stations/stations.parquet`

**Columnas:** `station_id`, `terminal_id`, `CVEGEO`, `estado`, `municipio`, `localidad`, `lat`, `lon`, flags

---

### Capa 1D — Precios internacionales

**Factory:** `raw_to_processed_int()` | **Script:** `R/Raw_to_Processed/process_international_prices.R`

**Inputs:** tres XLS (Regular USD/gal, Diesel USD/gal, Tipo de Cambio)

**Transformaciones:**
- Lee series diarias y hace merge por fecha
- Conversión: `mxn_per_l = (usd_per_gal × fx_mxn_usd) / 3.785411784`
- LOCF para días sin cotización
- Escritura particionada por año

**Outputs:** `data/processed/international/year=YYYY/international.parquet`

**Columnas:** `date`, `year`, `regular_int_mxn_l`, `diesel_int_mxn_l`, `fx_mxn_usd`

---

### Capa 1E — Marco Geoestadístico INEGI 2024

**Factory:** `analysis_to_map()` | **Script:** `R/Map/inegi_mg_2024_functions.R`

**Input:** `data/raw_public/inegi_mg_2024/` (ZIP con shapefiles)

**Transformaciones:**
- Descomprime y lee shapefiles de municipios (`00mun.shp`) y estados (`00ent.shp`)
- Convierte a GeoParquet
- Construye lookup: `CVEGEO → NOM_MUN, NOM_ENT, geometría`

**Outputs:**
- `data/map/inegi_mg_2024/unzipped/ONLY_MUNICIPIOS_00mun/00mun.shp`
- Targets: `inegi_municipios_geo`, `inegi_estados_geo`, `inegi_municipios_lookup`

---

### Capa 1F — Pobreza municipal (CONEVAL 2020)

**Factory:** `shaun_mun_month()` | **Script:** `R/Raw_to_Processed/process_coneval_poverty.R`

**Input:** `Indicadores_pobreza_grupos_municipal.xlsx`

**Output:** `data/processed/coneval/municipal_poverty_2020.parquet`

**Columnas:** `CVEGEO`, `poverty_rate`, `extreme_poverty`, `poverty_line`, y otros índices CONEVAL

---

### Capa 1G — Volúmenes de venta municipales

**Factory:** `shaun_mun_month()` | **Script:** `R/Raw_to_Processed/process_volumes.R`

**Input:** `data/raw_public/04_volumenes_venta_expendio_petroliferos.csv`

**Transformaciones:**
- Filtra filas de Regular y Premium
- Mapea nombres de estado → `CVE_ENT` (32 entradas, cobertura 100%)
- Normaliza nombres de municipio (sin acentos, UPPERCASE, solo alfanum) y hace join con shapefile INEGI por `(CVE_ENT, nombre_norm)`
- Overrides manuales para 3 discrepancias conocidas: San José Iturbide (GTO), Juchitán de Zaragoza (OAX), Solidaridad/Playa del Carmen (QRO)
- Descarta filas multi-municipio (~4.6% del total; no hay forma de asignar volumen sin supuestos arbitrarios)
- Cobertura de match esperada: ~99.5%

**Output:** `data/processed/volumes/mun_month_volumes.parquet`

**Columnas:** `CVEGEO`, `year`, `month`, `regular_volume_l`, `premium_volume_l`

---

### Capa 1H — Insumos para regresión pooled

**Factory:** `shaun_pooled_regression()`

| Target | Script | Input | Output |
|---|---|---|---|
| `ieps_monthly_parquet` | `process_ieps_combustibles.R` | `IEPS_Combustibles_Mexico.xlsx` | `data/processed/ieps/ieps_monthly.parquet` |
| `bloomberg_gasoline_parquet` | `process_gasoline_bloomberg.R` | `GASOLINE.xlsx` | `data/processed/bloomberg/gasoline_bloomberg.parquet` |
| `income_car_owners_parquet` | `process_inegi_vehiculos_income.R` | `data/raw_public/Inegi Vehiculos/` | `data/processed/inegi_vehiculos/municipal_income_car_owners.parquet` |

---

### Capa 2 — Panel estación × día (sin balance)

**Factory:** `processed_to_merged_panel()` | **Script:** `R/Processed_to_Merged/build_panel_station_day.R`

**Joins:**
```
retail  LEFT JOIN  stations    [por station_id]
        LEFT JOIN  terminal    [por terminal_id, date, year]
        LEFT JOIN  intl        [por date, year]
```

**Guard rail:** si <10% de `terminal_regular` son no-NA → error fatal (indica problema de matching de terminales).

**Output:** `data/merged/panel_station_day/year=YYYY/panel_station_day.parquet`

**Target:** `panel_station_day_parquets` (vector de rutas, 2017–2025)

---

### Capa 3 — Panel balanceado (LOCF)

**Factory:** `shaun_mun_month()` | **Script:** `R/Processed_to_Merged/build_balanced_panel.R`

**Descripción:** Garantiza una fila por cada par `(station_id, date)` para cada día calendario del año.

**Transformaciones:**
1. `expand_grid(station_id, date)` → grilla completa del año
2. Left-join de precios observados desde `panel_station_day`
3. LOCF (`tidyr::fill`) de precios minoristas dentro de cada `station_id`
4. Cap de 60 días: si `days_since_last_report > 60` → precio imputado se anula; `flag_stale_over_60d = TRUE`

**Output:** `data/merged/balanced_panel/year=YYYY/balanced_panel.parquet`

**Columnas clave:**

| Columna | Descripción |
|---|---|
| `station_id`, `date` | Llave primaria |
| `CVEGEO` | Código INEGI de municipio (5 dígitos) |
| `station_regular`, `station_premium`, `station_diesel` | Precios (NA si stale > 60 días) |
| `terminal_regular`, `terminal_premium`, `terminal_diesel` | Precios terminales (NA en filas sintéticas) |
| `regular_int_mxn_l`, `diesel_int_mxn_l` | Precios internacionales en MXN/L |
| `is_obs` | TRUE si el precio es observado directamente ese día |
| `flag_carry_forward` | TRUE si el precio viene de LOCF |
| `flag_stale_over_60d` | TRUE si el carry-forward supera 60 días (precio anulado) |
| `days_since_last_report` | Días desde el último reporte observado |

---

### Capa 4 — Spreads estación × día

**Factory:** `merged_to_analysis_spreads()` | **Script:** `R/Merged_to_Analysis/compute_spreads_station_day.R`

**Inputs:** `balanced_panel` (a través de `panel_station_day_parquets`)

**Spreads calculados (en MXN/L):**

| Variable | Definición | Interpretación |
|---|---|---|
| `spread_retail_terminal_regular` | precio estación − precio terminal | margen de la gasolinera (regular) |
| `spread_retail_terminal_premium` | precio estación − precio terminal | margen de la gasolinera (premium) |
| `spread_retail_terminal_diesel` | precio estación − precio terminal | margen de la gasolinera (diesel) |
| `spread_terminal_int_regular` | precio terminal − precio intl | margen de PEMEX terminal (regular) |
| `spread_terminal_int_diesel` | precio terminal − precio intl | margen de PEMEX terminal (diesel) |
| `spread_retail_int_regular` | precio estación − precio intl | margen total de la cadena (regular) |
| `spread_retail_int_diesel` | precio estación − precio intl | margen total de la cadena (diesel) |

**Output:** `data/analysis/spreads_station_day/year=YYYY/spreads_station_day.parquet`

---

### Capa 5 — Precios municipio × mes (double-average de Shaun)

**Factory:** `shaun_mun_month()` | **Script:** `R/Analysis/mun_month_functions.R`

**Método de agregación (dos pasos):**

**Paso 1 — Estaciones → Municipio × Día:**
```
Para cada (CVEGEO, date):
  mun_avg = mean(station_regular, station_premium, station_diesel)
  — cada estación pesa igual
  — se excluyen precios con flag_stale_over_60d = TRUE
```

**Paso 2 — Municipio × Día → Municipio × Mes:**
```
Para cada (CVEGEO, year, month):
  price_monthly = mean(mun_avg por día)
  — cada día pesa igual (evita sesgo si varía el nº de estaciones)
```

**Join de volúmenes:**
- Une `mun_month_volumes.parquet` por `(CVEGEO, year, month)`
- Calcula `premium_share = premium_volume_l / (regular_volume_l + premium_volume_l)`
- Si el archivo de volúmenes no existe, las columnas de volumen quedan en NA con mensaje de advertencia

**Output:** `data/analysis/mun_month_prices/mun_month_prices.parquet`

**Columnas:**

| Columna | Descripción |
|---|---|
| `CVEGEO` | Código INEGI de municipio (5 dígitos) |
| `CVE_ENT` | Código de entidad (2 dígitos) |
| `NOM_ENT` | Nombre del estado (INEGI oficial) |
| `NOM_MUN` | Nombre del municipio (shapefile 00mun.shp) |
| `year`, `month` | Año y mes |
| `regular_price_monthly` | Precio mensual gasolina regular |
| `premium_price_monthly` | Precio mensual gasolina premium |
| `premium_to_regular_price_ratio` | `premium_price_monthly / regular_price_monthly` |
| `regular_volume` | Volumen vendido de regular en litros (CRE/SENER) |
| `premium_volume` | Volumen vendido de premium en litros (CRE/SENER) |
| `premium_share` | Participación del premium en el volumen total |
| `n_days_in_month` | Días con al menos una estación activa en el municipio |
| `n_days_with_regular` | Días con precio regular no-NA |
| `n_days_with_premium` | Días con precio premium no-NA |

---

### Capa 6 — Enriquecimiento con pobreza

**Factory:** `shaun_mun_month()` | **Script:** `R/Analysis/mun_month_functions.R`

**Join:** `mun_month_prices` LEFT JOIN `municipal_poverty_2020` por `CVEGEO`

**Output:** `data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet`

Agrega las columnas de CONEVAL 2020: `poverty_rate`, `extreme_poverty`, `poverty_line`, etc.

---

### Capa 7 — Agregaciones pre/post reforma

**Factory:** `analysis_aggregations()` | **Script:** `R/Analysis/agg_functions.R`

**Fecha de reforma:** `2025-03-03` (definida en `agg_functions.R`)

**Targets generados:**

| Target | Granularidad | Ventana |
|---|---|---|
| `daily_cvegeo_parquets` | CVEGEO × día | — |
| `prepost_cvegeo_parquet_1m/3m/6m` | CVEGEO × período | 1, 3, 6 meses |
| `prepost_station_spreads_parquet_1m/3m/6m` | station_id × período | 1, 3, 6 meses |
| `prepost_station_prices_parquet_1m/3m/6m` | station_id × período | 1, 3, 6 meses |
| `station_regular/premium/diesel_quantiles_parquet_1m/3m/6m` | station_id × cuartil | 1, 3, 6 meses |

Los cuartiles se definen sobre la distribución del período **pre-reforma** (Q1–Q4 por distribución de precio pre).

---

### Capa 8 — Elasticidades de demanda × pobreza

**Factory:** `elasticity_poverty()` | **Script:** `R/Analysis/elasticity_poverty.R`

**Input:** `mun_month_prices_with_poverty.parquet`

**Dos especificaciones:**

| Target | FE temporal | Identificación |
|---|---|---|
| `mun_elasticities_flag` | Año | Variación mensual + estacional |
| `mun_elasticities_yr_month_flag` | Año-mes | Solo variación idiosincrática municipal |

**Parámetros:** mínimo 12 observaciones por municipio, winsorización 2%, bin width 2

**Outputs en `outputs/shaun/elasticity/`:**
- `elasticity_summary.xlsx`
- `form1_scatter.pdf`, `form1_bins.pdf`, `form2_gam.pdf`
- `data/analysis/elasticity/mun_elasticities.parquet`
- `elasticity_poverty_bins.parquet` y `.csv`

---

### Capa 9 — Regresiones pooled municipio × mes

**Factory:** `shaun_pooled_regression()` | **Script:** `R/Analysis/shaun_pooled_regression.R`

**Input:** `mun_month_prices_with_poverty.parquet` + IEPS + Bloomberg + ingresos INEGI

**Dos muestras:**

| Target | Muestra |
|---|---|
| `pooled_regression_outputs` | Todos los municipios |
| `pooled_regression_restricted_outputs` | Excluye Chiapas (07), Guerrero (12), Oaxaca (20) y Puebla (21) por mercados informales de gasolina |

**Outputs:** `outputs/shaun/pooled_regression/` y `outputs/shaun/pooled_regression_restricted/`

---

### Capa 10 — Brecha de importaciones EUA→México

**Factory:** `gasoline_imports_gap()` | **Script:** `R/Raw_to_Processed/process_gasoline_imports_gap.R`

**Input:** `data/raw_public/U.S._Exports_to_Mexico_of_Finished_Motor_Gasoline.csv` (EIA)

**Outputs:**
- `data/processed/imports_gap/gasoline_imports_gap.parquet`
- `outputs/imports_gap/import_gap_plots.pdf`
- `outputs/imports_gap/interpretation_note.txt`

---

### Capa 11 — Gráficos y visualizaciones

**Factory:** `analysis_to_graphs()` | **Script:** `R/Graphs/graphs_outputs_functions.R`

| Target | Descripción | Output |
|---|---|---|
| `national_price_graphs_png` | Series de tiempo de precios nacionales 2017–2025 | `outputs/graphs/national_prices/` |
| `station_spread_distributions_png_*m` | Distribuciones de spreads pre/post por ventana | `outputs/graphs/window=N/station_spreads/` |
| `station_price_distributions_png_*m` | Distribuciones de precios pre/post por ventana | `outputs/graphs/window=N/station_prices/` |
| `station_*_quantile_overlays_png_*m` | Overlays de distribuciones por cuartil PRE | `outputs/graphs/window=N/station_price_quantiles/` |

---

### Capa 12 — Transiciones de precios 2024–2025

**Factory:** `analysis_station_price_transitions()` | **Script:** `R/analysis_station_price_transitions.R`

Analiza cómo cambiaron los precios por estación en torno a la reforma de marzo 2025.

---

### Capa 13 — Mapas de calor

**Factory:** `analysis_to_map_heatmaps()` | **Script:** `R/Map/spread_heatmaps_functions.R`

Genera mapas municipales de spreads usando el Marco Geoestadístico INEGI 2024.

---

## 6. Joins y llaves principales

| Join | Llave | Tipo |
|---|---|---|
| retail ← stations | `station_id` | LEFT |
| panel ← terminal | `terminal_id`, `date`, `year` | LEFT |
| panel ← internacional | `date`, `year` | LEFT |
| balanced_panel → mun×día | `CVEGEO`, `date` | GROUP BY |
| mun×día → mun×mes | `CVEGEO`, `year`, `month` | GROUP BY |
| mun×mes ← volúmenes | `CVEGEO`, `year`, `month` | LEFT |
| mun×mes ← CONEVAL | `CVEGEO` | LEFT (cross-seccional) |
| mun×mes ← IEPS | `year`, `month` | LEFT |
| mun×mes ← Bloomberg | `year`, `month` | LEFT |
| mun×mes ← ingresos INEGI | `CVEGEO` | LEFT |
| volúmenes ← INEGI MG | `CVE_ENT`, nombre normalizado | INNER |

**Normalización de texto para matching de nombres:**
- `iconv(x, to = "ASCII//TRANSLIT")` → quita acentos
- `toupper()` → mayúsculas
- `gsub("[^A-Z0-9 ]", " ", x)` → solo alfanum + espacios
- `trimws()` + `gsub("\\s+", " ", x)` → espacios simples

---

## 7. Flags de calidad

| Flag | Dónde aparece | Significado |
|---|---|---|
| `flag_bad_date` | retail, terminal | Fecha no parseada correctamente |
| `flag_missing_any_price` | retail, terminal | Al menos un precio es NA |
| `flag_nonpositive_any_price` | retail, terminal | Al menos un precio ≤ 0 |
| `flag_dup_station_day` | retail | Duplicado por `station_id × date` |
| `flag_dup_terminal_day` | terminal | Duplicado por `terminal_id × date` |
| `flag_missing_terminal_id` | stations | `terminal_id` resultó NA |
| `flag_missing_cvegeo_mun` | stations | CVEGEO ausente, `"000NA"` o `"00000"` |
| `flag_carry_forward` | balanced_panel | Precio imputado por LOCF |
| `flag_stale_over_60d` | balanced_panel | LOCF supera 60 días (precio anulado) |

Estaciones con `flag_missing_cvegeo_mun = TRUE` quedan excluidas de todas las agregaciones municipales.

---

## 8. Parámetros globales del pipeline

| Parámetro | Valor | Ubicación |
|---|---|---|
| Años del panel | 2017–2025 | `targets/raw_to_processed.R` |
| Fecha de reforma | `2025-03-03` | `R/Analysis/agg_functions.R` |
| Cap LOCF (días) | 60 | `R/Processed_to_Merged/build_balanced_panel.R` |
| Mínimo obs para elasticidad | 12 | `targets/elasticity_poverty.R` |
| Winsorización elasticidad | 2% | `targets/elasticity_poverty.R` |
| Guard rail cobertura terminal | 10% | `R/Processed_to_Merged/build_panel_station_day.R` |
| Estados excluidos (muestra restringida) | 07, 12, 20, 21 | `targets/shaun_pooled_regression.R` |

---

## 9. Checklist para máquina nueva

1. Instalar R (≥ 4.2) y dependencias de sistema para `sf` (GDAL, PROJ, GEOS)
2. Instalar paquetes R:
   ```r
   install.packages(c(
     "targets", "tarchetypes",
     "dplyr", "tidyr", "purrr", "readr", "readxl", "arrow",
     "tibble", "stringr", "lubridate",
     "sf", "ggplot2", "scales", "grid", "gridExtra",
     "openxlsx", "fixest", "mgcv", "broom", "modelsummary"
   ))
   ```
3. Verificar que existan los inputs crudos en `data/raw_public/` y `data/raw_private/`
4. Correr desde la raíz del proyecto:
   ```r
   library(targets)
   tar_make()
   ```
