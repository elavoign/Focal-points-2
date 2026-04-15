YA escribe 
# Gasoline Spreads (Mexico) — Station × Day Panel + Map-Ready CVEGEO

Este repo construye un pipeline **targets** (modular y reproducible) para generar:

1) un **panel estación × día** con precios retail, terminal e internacional,  
2) **spreads estación × día** (retail–terminal, terminal–internacional, retail–internacional), y  
3) un dataset **listo para mapas** enriquecido con **CVEGEO** (INEGI Marco Geoestadístico).

**Unidad final de análisis:** `station_id × date`  
**Llave principal:** `(station_id, date)`

---

## 0) TL;DR (qué obtienes al final)

### Producto para análisis económico
- `data/analysis/spreads_station_day/year=YYYY/spreads_station_day.parquet`

### Producto para mapas (con CVEGEO)
- `data/map/spreads_station_day/year=YYYY/spreads_station_day_with_cvegeo.parquet`

---

## 1) Estructura del repo (lo importante)

- **`_targets.R`**: orquesta el pipeline completo.
- **`targets/`**: “factories” (grupos de targets) por capa.
- **`R/`**: funciones (lógica), sin ejecutar nada al cargar.
  - `R/Raw_to_Processed/`: limpieza y estandarización de fuentes.
  - `R/Processed_to_Merged/`: construcción del panel estación×día.
  - `R/Merged_to_Analysis/`: cómputo de spreads.
  - `R/Map/`: Marco Geoestadístico y enriquecimiento CVEGEO (map-ready).

Carpetas de datos:
- `data/raw_public/` : fuentes públicas (retail, terminal, internacional, INEGI MG).
- `data/raw_private/` : fuentes privadas (Stations.rda).
- `data/processed/` : salidas “limpias” por capa.
- `data/merged/` : panel consolidado por año.
- `data/analysis/` : spreads por año.
- `data/map/` : spreads con CVEGEO por año.

---

## 2) Cómo correr el pipeline

### Pipeline completo
En una sesión limpia de R, en la raíz del proyecto:

```r
library(targets)
tar_make()

Solo el Marco Geoestadístico (municipios)
tar_make(municipios_geo)

Solo generar el dataset map-ready (spreads + CVEGEO)
tar_make(spreads_with_cvegeo)

Ver el grafo de dependencias
tar_visnetwork()

3) _targets.R (master pipeline)

El archivo maestro hace:

tar_option_set(packages = ...) para declarar paquetes requeridos

tar_source("R") para cargar funciones

tar_source("targets") para cargar factories

ejecuta en orden lógico:

c(
  raw_to_processed(),
  raw_to_processed_int(),
  raw_to_processed_map(),   
  processed_to_merged_panel(),
  merged_to_analysis_spreads(),
  analysis_to_map()
)

4) Capas del pipeline y outputs
Capa 1: RAW → PROCESSED (precios y metadatos)
1A) Retail (por año)

Inputs:

data/raw_public/prices_retail/Retail_2017.csv

…

data/raw_public/prices_retail/Retail_2025.csv

Procesamiento:

parse robusto de fechas (parse_date_safe)

selección robusta de columna diesel (choose_diesel_column)

coerción numérica con flags de calidad (missing, nonpositive, bad_date)

detección de duplicados station_id × date

Outputs:

data/processed/retail/year=YYYY/retail.parquet (targets explícitos 2017–2025)

Targets:

retail_2017_parquet, …, retail_2025_parquet

1B) Terminal (1 CSV grande → parquets por año)

Input:

data/raw_public/terminal_prices/Terminal.csv

Procesamiento:

parse robusto de fecha

coerción numérica + flags

split por year y escritura por partición:

data/processed/terminal/year=YYYY/terminal.parquet

Output:

terminal_parquet (regresa vector de rutas)

1C) Stations (metadata / crosswalk)

Input (privado):

data/raw_private/stations/Stations.rda

Procesamiento:

carga aislada en environment dedicado (evita contaminación)

se espera un objeto llamado stations

construye crosswalk con:

station_id, numero_permiso

terminal_id (coalesce de varios campos pemex/cre*)

estado, municipio, localidad

lat, lon

genera versiones normalizadas tipo “Python”:

normalize_text() (ASCII, upper, solo alfanum)

validaciones: station_id no vacío y único

Output:

data/processed/stations/stations.parquet (target stations_parquet)

Capa 2: RAW → PROCESSED_INT (precios internacionales)

Inputs:

data/raw_public/international_prices/Regular_Dolars_per_Galon.xls

data/raw_public/international_prices/Disel_Dolars_per_Galon.xls

data/raw_public/international_prices/Tipo_de_Cambio.xls

Transformación:

lee series diarias desde Excel

merge por date

convierte:

USD/gal → MXN/L usando 1 gal = 3.785411784 L

mxn_per_l = (usd_per_gal * fx_mxn_usd) / 3.785411784

crea columnas:

regular_int_mxn_l, diesel_int_mxn_l, fx_mxn_usd

escribe por año:

Outputs:

data/processed/international/year=YYYY/international.parquet

Targets:

international_processed (list con data + paths)

international_parquet_paths (tibble year/path)

Capa 3: PROCESSED → MERGED (panel estación × día)

Objetivo:
Construir el panel final con joins reales:

Retail ⟵ Stations por station_id

Terminal por terminal_id + date + year

International por date + year

Función principal:

build_panel_station_day_year(year, out_dir)

Output por año:

data/merged/panel_station_day/year=YYYY/panel_station_day.parquet

Target:

panel_station_day_parquets (vector de rutas; years = 2017:2025)

Contenido típico del panel:

IDs: station_id, numero_permiso, terminal_id

geo/texto: estado, municipio, localidad, lat, lon

precios estación: station_regular, station_premium, station_diesel

precios terminal: terminal_regular, terminal_premium, terminal_diesel

internacional: regular_int_mxn_l, diesel_int_mxn_l, fx_mxn_usd

flags de calidad (station/terminal)

Capa 4: MERGED → ANALYSIS (spreads estación × día)

Función:

compute_spreads_station_day_year(year, out_dir)

Calcula:

Retail − Terminal

spread_retail_terminal_regular

spread_retail_terminal_premium

spread_retail_terminal_diesel

Terminal − Internacional

spread_terminal_int_regular

spread_terminal_int_diesel

Retail − Internacional

spread_retail_int_regular

spread_retail_int_diesel

Output por año:

data/analysis/spreads_station_day/year=YYYY/spreads_station_day.parquet

Target:

spreads_station_day_parquets (vector de rutas; years = 2017:2025)

Capa 5: RAW → PROCESSED_MAP (Marco Geoestadístico INEGI)

Input:

data/raw_public/inegi_mg_2024/794551163061_s.zip

Dentro del zip grande, se usa:

mg_2025_integrado.zip

Shapefiles:

Municipios: conjunto_de_datos/00mun.shp

Entidades: conjunto_de_datos/00ent.shp

Procesamiento:

lee con sf::st_read()

construye tabla municipio:

CVEGEO, CVE_ENT, CVE_MUN

estado, municipio

estado_norm, municipio_norm usando normalize_text

padding:

CVE_ENT a 2 dígitos, CVE_MUN a 3 dígitos

Output:

data/processed/geo/municipios.parquet

Target:

municipios_geo

Capa 6: ANALYSIS → MAP (spreads + CVEGEO)

Problema:

Enriquecer spreads con CVEGEO sin reventar memoria.

Estrategia:

Enumerar archivos de spreads en data/analysis/spreads_station_day/

Convertir rutas a branches con format="file"

Procesar archivo por archivo con dynamic branching

Targets:

spreads_file_paths: lista de rutas (no file)

spreads_files: branching de archivos (file)

spreads_with_cvegeo: branching final (file)

Función:

add_cvegeo_to_spreads_one(spreads_file, geo_file, out_dir)

Join:

estado_norm + municipio_norm

Output por año (map-ready):

data/map/spreads_station_day/year=YYYY/spreads_station_day_with_cvegeo.parquet

5) Llaves, joins y supuestos operativos
Unidad y llave

Unidad: station_id × date

Llave: (station_id, date)

Joins principales

Retail → Stations: station_id

Panel → Terminal: (terminal_id, date, year)

Panel → International: (date, year)

Spreads → CVEGEO: (estado_norm, municipio_norm)

Normalización para matching (texto)

Se usa normalize_text() para:

quitar acentos (ASCII)

uppercase

eliminar todo excepto [A-Z0-9]

Esto se usa para:

estado_norm, municipio_norm, localidad_norm

station_id_norm, terminal_id_norm

6) Flags de calidad (data quality)

En retail / terminal:

flag_bad_date

flag_missing_any_price

flag_nonpositive_any_price

retail además:

flag_dup_station_day

En panel/spreads se preservan flags para diagnóstico.

7) Reproducibilidad y diseño del pipeline (targets)

Principios usados:

targets de scripts como format="file" para invalidación correcta si cambia código.

outputs pesados guardados como parquet con compression = "zstd" (cuando aplica).

panel y spreads por año (reduce memoria y acelera incremental).

branching por archivo para map-ready CVEGEO (evita cargar todo en RAM).

Notas útiles de targets (para depurar):

tar_manifest()

tar_visnetwork()

tar_meta(fields = warnings, complete_only = TRUE)

(Para contexto general de buenas prácticas con targets, ver tus notas: 

Notas De Lectura_ Targets (r Op…

)

8) Checklist rápido para correr en una máquina nueva

Tener R y dependencias del sistema (especialmente para sf)

Instalar paquetes R:

targets, dplyr, readxl, lubridate, arrow, tibble, stringr, sf, readr, stringi

Verificar que existan los raw inputs en:

data/raw_public/...

data/raw_private/stations/Stations.rda

Correr:

tar_make()