# Mongolia HR Report 2025 - Data Visualizations

Documentacion del pipeline R que genera las graficas del reporte de derechos humanos de Mongolia 2025.

## Objetivo

El pipeline genera graficas SVG para comparar a Mongolia con pares regionales en indicadores seleccionados del WJP Rule of Law Index. Las salidas se crean en `data-viz/outputs/` y, cuando se usan en el reporte, deben copiarse a `html/charts_and_images/`.

## Paises De Comparacion

- Kazakhstan
- Hong Kong SAR, China
- China
- Kyrgyz Republic
- Uzbekistan
- Nepal
- Korea, Rep.
- Japan

## Tipos De Graficas

| Tipo | Funcion | Uso |
| --- | --- | --- |
| Dumbbell | `genDumbell()` | Comparacion de Mongolia contra pares regionales por sub-factor |
| Radar | `wjp_radar()` | Comparacion multi-metrica por ano |
| Bars | `genBars()` | Evolucion temporal de indicadores |

En las graficas dumbbell, los valores de Mongolia se dibujan con `geom_text()` en `code/data_viz.R`; esos labels usan `fontface = "bold"` para mantener el numero sobre la bola de Mongolia en negrilla.

## Estructura

```text
data-viz/
├── main.R
├── code/
│   ├── settings.R
│   ├── wrangleData.R
│   └── data_viz.R
├── inputs/
│   ├── Mongolia_outline.xlsx
│   └── FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx
└── outputs/
    ├── F1.svg
    ├── F2.svg
    ├── F3.svg
    ├── F4.svg
    ├── F5.svg
    ├── F6_A.svg
    ├── F6_B.svg
    ├── F7.svg
    └── F8.svg
```

## Ejecucion

Desde la carpeta `data-viz/`:

```bash
Rscript main.R
```

El script:

1. Carga configuracion, fuentes y funciones desde `code/settings.R`, `code/wrangleData.R` y `code/data_viz.R`.
2. Lee los datos desde `inputs/`.
3. Construye los datasets por figura.
4. Genera las figuras definidas en `inputs/Mongolia_outline.xlsx`.
5. Guarda los SVG en `outputs/`.

## Copia Al HTML

Despues de regenerar las graficas, copiar las salidas al HTML:

```bash
cp outputs/F1.svg ../html/charts_and_images/imgChart1/F1.svg
cp outputs/F2.svg ../html/charts_and_images/imgChart1/F2.svg
cp outputs/F3.svg ../html/charts_and_images/imgChart2/F3.svg
cp outputs/F4.svg ../html/charts_and_images/imgChart2/F4.svg
cp outputs/F5.svg ../html/charts_and_images/imgChart3/F5.svg
cp outputs/F6_A.svg ../html/charts_and_images/imgChart3/F6_A.svg
cp outputs/F6_B.svg ../html/charts_and_images/imgChart3/F6_B.svg
cp outputs/F7.svg ../html/charts_and_images/imgChart4/F7.svg
cp outputs/F8.svg ../html/charts_and_images/imgChart4/F8.svg
```

## Versionamiento

Los archivos de `data-viz/outputs/` estan ignorados por Git. Los SVG que se publican son los copiados a `html/charts_and_images/` y se agregan con `git add -f` porque `.gitignore` ignora `*.svg`.

Los inputs de datos tambien estan ignorados por Git. No subir archivos de datos privados o archivos generados que no sean necesarios para el reporte final.

## Requisitos

- R 4.x
- `pacman`
- `tidyverse`
- `openxlsx`
- `ggtext`
- `showtext`
- `sysfonts`
- `haven`
- `writexl`
- `janitor`
- `margins`
- `kableExtra`
- `glue`

`settings.R` intenta cargar `WJPr`; si no esta disponible, puede emitir advertencias. En la ultima ejecucion, esas advertencias no impidieron generar `F1.svg` a `F8.svg`.

## Configuracion

El pais principal y la lista de comparacion se definen en `main.R`:

```r
MAIN_COUNTRY <- "Mongolia"
COMP_COUNTRIES <- c(
  "Kazakhstan",
  "Hong Kong SAR, China",
  "China",
  "Kyrgyz Republic",
  "Uzbekistan",
  "Nepal",
  "Korea, Rep.",
  "Japan"
)
```

Las etiquetas de sub-factores se definen en `code/wrangleData.R`. La convencion actual usa sentence case de acuerdo con las correcciones del reporte.
