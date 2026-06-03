# Mongolia HR Report 2025 - Data Visualizations

Documentation for the R pipeline that generates charts for the 2025 Mongolia Human Rights Report.

## Purpose

The pipeline generates SVG charts comparing Mongolia with regional peers across selected WJP Rule of Law Index indicators. Outputs are created in `data-viz/outputs/` and, when used in the report, must be copied into `html/charts_and_images/`.

## Comparison Countries

- Kazakhstan
- Hong Kong SAR, China
- China
- Kyrgyz Republic
- Uzbekistan
- Nepal
- Korea, Rep.
- Japan

## Chart Types

| Type | Function | Use |
| --- | --- | --- |
| Dumbbell | `genDumbell()` | Comparison of Mongolia against regional peers by sub-factor |
| Radar | `wjp_radar()` | Multi-metric comparison by year |
| Bars | `genBars()` | Indicator trends over time |

For dumbbell charts, Mongolia values are drawn with `geom_text()` in `code/data_viz.R`; those labels use `fontface = "bold"` to keep the number above Mongolia's marker in bold.

## Structure

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

## Running The Pipeline

From the `data-viz/` directory:

```bash
Rscript main.R
```

The script:

1. Loads configuration, fonts, and functions from `code/settings.R`, `code/wrangleData.R`, and `code/data_viz.R`.
2. Reads input files from `inputs/`.
3. Builds figure-specific datasets.
4. Generates the figures defined in `inputs/Mongolia_outline.xlsx`.
5. Saves SVG files in `outputs/`.

## Copying Charts To The HTML

After regenerating charts, copy the outputs into the HTML asset folders:

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

## Versioning

Files in `data-viz/outputs/` are ignored by Git. Published SVG files are the copies stored under `html/charts_and_images/`; they must be staged with `git add -f` because `.gitignore` ignores `*.svg`.

Input data files are also ignored by Git. Do not commit private data files or generated files that are not required for the final report.

## Requirements

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

`settings.R` attempts to load `WJPr`; if unavailable, it may emit warnings. In the latest successful run, those warnings did not prevent generating `F1.svg` through `F8.svg`.

## Configuration

The main country and comparison list are defined in `main.R`:

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

Sub-factor labels are defined in `code/wrangleData.R`. The current convention uses sentence case in line with the report corrections.
