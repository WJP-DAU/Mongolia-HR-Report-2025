# Mongolia HR Report 2025

Repository for the World Justice Project 2025 Mongolia Human Rights Report HTML/PDF.

## Structure

```text
.
├── html/
│   ├── index.html                         # Main HTML report
│   └── charts_and_images/                 # SVG charts used by the HTML
├── data-viz/
│   ├── main.R                             # Main R chart pipeline
│   ├── code/
│   │   ├── settings.R                     # Configuration, fonts, and export helpers
│   │   ├── wrangleData.R                  # Data preparation
│   │   └── data_viz.R                     # Visualization functions
│   ├── inputs/                            # Local data files ignored by Git
│   └── outputs/                           # Generated outputs ignored by Git
└── README.md
```

## Workflow

1. Edit report content in `html/index.html`.
2. Edit chart logic in `data-viz/code/`.
3. Regenerate charts from `data-viz/` with:

```bash
Rscript main.R
```

4. Copy generated SVG files from `data-viz/outputs/` to the paths used by the HTML:

```text
F1.svg   -> html/charts_and_images/imgChart1/F1.svg
F2.svg   -> html/charts_and_images/imgChart1/F2.svg
F3.svg   -> html/charts_and_images/imgChart2/F3.svg
F4.svg   -> html/charts_and_images/imgChart2/F4.svg
F5.svg   -> html/charts_and_images/imgChart3/F5.svg
F6_A.svg -> html/charts_and_images/imgChart3/F6_A.svg
F6_B.svg -> html/charts_and_images/imgChart3/F6_B.svg
F7.svg   -> html/charts_and_images/imgChart4/F7.svg
F8.svg   -> html/charts_and_images/imgChart4/F8.svg
```

## Versioning Notes

`.gitignore` ignores `*.html`, `*.svg`, `*.md`, and other generated or binary file types. This repository still explicitly versions the files required to publish the report:

- `html/index.html`
- `html/charts_and_images/**/F*.svg`
- `README.md`
- Main R scripts in `data-viz/code/` and `data-viz/main.R`

Use `git add -f` when staging ignored files that are required for the report.

## Requirements

- R 4.x
- R packages loaded by `data-viz/code/settings.R`, including `tidyverse`, `openxlsx`, `ggtext`, `showtext`, `sysfonts`, `haven`, `writexl`, `janitor`, `margins`, `kableExtra`, `glue`, and `pacman`.
- Lato fonts available at the path configured in `settings.R`.

## Author

World Justice Project - Data Analytics Unit.
