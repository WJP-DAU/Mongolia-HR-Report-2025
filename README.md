# Mongolia HR Report 2025

Repositorio del reporte HTML/PDF de derechos humanos de Mongolia 2025 del World Justice Project.

## Estructura

```text
.
├── html/
│   ├── index.html                         # Reporte HTML principal
│   └── charts_and_images/                 # SVGs usados por el HTML
├── data-viz/
│   ├── main.R                             # Pipeline principal de graficas R
│   ├── code/
│   │   ├── settings.R                     # Configuracion, fuentes y guardado
│   │   ├── wrangleData.R                  # Preparacion de datos
│   │   └── data_viz.R                     # Funciones de visualizacion
│   ├── inputs/                            # Archivos de datos locales ignorados por Git
│   └── outputs/                           # Salidas generadas ignoradas por Git
└── README.md
```

## Flujo De Trabajo

1. Editar el contenido del reporte en `html/index.html`.
2. Editar la logica de graficas en `data-viz/code/`.
3. Regenerar graficas desde `data-viz/` con:

```bash
Rscript main.R
```

4. Copiar los SVG generados desde `data-viz/outputs/` a las rutas usadas por el HTML:

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

## Notas De Versionamiento

`.gitignore` ignora archivos `*.html`, `*.svg`, `*.md`, entre otros. Sin embargo, este repositorio versiona de forma explicita los archivos necesarios para publicar el reporte:

- `html/index.html`
- `html/charts_and_images/**/F*.svg`
- `README.md`
- scripts R principales en `data-viz/code/` y `data-viz/main.R`

Para agregar cambios en archivos ignorados que forman parte del reporte, usar `git add -f`.

## Cambios Recientes

- Se alineo el contenido del HTML con `html/WJPMongolia2025_Corrections.docx`.
- Se corrigieron nombres, titulos de graficas y texto de capitulos para Mongolia.
- Se mantuvo `Korea, Rep.` en leyendas de graficas.
- Se actualizaron labels de graficas dumbbell y se restauro negrilla en los valores de Mongolia.
- Se actualizaron los SVG del HTML generados desde R.

## Requisitos

- R 4.x
- Paquetes R usados por `data-viz/code/settings.R`, incluyendo `tidyverse`, `openxlsx`, `ggtext`, `showtext`, `sysfonts`, `haven`, `writexl`, `janitor`, `margins`, `kableExtra`, `glue` y `pacman`.
- Fuentes Lato disponibles en la ruta configurada en `settings.R`.

## Autor

World Justice Project - Data Analytics Unit.
