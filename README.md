# Escala DAVIS

App Shiny para analizar datos de escala Davis 0-9 desde tablas Scout con conteos por clase.

## Qué hace

- Carga Excel, CSV o TXT.
- Permite mapear columnas si los nombres cambian.
- Filtra `se_name = ES11AD2` y `assessment_type_code = COUNT`.
- Excluye `ABBOTT` del análisis.
- Clasifica Davis como:
  - 0 = Sin Daño
  - 1-2 = Low Damage
  - 3-6 = Medium Damage
  - 7-9 = High Damage
- Valida que `sum(assessment_value) = sample_size` por `trial + assessment_timing_code + treatment + replicate_number`.
- Genera análisis overall, por localidad y por grupos manuales de trials.
- Mantiene modelo ordinal con `ordinal::clm()` usando `weights = assessment_value`.
- Exporta PowerPoint.

## Estructura recomendada

```text
app.R
packages.txt
README.md
www/
  styles.css
  logo_bayer.jpg   # opcional
```

## Cómo correr localmente

```r
install.packages(readLines("packages.txt"))
shiny::runApp()
```

## Publicar en shinyapps.io

```r
install.packages("rsconnect")
rsconnect::deployApp()
```

Antes de publicar, probar localmente con el Excel modelo.
