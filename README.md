# Escala DAVIS

App Shiny para analizar datos Scout de escala Davis 0-9 a partir de conteos por clase.

## Qué hace

- Carga `.xlsx`, `.xls`, `.csv` o `.txt`.
- Permite mapear columnas si los nombres cambian.
- Filtra `se_name = ES11AD2` y `assessment_type_code = COUNT`.
- Excluye `ABBOTT` porque corresponde a otro tipo de dato.
- Clasifica Davis como:
  - `0 = Sin Daño`
  - `1-2 = Low Damage`
  - `3-6 = Medium Damage`
  - `7-9 = High Damage`
- Valida que, para cada `trial + assessment_timing_code + treatment + replicate_number`, la suma de `assessment_value` sea igual a `sample_size`.
- Genera salidas overall, por localidad y por grupos manuales de trials.
- Mantiene modelo ordinal con `ordinal::clm()` usando `weights = assessment_value`.
- Genera letras Tukey sobre score Davis ponderado.
- Exporta PowerPoint.

## Archivos

```text
app.R
packages.txt
README.md
www/
  styles.css
  logo_bayer.jpg
```

## Cómo correrla en tu computadora

1. Instalá R: https://cran.r-project.org/
2. Instalá RStudio Desktop: https://posit.co/download/rstudio-desktop/
3. Descargá este repo y abrí `app.R` en RStudio.
4. En la consola de RStudio ejecutá:

```r
install.packages(readLines("packages.txt"))
```

5. Después ejecutá:

```r
shiny::runApp()
```

La app se abre en el navegador.

## Publicar online

La forma más simple es usar shinyapps.io.

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="TU_USUARIO", token="TU_TOKEN", secret="TU_SECRET")
rsconnect::deployApp()
```

El token y secret se copian desde tu cuenta de shinyapps.io.
