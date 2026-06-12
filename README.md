# Escala DAVIS

Aplicación Shiny para análisis corporativo de Escala DAVIS 0-9 con datos tipo Scout.

## Novedades v4

- Pestaña **Información requerida** con todos los encabezados recomendados para construir la query.
- Carga por archivo o **copiar y pegar directo desde Excel/Google Sheets**.
- Tabla descriptiva separada del gráfico para evitar superposición.
- Exportación PowerPoint ajustada: gráficos en slides independientes y tablas en slides separadas.
- Modelo ordinal `ordinal::clm()` ponderado por `assessment_value`.

## Columnas recomendadas

- `trial`
- `se_name`
- `assessment_type_code`
- `assessment_timing_code`
- `treatment`
- `treatment_mod`
- `replicate_number`
- `assessment_class`
- `assessment_value`
- `sample_size`

La app filtra `se_name = ES11AD2` y `assessment_type_code = COUNT`.

## Ejecutar local

```r
install.packages(readLines("packages.txt"))
shiny::runApp()
```

## Publicar

```r
rsconnect::deployApp()
```
