library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ordinal)
library(emmeans)
library(multcompView)
library(officer)
library(DT)
library(scales)

options(shiny.maxRequestSize = 100 * 1024^2)

DAVIS_LEVELS <- c("Sin Daño", "Low Damage", "Medium Damage", "High Damage")
DAVIS_COLORS <- c("Sin Daño" = "#D1D5DB", "Low Damage" = "#66B512", "Medium Damage" = "#F59E0B", "High Damage" = "#EF4444")

find_col <- function(cols, candidates) {
  hit <- candidates[candidates %in% cols]
  if (length(hit) > 0) hit[1] else cols[1]
}

safe_num <- function(x) suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", ".")))

read_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext == "csv") {
    read.csv(path, check.names = FALSE)
  } else {
    read.delim(path, check.names = FALSE)
  }
}

parse_davis_class <- function(x) {
  suppressWarnings(as.numeric(str_extract(as.character(x), "[0-9]+")))
}

classify_davis <- function(score) {
  case_when(
    score == 0 ~ "Sin Daño",
    score %in% 1:2 ~ "Low Damage",
    score %in% 3:6 ~ "Medium Damage",
    score %in% 7:9 ~ "High Damage",
    TRUE ~ NA_character_
  )
}

make_analysis_df <- function(raw, input) {
  req(raw)
  raw %>%
    transmute(
      trial = as.character(.data[[input$col_trial]]),
      se_name = as.character(.data[[input$col_se]]),
      assessment_type_code = as.character(.data[[input$col_type]]),
      timing = as.character(.data[[input$col_timing]]),
      treatment = as.character(.data[[input$col_treatment]]),
      replicate = as.character(.data[[input$col_rep]]),
      sample_size = safe_num(.data[[input$col_sample]]),
      assessment_class = as.character(.data[[input$col_class]]),
      assessment_value = safe_num(.data[[input$col_value]])
    ) %>%
    mutate(
      davis_score = parse_davis_class(assessment_class),
      count = assessment_value,
      damage_level = factor(classify_davis(davis_score), levels = DAVIS_LEVELS, ordered = TRUE)
    )
}

validate_counts <- function(df) {
  df %>%
    group_by(trial, timing, treatment, replicate) %>%
    summarise(
      sample_size = first(sample_size),
      sum_assessment_value = sum(count, na.rm = TRUE),
      difference = sum_assessment_value - sample_size,
      .groups = "drop"
    ) %>%
    filter(is.na(sample_size) | is.na(sum_assessment_value) | abs(difference) > 1e-8)
}

make_summary <- function(df) {
  df %>%
    filter(!is.na(damage_level), !is.na(count), count > 0) %>%
    group_by(timing, treatment, damage_level) %>%
    summarise(n = sum(count, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(percent = 100 * n / sum(n, na.rm = TRUE)) %>%
    ungroup()
}

make_descriptive <- function(df) {
  df %>%
    filter(!is.na(damage_level), !is.na(count), count > 0) %>%
    group_by(timing, treatment) %>%
    summarise(
      plants = sum(count, na.rm = TRUE),
      mean_davis = weighted.mean(davis_score, count, na.rm = TRUE),
      pct_sin_dano = 100 * sum(count[damage_level == "Sin Daño"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_low = 100 * sum(count[damage_level == "Low Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_medium = 100 * sum(count[damage_level == "Medium Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_high = 100 * sum(count[damage_level == "High Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(timing, treatment)
}

incidence_first_utc <- function(df, utc_name) {
  req(utc_name)
  first_by_trial <- df %>%
    group_by(trial) %>%
    summarise(first_timing = sort(unique(as.character(timing)))[1], .groups = "drop")

  df %>%
    inner_join(first_by_trial, by = "trial") %>%
    filter(as.character(timing) == as.character(first_timing), as.character(treatment) == as.character(utc_name)) %>%
    group_by(trial, first_timing) %>%
    summarise(
      incidence_medium_high = 100 * sum(count[damage_level %in% c("Medium Damage", "High Damage")], na.rm = TRUE) / sum(count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(trial)
}

fit_ordinal <- function(df) {
  dat <- df %>%
    filter(!is.na(damage_level), !is.na(count), count > 0) %>%
    mutate(damage_level = ordered(as.character(damage_level), levels = DAVIS_LEVELS), treatment = factor(treatment))
  if (n_distinct(dat$damage_level) < 2 || n_distinct(dat$treatment) < 2) return(NULL)
  tryCatch(ordinal::clm(damage_level ~ treatment, weights = count, data = dat, link = "logit"), error = function(e) NULL)
}

ordinal_text <- function(df) {
  mod <- fit_ordinal(df)
  if (is.null(mod)) return("Modelo ordinal no ajustable. Revisar cantidad de tratamientos, niveles Davis o conteos válidos.")
  paste(capture.output(summary(mod)), collapse = "\n")
}

make_letters <- function(df) {
  dat <- df %>%
    filter(!is.na(davis_score), !is.na(count), count > 0) %>%
    mutate(treatment = factor(treatment))
  if (n_distinct(dat$treatment) < 2) return(tibble(treatment = unique(dat$treatment), letter = "a"))

  mod <- tryCatch(lm(davis_score ~ treatment, weights = count, data = dat), error = function(e) NULL)
  if (is.null(mod)) return(tibble(treatment = unique(dat$treatment), letter = NA_character_))

  emm <- tryCatch(emmeans::emmeans(mod, ~ treatment), error = function(e) NULL)
  if (is.null(emm)) return(tibble(treatment = unique(dat$treatment), letter = NA_character_))

  pairs_df <- tryCatch(as.data.frame(pairs(emm, adjust = "tukey")), error = function(e) NULL)
  if (is.null(pairs_df) || nrow(pairs_df) == 0) return(tibble(treatment = levels(dat$treatment), letter = "a"))

  pvals <- pairs_df$p.value
  names(pvals) <- gsub(" ", "", pairs_df$contrast)
  letters <- tryCatch(multcompView::multcompLetters(pvals, threshold = 0.05)$Letters, error = function(e) NULL)
  if (is.null(letters)) return(tibble(treatment = levels(dat$treatment), letter = NA_character_))
  tibble(treatment = names(letters), letter = unname(letters))
}

plot_stack <- function(df, title = "") {
  s <- make_summary(df)
  ggplot(s, aes(x = treatment, y = percent, fill = damage_level)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.18) +
    facet_wrap(~ timing, scales = "free_x") +
    scale_fill_manual(values = DAVIS_COLORS, drop = FALSE) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, .03))) +
    labs(title = title, x = "Tratamiento", y = "% de plantas", fill = "Nivel Davis") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", color = "#4B2E83"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

plot_letters <- function(df, title = "") {
  s <- make_summary(df)
  lets <- df %>% group_by(timing) %>% group_modify(~ make_letters(.x)) %>% ungroup()
  ann <- s %>% group_by(timing, treatment) %>% summarise(y = 104, .groups = "drop") %>% left_join(lets, by = c("timing", "treatment"))
  ggplot(s, aes(x = treatment, y = percent, fill = damage_level)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.18) +
    geom_text(data = ann, aes(x = treatment, y = y, label = letter), inherit.aes = FALSE, fontface = "bold", size = 4) +
    facet_wrap(~ timing, scales = "free_x") +
    scale_fill_manual(values = DAVIS_COLORS, drop = FALSE) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 112), expand = expansion(mult = c(0, .02))) +
    labs(title = title, x = "Tratamiento", y = "% de plantas", fill = "Nivel Davis") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", color = "#4B2E83"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

parse_groups <- function(txt, trials_available) {
  if (is.null(txt) || !nzchar(txt)) return(tibble(group = character(), trial = character()))
  lines <- str_split(txt, "\n")[[1]]
  map_dfr(lines, function(line) {
    if (!str_detect(line, ":")) return(tibble(group = character(), trial = character()))
    grp <- str_trim(str_split_fixed(line, ":", 2)[,1])
    trs <- str_split(str_split_fixed(line, ":", 2)[,2], ",")[[1]] %>% str_trim()
    tibble(group = grp, trial = trs)
  }) %>% filter(trial %in% trials_available, nzchar(group), nzchar(trial))
}

add_text_slide <- function(doc, title, body) {
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  ph_with(doc, body, location = ph_location_type(type = "body"))
}

add_plot_slide <- function(doc, plot, title, subtitle = NULL) {
  img <- tempfile(fileext = ".png")
  ggsave(img, plot, width = 11.2, height = 5.7, dpi = 180)
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  if (!is.null(subtitle)) doc <- ph_with(doc, subtitle, location = ph_location(left = .7, top = 1.02, width = 11.4, height = .35))
  ph_with(doc, external_img(img, width = 11.2, height = 5.7), location = ph_location(left = .7, top = 1.35, width = 11.2, height = 5.7))
}

ui <- page_navbar(
  title = "Escala DAVIS",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#4B2E83", secondary = "#00A3E0", success = "#66B512"),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  nav_panel("Inicio",
    div(class = "hero",
      h1("Escala DAVIS"),
      p("Aplicación en espacio de prueba para analizar conteos por clase Davis 0-9. El flujo está pensado para cargar una tabla Scout, validar consistencia, generar análisis overall, por localidad y por grupos manuales, y exportar una presentación PowerPoint."),
      span(class = "badge-davis badge-zero", "0 = Sin Daño"),
      span(class = "badge-davis badge-low", "1-2 = Low"),
      span(class = "badge-davis badge-med", "3-6 = Medium"),
      span(class = "badge-davis badge-high", "7-9 = High")
    ),
    div(class = "kpi-grid",
      div(class = "kpi-card", div(class="label", "Filtro fijo"), div(class="value", "ES11AD2"), div(class="small-muted", "se_name objetivo")),
      div(class = "kpi-card", div(class="label", "Tipo de dato"), div(class="value", "COUNT"), div(class="small-muted", "ABBOTT excluido")),
      div(class = "kpi-card", div(class="label", "Modelo"), div(class="value", "CLM"), div(class="small-muted", "ordinal logit")),
      div(class = "kpi-card", div(class="label", "Salida"), div(class="value", "PPTX"), div(class="small-muted", "slides editables"))
    ),
    div(class = "card-help",
      h3(class = "help-title", "Cómo usarla"),
      tags$ol(
        tags$li("Cargá el Excel o CSV en la pestaña Carga y mapeo."),
        tags$li("Revisá que cada rol de columna esté correctamente asignado."),
        tags$li("Elegí el testigo desde la lista de tratamientos detectados."),
        tags$li("Procesá los datos y revisá la pestaña Validación."),
        tags$li("Consultá los resultados overall, por localidad y por grupos."),
        tags$li("Exportá el PowerPoint para usar los gráficos en otras presentaciones.")
      )
    )
  ),

  nav_panel("Carga y mapeo",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("file", "Cargar tabla", accept = c(".xlsx", ".xls", ".csv", ".txt")),
        uiOutput("mapping_ui"),
        textInput("target_se", "se_name a analizar", "ES11AD2"),
        textInput("target_type", "assessment_type_code", "COUNT"),
        uiOutput("utc_ui"),
        actionButton("process", "Procesar datos", class = "btn-primary")
      ),
      div(class = "card-soft", h3("Vista previa"), DTOutput("preview"))
    )
  ),

  nav_panel("Validación",
    div(class = "card-help",
      h3(class = "help-title", "Regla de consistencia"),
      p("Para cada combinación trial + momento + tratamiento + repetición, la suma de assessment_value debe coincidir con sample_size. Si no coincide, el error aparece en la tabla inferior.")
    ),
    div(class = "kpi-grid",
      uiOutput("kpi_rows"), uiOutput("kpi_trials"), uiOutput("kpi_errors"), uiOutput("kpi_incidence")
    ),
    div(class = "card-soft", h3("Errores detectados"), DTOutput("validation_table")),
    div(class = "card-soft", h3("Incidencia del testigo en primera evaluación"), p("Incidencia = % Medium Damage + % High Damage en el testigo, usando el primer assessment_timing_code de cada localidad."), DTOutput("incidence_table"))
  ),

  nav_panel("Overall",
    div(class = "card-help", h3(class = "help-title", "Fundamento"), p("El análisis overall combina todos los trials válidos. Sirve como lectura general del comportamiento de los tratamientos, pero no reemplaza la interpretación por localidad si existe interacción ambiente/tratamiento.")),
    plotOutput("plot_overall", height = "620px"),
    plotOutput("plot_overall_letters", height = "620px"),
    div(class = "card-soft", h3("Tabla descriptiva"), DTOutput("desc_overall")),
    div(class = "card-soft", h3("Modelo ordinal CLM"), pre(textOutput("model_overall")))
  ),

  nav_panel("Por localidad",
    div(class = "card-help", h3(class = "help-title", "Lectura por localidad"), p("Cada trial se analiza como subconjunto independiente. Las letras y los modelos de una localidad no deben compararse directamente con letras calculadas para otra localidad.")),
    uiOutput("trial_selector"),
    plotOutput("plot_trial", height = "620px"),
    plotOutput("plot_trial_letters", height = "620px"),
    div(class = "card-soft", h3("Tabla descriptiva"), DTOutput("desc_trial")),
    div(class = "card-soft", h3("Modelo ordinal CLM"), pre(textOutput("model_trial")))
  ),

  nav_panel("Grupos",
    div(class = "card-help",
      h3(class = "help-title", "Segmentación manual"),
      p("Definí grupos de ensayos manualmente. Formato: Nombre del grupo: trial1, trial2, trial3"),
      p("Ejemplo: Alta presión: ARG001, ARG002")
    ),
    layout_columns(
      col_widths = c(4,8),
      div(class = "card-soft", h4("Definir grupos"), verbatimTextOutput("available_trials"), textAreaInput("group_text", "Grupos", rows = 8, placeholder = "Alta presión: Trial A, Trial B\nBaja presión: Trial C, Trial D"), uiOutput("group_selector")),
      div(class = "card-soft", h4("Trials asignados"), DTOutput("groups_table"))
    ),
    plotOutput("plot_group", height = "620px"),
    plotOutput("plot_group_letters", height = "620px"),
    div(class = "card-soft", h3("Modelo ordinal CLM"), pre(textOutput("model_group")))
  ),

  nav_panel("? Ayuda y fundamento",
    div(class = "card-help",
      h2(class = "help-title", "Columnas necesarias"),
      tags$ul(
        tags$li(strong("se_name:"), " debe contener ES11AD2."),
        tags$li(strong("assessment_type_code:"), " debe contener COUNT. ABBOTT queda excluido."),
        tags$li(strong("trial:"), " identifica localidad o ensayo."),
        tags$li(strong("assessment_timing_code:"), " identifica el momento de evaluación."),
        tags$li(strong("treatment / Trt. / treatment_mod:"), " identifica tratamiento."),
        tags$li(strong("replicate_number:"), " identifica repetición."),
        tags$li(strong("sample_size:"), " total de plantas evaluadas."),
        tags$li(strong("assessment_class:"), " clase Davis, por ejemplo CL. 0 a CL. 9."),
        tags$li(strong("assessment_value:"), " cantidad de plantas observadas en esa clase.")
      )
    ),
    div(class = "card-soft",
      h2("Fundamento de las letras"),
      p("Las letras resumen comparaciones múltiples entre tratamientos. Dos tratamientos que comparten al menos una letra no difieren significativamente para ese subconjunto y momento. Dos tratamientos con letras completamente diferentes sí presentan diferencias significativas bajo el criterio usado."),
      p("Ejemplo: si T1 = a, T2 = ab y T3 = b, T1 y T3 difieren; T2 queda en posición intermedia y no difiere claramente de T1 ni de T3."),
      p("En esta app las letras se calculan sobre el score Davis ponderado por conteos. El modelo ordinal CLM se mantiene como salida principal técnica para respetar la línea de presentación del script original.")
    ),
    div(class = "card-soft",
      h2("Modelo ordinal"),
      p("El modelo ordinal logit acumulativo evalúa la probabilidad de caer en categorías ordenadas de daño. La variable respuesta se trata como ordinal: Sin Daño < Low < Medium < High. Como los datos llegan agregados por clase, se utiliza assessment_value como peso del modelo.")
    )
  ),

  nav_panel("Exportar",
    div(class = "card-help", h3(class = "help-title", "PowerPoint"), p("Genera una presentación con portada, metodología, validación, overall, localidades, grupos definidos y anexos básicos.")),
    downloadButton("download_ppt", "Descargar PowerPoint", class = "btn-primary")
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file)
    read_any(input$file$datapath, input$file$name)
  })

  output$mapping_ui <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    tagList(
      selectInput("col_trial", "Trial / localidad", cols, selected = find_col(cols, c("trial", "trial_mod", "Trial"))),
      selectInput("col_se", "se_name", cols, selected = find_col(cols, c("se_name", "SE_NAME"))),
      selectInput("col_type", "assessment_type_code", cols, selected = find_col(cols, c("assessment_type_code", "assessment_type"))),
      selectInput("col_timing", "Momento", cols, selected = find_col(cols, c("assessment_timing_code", "assessment_number", "assessment_date"))),
      selectInput("col_treatment", "Tratamiento", cols, selected = find_col(cols, c("Trt.", "treatment", "treatment_mod", "treatment_name"))),
      selectInput("col_rep", "Repetición", cols, selected = find_col(cols, c("replicate_number", "rep", "replicate"))),
      selectInput("col_sample", "sample_size", cols, selected = find_col(cols, c("sample_size", "samplesize"))),
      selectInput("col_class", "assessment_class", cols, selected = find_col(cols, c("assessment_class", "class"))),
      selectInput("col_value", "assessment_value", cols, selected = find_col(cols, c("assessment_value", "value")))
    )
  })

  output$preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 100), options = list(scrollX = TRUE, pageLength = 10))
  })

  mapped_all <- reactive({
    req(raw_data(), input$col_trial, input$col_se, input$col_type, input$col_timing, input$col_treatment, input$col_rep, input$col_sample, input$col_class, input$col_value)
    make_analysis_df(raw_data(), input)
  })

  output$utc_ui <- renderUI({
    req(mapped_all())
    choices <- sort(unique(mapped_all()$treatment))
    selectInput("utc", "Elegir testigo", choices = choices, selected = choices[1])
  })

  processed <- eventReactive(input$process, {
    df <- mapped_all() %>%
      filter(se_name == input$target_se, assessment_type_code == input$target_type, davis_score %in% 0:9)
    validate <- validate_counts(df)
    list(data = df, validation = validate)
  })

  valid_data <- reactive({ req(processed()); processed()$data })
  validation_errors <- reactive({ req(processed()); processed()$validation })

  output$kpi_rows <- renderUI({ req(valid_data()); div(class="kpi-card", div(class="label","Filas COUNT válidas"), div(class="value", nrow(valid_data()))) })
  output$kpi_trials <- renderUI({ req(valid_data()); div(class="kpi-card", div(class="label","Trials"), div(class="value", n_distinct(valid_data()$trial))) })
  output$kpi_errors <- renderUI({ req(validation_errors()); div(class="kpi-card", div(class="label","Errores sample_size"), div(class="value", nrow(validation_errors()))) })
  output$kpi_incidence <- renderUI({ req(valid_data(), input$utc); inc <- incidence_first_utc(valid_data(), input$utc); val <- if (nrow(inc) == 0) "-" else paste0(round(mean(inc$incidence_medium_high, na.rm=TRUE),1), "%"); div(class="kpi-card", div(class="label","Incidencia media testigo"), div(class="value", val)) })

  output$validation_table <- renderDT({
    req(validation_errors())
    datatable(validation_errors(), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$incidence_table <- renderDT({
    req(valid_data(), input$utc)
    datatable(incidence_first_utc(valid_data(), input$utc), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$plot_overall <- renderPlot({ req(valid_data()); plot_stack(valid_data(), "Overall - Distribución Davis") })
  output$plot_overall_letters <- renderPlot({ req(valid_data()); plot_letters(valid_data(), "Overall - Letras Tukey sobre score Davis") })
  output$desc_overall <- renderDT({ req(valid_data()); datatable(make_descriptive(valid_data()), options = list(scrollX = TRUE, pageLength = 10)) })
  output$model_overall <- renderText({ req(valid_data()); ordinal_text(valid_data()) })

  output$trial_selector <- renderUI({ req(valid_data()); selectInput("selected_trial", "Localidad / trial", choices = sort(unique(valid_data()$trial))) })
  trial_data <- reactive({ req(valid_data(), input$selected_trial); valid_data() %>% filter(trial == input$selected_trial) })
  output$plot_trial <- renderPlot({ req(trial_data()); plot_stack(trial_data(), paste("Trial", input$selected_trial, "- Distribución Davis")) })
  output$plot_trial_letters <- renderPlot({ req(trial_data()); plot_letters(trial_data(), paste("Trial", input$selected_trial, "- Letras Tukey")) })
  output$desc_trial <- renderDT({ req(trial_data()); datatable(make_descriptive(trial_data()), options = list(scrollX = TRUE, pageLength = 10)) })
  output$model_trial <- renderText({ req(trial_data()); ordinal_text(trial_data()) })

  groups_df <- reactive({
    req(valid_data())
    parse_groups(input$group_text, sort(unique(valid_data()$trial)))
  })
  output$available_trials <- renderText({ req(valid_data()); paste(sort(unique(valid_data()$trial)), collapse = ", ") })
  output$groups_table <- renderDT({ datatable(groups_df(), options = list(pageLength = 10)) })
  output$group_selector <- renderUI({
    g <- groups_df()
    if (nrow(g) == 0) return(helpText("Todavía no hay grupos válidos."))
    selectInput("selected_group", "Grupo", choices = sort(unique(g$group)))
  })
  group_data <- reactive({
    req(valid_data(), input$selected_group)
    trs <- groups_df() %>% filter(group == input$selected_group) %>% pull(trial)
    valid_data() %>% filter(trial %in% trs)
  })
  output$plot_group <- renderPlot({ req(group_data()); plot_stack(group_data(), paste("Grupo", input$selected_group, "- Distribución Davis")) })
  output$plot_group_letters <- renderPlot({ req(group_data()); plot_letters(group_data(), paste("Grupo", input$selected_group, "- Letras Tukey")) })
  output$model_group <- renderText({ req(group_data()); ordinal_text(group_data()) })

  output$download_ppt <- downloadHandler(
    filename = function() paste0("Escala_DAVIS_", Sys.Date(), ".pptx"),
    content = function(file) {
      req(valid_data())
      doc <- read_pptx()
      doc <- add_text_slide(doc, "Escala DAVIS", paste0("Reporte generado: ", Sys.Date(), "\nFiltro: ", input$target_se, " + ", input$target_type, "\nTestigo: ", input$utc))
      doc <- add_text_slide(doc, "Metodología", "Se analizan conteos por clase Davis 0-9. Clasificación: 0 = Sin Daño; 1-2 = Low; 3-6 = Medium; 7-9 = High. Los registros ABBOTT quedan excluidos. El modelo ordinal usa ordinal::clm con assessment_value como peso.")
      doc <- add_text_slide(doc, "Validación", paste0("Filas válidas: ", nrow(valid_data()), "\nTrials: ", n_distinct(valid_data()$trial), "\nErrores sample_size: ", nrow(validation_errors())))
      doc <- add_plot_slide(doc, plot_stack(valid_data(), "Overall - Distribución Davis"), "Overall")
      doc <- add_plot_slide(doc, plot_letters(valid_data(), "Overall - Letras Tukey"), "Overall - Letras Tukey")

      inc <- incidence_first_utc(valid_data(), input$utc)
      for (tr in sort(unique(valid_data()$trial))) {
        dft <- valid_data() %>% filter(trial == tr)
        inc_txt <- inc %>% filter(trial == tr)
        sub <- if (nrow(inc_txt) == 0) "Incidencia testigo primera evaluación: no calculable" else paste0("Incidencia testigo primera evaluación: ", round(inc_txt$incidence_medium_high[1], 1), "%")
        doc <- add_plot_slide(doc, plot_stack(dft, paste("Trial", tr)), paste("Localidad / trial:", tr), sub)
        doc <- add_plot_slide(doc, plot_letters(dft, paste("Trial", tr, "- Letras Tukey")), paste("Localidad / trial:", tr, "- Letras"))
      }

      g <- groups_df()
      if (nrow(g) > 0) {
        for (gr in sort(unique(g$group))) {
          trs <- g %>% filter(group == gr) %>% pull(trial)
          dfg <- valid_data() %>% filter(trial %in% trs)
          doc <- add_plot_slide(doc, plot_stack(dfg, paste("Grupo", gr)), paste("Grupo:", gr), paste("Trials:", paste(trs, collapse = ", ")))
          doc <- add_plot_slide(doc, plot_letters(dfg, paste("Grupo", gr, "- Letras Tukey")), paste("Grupo:", gr, "- Letras"))
        }
      }
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
