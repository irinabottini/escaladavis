library(shiny)
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
library(bslib)

options(shiny.maxRequestSize = 100 * 1024^2)

std_level <- c("Sin Daño", "Low Damage", "Medium Damage", "High Damage")

find_col <- function(cols, candidates) {
  hit <- candidates[candidates %in% cols]
  if (length(hit) > 0) hit[1] else cols[1]
}

safe_num <- function(x) suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", ".")))

read_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) readxl::read_excel(path)
  else if (ext == "csv") read.csv(path, check.names = FALSE)
  else read.delim(path, check.names = FALSE)
}

parse_davis_class <- function(x) {
  out <- stringr::str_extract(as.character(x), "(?<![0-9])([0-9])(?![0-9])")
  suppressWarnings(as.integer(out))
}

classify_damage <- function(v) {
  dplyr::case_when(
    v == 0 ~ "Sin Daño",
    v %in% 1:2 ~ "Low Damage",
    v %in% 3:6 ~ "Medium Damage",
    v %in% 7:9 ~ "High Damage",
    TRUE ~ NA_character_
  )
}

validate_counts <- function(df) {
  df %>%
    group_by(trial, timing, treatment, replicate) %>%
    summarise(
      sample_size = dplyr::first(sample_size),
      sum_assessment_value = sum(count, na.rm = TRUE),
      difference = sum_assessment_value - sample_size,
      .groups = "drop"
    ) %>%
    filter(is.na(sample_size) | is.na(sum_assessment_value) | abs(difference) > 1e-8)
}

make_summary <- function(df) {
  df %>%
    group_by(timing, treatment, damage_level) %>%
    summarise(n = sum(count, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(percent = 100 * n / sum(n, na.rm = TRUE)) %>%
    ungroup()
}

make_descriptive_table <- function(df) {
  df %>%
    group_by(timing, treatment) %>%
    summarise(
      plants = sum(count, na.rm = TRUE),
      mean_davis = weighted.mean(davis_score, count, na.rm = TRUE),
      pct_sin_dano = 100 * sum(count[damage_level == "Sin Daño"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_low = 100 * sum(count[damage_level == "Low Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_medium = 100 * sum(count[damage_level == "Medium Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      pct_high = 100 * sum(count[damage_level == "High Damage"], na.rm = TRUE) / sum(count, na.rm = TRUE),
      .groups = "drop"
    )
}

incidence_utc_first <- function(df, utc_name) {
  if (nrow(df) == 0) return(tibble(trial = character(), first_timing = character(), incidence = numeric()))
  first_by_trial <- df %>%
    group_by(trial) %>%
    summarise(first_timing = sort(unique(as.character(timing)))[1], .groups = "drop")
  df %>%
    inner_join(first_by_trial, by = "trial") %>%
    filter(as.character(timing) == as.character(first_timing), as.character(treatment) == as.character(utc_name)) %>%
    group_by(trial, first_timing) %>%
    summarise(
      incidence = 100 * sum(count[damage_level %in% c("Medium Damage", "High Damage")], na.rm = TRUE) / sum(count, na.rm = TRUE),
      .groups = "drop"
    )
}

fit_ordinal <- function(df) {
  dat <- df %>%
    filter(!is.na(damage_level), count > 0) %>%
    mutate(damage_level = ordered(damage_level, levels = std_level), treatment = factor(treatment))
  if (n_distinct(dat$damage_level) < 2 || n_distinct(dat$treatment) < 2) return(NULL)
  tryCatch(ordinal::clm(damage_level ~ treatment, weights = count, data = dat, link = "logit"), error = function(e) NULL)
}

model_text <- function(df) {
  m <- fit_ordinal(df)
  if (is.null(m)) return("Modelo ordinal no ajustable: revisar cantidad de niveles Davis y tratamientos disponibles.")
  paste(capture.output(summary(m)), collapse = "\n")
}

make_letters <- function(df) {
  dat <- df %>% filter(!is.na(davis_score), count > 0) %>% mutate(treatment = factor(treatment))
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
    geom_col(width = 0.72, color = "white", linewidth = 0.15) +
    facet_wrap(~ timing, scales = "free_x") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    labs(title = title, x = "Tratamiento", y = "% de plantas", fill = "Nivel Davis") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"), legend.position = "bottom")
}

plot_with_letters <- function(df, title = "") {
  s <- make_summary(df)
  lets <- df %>% group_by(timing) %>% group_modify(~ make_letters(.x)) %>% ungroup()
  ann <- s %>% group_by(timing, treatment) %>% summarise(y = 104, .groups = "drop") %>% left_join(lets, by = c("timing", "treatment"))
  ggplot(s, aes(x = treatment, y = percent, fill = damage_level)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.15) +
    geom_text(data = ann, aes(x = treatment, y = y, label = letter), inherit.aes = FALSE, fontface = "bold", size = 4) +
    facet_wrap(~ timing, scales = "free_x") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 112)) +
    labs(title = title, x = "Tratamiento", y = "% de plantas", fill = "Nivel Davis") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"), legend.position = "bottom")
}

add_text_slide <- function(doc, title, body) {
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  doc <- ph_with(doc, body, location = ph_location_type(type = "body"))
  doc
}

add_plot_slide <- function(doc, plot, title, subtitle = NULL) {
  img <- tempfile(fileext = ".png")
  ggsave(img, plot, width = 11.5, height = 5.9, dpi = 180)
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  if (!is.null(subtitle)) doc <- ph_with(doc, subtitle, location = ph_location(left = 0.7, top = 1.0, width = 11.5, height = 0.35))
  ph_with(doc, external_img(img, width = 11.5, height = 5.9), location = ph_location(left = 0.7, top = 1.35, width = 11.5, height = 5.9))
}

ui <- navbarPage(
  title = div(img(src = "logo_bayer.jpg", class = "logo-top"), "Escala DAVIS"),
  theme = bs_theme(version = 3),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tabPanel("Carga y mapeo",
    fluidRow(
      column(3, wellPanel(
        fileInput("file", "Cargar tabla", accept = c(".xlsx", ".xls", ".csv", ".txt")),
        uiOutput("mapping_ui"),
        textInput("target_se", "se_name a analizar", "ES11AD2"),
        textInput("target_type", "assessment_type_code", "COUNT"),
        uiOutput("utc_ui"),
        actionButton("process", "Procesar datos", class = "btn-primary")
      )),
      column(9, div(class="help-card", h4("Espacio de prueba"), p("Cargá una tabla Scout, revisá el mapeo de columnas y procesá. Los registros ABBOTT quedan excluidos porque el análisis usa exclusivamente conteos COUNT.")), DTOutput("preview"))
    )
  ),
  tabPanel("? Columnas y reglas",
    div(class="help-card",
      h3("Columnas necesarias"),
      tags$ul(
        tags$li("se_name: debe contener ES11AD2."),
        tags$li("assessment_type_code: debe contener COUNT. ABBOTT queda excluido."),
        tags$li("trial o trial_mod: localidad / ensayo."),
        tags$li("assessment_timing_code: momento de evaluación. Si no está, se puede elegir otra columna."),
        tags$li("treatment o treatment_mod: tratamiento."),
        tags$li("replicate_number: repetición."),
        tags$li("sample_size: total de plantas evaluadas."),
        tags$li("assessment_class: clase Davis, por ejemplo CL. 0 a CL. 9."),
        tags$li("assessment_value: cantidad de plantas observadas en esa clase.")
      ),
      h3("Regla de validación"),
      p("Para cada trial + momento + tratamiento + repetición, la suma de assessment_value debe ser igual a sample_size."),
      h3("Escala usada"),
      p("0 = Sin Daño; 1-2 = Low Damage; 3-6 = Medium Damage; 7-9 = High Damage."),
      h3("Letras Tukey"),
      p("Tratamientos que comparten letra no difieren significativamente entre sí. Tratamientos con letras diferentes sí difieren para ese momento y ese subconjunto de análisis. Las letras no se comparan entre overall, localidades y grupos.")
    )
  ),
  tabPanel("Validación", fluidRow(column(3, uiOutput("metrics")), column(9, DTOutput("errors")))) ,
  tabPanel("Overall", plotOutput("overall_plot", height = "650px"), plotOutput("overall_letters", height = "650px"), verbatimTextOutput("overall_model"), DTOutput("overall_table")),
  tabPanel("Por localidad", fluidRow(column(3, uiOutput("trial_selector"), h4("Incidencia testigo"), DTOutput("incidence_tbl")), column(9, plotOutput("trial_plot", height="650px"), plotOutput("trial_letters", height="650px"), verbatimTextOutput("trial_model"), DTOutput("trial_table")))),
  tabPanel("Grupos", fluidRow(
    column(3, uiOutput("group_builder"), actionButton("add_group", "Agregar/actualizar grupo"), br(), br(), DTOutput("groups_tbl")),
    column(9, uiOutput("group_selector"), plotOutput("group_plot", height="650px"), verbatimTextOutput("group_model"), DTOutput("group_table"))
  )),
  tabPanel("Exportar", wellPanel(downloadButton("pptx", "Descargar PowerPoint"), span(class="small-note", " Incluye metodología, validación, overall, localidades, grupos e incidencia del testigo.")))
)

server <- function(input, output, session) {
  raw <- reactive({
    req(input$file)
    read_any(input$file$datapath, input$file$name)
  })

  groups <- reactiveVal(tibble(group = character(), trial = character()))

  output$preview <- renderDT({ req(raw()); datatable(head(raw(), 100), options = list(scrollX = TRUE, pageLength = 10)) })

  output$mapping_ui <- renderUI({
    req(raw()); cols <- names(raw())
    tagList(
      selectInput("col_se", "Columna se_name", cols, selected = find_col(cols, c("se_name"))),
      selectInput("col_type", "Columna assessment_type_code", cols, selected = find_col(cols, c("assessment_type_code"))),
      selectInput("col_trial", "Columna trial/localidad", cols, selected = find_col(cols, c("trial", "trial_mod"))),
      selectInput("col_timing", "Columna momento", cols, selected = find_col(cols, c("assessment_timing_code", "timing_mod"))),
      selectInput("col_treatment", "Columna tratamiento", cols, selected = find_col(cols, c("treatment", "treatment_mod", "Trt."))),
      selectInput("col_replicate", "Columna repetición", cols, selected = find_col(cols, c("replicate_number"))),
      selectInput("col_sample", "Columna sample_size", cols, selected = find_col(cols, c("sample_size"))),
      selectInput("col_class", "Columna assessment_class", cols, selected = find_col(cols, c("assessment_class"))),
      selectInput("col_value", "Columna assessment_value", cols, selected = find_col(cols, c("assessment_value", "Concatenate(assessment_value)")))
    )
  })

  mapped_before_filter <- reactive({
    req(raw(), input$col_se, input$col_type, input$col_trial, input$col_timing, input$col_treatment, input$col_replicate, input$col_sample, input$col_class, input$col_value)
    df <- raw()
    tibble(
      se_name = as.character(df[[input$col_se]]),
      assessment_type_code = as.character(df[[input$col_type]]),
      trial = as.character(df[[input$col_trial]]),
      timing = as.character(df[[input$col_timing]]),
      treatment = as.character(df[[input$col_treatment]]),
      replicate = as.character(df[[input$col_replicate]]),
      sample_size = safe_num(df[[input$col_sample]]),
      assessment_class = as.character(df[[input$col_class]]),
      count = safe_num(df[[input$col_value]])
    )
  })

  output$utc_ui <- renderUI({
    if (!is.null(input$file) && !is.null(input$col_treatment)) {
      vals <- sort(unique(na.omit(as.character(mapped_before_filter()$treatment))))
      selectInput("utc", "Tratamiento testigo", choices = vals, selected = vals[1])
    } else {
      textInput("utc", "Tratamiento testigo", "1-Testigo")
    }
  })

  processed <- eventReactive(input$process, {
    mapped_before_filter() %>%
      filter(se_name == input$target_se, assessment_type_code == input$target_type) %>%
      mutate(
        davis_score = parse_davis_class(assessment_class),
        damage_level = classify_damage(davis_score),
        damage_level = factor(damage_level, levels = std_level),
        trial = ifelse(is.na(trial) | trial == "", "Sin trial", trial),
        timing = ifelse(is.na(timing) | timing == "", "Sin timing", timing),
        treatment = ifelse(is.na(treatment) | treatment == "", "Sin tratamiento", treatment)
      ) %>%
      filter(!is.na(davis_score), davis_score %in% 0:9, !is.na(count))
  })

  errors <- reactive({ req(processed()); validate_counts(processed()) })

  output$metrics <- renderUI({
    req(processed())
    e <- errors()
    tagList(
      div(class="metric-card", "Filas válidas", br(), tags$b(nrow(processed()))),
      div(class="metric-card", "Trials", br(), tags$b(n_distinct(processed()$trial))),
      div(class="metric-card", "Tratamientos", br(), tags$b(n_distinct(processed()$treatment))),
      div(class="metric-card", "Grupos con error", br(), tags$b(nrow(e)))
    )
  })
  output$errors <- renderDT({ req(errors()); datatable(errors(), options = list(scrollX=TRUE, pageLength=20)) })

  output$overall_plot <- renderPlot({ req(processed()); plot_stack(processed(), "Overall - Distribución Davis") })
  output$overall_letters <- renderPlot({ req(processed()); plot_with_letters(processed(), "Overall - Letras Tukey sobre score Davis ponderado") })
  output$overall_model <- renderText({ req(processed()); model_text(processed()) })
  output$overall_table <- renderDT({ req(processed()); datatable(make_descriptive_table(processed()), options = list(scrollX=TRUE, pageLength=20)) })

  output$trial_selector <- renderUI({ req(processed()); selectInput("trial_sel", "Localidad / trial", sort(unique(processed()$trial))) })
  trial_df <- reactive({ req(processed(), input$trial_sel); processed() %>% filter(trial == input$trial_sel) })
  output$trial_plot <- renderPlot({ req(trial_df()); plot_stack(trial_df(), paste("Localidad:", input$trial_sel)) })
  output$trial_letters <- renderPlot({ req(trial_df()); plot_with_letters(trial_df(), paste("Letras Tukey -", input$trial_sel)) })
  output$trial_model <- renderText({ req(trial_df()); model_text(trial_df()) })
  output$trial_table <- renderDT({ req(trial_df()); datatable(make_descriptive_table(trial_df()), options = list(scrollX=TRUE, pageLength=20)) })
  output$incidence_tbl <- renderDT({ req(processed()); datatable(incidence_utc_first(processed(), input$utc), options = list(dom='t', scrollX=TRUE)) })

  output$group_builder <- renderUI({ req(processed()); tagList(textInput("group_name", "Nombre del grupo", "Grupo 1"), checkboxGroupInput("group_trials", "Trials incluidos", choices = sort(unique(processed()$trial)))) })
  observeEvent(input$add_group, {
    req(input$group_name, input$group_trials)
    g <- groups() %>% filter(group != input$group_name)
    groups(bind_rows(g, tibble(group = input$group_name, trial = input$group_trials)))
  })
  output$groups_tbl <- renderDT({ datatable(groups(), options = list(dom='t', scrollX=TRUE)) })
  output$group_selector <- renderUI({ req(nrow(groups()) > 0); selectInput("group_sel", "Grupo a visualizar", unique(groups()$group)) })
  group_df <- reactive({ req(processed(), input$group_sel); trs <- groups() %>% filter(group == input$group_sel) %>% pull(trial); processed() %>% filter(trial %in% trs) })
  output$group_plot <- renderPlot({ req(group_df()); plot_with_letters(group_df(), paste("Grupo:", input$group_sel)) })
  output$group_model <- renderText({ req(group_df()); model_text(group_df()) })
  output$group_table <- renderDT({ req(group_df()); datatable(make_descriptive_table(group_df()), options = list(scrollX=TRUE, pageLength=20)) })

  output$pptx <- downloadHandler(
    filename = function() paste0("Reporte_Escala_DAVIS_", Sys.Date(), ".pptx"),
    content = function(file) {
      req(processed())
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title Slide", master = "Office Theme")
      doc <- ph_with(doc, "Escala DAVIS - ES11AD2", location = ph_location_type(type = "ctrTitle"))
      doc <- ph_with(doc, paste("Filtro:", input$target_se, "+", input$target_type, "|", Sys.Date()), location = ph_location_type(type = "subTitle"))
      doc <- add_text_slide(doc, "Metodología", "Análisis basado en escala Davis 0-9. Se filtran registros se_name = ES11AD2 y assessment_type_code = COUNT. La clasificación usada es: 0 = Sin Daño, 1-2 = Low Damage, 3-6 = Medium Damage y 7-9 = High Damage. El modelo ordinal se ajusta como clm(Damage_Level ~ treatment, weights = assessment_value, link = 'logit').")
      doc <- add_text_slide(doc, "Validación de datos", paste0("Filas válidas: ", nrow(processed()), "\nTrials: ", n_distinct(processed()$trial), "\nTratamientos: ", n_distinct(processed()$treatment), "\nGrupos con error sample_size: ", nrow(errors()), "\nRegla: por trial + momento + tratamiento + repetición, sum(assessment_value) debe ser igual a sample_size."))
      doc <- add_plot_slide(doc, plot_with_letters(processed(), "Overall - Distribución Davis con letras"), "Overall")
      doc <- add_text_slide(doc, "Overall - Modelo ordinal", substr(model_text(processed()), 1, 3500))
      inc <- incidence_utc_first(processed(), input$utc)
      for (tr in sort(unique(processed()$trial))) {
        dtr <- processed() %>% filter(trial == tr)
        inc_txt <- inc %>% filter(trial == tr)
        sub <- if (nrow(inc_txt)) paste0("Incidencia testigo primera evaluación: ", round(inc_txt$incidence[1], 1), "% (Medium + High)") else "Incidencia testigo primera evaluación: no calculable"
        doc <- add_plot_slide(doc, plot_with_letters(dtr, paste("Localidad:", tr)), paste("Localidad:", tr), sub)
        doc <- add_text_slide(doc, paste("Modelo ordinal -", tr), substr(model_text(dtr), 1, 3500))
      }
      if (nrow(groups()) > 0) {
        for (gr in unique(groups()$group)) {
          trs <- groups() %>% filter(group == gr) %>% pull(trial)
          dg <- processed() %>% filter(trial %in% trs)
          doc <- add_plot_slide(doc, plot_with_letters(dg, paste("Grupo:", gr)), paste("Grupo:", gr), paste("Trials:", paste(trs, collapse = ", ")))
          doc <- add_text_slide(doc, paste("Modelo ordinal -", gr), substr(model_text(dg), 1, 3500))
        }
      }
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
