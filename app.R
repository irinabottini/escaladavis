library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(ordinal)
library(emmeans)
library(multcompView)
library(officer)
library(rvg)
library(flextable)
library(scales)

options(shiny.maxRequestSize = 80*1024^2)

bayer_cols <- c("Sin Daño"="#1f78b4", "Low Damage"="#66B512", "Medium Damage"="#F5C542", "High Damage"="#D71920")
level_order <- c("Sin Daño","Low Damage","Medium Damage","High Damage")

find_col <- function(nms, candidates){
  hit <- candidates[candidates %in% nms][1]
  ifelse(is.na(hit), nms[1], hit)
}
read_any <- function(path){
  ext <- tools::file_ext(path) |> tolower()
  if(ext %in% c("xlsx","xls")) readxl::read_excel(path) |> as.data.frame()
  else readr::read_delim(path, delim=NULL, show_col_types=FALSE) |> as.data.frame()
}
read_paste <- function(txt){
  validate(need(nchar(trimws(txt))>0, "Pegá una tabla para continuar."))
  # Soporta pegado directo desde Excel/Google Sheets: columnas separadas por TAB y filas por salto de línea.
  txt <- gsub("\r\n", "\n", txt)
  txt <- gsub("\r", "\n", txt)
  out <- try(readr::read_tsv(I(txt), show_col_types=FALSE, na=c("", "NA", "-")) |> as.data.frame(), silent=TRUE)
  if(inherits(out, "try-error") || ncol(out) < 2){
    out <- try(readr::read_delim(I(txt), delim=";", show_col_types=FALSE, na=c("", "NA", "-")) |> as.data.frame(), silent=TRUE)
  }
  if(inherits(out, "try-error") || ncol(out) < 2){
    out <- readr::read_csv(I(txt), show_col_types=FALSE, na=c("", "NA", "-")) |> as.data.frame()
  }
  out
}
classify_davis <- function(x){
  x <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
  case_when(x == 0 ~ "Sin Daño", x %in% 1:2 ~ "Low Damage", x %in% 3:6 ~ "Medium Damage", x %in% 7:9 ~ "High Damage", TRUE ~ NA_character_)
}
prep_data <- function(df, map){
  out <- tibble(
    trial = as.character(df[[map$trial]]),
    se_name = as.character(df[[map$se_name]]),
    assessment_type_code = as.character(df[[map$atype]]),
    assessment_timing_code = as.character(df[[map$timing]]),
    treatment = as.character(df[[map$treatment]]),
    treatment_mod = if(!is.null(map$treatment_mod) && map$treatment_mod %in% names(df)) as.character(df[[map$treatment_mod]]) else as.character(df[[map$treatment]]),
    replicate_number = as.character(df[[map$rep]]),
    assessment_class = as.character(df[[map$aclass]]),
    assessment_value = suppressWarnings(as.numeric(df[[map$avalue]])),
    sample_size = suppressWarnings(as.numeric(df[[map$ssize]]))
  ) %>%
    filter(se_name == "ES11AD2", assessment_type_code == "COUNT") %>%
    mutate(davis = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", assessment_class))),
           Damage_Level = factor(classify_davis(davis), levels=level_order),
           assessment_value = replace_na(assessment_value,0)) %>%
    filter(!is.na(Damage_Level), davis %in% 0:9)
  out
}
validate_counts <- function(d){
  d %>% group_by(trial, assessment_timing_code, treatment, treatment_mod, replicate_number) %>%
    summarise(sample_size = first(sample_size), sum_assessment_value = sum(assessment_value, na.rm=TRUE), difference = sum_assessment_value - sample_size, .groups="drop") %>%
    mutate(status = ifelse(abs(difference) < 1e-8, "OK", "ERROR"))
}
descriptive_summary <- function(d, xvar="treatment_mod"){
  req(nrow(d)>0)
  totals <- d %>% group_by(assessment_timing_code, .data[[xvar]]) %>%
    summarise(total_plants=sum(assessment_value, na.rm=TRUE), mean_davis=weighted.mean(davis, assessment_value, na.rm=TRUE), .groups="drop")
  pct <- d %>% group_by(assessment_timing_code, .data[[xvar]], Damage_Level) %>%
    summarise(n=sum(assessment_value, na.rm=TRUE), .groups="drop") %>%
    group_by(assessment_timing_code, .data[[xvar]]) %>% mutate(pct=n/sum(n)) %>% ungroup() %>%
    select(assessment_timing_code, !!xvar := .data[[xvar]], Damage_Level, pct) %>%
    tidyr::pivot_wider(names_from=Damage_Level, values_from=pct, values_fill=0)
  out <- totals %>% left_join(pct, by=c("assessment_timing_code", xvar))
  for(col in level_order){ if(!col %in% names(out)) out[[col]] <- 0 }
  out %>% mutate(across(all_of(level_order), ~scales::percent(.x, accuracy=0.1)), mean_davis=round(mean_davis,2)) %>%
    arrange(assessment_timing_code, .data[[xvar]])
}
column_requirements <- tibble::tribble(
  ~encabezado_recomendado, ~obligatoria, ~uso_en_la_app, ~regla_esperada,
  "trial", "Sí", "Identifica ensayo/localidad. Se usa para análisis por localidad y armado de grupos.", "Debe tener un valor por fila. Los grupos se definen seleccionando estos valores.",
  "se_name", "Sí", "Filtra la variable de evaluación.", "La app usa ES11AD2 para este análisis. Otras se_name se excluyen.",
  "assessment_type_code", "Sí", "Filtra el tipo de dato.", "Debe ser COUNT. Las filas ABBOTT u otros tipos se excluyen.",
  "assessment_timing_code", "Sí", "Define el momento de evaluación y las facetas de los gráficos.", "Si no existe con ese nombre, la app permite elegir otra columna en el mapeo.",
  "treatment", "Sí", "Código base de tratamiento.", "Puede ser T1/T2 o código numérico. También puede usarse como eje X.",
  "treatment_mod", "Recomendada", "Nombre/código de tratamiento para mostrar en eje X.", "Es la opción recomendada para gráficos. Si no existe, puede mapearse treatment.",
  "replicate_number", "Sí", "Identifica repetición.", "Se usa en la validación sample_size por repetición.",
  "assessment_class", "Sí", "Nivel Davis observado.", "Debe contener la clase 0 a 9, por ejemplo CL. 0, CL. 1 ... CL. 9.",
  "assessment_value", "Sí", "Cantidad de plantas en cada clase Davis.", "Debe ser numérica. Es el peso usado por el modelo ordinal.",
  "sample_size", "Sí", "Cantidad total de plantas evaluadas para esa combinación.", "La suma de assessment_value por trial + momento + tratamiento + repetición debe coincidir con sample_size."
)
plot_stack <- function(d, xvar="treatment_mod", title="", subtitle="", letters_df=NULL){
  req(nrow(d)>0)
  pdat <- d %>% group_by(assessment_timing_code, .data[[xvar]], Damage_Level) %>%
    summarise(n=sum(assessment_value,na.rm=TRUE), .groups="drop_last") %>%
    mutate(total=sum(n), pct=ifelse(total>0,n/total,0), label=ifelse(pct>=.045, percent(pct, accuracy=1), "")) %>% ungroup()
  ymax <- pdat %>% group_by(assessment_timing_code, .data[[xvar]]) %>% summarise(y=1.03,.groups="drop")
  if(!is.null(letters_df) && nrow(letters_df)>0){
    ymax <- ymax %>% left_join(letters_df, by=setNames(c("assessment_timing_code",xvar), c("assessment_timing_code",xvar)))
  }
  ggplot(pdat, aes(x=.data[[xvar]], y=pct, fill=Damage_Level))+
    geom_col(width=.72, color="white", linewidth=.35)+
    geom_text(aes(label=label), position=position_stack(vjust=.5), size=3.2, fontface="bold", color="#102A43")+
    {if(!is.null(letters_df) && nrow(letters_df)>0) geom_text(data=ymax, aes(x=.data[[xvar]], y=y, label=letters), inherit.aes=FALSE, fontface="bold", size=4.3, color="#10384F") }+
    facet_wrap(~assessment_timing_code, scales="free_x")+
    scale_fill_manual(values=bayer_cols, breaks=level_order, drop=FALSE)+
    scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1.12), expand=expansion(mult=c(0,.02)))+
    labs(title=title, subtitle=subtitle, x=NULL, y="Distribución porcentual", fill="Nivel de daño")+
    theme_minimal(base_size=13)+
    theme(plot.title=element_text(face="bold", size=18, color="#10384F"), plot.subtitle=element_text(color="#64748B"),
          axis.text.x=element_text(angle=45,hjust=1, size=9), panel.grid.major.x=element_blank(),
          legend.position="bottom", strip.text=element_text(face="bold", color="#10384F"), plot.background=element_rect(fill="white", color=NA))
}
expand_weighted <- function(d, max_n=60000){
  d2 <- d %>% filter(assessment_value>0) %>% mutate(w=round(assessment_value))
  n <- sum(d2$w, na.rm=TRUE)
  if(n <= max_n) d2[rep(seq_len(nrow(d2)), d2$w), ] else d2
}
tukey_letters <- function(d, xvar="treatment_mod"){
  res <- map_dfr(unique(d$assessment_timing_code), function(tm){
    df <- d %>% filter(assessment_timing_code==tm, assessment_value>0)
    if(n_distinct(df[[xvar]])<2) return(tibble())
    ex <- expand_weighted(df)
    fit <- try(aov(davis ~ as.factor(.data[[xvar]]), data=ex, weights=if("w" %in% names(ex)) w else NULL), silent=TRUE)
    if(inherits(fit,"try-error")) return(tibble())
    tk <- try(TukeyHSD(fit)[[1]], silent=TRUE)
    if(inherits(tk,"try-error")) return(tibble())
    pv <- tk[,"p adj"]; names(pv) <- rownames(tk)
    lets <- multcompView::multcompLetters(pv)$Letters
    tibble(assessment_timing_code=tm, !!xvar := names(lets), letters=unname(lets))
  })
  res
}
ordinal_summary <- function(d, xvar="treatment_mod", control=NULL){
  map_dfr(unique(d$assessment_timing_code), function(tm){
    df <- d %>% filter(assessment_timing_code==tm, assessment_value>0, !is.na(Damage_Level))
    if(n_distinct(df[[xvar]])<2 || n_distinct(df$Damage_Level)<2) return(tibble())
    df[[xvar]] <- factor(df[[xvar]])
    fit <- try(ordinal::clm(Damage_Level ~ get(xvar), data=df, weights=assessment_value, link="logit"), silent=TRUE)
    if(inherits(fit,"try-error")) return(tibble(assessment_timing_code=tm, note="No ajustó CLM"))
    em <- try(emmeans::emmeans(fit, specs=as.formula(paste("~", xvar))), silent=TRUE)
    contr <- tibble()
    if(!inherits(em,"try-error") && !is.null(control) && control %in% levels(df[[xvar]])){
      contr <- try(as.data.frame(emmeans::contrast(em, method="trt.vs.ctrl", ref=which(levels(df[[xvar]])==control))))
      if(!inherits(contr,"try-error")) contr <- as_tibble(contr) %>% mutate(assessment_timing_code=tm)
    }
    tibble(assessment_timing_code=tm, AIC=AIC(fit), n=sum(df$assessment_value), model="CLM ordinal logit") %>% bind_cols(list(comparisons=list(contr)))
  })
}
incidence_control <- function(d, control, xvar="treatment_mod"){
  first_tm <- sort(unique(d$assessment_timing_code))[1]
  d %>% filter(assessment_timing_code==first_tm, .data[[xvar]]==control, Damage_Level %in% c("Medium Damage","High Damage")) %>%
    group_by(trial) %>% summarise(incidence=sum(assessment_value)/sum(d$assessment_value[d$trial==first(trial) & d$assessment_timing_code==first_tm & d[[xvar]]==control], na.rm=TRUE), .groups="drop") %>%
    mutate(assessment_timing_code=first_tm, incidence=percent(incidence, accuracy=0.1))
}
make_ppt <- function(file, plots, val, ord_tab, desc_tab=NULL){
  ppt <- read_pptx()
  ppt <- add_slide(ppt, layout="Title Slide", master="Office Theme")
  ppt <- ph_with(ppt, "Escala DAVIS", location=ph_location_type(type="ctrTitle"))
  ppt <- ph_with(ppt, "Reporte corporativo - análisis ordinal, validación y gráficos", location=ph_location_type(type="subTitle"))

  ppt <- add_slide(ppt, layout="Title and Content", master="Office Theme")
  ppt <- ph_with(ppt,"Validación de datos",location=ph_location_type(type="title"))
  ppt <- ph_with(ppt, flextable(head(val,18)) |> fontsize(size=8, part="all") |> autofit(), location=ph_location(left=.45, top=1.15, width=12.4, height=5.8))

  for(nm in names(plots)){
    ppt <- add_slide(ppt, layout="Blank", master="Office Theme")
    ppt <- ph_with(ppt, nm, location=ph_location(left=.35, top=.18, width=12.6, height=.35))
    # Gráfico dentro de márgenes 16:9, sin tabla encima.
    ppt <- ph_with(ppt, rvg::dml(ggobj=plots[[nm]]), location=ph_location(left=.35, top=.68, width=12.65, height=6.35))
  }

  if(!is.null(desc_tab) && nrow(desc_tab)>0){
    ppt <- add_slide(ppt, layout="Title and Content", master="Office Theme")
    ppt <- ph_with(ppt,"Tabla descriptiva - Overall",location=ph_location_type(type="title"))
    ppt <- ph_with(ppt, flextable(head(desc_tab,20)) |> fontsize(size=7.5, part="all") |> autofit(), location=ph_location(left=.35, top=1.05, width=12.7, height=5.95))
  }

  ppt <- add_slide(ppt, layout="Title and Content", master="Office Theme")
  ppt <- ph_with(ppt,"Modelo ordinal",location=ph_location_type(type="title"))
  ppt <- ph_with(ppt, flextable(head(ord_tab %>% select(-comparisons),20)) |> fontsize(size=8, part="all") |> autofit(), location=ph_location(left=.5, top=1.15, width=12.1, height=5.65))
  print(ppt, target=file)
}

ui <- page_navbar(
  title=span(class="brand-title", "Escala DAVIS"), theme=bs_theme(version=5, bootswatch="flatly"), header=tags$head(tags$link(rel="stylesheet", href="styles.css")),
  nav_panel("Inicio", div(class="hero", h1("Escala DAVIS"), p("Análisis corporativo de daño Davis 0-9 para datos Scout. Cargá archivo o pegá tabla, validá conteos, revisá modelos ordinales y exportá gráficos en PowerPoint.")),
            div(class="kpi", uiOutput("kpis")), div(class="card", h3("Cómo usarla"), tags$ol(tags$li("Cargá o pegá una tabla."), tags$li("Mapeá columnas; el eje X puede ser treatment_mod."), tags$li("Elegí testigo y armá grupos."), tags$li("Revisá gráficos, letras y modelo ordinal."), tags$li("Descargá PowerPoint.")))),
  nav_panel("Información requerida", div(class="card", h2("¿Qué tabla tengo que traer desde la plataforma?"), p("La aplicación espera una tabla en formato largo: una fila por combinación de ensayo, momento, tratamiento, repetición y clase Davis. Para este análisis se toman únicamente las filas con se_name = ES11AD2 y assessment_type_code = COUNT."), div(class="warning-box", "No incluir como dato principal las filas ABBOTT: reflejan otro cálculo y se excluyen automáticamente del análisis DAVIS por conteos."), h3("Encabezados recomendados para la query"), DTOutput("requirements_table"), hr(), h3("Regla de validación principal"), p("Para cada combinación trial + assessment_timing_code + treatment/treatment_mod + replicate_number, la suma de assessment_value entre las clases Davis debe ser igual a sample_size."), tags$pre("Ejemplo: CL.0 + CL.1 + ... + CL.9 = sample_size"), h3("Clasificación de daño"), tags$ul(tags$li("0 = Sin Daño"), tags$li("1-2 = Low Damage"), tags$li("3-6 = Medium Damage"), tags$li("7-9 = High Damage")))),
  nav_panel("Carga y mapeo", div(class="card", h3("Cargar información"), p("Podés subir un archivo o copiar la tabla directamente desde Excel/Google Sheets y pegarla en el cuadro. El pegado directo debe incluir la fila de encabezados."), radioButtons("input_mode","Modo de carga", c("Archivo"="file","Copiar y pegar"="paste"), inline=TRUE), conditionalPanel("input.input_mode=='file'", fileInput("file","Archivo Excel/CSV/TXT", accept=c(".xlsx",".xls",".csv",".txt"))), conditionalPanel("input.input_mode=='paste'", textAreaInput("paste_text","Pegá la tabla desde la hoja de cálculo", rows=12, placeholder="Copiá desde Excel incluyendo encabezados y pegá aquí. Ejemplo de columnas: trial, se_name, assessment_type_code, assessment_timing_code, treatment, treatment_mod, replicate_number, assessment_class, assessment_value, sample_size")), actionButton("load_data","Cargar tabla", class="btn-primary")), div(class="card", h3("Mapeo de columnas"), p("Si tu query trae otros nombres de encabezado, elegí aquí qué columna corresponde a cada campo requerido."), uiOutput("mapping_ui"), actionButton("apply_map","Aplicar mapeo", class="btn-primary")), div(class="card", h3("Vista previa"), DTOutput("preview"))),
  nav_panel("Validación", div(class="card", h3("Control de datos"), uiOutput("validation_msg"), DTOutput("validation_table"))),
  nav_panel("Configuración", div(class="card", h3("Análisis"), uiOutput("config_ui")), div(class="card", h3("Grupos manuales"), p("Escribí un nombre de grupo y seleccioná los trials incluidos."), textInput("group_name","Nombre del grupo"), uiOutput("group_trials_ui"), actionButton("add_group","Agregar grupo"), DTOutput("groups_table"))),
  nav_panel("Resultados", div(class="card", h3("Gráfico overall"), plotOutput("plot_overall", height="620px"), downloadButton("dl_overall_png","Descargar PNG")), div(class="card", h3("Tabla descriptiva overall"), p("Se muestra debajo del gráfico para evitar superposición y mantener la lectura clara."), DTOutput("descriptive_table")), div(class="card", h3("Modelo ordinal"), DTOutput("ordinal_table"))),
  nav_panel("Localidades", div(class="card", h3("Gráficos por localidad"), uiOutput("location_selector"), uiOutput("incidence_box"), plotOutput("plot_location", height="620px"))),
  nav_panel("Grupos", div(class="card", h3("Gráficos por grupo"), uiOutput("group_selector"), plotOutput("plot_group", height="620px"))),
  nav_panel("Exportar", div(class="card", h3("PowerPoint"), p("Exporta slides 16:9 con gráficos ajustados al tamaño de diapositiva."), downloadButton("dl_ppt","Descargar PPTX", class="btn-primary"))),
  nav_panel("? Ayuda", div(class="card", h3("Columnas necesarias"), HTML("<span class='help-pill'>trial</span><span class='help-pill'>se_name</span><span class='help-pill'>assessment_type_code</span><span class='help-pill'>assessment_timing_code</span><span class='help-pill'>treatment / treatment_mod</span><span class='help-pill'>replicate_number</span><span class='help-pill'>assessment_class</span><span class='help-pill'>assessment_value</span><span class='help-pill'>sample_size</span>"), hr(), h4("Fundamento"), p("El modelo principal es ordinal logit con ordinal::clm(), ponderado por assessment_value. Las letras Tukey se calculan sobre score Davis expandido/ponderado y se interpretan por cada momento y segmento: tratamientos que comparten letra no difieren significativamente."))))
)

server <- function(input, output, session){
  raw <- reactiveVal(NULL); dat <- reactiveVal(NULL); groups <- reactiveVal(tibble(group=character(), trial=character()))
  output$requirements_table <- renderDT({ datatable(column_requirements, rownames=FALSE, options=list(scrollX=TRUE, pageLength=10, dom='tip')) })
  observeEvent(input$load_data,{ df <- if(input$input_mode=="file") { req(input$file); read_any(input$file$datapath) } else read_paste(input$paste_text); raw(df) })
  output$preview <- renderDT({ req(raw()); datatable(head(raw(),100), options=list(scrollX=TRUE,pageLength=8)) })
  output$mapping_ui <- renderUI({ req(raw()); nms <- names(raw()); tagList(fluidRow(column(4, selectInput("m_trial","Trial/localidad",nms, selected=find_col(nms,c("trial")))), column(4, selectInput("m_se","se_name",nms, selected=find_col(nms,c("se_name")))), column(4, selectInput("m_atype","assessment_type_code",nms, selected=find_col(nms,c("assessment_type_code"))))), fluidRow(column(4, selectInput("m_timing","Momento",nms, selected=find_col(nms,c("assessment_timing_code")))), column(4, selectInput("m_treat","Treatment",nms, selected=find_col(nms,c("treatment","Trt.","trt")))), column(4, selectInput("m_treatmod","Treatment para eje X",nms, selected=find_col(nms,c("treatment_mod","treatment","Trt."))))), fluidRow(column(4, selectInput("m_rep","Repetición",nms, selected=find_col(nms,c("replicate_number","rep")))), column(4, selectInput("m_aclass","assessment_class",nms, selected=find_col(nms,c("assessment_class")))), column(4, selectInput("m_avalue","assessment_value",nms, selected=find_col(nms,c("assessment_value"))))), fluidRow(column(4, selectInput("m_ssize","sample_size",nms, selected=find_col(nms,c("sample_size")))))) })
  observeEvent(input$apply_map,{ req(raw()); map <- list(trial=input$m_trial,se_name=input$m_se,atype=input$m_atype,timing=input$m_timing,treatment=input$m_treat,treatment_mod=input$m_treatmod,rep=input$m_rep,aclass=input$m_aclass,avalue=input$m_avalue,ssize=input$m_ssize); dat(prep_data(raw(),map)) })
  val <- reactive({ req(dat()); validate_counts(dat()) })
  output$validation_msg <- renderUI({ req(val()); er <- sum(val()$status=="ERROR"); if(er==0) div(class="ok-box", "Validación OK: todas las sumas por trial + momento + tratamiento + repetición coinciden con sample_size.") else div(class="warning-box", paste("Hay",er,"combinaciones con diferencias contra sample_size.")) })
  output$validation_table <- renderDT({ req(val()); datatable(val(), options=list(scrollX=TRUE,pageLength=10)) })
  output$config_ui <- renderUI({ req(dat()); tagList(fluidRow(column(4, selectInput("xvar","Eje X", c("treatment_mod","treatment"), selected="treatment_mod")), column(4, selectInput("control","Testigo", sort(unique(dat()$treatment_mod)))), column(4, checkboxGroupInput("analyses","Salidas", c("Overall","Por localidad","Por grupos"), selected=c("Overall","Por localidad","Por grupos")))) ) })
  output$group_trials_ui <- renderUI({ req(dat()); selectizeInput("group_trials","Trials", choices=sort(unique(dat()$trial)), multiple=TRUE) })
  observeEvent(input$add_group,{ req(input$group_name, input$group_trials); groups(bind_rows(groups(), tibble(group=input$group_name, trial=input$group_trials))) })
  output$groups_table <- renderDT({ datatable(groups(), options=list(pageLength=8)) })
  letters_all <- reactive({ req(dat(), input$xvar); tukey_letters(dat(), input$xvar) })
  overall_plot <- reactive({ req(dat(), input$xvar); plot_stack(dat(), input$xvar, "Distribución de daño Davis - Overall", "Barras apiladas con % y letras Tukey por momento", letters_all()) })
  output$plot_overall <- renderPlot({ overall_plot() }, res=120)
  desc_overall <- reactive({ req(dat(), input$xvar); descriptive_summary(dat(), input$xvar) })
  output$descriptive_table <- renderDT({ req(desc_overall()); datatable(desc_overall(), rownames=FALSE, options=list(scrollX=TRUE, pageLength=10)) })
  output$dl_overall_png <- downloadHandler(filename=function()"overall_davis.png", content=function(file){ ggsave(file, overall_plot(), width=13, height=7.2, dpi=220) })
  ord <- reactive({ req(dat(), input$xvar); ordinal_summary(dat(), input$xvar, input$control) })
  output$ordinal_table <- renderDT({ req(ord()); datatable(ord() %>% select(-comparisons), options=list(scrollX=TRUE)) })
  output$location_selector <- renderUI({ req(dat()); selectInput("sel_trial","Localidad", sort(unique(dat()$trial))) })
  output$incidence_box <- renderUI({ req(dat(), input$control); inc <- incidence_control(dat(), input$control, input$xvar) %>% filter(trial==input$sel_trial); div(class="ok-box", paste0("Incidencia testigo en primera evaluación (Medium + High): ", ifelse(nrow(inc), inc$incidence, "sin datos"))) })
  output$plot_location <- renderPlot({ req(input$sel_trial); dd <- dat()%>%filter(trial==input$sel_trial); lets <- tukey_letters(dd,input$xvar); plot_stack(dd,input$xvar,paste("Localidad:",input$sel_trial),"% por nivel de daño + letras Tukey",lets) }, res=120)
  output$group_selector <- renderUI({ req(groups()); selectInput("sel_group","Grupo", unique(groups()$group)) })
  output$plot_group <- renderPlot({ req(input$sel_group); tr <- groups()%>%filter(group==input$sel_group)%>%pull(trial); dd <- dat()%>%filter(trial %in% tr); lets <- tukey_letters(dd,input$xvar); plot_stack(dd,input$xvar,paste("Grupo:",input$sel_group),"Trials seleccionados manualmente",lets) }, res=120)
  output$kpis <- renderUI({ if(is.null(dat())) return(tagList(div(class="kpi-box", b("-"), span("Trials")),div(class="kpi-box", b("-"), span("Tratamientos")),div(class="kpi-box", b("-"), span("Momentos")))); d<-dat(); tagList(div(class="kpi-box", b(n_distinct(d$trial)), span("Trials")), div(class="kpi-box", b(n_distinct(d$treatment_mod)), span("Tratamientos")), div(class="kpi-box", b(n_distinct(d$assessment_timing_code)), span("Momentos")), div(class="kpi-box", b(sum(d$assessment_value,na.rm=TRUE)), span("Plantas evaluadas"))) })
  output$dl_ppt <- downloadHandler(filename=function() paste0("Escala_DAVIS_Reporte_",Sys.Date(),".pptx"), content=function(file){ req(dat()); plots <- list("Overall"=overall_plot()); for(tr in unique(dat()$trial)){ dd<-dat()%>%filter(trial==tr); plots[[paste("Localidad",tr)]] <- plot_stack(dd,input$xvar,paste("Localidad:",tr),"% por nivel de daño + letras Tukey",tukey_letters(dd,input$xvar)) }; for(gr in unique(groups()$group)){ tr<-groups()%>%filter(group==gr)%>%pull(trial); dd<-dat()%>%filter(trial %in% tr); plots[[paste("Grupo",gr)]]<-plot_stack(dd,input$xvar,paste("Grupo:",gr),"Trials seleccionados",tukey_letters(dd,input$xvar))}; make_ppt(file, plots, val(), ord(), desc_overall()) })
}
shinyApp(ui, server)
