#backup version
# RShiny app for UI and server; needs dynamic_helper.R function
# Changes to be made to adapt in this script have been commented with 'Provide'

# # Install + load libraries as required ----

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny, warn.conflicts = F)

if (!requireNamespace("waiter", quietly = TRUE)) {
  install.packages("waiter")
}
library(waiter, warn.conflicts = F)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl, warn.conflicts = F)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib, warn.conflicts = F)

if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
library(shinythemes, warn.conflicts = F)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets, warn.conflicts = F)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport, warn.conflicts = F)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse, warn.conflicts = F)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr, warn.conflicts = F)  # For reading and writing CSV files

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges, warn.conflicts = F)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2, warn.conflicts = F)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr, warn.conflicts = F)

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here, warn.conflicts = F)

if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")
}
library(ggtext, warn.conflicts = F)

if (!requireNamespace("ggh4x", quietly = TRUE)) {
  install.packages("ggh4x")
}
library(ggh4x, warn.conflicts = F)
# Provide specific packages you have used other than the ones mentioned above
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")
}
library(ggtext, warn.conflicts = F)

if (!requireNamespace("png", quietly = TRUE)) {
  install.packages("png")
}
library(png, warn.conflicts = F)

if (!requireNamespace("grid", quietly = TRUE)) {
  install.packages("grid")
}
library(grid, warn.conflicts = F)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork, warn.conflicts = F)

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate, warn.conflicts = F)

if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo, warn.conflicts = F)

if (!requireNamespace("RcppRoll", quietly = TRUE)) {
  install.packages("RcppRoll")
}
library(RcppRoll, warn.conflicts = F)

if (!requireNamespace("compiler", quietly = TRUE)) {
  install.packages("compiler")
}
library(compiler, warn.conflicts = F)

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table, warn.conflicts = F)

# Load functins and inputs ----
# Provide Location of DA model script(s), dynamic-helper
# source("functions/saveLoad-module.R")

#asparagus scripts
source("functions/youtputs_to_xinputs_scenarios.R")
source("functions/plot_yield_asparagus.R")
source("functions/asparagus_sim_scen_app.R")

#onion scripts
source("functions/01_weather_shiny.R")
source("functions/02_helpers_shiny.R")
source("functions/onion_model_shiny.R")

#strawberry scripts


# Dynamic helper to load the UI variables from the excel sheet
source("functions/dynamic_helper_v3.R")
# Provide Location of excel workbook containing the input parameters (prepared for the dynamic-helper)
file_path_vars <- "data/ASP_input_parameters_german1.xlsx"
sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)

#load additional input table, weather and scenario_ids
risk_path<-file.path("data", "risk_df.csv")
scenario_path<-file.path("data", "scenarios.csv")
risk_df<-read.csv(risk_path)
scenarios<-read.csv(scenario_path)

onion_weather_path<-file.path("data", "weather_koeln-bonn_final_compressed.rds")
onion_weather<-readRDS(onion_weather_path)


# Interface ----
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  
  use_waiter(),
  # Set actual browser tab title and favicon
  tags$head(
    tags$title("Agriculture Decision Support Tool"),
    tags$link(rel = "shortcut icon", href = "INRES.png"),
    
    tags$style(HTML("
    /* Scroll wrapper: scrolls horizontally *and* vertically only when needed */
    .scroll-xy {
      overflow-x: auto;                 /* left–right scroll  */
      overflow-y: auto;                 /* top–bottom scroll  */
      -webkit-overflow-scrolling: touch;/* smooth on iOS      */
      max-height: 80vh;                 /* optional: stop it taking more than
                                         80 % of the viewport height       */
  }
  
  /* Keep any Shiny plot inside that wrapper from shrinking */
  .scroll-xy .shiny-plot-output {
    min-width: 900px;                 /* choose your desktop width */
  }
                    ")
    )
  ),
  ## Header ----
  tags$div(
    style = "display:flex; align-items:center;justify-content:space-between;
      width: 100% !important; margin: 20px; padding: 0 15px;
      box-sizing: border-box; background-color: #f2f2f2;",
    
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    tags$img(src = "UniBonnHortiBonn_logo_transparent.png", height = "100px",
             style = "margin-left: auto; max-width: 20%; height: auto; cursor: pointer;"),
    # ),
    # Provide Title of the DA model
    tags$h2(tags$div("Auswirkungen des Klimawandels auf künftige Ernteerträge in 2075"),
            #tags$div("conventional asparagus growing in 2075"),
            style = "text-align: center; flex-grow: 1;"),
    # Provide Project Logo
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    tags$img(src = "mlv-logo.png", height = "100px",
             style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;")
    # ),
  ),
  
  
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(width = 4,
                 style = "height: 100%; overflow-y: auto",
                 
                 accordion(
                   id = "collapseSidebar",
                   open = FALSE,
                   
                   div(
                     class = "text-center",
                     actionButton("run_simulation", "Modell ausführen",
                                  icon = icon("play"), class = "btn-primary")
                   ),
                   br(),
                   ### Accordions ----
                   #### Save/Load functionality ----
                   # saveLoadUI("savemod"),
                   accordion_panel(
                     title = "Projekt speichern/laden", value="save_load", icon = icon("folder-open"), #id="sll",
                     tagList(
                       textInput("state_name", "Projektname"),
                       actionButton("save_btn",  label = tagList(icon("floppy-disk"),  "Speichern"  ), class = "btn btn-dark"),
                       
                       br(), br(),
                       selectInput("state_picker", "Gespeicherte Versionen", choices = NULL),
                       
                       fluidRow(
                         column(6, actionButton("load_btn",   tagList(icon("rotate"),  "Laden"  ), class = "btn btn-secondary")),
                         column(6, actionButton("delete_btn", tagList(icon("trash"),   "Löschen"), class = "btn btn-secondary"))
                       ),
                       hr(),
                       downloadButton("download_csv", label = tagList(icon("download"), "Aktuelle Eingaben herunterladen (.csv)"))
                     )
                   ),
                   #### Crop filter ----
                   accordion_panel(
                     title = "Pflanzenauswahl",
                     icon = icon("clipboard-question"),
                     tagList(
                       tags$h5(
                         #"Crop",
                         # tags$span(
                         #   icon("circle-question"),
                         #   title = "Select your main expertise to view and edit only relevant variables.\nNot selecting any box shows all variables.\nDefaults apply to unselected categories in simulations.",
                         #   style = "cursor: help; margin-left: 8px;"
                         # )
                       ),
                       uiOutput("crop_filter_ui")
                     )
                   ),
                   
                   #### Expertise filter ----
                   accordion_panel(
                     title = "Fachgebietskategorien",
                     icon = icon("clipboard-question"),
                     tagList(
                       tags$h5(
                         "Fachwissen",
                         tags$span(
                           icon("circle-question"),
                           title = "Wählen Sie Ihr Hauptfachgebiet aus, um nur relevante Variablen anzuzeigen und zu bearbeiten.\n Wenn Sie kein Kästchen auswählen, werden alle Variablen angezeigt.\n In Simulationen gelten die Standardeinstellungen für nicht ausgewählte Kategorien.",
                           style = "cursor: help; margin-left: 8px;"
                         )
                       ),
                       uiOutput("category_filter_ui")
                     )
                   ),
                   
                   #### Dynamic elements ----
                   uiOutput("dynamic_element_ui")
                 )
    ),
    
    ## Main Panel ----
    mainPanel(width = 8,
              ###Tool description ----
              # Provide brief explanation of the DA model
              tags$h6(
                "Diese App simuliert die potenziellen Erträge der ausgewählten Kulturpflanze im Jahr 2075 unter verschiedenen SSP-Klimaszenarien.",
                tags$br(),
                tags$br(),
                "Verwenden Sie die Register auf der linken Seite, um die Variablenbereiche entsprechend Ihren örtlichen Gegebenheiten anzupassen.",
                tags$br(),
                tags$br(),
                "Klicken Sie auf „Modell ausführen“, um eine Monte-Carlo-Simulation mit zufälligen Kombinationen aus den von Ihnen definierten Bereichen durchzuführen. Sie können Eingaben speichern/laden. Sobald das Modell ausgeführt wird, werden die Ergebnisse unten angezeigt und Sie können diese Zahlen speichern.",
                tags$br(),
                tags$br(),
                "Über Rückmeldungen würden wir uns sehr freuen. Bitte kontaktieren Sie:",
                tags$br(),
                tags$ul(
                  tags$li(tags$a(href = "mailto:jbsl@uni-bonn.de", "Jan-Bernd Schulze Lutum"), "für die Spargel- und Erdbeermodelle,"),
                  #tags$br(),
                  tags$li(tags$a(href = "mailto:s7padaib@uni-bonn.de", "Paul Daiber"), "für das Zwiebelmodell,"),
                  #tags$br(),
                  tags$li(tags$a(href = "mailto:pkasargo@uni-bonn.de", "Prajna Kasargodu Anebagilu"), "oder", tags$a(href = "mailto:afuelle1@uni-bonn.de", "Adrian Fülle."), "für Fragen die dieses Tool betreffen."),
                )
              ),
              br(), br(),
              
              ### Plots ----
              #imageOutput("myImage"),
              div(class = "scroll-xy",
                  imageOutput("myImage", height = "550px"),
              ),
              br(),
              uiOutput("download_ui"),
              br(), br(),br(), br(),
              #downloadButton("downloadImage", "Konzeptmodell herunterladen"),
              
              br(), br(),
              div(class = "scroll-xy",
                  plotOutput("plot1_ui", height = "550px"),
              ),
              br(),
              uiOutput("plot1_dl_ui"),
              br(), br(),br(), br(),
              
              div(class = "scroll-xy",
                  plotOutput("plot2_ui", height = "550px"),
              ),
              br(),
              uiOutput("plot2_dl_ui"),
              br(), br(),br(), br(),
              
              div(class = "scroll-xy",
                  plotOutput("plot3_ui", height = "700px"),
              ),
              br(),
              uiOutput("plot3_dl_ui"),
              br(), br(),br(), br(),
              
              div(class = "scroll-xy",
                  plotOutput("plot4_ui", height = "550px"),
              ),
              br(),
              uiOutput("plot4_dl_ui"),
              br(), br(),br(), br(),
              
              div(class = "scroll-xy",
                  plotOutput("plot5_ui", height = "550px"),
              ),
              br(),
              uiOutput("plot5_dl_ui"),
              br(), br(),br(), br(),
              
              # div(class = "scroll-xy",
              # plotOutput("plot6_ui", height = "550px"),
              # ),
              # br(),
              # uiOutput("plot6_dl_ui"),
              # br(), br(),br(), br(),
              # 
              # div(class = "scroll-xy", plotOutput("plot7_ui", height = "550px"),),
              # br(),
              # uiOutput("plot7_dl_ui"),
              # br(), br(),br(), br(),
              
              # div(class = "scroll-xy", 
              #     plotOutput("plot8_ui", height = "550px"),
              # ),
              # br(),
              # uiOutput("plot8_dl_ui"),
              
              ### Requisites ----
              # Provide Funding declaration
              tags$img(src = "Funding_declaration.png", height = "100px",
                       style = "margin-right: auto; max-width: 100%; height: auto; cursor: pointer;"),
              #Provide tool usage disclaimer
              tags$p(
                tags$a("Disclaimer", href = "https://www.gartenbauwissenschaften.uni-bonn.de/imprint",
                       target = "_blank"),
                " | ",  
                #Provide the correct link once the app and codes are hosted in HortiBonn repo
                tags$a("View Source", href = "https://github.com/JBSLutum/asparagus_cif_app", # temp link
                       target = "_blank")
              ),
              br(), br(),br(), br(),
              
    )
  )
  
)


# Server ----
server <- function(input, output, session) {
  
  ## Dynamic UI inputs ----
  
  # read in input xlsx file
  excelData <- reactive({
    sheet_number <- seq_along(sheet_names)+1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars, sheet = sht,
                         col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text", "text")
      )
    })
    names(all_sheets) <- sheet_names
    all_sheets
  })
  
  ## Dynamic expertise-filter module ----
  # helper that sanitises category names into safe IDs
  sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  
  # all categories across every sheet
  categories <- reactive({
    cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
    cats <- cats[!is.na(cats) & cats != ""]
    trimws(unique(unlist(strsplit(cats, ";"))))
  })
  
  # all crop across every sheet from input excel
  crop_categories <- reactive({
    crop_cats <- unique(unlist(lapply(excelData(), function(df) df$Crop)))
    crop_cats <- crop_cats[!is.na(crop_cats) & crop_cats != ""]
    trimws(unique(unlist(strsplit(crop_cats, ";"))))
  })
  
  # Expertise filter UI (unchanged)
  output$category_filter_ui <- renderUI({
    if (length(categories()) == 0) return(NULL)
    tagList(lapply(categories(), function(cat){
      checkboxInput(paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
    }))
  })
  
  # Crop filter UI (prefix -> crop_)
  output$crop_filter_ui <- renderUI({
    crops <- crop_categories()
    if (!length(crops)) return(NULL)
    
    tagList(
      # group them in a container so we can scope the JS
      tags$div(class = "crop-filter",
               lapply(crops, function(crop_cat){
                 checkboxInput(paste0("crop_", sanitize_id(crop_cat)), crop_cat, value = FALSE)
               })
      ),
      # JS: when one crop box is checked, uncheck all others in the same container
      tags$script(HTML("
      $(document).on('change', '.crop-filter input[type=checkbox]', function(){
        var $box = $(this);
        if ($box.is(':checked')) {
          $('.crop-filter input[type=checkbox]').not($box).prop('checked', false).trigger('change');
        }
      });
    "))
    )
  })
  
  # util: turns a category vector into a JS condition 
  ## render but hide unchecked expertise categories - default show-all
  panel_condition <- function(cat_vec) {
    cat_vec <- trimws(cat_vec)
    cat_vec <- cat_vec[cat_vec != "" & !is.na(cat_vec)]
    if (length(cat_vec) == 0) return("true")
    
    ids <- sanitize_id(cat_vec)
    
    # Expertise (cat_)
    exp_ids <- sprintf("input['cat_%s']", ids)
    exp_show_all <- paste0(
      "Object.keys(input).filter(k => k.startsWith('cat_')).",
      "every(k => input[k] === false)"
    )
    exp_clause <- sprintf("(%s) || (%s)", exp_show_all, paste(exp_ids, collapse = " || "))
    
    # Crops (crop_)
    crop_ids <- sprintf("input['crop_%s']", ids)
    crop_show_all <- paste0(
      "Object.keys(input).filter(k => k.startsWith('crop_')).",
      "every(k => input[k] === false)"
    )
    crop_clause <- sprintf("(%s) || (%s)", crop_show_all, paste(crop_ids, collapse = " || "))
    
    # Beide Gruppen müssen passen
    sprintf("(%s) && (%s)", crop_clause, exp_clause)
  }
  
  output$dynamic_element_ui <- renderUI({
    
    data_list   <- excelData()
    sheet_names <- names(data_list)
    
    # build one accordion panel per sheet
    # the elements are generated via the external function create_ui_element()
    panels <- lapply(seq_along(data_list), function(j) {
      
      sheet <- data_list[[j]]
      
      cats_exp  <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats_crop  <- unique(trimws(unlist(strsplit(sheet$Crop %||% "", ";|,"))))
      cats <- unique(c(cats_exp[cats_exp!=""], cats_crop[cats_crop!=""]))
      
      ui_elems <- lapply(seq_len(nrow(sheet)), function(i) {
        create_ui_element(sheet[i, ])
      })
      
      conditionalPanel(
        condition = panel_condition(cats),   # hide panel is empty
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    tagList(panels)   # render the list
  })
  
  
  ## Save, Load and Delete module
  all_inputs <- reactive({
    names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))]
  })
  
  current_input_table <- reactive({
    variables <- all_inputs()
    
    lower_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    })
    upper_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    })
    
    # 2. Re-read Excel (keeps original bounds & distributions)
    all_sheets <- excelData()            # list of data-frames
    input_file <- bind_rows(all_sheets)  # one big table
    
    # Overwrite lower/upper with current UI inputs
    input_file <- input_file %>%
      left_join(
        tibble(variable = variables,
               lower    = lower_values,
               upper    = upper_values),
        by = "variable",
        suffix = c("", ".new")
      ) %>%
      mutate(
        lower = coalesce(lower.new, lower),
        upper = coalesce(upper.new, upper)
      ) %>%
      select(-ends_with(".new"))
    
    #View(input_file)
    
    # # 3. Save UI snapshot (optional)
    # saveRDS(list(sheet_names, input_file), "data/Walnut_grain_veg_tub_ui_updated.RDS")
    
    # 4. clean-up: keep only numeric rows
    input_file <- input_file %>%
      filter(
        !is.na(lower), !is.na(upper),
        is.finite(lower), is.finite(upper)
      )
    
    # write.csv(input_file,"data/input_table.csv",row.names = F)
    
    input_file
  })
  
  ## Save/Load functionality ----
  # saveLoadServer("savemod", current_input_table)
  # Provide Folder name instead of the current 'Germany' to store user saves
  get_base_dir <- function() {
    if (Sys.info()[["sysname"]] == "Windows")
      "user-states/Germany_onion"
    else
      "/srv/shiny-app-data/user-states/Germany_onion"
  }
  
  get_user_dir <- function() {
    uid <- session$user
    safe_uid <- if (is.null(uid) || uid == "") "anon"
    else gsub("[^A-Za-z0-9_.-]", "_", uid)
    dir <- file.path(get_base_dir(), safe_uid)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
  }
  
  timestamp_name <- function(raw) {
    paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"_",
           gsub("[^A-Za-z0-9_.-]", "_", raw), ".rds")
  }
  
  observeEvent(input$save_btn, {
    dir  <- get_user_dir()
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(files) >= 1000) {
      showModal(modalDialog("You already have several versions. Delete one first.",
                            easyClose = TRUE))
      return()
    }
    
    req(nzchar(input$state_name))
    saveRDS(
      list(input_table = current_input_table(),
           raw_inputs  = reactiveValuesToList(input)),
      file.path(dir, timestamp_name(input$state_name)), 
      compress="xz"
    )
  })
  
  saved_files <- reactiveFileReader(
    2000, session, get_user_dir(),
    function(dir) sort(list.files(dir, pattern = "\\.rds$", full.names = TRUE),decreasing = T)
  )
  
  observe({
    updateSelectInput(session, "state_picker",
                      choices = basename(saved_files()))
  })
  
  observeEvent(input$load_btn, {
    req(input$state_picker)
    obj <- readRDS(file.path(get_user_dir(), input$state_picker))
    bslib::accordion_panel_open("collapseSidebar",TRUE,session)
    vals <- obj$raw_inputs
    
    restore_one <- function(id, val) {
      if (is.null(val)) return()
      switch(class(val)[1],
             numeric   = updateNumericInput(session, id, value = val),
             integer   = updateNumericInput(session, id, value = val),
             character = updateTextInput   (session, id, value = val),
             logical   = updateCheckboxInput(session, id, value = val),
             factor    = updateSelectInput (session, id, selected = as.character(val)),
             # length-2 numeric == slider
             { if (is.numeric(val) && length(val) == 2)
               updateSliderInput(session, id, value = val) }
      )
    }
    
    # ordinary widgets
    lapply(names(vals), \(id) try(restore_one(id, vals[[id]]), silent = TRUE))
  })
  
  observeEvent(input$delete_btn, {
    req(input$state_picker)
    unlink(file.path(get_user_dir(), input$state_picker))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("current_input_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(current_input_table(), file)
  )
  
  selected_crop <- reactive({
    crop_ids <- grep("^crop_", names(input), value = TRUE)
    if (length(crop_ids)== 0) return(NULL)
    #print(crop_ids)
    checked  <- crop_ids[sapply(crop_ids, function(id) isTRUE(input[[id]]))]
    if (length(checked) == 0) return(NULL)
    sub("^crop_", "", checked[1])
  })
  
  
  ## Monte Carlo Simulation ----
  mcSimulation_results <- eventReactive(input$run_simulation, {
    input_file <- current_input_table()
    crop <- selected_crop()
    #print(crop)
    # bind into the function's environment:
    
    if (is.null(crop) || !nzchar(crop)) {
      showModal(
        modalDialog(
          title = "Keine Kulturpflanze ausgewählt",
          "Bitte wählen Sie eine Kulturpflanze aus, bevor Sie die Simulation starten.",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return(NULL)  # do not continue
    }
    
    else if (crop=="Spargel"){
      environment(asparagus_sim_scen)$scenarios <- scenarios
      environment(asparagus_sim_scen)$risk_df <- risk_df
      # loading animation
      waiter_show(
        html = tagList(
          spin_fading_circles(),
          "Modell ausführen und Abbildungen erstellen ..."
        ),
        color = "rgba(0, 0, 0, 0.8)"
      )
      
      # If something throws, hide the waiter so users aren't stuck
      ok <- FALSE
      on.exit({
        if (!ok) waiter_hide()
      }, add = TRUE)
      # end of loading animation
      decisionSupport::mcSimulation(
        estimate          = decisionSupport::as.estimate(input_file),
        model_function    = asparagus_sim_scen,
        numberOfModelRuns = input$num_simulations_c,
        functionSyntax    = "plainNames"
      )
      # #part of loading animation
      # session$onFlushed(function() waiter_hide(), once = TRUE)
      # ok <- TRUE
      
    }
    else if (crop=="Zwiebel"){
      #environment(onion_model_shiny)$onion_weather <- onion_weather
      environment(process_weather_data)$onion_weather <- onion_weather # get weather file
      
      # weather_precomputed <- process_weather_data(
      #   file_path = onion_weather_path,
      #   base_temp = 1
      # )
      # loading animation
      waiter_show(
        html = tagList(
          spin_fading_circles(),
          "Modell ausführen und Abbildungen erstellen ..."
        ),
        color = "rgba(0, 0, 0, 0.8)"
      )
      
      # If something throws, hide the waiter so users aren't stuck
      ok <- FALSE
      on.exit({
        if (!ok) waiter_hide()
      }, add = TRUE)
      # end of loading animation
      
      decisionSupport::mcSimulation(
        estimate = decisionSupport::as.estimate(input_file),
        model_function = onion_climate_impact,
        numberOfModelRuns = input$num_simulations_c,
        functionSyntax = "plainNames"
      )
      # #part of loading animation
      # session$onFlushed(function() waiter_hide(), once = TRUE)
      # ok <- TRUE
      # 
    }
    else{
      shiny::showNotification(
        "keine Nutzpflanze ausgewählt",
        type = "message"
      )}
    
  })
  
  ## Generating plots ----
  # helper to add title subtile caption etc
  add_meta <- function(p, title, subtitle = NULL, caption = NULL,
                       legend = "bottom") {
    
    p +
      labs(title = title, subtitle = subtitle, caption = caption) +
      theme(
        plot.title = element_textbox_simple(
          size   = 24,
          face   = "bold",
          width  = unit(1, "npc"),  # full plot width
          halign = 0.5,              # centered
          margin = margin(t = 6,b = 20)
        ),
        plot.subtitle = element_textbox_simple(
          size   = 18,
          width  = unit(1, "npc"),
          halign = 0.5,
          margin = margin(t = 6,b = 20)
        ),
        plot.caption  = element_textbox_simple(
          size   = 16,
          width  = unit(0.98, "npc"),
          halign = 0,              # left-aligned
          margin = margin(t = 6,b = 20),
          hjust = 0,
          vjust = 1
        ),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text     = element_text(size = 14, hjust = 0.5),
        legend.position = legend,
        plot.margin = margin(t = 50, r = 10, b = 50, l = 10, unit = "pt")
        
      )  }
  
  # download helper
  make_download <- function(id, plot_obj, filename, width = 13, height = 5, dpi = 300, scale = 2) {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content  = function(file) {
        # device is inferred from file extension; here "png"
        ggsave(file, plot_obj, width = width, height = height, dpi = dpi, scale = scale)
      }
    )
  }
  
  
  
  observeEvent(mcSimulation_results(), {
    mc_data <- mcSimulation_results()
    # Provide correct variables for plots
    # plot1 <-
    #   decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
    #                      vars =c("marketable_yield_today", "marketable_yield_ssp1", "marketable_yield_ssp2", "marketable_yield_ssp3", "marketable_yield_ssp5"),
    #                      old_names = c("marketable_yield_today", "marketable_yield_ssp1", "marketable_yield_ssp2", "marketable_yield_ssp3", "marketable_yield_ssp5"),
    #                      new_names = c("marketable yield\n2020", "marketable yield\n2075 SSP1 scenario", "marketable yield\n2075 SSP2 scenario", "marketable yield\n2075 SSP3 scenario", "marketable yield\n2075 SSP5 scenario"),
    #                      method = 'boxplot', 
    #                      base_size = 20, 
    #                      x_axis_name = "Compare of marketable yield outcomes")
    # plot1 <- plot1 + ggplot2::coord_flip()
    
    crop <- selected_crop()
    # bind into the function's environment:
    
    
    if (crop=="Spargel"){
      #restructure output, write additional parameters that are used in the model
      #form output to input side for analysis
      outputs<-c("water_stress_risk",
                 "insect_risk",
                 "disease_risk",
                 "photosynthetic_active_days",
                 "weather_damage_risk",
                 "growth_start_doy",
                 "speargrowth",
                 "chill_portions",
                 "late_frost_risk",
                 "temp_fluctuation_risk",
                 "extreme_rainfall_risk",
                 "extreme_heat_risk",
                 "Tsoil_mean")
      
      mc_data_order<-youtputs_to_xinputs_scenarios(mc_data, outputs)
      source("functions/plot_yield_asparagus.R")
      plot1<-plot_yield_asparagus(mc_data_order) |>
        add_meta(
          title    = "Figure 1. Distribution of the *incremental* NPV",
          subtitle = "Difference between agroforestry and treeless farming under identical conditions",
          caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
                      as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
                      The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
          , legend = "none")
      
      source("functions/VIP_plot.R") 
      plot2<-VIP_plot(mc_data_order)
      # |>
      #   add_meta(
      #     title    = "Figure 1. Distribution of the *incremental* NPV",
      #     subtitle = "Difference between agroforestry and treeless farming under identical conditions",
      #     caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
      #                 as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
      #                 The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
      #     , legend = "none")
      
      # No plot3 for Spargel → clear outputs
      output$plot3_ui    <- renderPlot(NULL)
      output$plot3_dl_ui <- renderUI(NULL)
    }
    
    else if (crop=="Zwiebel"){
      
      source("functions/yield_boxplot_onion.R")
      plot1<-plot_yield_onion(mc_data) |>
        add_meta(
          title    = "Figure 1. Distribution of the *incremental* NPV",
          subtitle = "Difference between agroforestry and treeless farming under identical conditions",
          caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
                      as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
                      The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
          , legend = "none")
      
      source("functions/VIP_Plot_onion.R")
      plot2<-make_onion_vip_combined_plot(mc_data) 
      # |>
        # add_meta(
        #   title    = "Figure 1. Distribution of the *incremental* NPV",
        #   subtitle = "Difference between agroforestry and treeless farming under identical conditions",
        #   caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
        #               as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
        #               The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
        #   , legend = "none")
      # replace this line with codes in lines 911 - 919
      plot3<-make_onion_vip_combined_plot(mc_data)
      # |>
      #   add_meta(
      #     title    = "Figure 1. Distribution of the *incremental* NPV",
      #     subtitle = "Difference between agroforestry and treeless farming under identical conditions",
      #     caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
      #                 as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
      #                 The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
      #     , legend = "none")
      
      # source("functions/yield_reduction_plot_onion.R")
      # plot3<-make_yield_reduction_heatmap(mc_data) |>
      # add_meta(
      #   title    = "Figure 1. Distribution of the *incremental* NPV",
      #   subtitle = "Difference between agroforestry and treeless farming under identical conditions",
      #   caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
      #                 as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
      #                 The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
      #   , legend = "none")
    }
    # source("functions/plot_yield_asparagus.R")
    # plot1<-plot_yield_asparagus(mc_data_order)
    # 
    # source("functions/VIP_plot.R")
    # plot2<-VIP_plot(mc_data_order)
    #     plot2 <- decisionSupport::plot_distributions(
    #       mc_data, "NPV_decis_AF_ES3",
    #       method     = "smooth_simple_overlay",
    #       old_names  = "NPV_decis_AF_ES3",
    #       new_names  = "Agroforestry – Treeless",
    #       x_axis_name= "NPV (€)",
    #       y_axis_name= "Probability") |>
    #       add_meta(
    #         title    = "Figure 2. Distribution of the *incremental* NPV",
    #         subtitle = "Difference between agroforestry and treeless farming under identical conditions",
    #         caption  = "Figure 2 shows the NPV distributions of the decision to establish the apple alley cropping system
    #                 as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
    #                 The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
    #         , legend = "none")
    #     
    #     plot3 <- decisionSupport::plot_distributions(
    #       mc_data,
    #       vars      = c("NPV_decis_no_fund", "NPV_decis_AF_ES3", "NPV_decis_DeFAF"),
    #       method    = "boxplot",
    #       old_names = c("NPV_decis_no_fund", "NPV_decis_AF_ES3", "NPV_decis_DeFAF"),
    #       new_names = c("Agroforestry without\nfunding - Treeless", "Agroforestry with\ncurrent funding - Treeless", "Agroforestry with\nDeFAF-suggested funding - Treeless"),
    #       x_axis_name = "NPV (€)",
    #       y_axis_name = "Funding Options") |>
    #       add_meta(
    #         title    = "Figure 3. Net Present Value (NPV) Outcomes Across Funding Schemes for Apple Alley Cropping",
    #         subtitle = "Agroforestry intervention with, without and DeFAF-suggested funding",
    #         caption  = 'Figure 3 shows the comparison of net present value (NPV) outcomes for the decision of different agroforestry funding schemes. The x-axis displays NPV values (i.e., the sum of discounted annual cash flows); each colored boxplot represents a funding scheme, showing the range and distribution of simulation results from the probabilistic model.
    #         The higher and wider the box, the greater the potential return and variability in outcomes under that funding.
    # Scenarios involving funding (like DeFAF-suggested or EcoScheme3 and regional) generally show higher NPV ranges than the No funding, however it not necessarily better suggesting the current financial support is insufficient to sustain agroforestry.'
    #       )
    #     
    #     plot4 <- decisionSupport::plot_cashflow(
    #       mc_data, "AF_CF",
    #       x_axis_name = "",
    #       y_axis_name = "Annual cash-flow from Agroforestry (€)",
    #       color_25_75 = "navajowhite",
    #       color_5_95 = "green4",
    #       color_median = "darkblue",
    #       facet_labels = "") |>
    #       add_meta(
    #         title   = "Figure 4. Annual cash-flow of the agroforestry intervention", 
    #         subtitle = "Projected yearly cash-flow variability for an agroforestry system over time",
    #         caption = 'Figure 4 shows how annual cash-flow from an agroforestry intervention is expected to evolve, based on a probabilistic simulation. The shaded areas represent uncertainty ranges (from lower to upper quantiles), while the blue line shows the median outcome (expressed in €). While early years may involve negative cash flow, profitability tends to improve over time, with increasing stability. The graph highlights the long-term financial potential and risk spread of adopting agroforestry practices.'
    #       )
    #     
    #     plot5 <- decisionSupport::plot_cashflow(
    #       mc_data, "AF_CCF_ES3",
    #       x_axis_name = "",
    #       y_axis_name = "Cumulative cash-flow from Agroforestry (€)",
    #       color_25_75 = "navajowhite",
    #       color_5_95 = "green4",
    #       color_median = "darkblue",
    #       facet_labels = "") |>
    #       add_meta(
    #         title   = "Figure 5. Cumulative cash-flow of the agroforestry intervention", 
    #         subtitle = "Long-term cumulative cash-flow projection for an agroforestry system",
    #         caption = "Figure 5  illustrates how total cash-flow (expressed in €) accumulates over time from an agroforestry intervention, based on a range of simulated outcomes. The shaded areas represent uncertainty (spread of possible results), and the blue line indicates the median trajectory. Cumulative returns grow steadily over time, showing the long-term profitability potential of agroforestry. Despite initial variability, the system trends positively, reinforcing the case for agroforestry as a viable financial investment over the long run."
    #       )
    #     
    # plot6 <- decisionSupport::plot_cashflow(
    #   mc_data, "Cashflow_AF1_decision",
    #   x_axis_name = "",
    #   y_axis_name = "Annual cash-flow (€)",
    #   facet_labels = "") |>
    #   add_meta(
    #     title   = "Figure 6. Incremental annual cash-flow",
    #     subtitle= "Agroforestry minus baseline farming",
    #     caption = "Figure 6 shows the difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.")
    # 
    # plot7 <- decisionSupport::plot_cashflow(
    #   mc_data, "Cum_Cashflow_AF1_decision",
    #   x_axis_name = "",
    #   y_axis_name = "Cumulative cash-flow (€)",
    #   facet_labels = "") |>
    #   add_meta(
    #     title   = "Figure 7. Incremental cumulative cash-flow",
    #     subtitle= "Agroforestry minus baseline farming",
    #     caption = 'Figure 7 shows the cumulative difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.')
    # 
    
    # Send plots to UI
    
    
    
    output$plot1_ui <- renderPlot({ plot1 })
    make_download("download_plot1", plot1, "Figure1_marketable_yield.png")
    output$plot1_dl_ui <- renderUI({
      downloadButton("download_plot1", "Abbildung herunterladen")
    })
    
    output$plot2_ui <- renderPlot({ plot2 })
    make_download("download_plot2", plot2, "Figure2_VIP_compare.png")
    output$plot2_dl_ui <- renderUI({
      downloadButton("download_plot2", "Abbildung herunterladen")
    })
    
    crop <- selected_crop()
    if (crop == "Zwiebel") {
      output$plot3_ui <- renderPlot({ plot3 })
      make_download("download_plot3", plot3, "Figure3_Yield_Reduction.png")
      output$plot3_dl_ui <- renderUI({
        downloadButton("download_plot3", "Abbildung herunterladen")
      })
    }
    # 
    # output$plot4_ui <- renderPlot({ plot4 })
    # make_download("download_plot4", plot4, "Figure4_Annual_Cashflow.png")
    # output$plot4_dl_ui <- renderUI({
    #   downloadButton("download_plot4", "Abbildung herunterladen")
    # })
    # 
    # output$plot5_ui <- renderPlot({ plot5 })
    # make_download("download_plot5", plot5, "Figure5_Cumulative_Cashflow.png")
    # output$plot5_dl_ui <- renderUI({
    #   downloadButton("download_plot5", "Download Figure 5")
    # })
    
    # output$plot6_ui <- renderPlot({ plot6 })
    # make_download("download_plot6", plot6, "Figure6_Incremental_Annual_CF.png")
    # output$plot6_dl_ui <- renderUI({
    #   downloadButton("download_plot6", "Download Figure 6")
    # })
    # 
    # output$plot7_ui <- renderPlot({ plot7 })
    # make_download("download_plot7", plot7, "Figure7_Incremental_Cumulative_CF.png")
    # output$plot7_dl_ui <- renderUI({
    #   downloadButton("download_plot7", "Download Figure 7")
    # })
    
    
    # Ask user whether to run EVPI (takes time!)
    showModal(
      modalDialog( title = "Kurze Erinnerung", "Wenn Sie mit den von Ihnen angegebenen Schätzungen zufrieden sind, speichern Sie bitte das Projekt, damit wir Ihre Eingaben überprüfen können.", 
                   footer = tagList( actionButton("open_saveload", "Okay – Akkordeon-Panel „Speichern/Laden“ öffnen."), 
                                     modalButton("Nein – ich möchte noch ein wenig mit den Eingabewerten experimentieren.") 
                   ) 
      )
    )
    
    observeEvent(input$open_saveload, {
      accordion_panel_open("collapseSidebar", values = "save_load")
      removeModal()
    })
    
    # Handle user confirmation to run EVPI
    # observeEvent(input$confirm_evpi, {
    #   
    #   removeModal()  # remove popup
    #   
    #   # Try running EVPI only if it can return meaningful values
    #   tryCatch({
    #     evpi_input <- as.data.frame(cbind(
    #       mc_data$x,
    #       #Provide correct variable
    #       NPV_decision_AF1 = mc_data$y$NPV_decis_AF_ES3
    #     ))
    #     # Provide the NPV_decision variable to calculate EVPI
    #     evpi_result <- decisionSupport::multi_EVPI(evpi_input, "NPV_decis_AF_ES3")
    #     
    #     # saveRDS(evpi_input, "evpi_input_test.rds")
    #     # evpi_input <- readRDS("evpi_input_test.rds")
    #     
    #     # saveRDS(evpi_result, "evpi_result_test.rds")
    #     # evpi_result <- readRDS("evpi_result_test.rds")
    #     
    #     var_lookup <- bind_rows(excelData()) %>%
    #       filter(!is.na(variable), !is.na(name)) %>%
    #       distinct(variable, name) %>%
    #       deframe()
    #     #Provide correct variable
    #     plot8 <- plot_evpi(evpi_result, decision_vars = "NPV_decis_AF_ES3",
    #                        new_names = "") +
    #       scale_y_discrete(labels = var_lookup)
    #     
    #     plot8 <- plot8 |>
    #       add_meta(title = "Figure 8. EVPI for Each Variable",
    #                subtitle = "Maximum amount worth paying for perfect information on each variable."
    #       )
    #     
    #     output$plot8_ui <- renderPlot({ plot8 })
    #     
    #     make_download("download_plot8", plot8, "Figure8_EVPI.png")
    #     
    #     output$plot8_dl_ui <- renderUI({
    #       downloadButton("download_plot8", "Download Figure 8")
    #     })
    #     
    #   }, error = function(e) {
    #     warning("EVPI plot skipped due to error: ", e$message)
    #     output$plot8_ui <- renderPlot({
    #       plot.new()
    #       text(0.5, 0.5, "There are no variables with a positive EVPI.\nGetting better information will \nnot reduce the level of uncertainty of the decision.", cex = 1.2)
    #     })
    #   })
    # })
    
  })
  
  # display of conceptual model image from www folder
  img_info <- eventReactive(selected_crop(), {
    crop <- selected_crop()
    validate(need(!is.null(crop), "Bitte zuerst eine Nutzpflanze auswählen."))
    
    crop_lower <- tolower(crop)
    
    path <- if (crop_lower == "spargel") {
      file.path("www", "Spargel_CM.png")
    } else if (crop_lower == "zwiebel") {
      file.path("www", "Zwiebel_CM.jpeg")
    } else {
      NULL
    }
    
    list(
      crop = crop,
      path = path
    )
  })
  
  output$myImage <- renderImage({
    info <- img_info()        
    req(info)
    
    list(
      src         = normalizePath(info$path),
      contentType = "image/png",
      width       = "auto",
      height      = 1050,
      alt         = paste("Image for", info$crop)
    )
  }, deleteFile = FALSE)
  
  output$download_ui <- renderUI({
    req(img_info())
    downloadButton("downloadImage", "Bild herunterladen")
  })
  
  
  output$downloadImage <- downloadHandler(
    filename = function() {
      info <- img_info()
      req(info)
      paste0(tolower(info$crop), ".png")
    },
    content = function(file) {
      info <- img_info()
      req(info)
      file.copy(from = info$path, to = file, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui = ui, server = server)