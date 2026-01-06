# Changes to be made to adapt in this script have been commented with 'Provide'

# Config ------------------------------------------------------
## Install + load libraries as required  ------------------------------------------------------
source("functions/libraries.R")


## Load functions ------------------------------------------------------
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

# Feedback SQL helper
source("functions/SQL_helper.R")

## Load Inputs ------------------------------------------------------
# Provide Location of excel workbook containing the input parameters (prepared for the dynamic-helper)
file_path_vars <- "data/ASP_input_parameters_german2.xlsx"
sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)

#load additional input table, weather and scenario_ids
risk_path<-file.path("data", "risk_df.csv")
scenario_path<-file.path("data", "scenarios.csv")
risk_df<-read.csv(risk_path)
scenarios<-read.csv(scenario_path)

# onion_weather_path<-file.path("data", "weather_koeln-bonn_final_compressed.rds")
# onion_weather<-readRDS(onion_weather_path)


# UI ------------------------------------------------------
ui <- fluidPage(
  
  ## Theme ------------------------------------------------------
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  
  use_waiter(),
  
  ## Head ------------------------------------------------------
  # Set actual browser tab title and favicon
  tags$head(
    ### Tab ------------------------------------------------------
    tags$title("Agriculture Decision Support Tool"),
    tags$link(rel = "shortcut icon", href = "INRES.png"),
    
    ### CSS ------------------------------------------------------
    tags$style(HTML("
      .flex-container {
        display: flex;
        justify-content: flex-start;  /* <– anchor left edge */
        align-items: flex-start;
        overflow: auto;               /* scroll if too big */
        margin: 0 auto;
      }
      .flex-container img {
        max-width: 100%;
        max-height: 100%;
        object-fit: contain;
      }
      .flex-container .shiny-plot-output {
        min-width: 900px;  /* or whatever you designed for */
        cursor: pointer;
      }
      #cm-image { cursor: pointer; }
      
      
          /* Center the tab navigation */
    .nav-tabs {
      display: flex;
      justify-content: center;
      border-bottom: none;
      margin-bottom: 25px;
    }

    /* Base tab style */
    .nav-tabs > li > a {
      background: #f1f5f9;
      color: #4a5568;
      border: none;
      border-radius: 999px;
      padding: 10px 22px;
      margin: 0 6px;
      font-weight: 600;
      font-size: 18px;
      transition: all 0.25s ease;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
    }

    /* Hover state */
    .nav-tabs > li > a:hover {
      background: #e2e8f0;
      color: #2d3748;
    }

    /* Active tab */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      box-shadow: 0 6px 15px rgba(102, 126, 234, 0.35);
      border: none;
    }

    /* Remove default underline indicator */
    .nav-tabs > li > a:after {
      display: none;
    }
    
      "
    )),
    ### JS ------------------------------------------------------
    tags$script(HTML("
      // Click any element with class 'fullscreen-toggle' to toggle fullscreen
      document.addEventListener('click', function(e) {
        const box = e.target.closest('.fullscreen-toggle');
        if (!box) return;
        
        if (!document.fullscreenElement) {
          if (box.requestFullscreen) box.requestFullscreen();
        } else {
          if (document.exitFullscreen) document.exitFullscreen();
        }
      });
      "
    ))
  ),
  
  ### Title ------------------------------------------------------
  tags$div(
    style = "display:flex; align-items:center;justify-content:space-between;
      width: 100% !important; margin: 20px; padding: 0 15px;
      box-sizing: border-box; background-color: #f2f2f2;",
    
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    tags$img(src = "logo-mix-nobg.png", height = "100px",
             style = "margin-left: auto; max-width: 20%; height: auto; cursor: pointer;"),
    # ),
    # Provide Title of the DA model
    tags$h2(tags$div("Auswirkungen des Klimawandels auf künftige Ernteerträge in 2075"),
            #tags$div("conventional asparagus growing in 2075"),
            style = "text-align: center; flex-grow: 1;")
    # Provide Project Logo
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    # tags$img(src = "mlv-logo.png", height = "100px",
    #          style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;")
    # # ),
  ),
  
  
  ## Sidebar ------------------------------------------------------
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
                   ### Accordions ------------------------------------------------------
                   #### Save/Load functionality ------------------------------------------------------
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
                         #column(6, actionButton("delete_btn", tagList(icon("trash"),   "Löschen"), class = "btn btn-secondary"))
                       ),
                       hr(),
                       downloadButton("download_csv", label = tagList(icon("download"), "Aktuelle Eingaben herunterladen (.csv)"))
                     )
                   ),
                   #### Crop filter ------------------------------------------------------
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
                   
                   #### Expertise filter ------------------------------------------------------
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
                   
                   #### Dynamic elements ------------------------------------------------------
                   uiOutput("dynamic_element_ui")
                 )
    ),
    
    ## Main Panel ------------------------------------------------------
    mainPanel(width = 8,
              
              tabsetPanel(
                id = "main_tabs",
                selected = "Intro",
                
                tabPanel(
                  title = "📋 Beschreibung",
                  value = "Intro",
                  
                  ### App description text ------------------------------------------------------
                  # Provide brief explanation of the DA model
                  tags$div(
                    style = "max-width: 100%; margin: 0 auto; padding: 30px; background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.1);",
                    
                    # Header Section with Icon
                    tags$div(
                      style = "text-align: center; margin-bottom: 40px;",
                      tags$div(
                        style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; border-radius: 50%; margin-bottom: 20px; box-shadow: 0 8px 20px rgba(102, 126, 234, 0.3);",
                        tags$span(style = "font-size: 48px;", "🌱")
                      ),
                      tags$h2(
                        style = "color: #2d3748; font-weight: 700; font-size: 32px; margin: 15px 0 10px 0; letter-spacing: -0.5px;",
                        "Wir brauchen Ihre Hilfe!"
                      ),
                    ),
                    
                    # Intro Box
                    tags$div(
                      style = "background: white; padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 15px rgba(0,0,0,0.08); border-left: 5px solid #667eea;",
                      tags$p(
                        style = "font-size: 16px; line-height: 1.7; color: #4a5568; margin: 0;",
                        "Diese App ermöglicht Ihnen, die Zukunft des Anbaus von Spargel, Zwiebeln und bald Erdbeeren unter verschiedenen Klimaszenarien zu erkunden. Doch wahre Präzision entsteht erst durch Ihr Know-how: Ihre Beobachtungen und Erfahrungswerte aus der Praxis machen unsere Simulationen realistischer."
                      )
                    ),
                    
                    # Steps Section
                    tags$div(
                      style = "margin-bottom: 30px;",
                      tags$h3(
                        style = "color: #2d3748; font-weight: 600; font-size: 24px; margin-bottom: 25px; text-align: center;",
                        "So einfach geht's"
                      ),
                      
                      # Step 1
                      tags$div(
                        style = "display: flex; align-items: start; background: white; padding: 20px; border-radius: 12px; margin-bottom: 15px; box-shadow: 0 3px 10px rgba(0,0,0,0.06); transition: transform 0.2s;",
                        tags$div(
                          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; flex-shrink: 0; margin-right: 20px;",
                          "1"
                        ),
                        tags$div(
                          style = "flex: 1;",
                          tags$h4(style = "margin: 0 0 8px 0; color: #2d3748; font-size: 18px; font-weight: 600;", "Wählen Sie Ihre Kulturpflanze"),
                          tags$p(style = "margin: 0; color: #718096; font-size: 15px; line-height: 1.6;", "und definieren Sie Ihr Fachgebiet")
                        )
                      ),
                      
                      # Step 2
                      tags$div(
                        style = "display: flex; align-items: start; background: white; padding: 20px; border-radius: 12px; margin-bottom: 15px; box-shadow: 0 3px 10px rgba(0,0,0,0.06);",
                        tags$div(
                          style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; flex-shrink: 0; margin-right: 20px;",
                          "2"
                        ),
                        tags$div(
                          style = "flex: 1;",
                          tags$h4(style = "margin: 0 0 8px 0; color: #2d3748; font-size: 18px; font-weight: 600;", "Kalibrieren Sie das Modell"),
                          tags$p(style = "margin: 0 0 8px 0; color: #718096; font-size: 15px; line-height: 1.6;", "Passen Sie in den Registern links die Wertebereiche nach bestem Wissen an. Exakte Zahlen sind nicht nötig – realistische Spannen (z.B. Minimum bis Maximum) reichen völlig aus. Gerade diese Unsicherheit macht unsere Analyse robust und aussagekräftig."),
                          tags$div(
                            style = "background: #edf2f7; padding: 10px 15px; border-radius: 8px; margin-top: 10px;",
                            tags$span(style = "color: #4299e1; font-size: 14px;", "💡 "),
                            tags$em(style = "color: #4a5568; font-size: 14px;", "Tipp: Fahren Sie mit der Maus über Parameter für detaillierte Erklärungen")
                          )
                        )
                      ),
                      
                      # Step 3
                      tags$div(
                        style = "display: flex; align-items: start; background: white; padding: 20px; border-radius: 12px; margin-bottom: 15px; box-shadow: 0 3px 10px rgba(0,0,0,0.06);",
                        tags$div(
                          style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; flex-shrink: 0; margin-right: 20px;",
                          "3"
                        ),
                        tags$div(
                          style = "flex: 1;",
                          tags$h4(style = "margin: 0 0 8px 0; color: #2d3748; font-size: 18px; font-weight: 600;", "Starten Sie die Simulation"),
                          tags$p(style = "margin: 0; color: #718096; font-size: 15px; line-height: 1.6;", "Klicken Sie auf 'Modell ausführen', um eine Monte-Carlo-Simulation mit zufälligen Kombinationen aus den von Ihnen definierten Bereichen durchzuführen.")
                        )
                      ),
                      
                      # Step 4
                      tags$div(
                        style = "display: flex; align-items: start; background: white; padding: 20px; border-radius: 12px; box-shadow: 0 3px 10px rgba(0,0,0,0.06);",
                        tags$div(
                          style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; flex-shrink: 0; margin-right: 20px;",
                          "4"
                        ),
                        tags$div(
                          style = "flex: 1;",
                          tags$h4(style = "margin: 0 0 8px 0; color: #2d3748; font-size: 18px; font-weight: 600;", "Speichern & Analysieren"),
                          tags$p(style = "margin: 0; color: #718096; font-size: 15px; line-height: 1.6;", "Sie können Eingaben speichern/laden. Sobald das Modell ausgeführt wird, werden die Ergebnisse unten angezeigt und Sie können diese Zahlen speichern.")
                        )
                      )
                    ),
                    
                    # Why You Matter Section
                    tags$div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 25px rgba(102, 126, 234, 0.3);",
                      tags$h3(
                        style = "color: white; font-weight: 600; font-size: 22px; margin: 0 0 15px 0; text-align: center;",
                        HTML("&#10024; Warum Sie wichtig sind")
                      ),
                      tags$p(
                        style = "font-size: 16px; line-height: 1.7; margin: 0 0 15px 0; text-align: center;",
                        "Mit jeder Ihrer Eingaben wird unser Modell präziser. Unser Ziel ist es, ein Werkzeug zu entwickeln, das zukünftige Probleme im Anbau rechtzeitig identifiziert – Informationen, die dann direkt an politische Entscheidungsträger fließen. Sollten Sie sich fragen: 'Was machen die schon wieder für einen Quatsch an der Uni, haben die nicht wieder den wichtigen Punkt XY übersehen?' – genau dann brauchen wir Ihr Feedback! Sagen Sie uns, welche Aspekte aus Ihrer täglichen Praxis unbedingt berücksichtigt werden müssen."
                      ),
                    ),
                    
                    # Contact Section
                    tags$div(
                      style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
                      tags$h4(
                        style = "color: #2d3748; font-weight: 600; font-size: 18px; margin: 0 0 20px 0;",
                        HTML("&#128236; Geben Sie uns Feedback")
                      ),
                      tags$p(
                        style = "color: #4a5568; font-size: 15px; margin: 0 0 15px 0;",
                        "Über Rückmeldungen würden wir uns sehr freuen. Bitte kontaktieren Sie:"
                      ),
                      tags$div(
                        style = "display: grid; gap: 12px;",
                        tags$div(
                          style = "background: #f7fafc; padding: 15px; border-radius: 8px; border-left: 3px solid #667eea;",
                          tags$a(
                            href = "mailto:jbsl@uni-bonn.de",
                            style = "color: #667eea; font-weight: 600; text-decoration: none; font-size: 15px;",
                            "Jan-Bernd Schulze Lutum"
                          ),
                          tags$span(style = "color: #718096; font-size: 14px;", " - für die Spargel- und Erdbeermodelle")
                        ),
                        tags$div(
                          style = "background: #f7fafc; padding: 15px; border-radius: 8px; border-left: 3px solid #f093fb;",
                          tags$a(
                            href = "mailto:s7padaib@uni-bonn.de",
                            style = "color: #f093fb; font-weight: 600; text-decoration: none; font-size: 15px;",
                            "Paul Daiber"
                          ),
                          tags$span(style = "color: #718096; font-size: 14px;", " - für das Zwiebelmodell")
                        ),
                        tags$div(
                          style = "background: #f7fafc; padding: 15px; border-radius: 8px; border-left: 3px solid #4facfe;",
                          tags$a(
                            href = "mailto:pkasargo@uni-bonn.de",
                            style = "color: #4facfe; font-weight: 600; text-decoration: none; font-size: 15px;",
                            "Prajna Kasargodu Anebagilu"
                          ),
                          tags$span(style = "color: #718096; font-size: 14px;", " oder "),
                          tags$a(
                            href = "mailto:afuelle1@uni-bonn.de",
                            style = "color: #4facfe; font-weight: 600; text-decoration: none; font-size: 15px;",
                            "Adrian Fülle"
                          ),
                          tags$span(style = "color: #718096; font-size: 14px;", " - für Fragen die dieses Tool betreffen")
                        )
                      )
                    )
                  ),  
                  br(),
                  
                ),
                
                tabPanel(
                  title = "🧩 Konzeptmodell",
                  value = "ConcModel",
                  
                  ### Conceptual Model ------------------------------------------------------
                  conditionalPanel(
                    condition = "output.show_image",
                    uiOutput("concepImg_container"),
                    br(), br(),
                    uiOutput("download_ui"),
                    br(), br(),
                    uiOutput("model_description_ui")
                  ),
                ),
                
                
                tabPanel(
                  title = "📊 Simulations Ergebnisse",
                  value = "SimRes",
                  
                  ### Plots ------------------------------------------------------
                  conditionalPanel(
                    condition = "output.show_results",
                    br(),
                    
                    uiOutput("plot1_container"),
                    br(),
                    uiOutput("plot1_dl_ui"),
                    br(),
                    
                    uiOutput("plot2_container"),
                    br(),
                    uiOutput("plot2_dl_ui"),
                    br(),
                    
                    uiOutput("plot3_container"),
                    br(),
                    uiOutput("plot3_dl_ui"),
                    
                    uiOutput("plot4_container"),
                    br(),
                    uiOutput("plot4_dl_ui"),
                  ),
                  
                  ### Feedback ------------------------------------------------------
                  if (NOTES_ENABLED) {
                    tagList(
                      h4("Geben Sie uns Feedback"),
                      textInput("email", label = NULL, placeholder = "Ihre@E-Mail.Adresse (optional)*", width = "100%"),
                      textAreaInput("note", label = NULL, placeholder = "Ihr Feedback…", width = "100%", height = "200px"),
                      actionButton("submit", "Feedback absenden", class = "btn btn-primary"),
                      br(),br(),
                      p(style = "color: grey; font-style: italic; font-size: 0.9em;",
                        "* Die Angabe Ihrer E-Mail-Adresse ist optional und wird ausschließlich verwendet, um Sie bezüglich Ihres Feedbacks zu kontaktieren. Gespeicherte personenbezogene Daten werden mit Ende des Projekts oder nach einem Jahr gelöscht."),
                      br(),
                      uiOutput("viewer_panel"),
                    )
                  } else {
                    NULL
                  },
                  
                )),
              
              ### Requisites ------------------------------------------------------
              # Provide Funding declaration
              br(),
              tags$h6(
                "Projekt finanziert durch",
                tags$br(),
              ),
              tags$img(src = "mlv-logo.png",
                       style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;"),
              # #Provide tool usage disclaimer
              tags$p(
                tags$a("Disclaimer", href = "https://www.gartenbauwissenschaften.uni-bonn.de/imprint",
                       target = "_blank"),
                " | ",  
                #Provide the correct link once the app and codes are hosted in HortiBonn repo
                tags$a("View Source Code", href = "https://github.com/hortibonn/Climate_Impact_on_Crop_Yield", # temp link
                       target = "_blank")
              ),
              br(), br(),br(), br(),
              
              
    )
  )
  
)


# Server ------------------------------------------------------
server <- function(input, output, session) {
  
  ## Dynamic UI START ------------------------------------------------------
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
  
  ## Dynamic UI inputs END
  
  
  
  ## Dynamic expertise-filter module START ------------------------------------------------------
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
                       "
      ))
    )
  })
  
  ## Dynamic expertise-filter module END
  
  
  
  ## Save/Load functionality START ------------------------------------------------------
  # Save, Load and Delete module
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
  
  # saveLoadServer("savemod", current_input_table)
  # Provide Folder name instead of the current 'Germany' to store user saves
  get_base_dir <- function() {
    if (Sys.info()[["sysname"]] == "Windows")
      "user-states/German_crops"
    else
      "/srv/shiny-app-data/user-states/German_crops"
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
      showModal(modalDialog("Es gibt zu viele Speicherstände auf dem Server. Bitte wenden Sie sich an den Administrator, wenn Sie weitere Speicherstände benötigen.",
                            #showModal(modalDialog("You already have several versions. Delete one first.",
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
  ## Save/Load functionality END
  
  # Switch to Simulation tab as soon as a crop is selected
  last_crop <- reactiveVal(NULL)
  
  observeEvent(selected_crop(), {
    req(selected_crop())
    new <- selected_crop()
    old <- last_crop()
    last_crop(new)
    
    if (!is.null(old) && new != old) {
      nav_locked(FALSE)  # allow auto-jump again for a new crop
    }
    
    if (isTRUE(nav_locked())) return()
    updateTabsetPanel(session, "main_tabs", selected = "ConcModel")
  }, ignoreInit = TRUE)
  
  # Conceptual Model image when Crop selected START ------------------------------------------------------
  # image flag
  output$show_image <- reactive({
    !is.null(selected_crop())
  })
  outputOptions(output, "show_image", suspendWhenHidden = FALSE)
  
  # image info
  img_info <- eventReactive(selected_crop(), {
    crop <- selected_crop()
    validate(need(!is.null(crop), "Bitte zuerst eine Nutzpflanze auswählen."))
    
    crop_lower <- tolower(crop)
    
    file <- if (crop_lower == "spargel") {
      "Spargel_CM.png"
    } else if (crop_lower == "zwiebel") {
      "Zwiebel_CM.jpeg"
    } else {
      NULL
    }
    
    list(
      crop = crop,
      file = file
    )
  })
  
  output$concepImg_container <- renderUI({
    info <- img_info()
    req(info$file)
    
    # Setze Breite basierend auf der Pflanze
    container_width <- if (tolower(info$crop) == "zwiebel") {
      "100%"
    } else {
      "65%"
    }
    
    div(
      class = "flex-container fullscreen-toggle",
      style = paste0("max-width: ", container_width, ";"),
      uiOutput("concepImg")
    )
  })
  
  output$concepImg <- renderUI({
    info <- img_info()
    req(info$file)
    
    tags$img(
      src = info$file,
      alt = paste("Image for", info$crop),
      id  = "cm-image",
      style = "width: 100%; height: auto; max-height: none;"
    )
  })
  
  output$download_ui <- renderUI({
    req(img_info())
    div(
      style = "text-align: center;",
      downloadButton("downloadImage", "Bild herunterladen")
    )
  })
  
  output$model_description_ui <- renderUI({
    info <- img_info()
    req(info$crop)
    
    if (tolower(info$crop) == "zwiebel") {
      tags$div(
        style = "max-width: 900px; margin: 20px auto; padding: 25px; background: #f8f9fa; border-radius: 12px; border-left: 5px solid #667eea;",
        tags$h4(
          style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;",
          "Wie das Klima Ihre Zwiebelernte beeinflusst"
        ),
        tags$p(
          style = "font-size: 15px; line-height: 1.7; color: #4a5568; text-align: justify;",
          "Dieses Modell simuliert den Weg von konventionell angebauten gesähten Sommerzwiebeln vom Feldaufgang bis zur Ernte und zeigt, wie Wettermuster den Ertrag in jeder Phase beeinflussen. Die Biomasseakkumulation wird durch Sonneneinstrahlung (PAR), Temperatur, Niederschlag und Bewässerung beeinflusst und durchläuft vier kritische Wachstumsphasen: Feldaufgang, vegetatives Wachstum, Bulbenbildung und Reifephase."
        ),
        tags$p(
          style = "font-size: 15px; line-height: 1.7; color: #4a5568; text-align: justify;",
          "Doch die Natur stellt Herausforderungen: Abiotische Stressfaktoren wie Starkregen, Hagel und Hitzewellen können die Pflanzen beeinträchtigen. Gleichzeitig entstehen biotische Bedrohungen durch Schädlinge und Krankheiten wie Fusarium, Falschen Mehltau,Zwiebelhals-Fäule und Thripse, deren Auftreten von Witterungsbedingungen abhängt."
        ),
        tags$p(
          style = "font-size: 15px; line-height: 1.7; color: #4a5568; text-align: justify;",
          "Jeder Stressfaktor kann das Ertragspotenzial beeinflussen. Das Modell führt 1.000 Monte-Carlo-Simulationen mit historischen Wetterdaten und zukünftigen Klimaszenarien (SSP1-2.6 bis SSP5-8.5) durch. Ihr Expertenwissen macht das Modell präziser: Kalibrieren Sie Schwellenwerte, Empfindlichkeiten und Ertragsverluste basierend auf Ihrer Praxiserfahrung. Ihre Eingaben helfen uns, das Modell kontinuierlich zu verbessern und an reale Anbaubedingungen anzupassen."
        )
      )
    } else {
      NULL
    }
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
      from <- file.path("www", info$file)
      file.copy(from = from, to = file, overwrite = TRUE)
    }
  )
  
  # Conceptual Model image when Crop selected END 
  
  
  
  ## Monte Carlo Simulation AND Plotting START ------------------------------------------------------
  
  ### Monte Carlo Simulation START ------------------------------------------------------
  nav_locked <- reactiveVal(FALSE)  
  
  mcSimulation_results <- reactiveVal(NULL)
  
  observeEvent(input$run_simulation, {
    input_file <- current_input_table()
    crop <- selected_crop()
    
    if (is.null(crop) || !nzchar(crop)) {
      showModal(modalDialog(
        title = "Keine Kulturpflanze ausgewählt",
        "Bitte wählen Sie eine Kulturpflanze aus, bevor Sie die Simulation starten.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    input_file <- isolate(current_input_table())      # important
    n_runs     <- isolate(input$num_simulations_c)    # important
    
    # If your model depends on other input values indirectly, capture them too
    # (example placeholders — include whatever your app uses)
    # expected_yield_p <- isolate(input$expected_yield_p)
    # some_slider      <- isolate(input$some_slider)
    
    # 1) Switch tab immediately
    nav_locked(TRUE) # so it does not switch back to conceptual model after flushing
    updateTabsetPanel(session, "main_tabs", selected = "SimRes")
    
    # 2) Let the UI flush (so the tab change actually renders),
    #    then run the expensive simulation
    session$onFlushed(function() {
      
      waiter_show(
        html = tagList(spin_fading_circles(), "Modell ausführen und Abbildungen erstellen ..."),
        color = "rgba(0, 0, 0, 0.8)"
      )
      
      on.exit(waiter_hide(), add = TRUE)
      
      res <- if (crop == "Spargel") {
        environment(asparagus_sim_scen)$scenarios <- scenarios
        environment(asparagus_sim_scen)$risk_df <- risk_df
        
        decisionSupport::mcSimulation(
          estimate          = decisionSupport::as.estimate(input_file),
          model_function    = asparagus_sim_scen,
          numberOfModelRuns = n_runs,
          functionSyntax    = "plainNames"
        )
        
      } else if (crop == "Zwiebel") {
        
        decisionSupport::mcSimulation(
          estimate          = decisionSupport::as.estimate(input_file),
          model_function    = onion_climate_impact,
          numberOfModelRuns = n_runs,
          functionSyntax    = "plainNames"
        )
        
      } else {
        showNotification("Unbekannte Nutzpflanze ausgewählt", type = "error")
        NULL
      }
      
      mcSimulation_results(res)
      
    }, once = TRUE)
    
  }, ignoreInit = TRUE)
  
  
  
  ### Monte Carlo Simulation END
  
  
  
  ### Generating plots START ------------------------------------------------------
  # helper to add title subtile caption etc
  add_meta <- function(p, title, subtitle = NULL, caption = NULL, legend = "bottom") {
    
    # p +
    #   labs(title = title, subtitle = subtitle, caption = caption) +
    #   theme(
    
    p +
      # global title / subtitle / caption for the whole patchwork
      plot_annotation(
        title    = title,
        subtitle = subtitle,
        caption  = caption
      ) &
      # this theme is applied to ALL subplots *and* the annotation
      theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_textbox_simple(
          size   = 24,
          face   = "bold",
          width  = unit(1, "npc"),  # full plot width
          halign = 0.5,              # centered
          margin = margin(t = 6,b = 20)
        ),
        plot.subtitle = element_textbox_simple(
          size   = 14,
          width  = unit(1, "npc"),
          halign = 0.5,
          margin = margin(t = 6,b = 20)
        ),
        plot.caption  = element_textbox_simple(
          size   = 16,
          width  = unit(1, "npc"),
          halign = 0,              # left-aligned
          margin = margin(t = 6,b = 20),
          hjust = 0,
          vjust = 1
        ),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        #legend.text     = element_text(size = 16, hjust = 0.5),
        #legend.position = legend,
        plot.margin = margin(t = 40, r = 10, b = 40, l = 10, unit = "pt")
        
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
  
  # Boolean flag: do we have results yet?
  output$show_results <- reactive({
    !is.null(mcSimulation_results())
  })
  # needed so conditionalPanel can see it
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
  observeEvent(mcSimulation_results(), {
    mc_data <- mcSimulation_results()
    # Provide correct variables for plots
    crop <- selected_crop()
    # bind into the function's environment
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
          title    = "2020 vs. 2075: Wie Klimaszenarien die Zukunft der Ernte bestimmen",
          #subtitle = "",
          caption  = "Diese Abbildung zeigt den Unterschied zwischen dem simulierten Ertrag unter den Wetterbedingungen des Jahr 2020 und dem simulierten Ertrag für Wettervorhersagen des Jahr 2075.
          Die Boxplots stellen für jedes Klimaszenario die Ergebnissverteilungen der Monte-Carlo-Simulationen dar. 
        Die Prozentwerte darüber zeigen, wie stark der durchschnittliche Ertrag reduziert wurde. Die gestrichelte Linie markiert den 
        Spargel-Ertrag im Regierungsbezirk Köln im Jahr 2020 und dient als Orientierung. Die SSP-Szenarien stehen für mögliche zukünftige Klimaentwicklungen – von geringerer 
        Erwärmung (SSP1–2.6) bis hin zu hohen und sehr hohen Emissionen (SSP3–7.0 und SSP5–8.5). So lässt sich einschätzen, wie Klimawandel und Stressereignisse       
        zukünftige Spargelerträge beeinflussen könnten und welche Ertragsspannen realistisch sind.")
      #, legend = "none")
      
      source("functions/VIP_plot.R") 
      plot2<-VIP_plot(mc_data_order)|>
        add_meta(
          title    = "Wichtige Einflussfaktoren auf den Ertrag in 2020 und 2075 unter verschiedenen Klimaszenarien",
          #subtitle = ''
          caption  = "Das VIP-Schaubild ((Variable Importance in Projection – Variablenwichtigkeit in der Projektion)  zeigt, welche Stressfaktoren den Spargelertrag
        am stärksten beeinflussen. Jeder Punkt steht für einen Faktor. Je größer der Punkt, desto wichtiger ist dieser Faktor dafür, warum sich die Erträge unterscheiden.
        Das Schaubild zeigt nicht, wie groß der Ertragsverlust durch einen einzelnen Stressor ist. Es zeigt, welche Faktoren insgesamt besonders wichtig für den Ertrag sind.
        Darauf können Entscheidungen zu Risikomanagement, Sortenwahl oder Schutzmaßnahmen aufbauen. Die SSP-Szenarien stehen für mögliche Klimaentwicklungen: 
        von wenig Erwärmung (SSP1–2.6) bis zu sehr hohen Emissionen und starker Erwärmung (SSP5–8.5).")
      #, legend = "none")
    }
    
    else if (crop=="Zwiebel"){
      
      source("functions/yield_boxplot_onion.R")
      plot1<-plot_yield_onion(mc_data) |>
        add_meta(
          title    = "2020 vs. 2075: Wie Klimaszenarien die Zukunft der Ernte bestimmen",
          #subtitle = ''
          caption  = "Diese Abbildung zeigt den Unterschied zwischen dem potenziellen Ertrag (mit Bewässerung) ohne Stressereignisse und dem tatsächlichen Ertrag (mit Bewässerung), 
        bei dem alle simulierten Stressfaktoren berücksichtigt wurden. Die Boxplots stellen für jedes Klimaszenario die Ergebnisse der Monte-Carlo-Simulationen dar. 
        Die Prozentwerte darüber zeigen, wie stark der durchschnittliche Ertrag durch Stress reduziert wurde. Die gestrichelte Linie markiert einen typischen 
        Zwiebel-Ertrag im Regierungsbezirk Köln und dient als Orientierung. Die SSP-Szenarien stehen für mögliche zukünftige Klimaentwicklungen – von geringerer 
        Erwärmung (SSP1–2.6) bis hin zu hohen und sehr hohen Emissionen (SSP3–7.0 und SSP5–8.5). So lässt sich einschätzen, wie Klimawandel und Stressereignisse       
        zukünftige Zwiebelerträge beeinflussen könnten und welche Ertragsspannen realistisch sind.")
      #, legend = "none")
      
      source("functions/yield_boxplot_irrig_rainfed.R")
      plot2<-plot_onion_irrigated_vs_rainfed(mc_data) |>
        add_meta(
          title    = "Bewässert vs. Unbewässert: Ertragsvergleich unter verschiedenen Klimaszenarien",
          #subtitle = '',
          caption  = "Diese Abbildung vergleicht den tatsächlichen Ertrag unter bewässerten und unbewässerten Bedingungen für verschiedene Klimaszenarien. 
        Die Boxplots stellen für jedes Szenario die Ergebnisse der Monte-Carlo-Simulationen dar. Die gestrichelte Linie markiert einen typischen 
        Zwiebel-Ertrag im Regierungsbezirk Köln und dient als Orientierung. Die SSP-Szenarien stehen für mögliche zukünftige Klimaentwicklungen – von geringerer 
        Erwärmung (SSP1–2.6) bis hin zu hohen und sehr hohen Emissionen (SSP3–7.0 und SSP5–8.5). So lässt sich einschätzen, wie wichtig Bewässerung 
        für zukünftige Zwiebelerträge unter Klimawandel sein wird.")
      #, legend = "none")
      
      source("functions/VIP_Plot_onion.R")
      plot3<-make_onion_vip_combined_plot(mc_data) |>
        add_meta(
          title    = "Wichtige Einflussfaktoren auf den Ertrag in 2020 und 2075 unter verschiedenen Klimaszenarien",
          #subtitle = '',
          caption  = "Das VIP-Schaubild ((Variable Importance in Projection – Variablenwichtigkeit in der Projektion)  zeigt, welche Stressfaktoren den Zwiebelertrag
        am stärksten beeinflussen. Jeder Punkt steht für einen Faktor. Je größer der Punkt, desto wichtiger ist dieser Faktor dafür, warum sich die Erträge unterscheiden.
        Das Schaubild zeigt nicht, wie groß der Ertragsverlust durch einen einzelnen Stressor ist. Es zeigt, welche Faktoren insgesamt besonders wichtig für den Ertrag sind.
        Darauf können Entscheidungen zu Risikomanagement, Sortenwahl oder Schutzmaßnahmen aufbauen. Die SSP-Szenarien stehen für mögliche Klimaentwicklungen: 
        von wenig Erwärmung (SSP1–2.6) bis zu sehr hohen Emissionen und starker Erwärmung (SSP5–8.5)."
          , legend = "bottom")
      
      source("functions/yield_reduction_plot_onion.R")
      plot4<-make_yield_reduction_heatmap(mc_data) |>
        add_meta(
          title    = "Stressfaktoren im Vergleich: Wie Klimaeinflüsse den Biomassezuwachs prägen",
          #subtitle = '',
          caption  = "Diese Heatmap zeigt, wie stark verschiedene Stressfaktoren den Biomassezuwachs der Zwiebel in den wichtigsten Wachstumsphasen beeinflussen.   
        Die Farbintensität gibt die durchschnittliche prozentuale Minderung an: Je dunkler das Rot, desto stärker wird das Pflanzenwachstum eingeschränkt.
        Dargestellt sind sowohl die Ergebnisse für das Basisjahr 2020 als auch für das Jahr 2075 unter vier möglichen Klimaszenarien (SSP) – von starkem 
        Klimaschutz (SSP1–2.6) bis hin zu sehr hohen Emissionen (SSP5–8.5).")
    }
    
    
    
    # Send plots to UI
    output$plot1_ui <- renderPlot({ plot1 })
    output$plot2_ui <- renderPlot({ plot2 })
    if (crop == "Zwiebel") {
      output$plot3_ui <- renderPlot({ plot3 })
      output$plot4_ui <- renderPlot({ plot4 })
    }
    
    output$plot1_container <- renderUI({
      div(
        class = "flex-container fullscreen-toggle",
        style = "max-width: 100%;",
        plotOutput("plot1_ui", height = "700px")
      )
    })
    make_download("download_plot1", plot1, "Figure1_marketable_yield.png")
    output$plot1_dl_ui <- renderUI({
      div(
        style = "text-align: center;",
        downloadButton("download_plot1", "Abbildung herunterladen")
      )
    })
    
    output$plot2_container <- renderUI({
      div(
        class = "flex-container fullscreen-toggle",
        style = "max-width: 100%;",
        plotOutput("plot2_ui", height = "900px")
      )
    })
    make_download("download_plot2", plot2, "Figure2_VIP_compare.png")
    output$plot2_dl_ui <- renderUI({
      div(
        style = "text-align: center;",
        downloadButton("download_plot2", "Abbildung herunterladen")
      )
    })
    
    if (crop == "Zwiebel") {
      output$plot3_container <- renderUI({
        div(
          class = "flex-container fullscreen-toggle",
          style = "max-width: 100%;",
          plotOutput("plot3_ui", height = "900px")
        )
      })
      make_download("download_plot3", plot3, "Figure3_Yield_Reduction.png")
      output$plot3_dl_ui <- renderUI({
        div(
          style = "text-align: center;",
          downloadButton("download_plot3", "Abbildung herunterladen")
        )
      })
      
      output$plot4_container <- renderUI({
        div(
          class = "flex-container fullscreen-toggle",
          style = "max-width: 100%;",
          plotOutput("plot4_ui", height = "900px")
        )
      })
      make_download("download_plot4", plot4, "Figure4_irrigation-vs-rainfall.png")
      output$plot4_dl_ui <- renderUI({
        div(
          style = "text-align: center;",
          downloadButton("download_plot4", "Abbildung herunterladen")
        )
      })
    } else {
      # no plot 3 → no container → no whitespace
      output$plot3_container <- renderUI(NULL)
      output$plot3_dl_ui     <- renderUI(NULL)
      output$plot4_container <- renderUI(NULL)
      output$plot4_dl_ui     <- renderUI(NULL)
    }
    ### Generating plots END
    
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
    
  })
  
  ## Monte Carlo Simulation END
  
  
  
  
  ## Feedback logic START ------------------------------------------------------
  # Auth: current user + can_view flag
  user <- reactiveVal(NA_character_)
  observe({ user(current_user(session)) })
  can_view_db <- reactive({
    u <- user()
    !is.na(u) && u %in% ALLOWED_VIEWERS
  })
  
  # # Simple email checker: optional field, but if present must look like an email
  # is_valid_email <- function(x) {
  #   if (!nzchar(x)) return(TRUE)  # optional
  #   # simple pattern: something@something.suffix (no spaces)
  #   grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x)
  # }
  
  last_submit <- reactiveVal(as.POSIXct(0, origin = "1970-01-01", tz = "UTC"))
  
  observeEvent(input$submit, {
    if (!NOTES_ENABLED) return(NULL)
    now <- Sys.time()
    if (difftime(now, last_submit(), units = "secs") < 10) {  # 10s cooldown
      showModal(modalDialog(
        title = "Zu viele Anfragen!",
        "Bitte versuchen Sie es nach einigen Sekunden noch einmal.",
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return(invisible())
    }
    last_submit(now)
    msg   <- trimws(input$note %||% "")
    email <- tolower(trimws(input$email %||% ""))
    
    # Inline validations -> modals for feedback
    if (!nzchar(msg)) {
      showModal(modalDialog(title = "Fehlendes Feedback",
                            "Kein Text im Feedback!",
                            easyClose = TRUE, footer = modalButton("Close")))
      return(invisible())
    }
    if (nchar(msg) > 5000) {
      showModal(modalDialog(title = "Feedback zu lang",
                            "Maximal 5000 Zeichen zulässig.",
                            easyClose = TRUE, footer = modalButton("Close")))
      return(invisible())
    }
    # if (!is_valid_email(email)) {
    #   showModal(modalDialog(title = "Invalid email",
    #                         "Please enter a valid email address or leave it blank.",
    #                         easyClose = TRUE, footer = modalButton("Close")))
    #   return(invisible())
    # }
    if (!NOTES_ENABLED) {
      showModal(modalDialog(title = "Feedback disabled",
                            "The feedback database is disabled in this environment.",
                            easyClose = TRUE, footer = modalButton("Close")))
      return(invisible())
    }
    
    db_insert_note(submitted_at = now_iso(), user_label = user(), email = email, note = msg)
    updateTextAreaInput(session, "note", value = "")
    updateTextInput(session, "email", value = "")
    
    showModal(modalDialog(
      title = "Feedback gespeichert",
      "Danke! Ihr Feedback wurde aufgenommen.",
      easyClose = TRUE, footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)
  
  # CSV export now includes email
  output$dl_csv <- downloadHandler(
    filename = function() sprintf("Feedback-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      req(NOTES_ENABLED); req(can_view_db())
      dat <- db_get_all_notes()
      write.csv(dat, file, row.names = FALSE)
    }
  ) 
  
  # Admin panel only when enabled + authorized
  output$viewer_panel <- renderUI({
    if (isTRUE(NOTES_ENABLED) && isTRUE(can_view_db())) {
      tagList(
        h4("Admin only"),
        textOutput("note_count"),
        br(),
        p("Download Feedback"),
        downloadButton("dl_csv", "Export CSV")
      )
    } else NULL
  })
  
  # Optional: small note in dev
  output$dev_notice <- renderUI({
    if (!NOTES_ENABLED) {
      tags$p(class = "text-muted", style = "margin-top:10px;",
             "Feedback are disabled in this environment.")
    } else NULL
  })
  
  # Count of rows in DB table
  output$note_count <- renderText({
    req(NOTES_ENABLED); req(can_view_db())
    n <- db_count_notes()
    sprintf("Total number of Feedbacks in database: %d", n)
  })
  
  # Feedback logic END
}


shinyApp(ui = ui, server = server)
