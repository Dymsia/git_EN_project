require(shiny);
library(shinydashboard);
library(GGally)
# comment
require(shinyjs)
require(shinyBS)
require(DT)
require(plotly)
require(shinythemes)

source("./helpers/helpers_ui.R", local = TRUE)$value

ui.files = list.files(path = "./ui", pattern = "*.R")
ui.files = paste0("ui/", ui.files)

for (i in seq_along(ui.files)) {
  source(ui.files[i], local = TRUE)
}

shinyUI(
  dashboardPage(
  dashboardHeader(
    title = "Eneriss ML demo"
                  
                  ), # konec dashboardHeader

  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard help", tabName = "dashboardhelp", icon = icon("dashboard")),
      menuItem("Data Preparation", tabName = "datapreparation", icon = icon("wrench")
               ),
      menuItem("Train ML model(s)", tabName = "analysis", icon = icon("cogs"),
               menuSubItem("Train Control",icon = icon("cog"), tabName = "trainvalidation"),
               menuSubItem("Advanced tune",icon = icon("cog"), tabName = "algorithm")
               ),
      menuItem("Results", tabName = "results", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Code", tabName = "code", icon = icon("code"))
    ) # end sidebarMenu
    
    ), # end dashboardSideBar
  
  
  dashboardBody(
    
    ######################## 1. Dla Dashboard help item:
    tabItems( # opredelaem dla kagdogo elementa  is siderBar menu BODY
      
    tabItem(tabName = "dashboardhelp",
            fluidRow(
              box(title = "How To Use", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,collapsed = TRUE, width = 8,
                  
                  
                  h4("Step 1: Data preparation"),
                  tags$ol(
                    tags$li("upload csv file"),
                    tags$li("read the file"),
                    tags$li("choose the target variable"),
                    tags$li("see some EDA")
                  ),
                  
                  
                  h4("Step 2: Train ML model "),
                  tags$ol(
                    tags$li("basic tuning"),
                    tags$li("set up cross-validation"),
                    tags$li("choose pre-processing method"),
                    tags$li("choose the model(s)"),
                    tags$li("advanced hyperparameter tuning")
                  ),
                  
                  h4("Step 3: Run Application"),
                  h4("Step 3: Resalts"),
                  tags$ol(
                    tags$li("best results for each chosen model"),
                    tags$li("train results"),
                    tags$li("Accuracy plot")
                  ),
                  imageOutput("image2", width = "100%", height = "25px")
              )
              
            ),  # konec 1-go fludRow
            
            #1_2. Libraries
            fluidRow(
              box(title = "Libraries/Dependencies",status = "info", solidHeader = TRUE,
                  collapsible = TRUE,collapsed = TRUE, width = 8,
                  h4("- The caret package "),
                  h4("- Shiny Dashboard "),
                  h4("- Google Cloud Platform(GCP)"),
                  imageOutput("image3", width = "100%", height = "25px")
              )
            ) # konec 2-go fluid row Libraries
            
    ), # konec tabItem = dashboardhelp
    
    ######################################
    #2. Data Preparation Tab Contents
    ######################################
    
    # Second tab content
    tabItem(
      tabName = "datapreparation",
            
            fluidPage(
              
              tabBox(width = 12,
                     id = "datapreptab",
                     
                      tabPanel("Import", tabpanel.import,
                               icon = icon("folder-open")),
                      # tabPanel("DataOld", icon = icon("folder-open"),
                      #          flowLayout(
                      #            fileInput('rawInputFile','Upload Data File',
                      #                      accept=c('text/csv',
                      #                               'text/comma-separated-values,text/plain',
                      #                               '.csv')
                      #            ),
                      # 
                      #            checkboxInput('headerUI','Header',TRUE),
                      #            uiOutput("labelSelectUI"),
                      # 
                      #           uiOutput("PredictorVarsUI"),
                      # 
                      #            splitLayout(
                      # 
                      # 
                      #              radioButtons('sepUI','Seperator',
                      #                           c(Comma=',',
                      #                            Semicolon=';',
                      #                             Tab='\t'),
                      #                           'Comma'),
                      # 
                      #              radioButtons('quoteUI','Quote',
                      #                           c(None='',
                      #                             'Double Quote'='"',
                      #                             'Single Quote'="'"),
                      #                           'Double Quote')
                      #            ),
                      # 
                      # 
                      # 
                      #            plotOutput("caretPlotUI", width = "450px", height = "450px")
                      #          )
                      # 
                      # ), # end tabPanel Load data

                     
                     
                     tabPanel(title = "View data", solidHeader = TRUE,
                              collapsible = TRUE,
                              DT::dataTableOutput("pre.data"),
                              uiOutput("labelSelectUI"),
                              plotOutput("caretPlotUI", width = "450px", height = "450px"),
                              uiOutput("varSelectUI"),
                              
                              plotOutput("plotHist", height = 250),
                              
                              
                              # Input: Slider for the number of bins ----
                              sliderInput(inputId = "bins",
                                          label = "Number of bins:",
                                          min = 2,
                                          max = 30,
                                          value = 10),
                              downloadButton("report", "Generate report")
                              ), # end tabPanel View data
                     
                     tabPanel(title = "Interactive plots", solidHeader = TRUE,
                              collapsible = TRUE,
                              # This will hold column dropdowns and "Add plot" button
                              uiOutput("column_ui"),
                              # This <div> will hold all of the plots we're going to
                              # dynamically add. It's going to be super fun!
                              div(id = "plot_container"),
                              # knopka remove 
                              actionButton("remove_button", "Remove"),
                              # Disable fading effect when processing
                              tags$style(".recalculating { opacity: 1; }")
                              
                     ),
                     
                     tabPanel(title = "Preprocessing", solidHeader = TRUE,
                              collapsible = TRUE,
                              
                             
                             
                             fluidRow( 
                               box(width = 3, title = "Settings",
                                   uiOutput("preproc_out"),
                                   plotlyOutput("plot.feature.selection")
                               ),
                               tabBox(width = 9,
                                      tabPanel(title = "Data",
                                               dataTableOutput("preproc_data")
                                      ),
                                      tabPanel(title = "Statistics",
                                               dataTableOutput("summary.datatable2")
                                      )
                               )
                             ),
                              selectInput("preproc_method", "",
                                          choices = list("On data" = c("Drop variable(s)",
                                                                       "Convert variable",
                                                                       "Normalize variables",
                                                                      # "Remove constant variables",
                                                                       "Recode factor levels",
                                                                       #"Cap large values",
                                                                       #"Subset",
                                                                       "Create dummy features"
                                                                       #"Impute"
                                                                       )
                                                         
                                                         ), selected = "Drop variable(s)"
                                          ),
                                br(),
                               # tags$hr(),
                               # br(),
                              uiOutput("preproc.go"),
                              br(),
                             # tags$hr(),
                              bsButton("preproc_undo", "undo", icon = icon("undo")),
                             # tags$hr(),
                              downloadButton("preproc.data.download", "save processed data")
                              
                              
                              #DT::dataTableOutput("preproc_data")
                            
                              
                              
                              
                     ),
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     tabPanel(title = "Summary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("featurematrixUI")
                              
                     )
                     
                     
                     
                     ) # end tabBox
                     ) # end fluidPage Data Preparation
            
    ) # konec  tabItem datapreparation
    
   )#konec tabItems()
  ) # end dashboardBody
  
  ) # end dashboardPage()
  ) # end ui
