require(shiny);
library(shinydashboard);


shinyUI(
  dashboardPage(
  dashboardHeader(
    title = "Eneriss ML demo"
                  
                  ), # konec dashboardHeader

  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard help", tabName = "dashboardhelp", icon = icon("dashboard")),
      menuItem("Data Preparation", tabName = "datapreparation", icon = icon("wrench")),
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
                     tabPanel(title = "Data", solidHeader = TRUE,
                              collapsible = TRUE,
                              flowLayout(
                                fileInput('rawInputFile','Upload Data File',
                                          accept=c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv')
                                ),
                                
                                checkboxInput('headerUI','Header',TRUE),
                                uiOutput("labelSelectUI"),
                                
                                uiOutput("PredictorVarsUI"),
                                
                                splitLayout(
                                  
                                  
                                  radioButtons('sepUI','Seperator',
                                               c(Comma=',',
                                                 Semicolon=';',
                                                 Tab='\t'),
                                               'Comma'),
                                  
                                  radioButtons('quoteUI','Quote',
                                               c(None='',
                                                 'Double Quote'='"',
                                                 'Single Quote'="'"),
                                               'Double Quote')
                                ),
                                
                                
                                
                                plotOutput("caretPlotUI", width = "450px", height = "450px")
                              )
                              
                     ), # end tabPanel Data
                     
                     tabPanel(title = "View data", solidHeader = TRUE,
                              collapsible = TRUE,
                              DT::dataTableOutput("pre.data")
                              ) # end tabPanel View data
                     
                     
                     
                     ) # end tabBox
                     ) # end fluidPage Data Preparation
            
    ) # konec  tabItem datapreparation
    
   )#konec tabItems()
  ) # end dashboardBody
  
  ) # end dashboardPage()
  ) # end ui
