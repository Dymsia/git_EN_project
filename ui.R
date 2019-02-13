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
    
  ) # end dashboardBody
  
  ) # end dashboardPage()
  ) # end ui
