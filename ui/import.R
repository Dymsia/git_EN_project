tabpanel.import = fluidPage(theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(width = 3,
      uiOutput("import.ui")
    ),
    mainPanel(width = 9,
      htmlOutput("import.text"),
      box(width = 12, DT::dataTableOutput("import.preview")),
      uiOutput("tabpanel.browse.openml")
    )
  )
)

