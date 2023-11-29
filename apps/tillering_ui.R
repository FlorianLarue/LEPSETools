til.tab1 <- tabItem(tabName = "tillering",
  fluidRow(
    box(
      title = "Parameters and variables",
      icon = icon("sliders"),
      status = "primary",
      width = 6, height = "750px", br(),
      splitLayout(
        numericInput("tilleringLSD", "Lower S/D bound", value=0.7),
        numericInput("tilleringUSD", "Upper S/D bound", value=1.7)
      ),
      bs4Card(width = 12, title = "Sensitiviy function", collapsed = TRUE,
        fluidRow(
          column(width = 6,
                 DT::dataTableOutput("til.df", height="100%")
          ),
          column(width = 6,
                 plotOutput("sensplot")
          ),
        )
      )
      
      
    ),
    box(
      title = "Tillering diagram",
      icon = icon("diagram-project"),
      status = "info",
      width = 6, height = "750px", br(),
      plotOutput("tilgrid", width="100%")
    )
  ),
  box(
    title = "Data and simulation",
    icon = icon("diagram-project"),
    status = "warning",
    width = 12, height = "500px", br()
    
  )
)