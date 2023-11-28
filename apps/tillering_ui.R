til.tab1 <- tabItem(tabName = "tillering",
  fluidRow(
    box(
      title = "Parameters and variables",
      icon = icon("sliders"),
      status = "primary",
      width = 6, height = "500px",
      br(), h5("This is a test box"), br(),
      actionButton("test.submit", "Test button")
    ),
    box(
      title = "Tillering diagram",
      icon = icon("diagram-project"),
      status = "info",
      width = 6, height = "500px",
      br(), h5("This is a test box"), br(),
      plotOutput("tilgrid", width="100%")
    )
  )
)