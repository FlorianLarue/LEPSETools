til.tab1 <- tabItem(tabName = "tillering",
  fluidRow(
    box(
      title = "Parameters and variables",
      icon = icon("sliders"),
      status = "primary", collapsed = TRUE,
      width = 6, height = "750px", br(),
      splitLayout(
        numericInput("tilleringLSD", "Lower S/D bound", value=0.7),
        numericInput("tilleringUSD", "Upper S/D bound", value=1.7)
      ),
      bs4Card(width = 12, title = "Sensitiviy function",
        fluidRow(
          column(width = 6,
            DT::dataTableOutput("til.df.sens", height="100%")
          ),
          column(width = 6,
            plotOutput("til.plot.sens")
          )
        )
      )
    ),
    box(
      title = "Tillering diagram",
      icon = icon("diagram-project"),
      status = "info", collapsed = TRUE,
      width = 6, height = "750px", br(),
      plotOutput("tilgrid", width="100%"), br(), br(),
      sliderInput("tilleringphyllo", "Phyllochron", min = 2, max = 8,
                  value = 2, step = 1, animate = animationOptions(loop = TRUE)),
      fluidRow(
        column(width=4, checkboxInput("til.showall", "Show non emerged tillers", value = FALSE)),
        column(width=4, checkboxInput("til.color", "Color tiller cohorts", value = FALSE)),
        column(width=4, selectizeInput("til.prio", "Select prioritization rule", 
                                       c("Low order, low rank", "Low order, high rank",
                                         "High order, low rank", "High order, high rank")))
      )
    )
  ),
  box(
    title = "Data and simulation",
    icon = icon("diagram-project"),
    status = "warning",
    width = 12, height = "600px", br(),
    fluidRow(
      column(width = 6,
        textInput("til.sdivec", "Enter a vector of SDi values (comma delimited, 8 values)", 
                  "1.8,1.8,1.8,1.6,1.4,1.2,1.0,0.8"),
        DT::dataTableOutput("til.df.till", height="100%")
      ),
      column(width = 6,
        plotOutput("til.plot.till")
      )
    )
    
  )
)