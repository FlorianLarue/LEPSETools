tabItem(tabName = "leafprofile",
  tabsetPanel(type = "tabs",
    tabPanel("Settings", br(),
      "Test"
    ),
    tabPanel("Analysis", br(),
      shinydashboardPlus::box(title="Data", width=12, status="info",
        fileInput("leafprofiledf","Import data"),
        splitLayout(
          tags$head(tags$style(
            HTML(".shiny-split-layout > div {overflow: visible;}"))),
          cellWidths = c("0%","33%","33%","33%"),
          uiOutput("lp.selVar"),
          uiOutput("lp.selIrrig"),
          uiOutput("lp.selSemis")
        ),
        column(width=12, align="center",
          actionButton("previousS", "Previous"),
          actionButton("nextS", "Next")
        )
      ),
      shinydashboardPlus::box(
        title=div("Visualization", downloadBttn("downloadPlots", "")), 
        height="100%", width=12, status = "success",
        column(width=6, align="center",
          actionButton("leftshift", label= div(icon("arrow-left"), "Shift left")),
          actionButton("rightshift", label=div("Shift right", icon("arrow-right"))),
          actionButton("inverse", label="Inverse"),
          actionButton("reset", label="Reset"),
          plotOutput("lp.plot", height="100%", click="plot_click")
        ),
        column(width=6, align="center",
          actionButton("pp.leftshift", label= div(icon("arrow-left"), "Shift left")),
          actionButton("pp.rightshift", label=div("Shift right", 
                                                  icon("arrow-right"))),
          actionButton("pp.inverse", label="Inverse"),
          actionButton("pp.isolate", label="Isolate"),
          plotOutput("pp.plot", height="100%", click="plot_click2")
        ),
        column(width=6, align="center",
          uiOutput("lp.selRank"),
        ),
        column(width=6, align="center",
          uiOutput("lp.selPlant"),
        )
      ),
      shinydashboardPlus::box(title="History", height="100%", width=12, 
        status = "warning",
        column(width=6, align="center",
          actionButton("saveChanges", "Save changes"),
          downloadButton("downloadData", "Download dataframe"),
          actionButton("resetall", "Reset all"),
          DT::dataTableOutput("lp.df")
        ),
        column(width=6, align="center",
          actionButton("revert", label="Revert selected changes"),
          downloadButton("downloadHistory", "Download history"),
          DT::dataTableOutput("changes.df")
        )
      )
    )
  )
)