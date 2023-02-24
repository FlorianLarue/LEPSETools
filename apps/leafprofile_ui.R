tabItem(tabName = "leafprofile",
  tabsetPanel(type = "tabs",
    tabPanel("Settings", br(),
      shinydashboardPlus::box(title="Data import", width=12, status="info",
        fileInput("leafprofiledf","Import data")
      ),
      shinydashboardPlus::box(title="Select column names", width=12, 
                              status="success",
        splitLayout(
          textInput("cn.Variety", "Variety", value = "Variety"),
          textInput("cn.Sowing", "Sowing", value = "Semis"),
          textInput("cn.Irrigation", "Irrigation", value = "Irrigation")
        ),
        splitLayout(
          textInput("cn.Date", "Date", value="Date_de_plv"),
          textInput("cn.Leafrank", "Leaf rank", value="leafrank"),
          textInput("cn.Leafarea", "Leaf area", value="areal")
        ),
        splitLayout(
          textInput("cn.Medianarea", "Median area", value="medianareal"),
          textInput("cn.Meanarea", "Mean area", value="meanareal"),
          textInput("cn.SDarea", "Sd area", value="sdareal")
        ),
        splitLayout(
          textInput("cn.PlantNb", "Plant number", value="N_Plante"),
          textInput("cn.LeafNb", "Leaf number", value="N_Feuille"),
          textInput("cn.MaxLeaves", "Max number of leaves", value="nbmaxleaves")
        ),
        actionButton("cn.Submit", "Submit")
      )
    ),
    tabPanel("Analysis", br(),
      shinydashboardPlus::box(title="Data", width=12, status="info",
        splitLayout(
          tags$head(tags$style(
            HTML(".shiny-split-layout > div {overflow: visible;}"))),
          cellWidths = c("0%","33%","33%","33%"),
          selectizeInput("leafprofilevar", "Select a variety", 
                         choices=NULL),
          selectizeInput("leafprofileirrig", "Select an irrigation", 
                         choices=NULL),
          selectizeInput("leafprofilesemis", "Select a sowing",
                         choices=NULL)
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