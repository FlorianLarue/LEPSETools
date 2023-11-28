#######################################################################
##                   LEPSE tools shiny application                   ##
##  This app contains several tools to help with work done at LEPSE  ##
##               around phenotyping and crop modelling               ##
##                       Author: Florian Larue                       ##
##                      Last updated: 2023-12-03                     ##
#######################################################################

##----------------------------------------------------------------
##      Define the User Interface of the LEPSEtool Shiny App     -
##----------------------------------------------------------------
source("./apps/leafprofile_ui.R", local=TRUE)
source("./apps/phenodynmonitor_ui.R", local=TRUE)
source("./apps/tillering_ui.R", local=TRUE)

shinyUI(
  ui = function(request) {
    bs4DashPage(
      header = dashboardHeader(
        title = "LEPSEtools"
      ),
      
      sidebar = dashboardSidebar(
        sidebarMenu(
          br(),
          div(img(src="logo.png", height=105, width=123), 
              style="text-align: center;"
          ), br(),
          menuItem("Home", tabName="home", icon=icon("home")),
          menuItem("Leaf Profile", tabName="leafprofile", 
                   icon=icon("seedling")),
          menuItem("Phenodyn Monitor", tabName="dashboard", 
            icon=icon("dashboard"),
            menuSubItem("Import data", tabName="importdata", 
                        icon=icon("file-import")),
            menuSubItem("Export data", tabName="exportdata", 
                        icon=icon("file-export")),
            menuSubItem("Messages", tabName="messagedata", 
                        icon=icon("message"))
          ),
          menuItem("Tillering", tabName="tillering", 
                   icon=icon("plant-wilt")),
          menuItem("About", tabName="about", icon=icon("user")),
          bookmarkButton()
        )
      ),
      
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "home",
            box(width=12,
              includeMarkdown("home.md"),
              controlbar = dashboardControlbar(skinSelector(), pinned = TRUE)
            )
          ),
          lp.tab1,
          pm.tab1,
          pm.tab2,
          pm.tab3,
          til.tab1,
          tabItem(tabName = "about",
            userBox(
              title = userDescription(
                title = "Florian Larue",
                subtitle = "Postdoctoral researcher",
                type = 1,
                image = "florian.png",
                backgroundImage = "bg_user.jpg"
              ),
              footer = "florian.larue@outlook.com"
            )
          )
        )
      ),
      
      footer = dashboardFooter(
        left = "MAGE, LEPSE, INRAe",
        right = "By Florian Larue"
      )
    )
  }
)
