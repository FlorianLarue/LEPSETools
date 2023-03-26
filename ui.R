#######################################################################
##                   LEPSE tools shiny application                   ##
##  This app contains several tools to help with work done at LEPSE  ##
##               around phenotyping and crop modelling               ##
##                       Author: Florian Larue                       ##
##                      Last updated: 2023-01-20                     ##
#######################################################################

##---------------------------------------------------------------
##                      Library and imports                     -
##---------------------------------------------------------------
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(data.table)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(dplyr)

##----------------------------------------------------------------
##      Define the User Interface of the LEPSEtool Shiny App     -
##----------------------------------------------------------------

shinyUI(
  ui = dashboardPage(
    skin="green",
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
        menuItem("Leaf profile", tabName="leafprofile", icon=icon("seedling")),
        menuItem("Phenodyn Monitor", tabName="dashboard", 
          icon=icon("dashboard"),
          menuSubItem("Import data", tabName="importdata", 
                      icon=icon("file-import")),
          menuSubItem("Export data", tabName="exportdata", 
                      icon=icon("file-export"))
        )
      )
    ),
    
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "home",
          h2("Home")
        ),
        source("./apps/leafprofile_ui.R", local=TRUE)$value,
        source("./apps/phenodynmonitor_ui.R", local=TRUE)$value
      )
    ),
    
    footer = dashboardFooter(
      left = "MAGE, LEPSE, INRAe",
      right = "By Florian Larue"
    )
  )
)