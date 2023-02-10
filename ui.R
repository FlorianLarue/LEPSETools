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
    header = dashboardHeader(
      title = "LEPSEtools"
    ),
    
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName="home", icon=icon("home")),
        menuItem("Leaf profile", tabName="leafprofile", icon=icon("seedling"))
      )
    ),
    
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "home",
          h2("Home")
        ),
        source("./apps/leafprofile_ui.R", local=TRUE)$value
      )
    ),
    
    footer = dashboardFooter(
      left = "MAGE, LEPSE, INRAe",
      right = "By Florian Larue"
    )
  )
)