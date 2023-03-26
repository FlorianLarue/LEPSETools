#######################################################################
##                   LEPSE tools shiny application                   ##
##  This app contains several tools to help with work done at LEPSE  ##
##               around phenotyping and crop modelling               ##
##                       Author: Florian Larue                       ##
##                      Last updated: 2023-02-13                     ##
#######################################################################

##---------------------------------------------------------------
##                      Library and imports                     -
##---------------------------------------------------------------
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFiles)
library(data.table)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(dplyr)

options(warn=-1)

##---------------------------------------------------------------
##    Define the Server behavior of the LEPSEtool Shiny App     -
##---------------------------------------------------------------

shinyServer(function(input, output, session) {
  source("./apps/leafprofile_server.R", local=TRUE)
  #source("./apps/phenodynmonitor_server.R", local=TRUE)
})