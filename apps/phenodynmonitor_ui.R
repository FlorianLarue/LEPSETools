pm.tab1 <-  tabItem("importdata",
  shinydashboardPlus::box(
    title = "Data import",
    icon = icon("file-import"),
    status = "info",
    width = 12,
    collapsible = TRUE,
    tabsetPanel(
      tabPanel(
        title = "Upload",
        h5("The first step is to upload the Excel file 'name.xlsx' 
            completed with the information of the position of the sensors 
            and the duration of the measurements"), br(),
        fileInput("phenodynmonitordf", "Upload Excel file"),
        actionButton("pm.Submit", "Import data to PhenodynDB")
      ),
      tabPanel(
        title = "Data table"
      )
    )
  ),
  shinydashboardPlus::box(
    title = "Monitor sensors",
    icon = icon("chart-line"),
    status = "success",
    width = 12,
    collapsible = TRUE,
    h5("Here you can monitor sensor data based on the information provided
        above and fix potential errors")
  )
)

pm.tab2 <-  tabItem("exportdata",
  shinydashboardPlus::box(
    title = "Data export",
    icon = icon("file-export"),
    status = "info",
    width = 12,
    collapsible = TRUE,
    tabsetPanel(
      tabPanel(
        title = "Download",
        h5("The first step is to download the data from PhenodynDB and 
           process it using the PhenoDynDataCollector R package"), br(),
        actionButton("pm.DL", "Export data from PhenodynDB")
      ),
      tabPanel(
        title = "Data table"
      )
    )
  ),
  shinydashboardPlus::box(
    title = "Monitor results",
    icon = icon("chart-line"),
    status = "success",
    width = 12,
    collapsible = TRUE,
    h5("Here you can monitor the processed data")
  )
)
  
pm.tab3 <-  tabItem("messagedata",
  shinydashboardPlus::box(
    title = "Leave messages",
    icon = icon("message"),
    width = 12,
    status = "success",
    h5("Here you can write messages to leave for futur users")
  )
)


