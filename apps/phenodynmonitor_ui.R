pm.tab1 <-  tabItem("importdata",
  shinydashboardPlus::box(
    title = "Data import",
    icon = icon("file-import"),
    status = "info",
    width = 12,
    collapsible = TRUE,
    br(), h5("The first step is to upload the Excel file 'name.xlsx' 
             completed with the information of the position of the sensors
             and the duration of the measurements"), br(),
    fileInput("phenodynmonitordf", "Upload Excel file"),
    actionButton("pm.Submit", "Process data")
  ),
  shinydashboardPlus::box(
    title = "Information",
    icon = icon("circle-exclamation"),
    status = "success",
    width = 12,
    collapsible = TRUE,
    verbatimTextOutput("pm.Info")
  ),
  shinydashboardPlus::box(
    title = "Data table",
    icon = icon("table"),
    status = "warning",
    width = 12,
    collapsible = TRUE,
    DT::DTOutput("pm.df")
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
    h5("Here you can write messages to leave for futur users"),
    splitLayout(
      textInput("pm.name", "Name"),
      dateInput("pm.date", "Date"),
      selectInput("pm.type", "Type", choices = c("Info","Error"), 
                  selected = "Info")
    ),
    textAreaInput("pm.message","Message"),
    actionButton("pm.send", "Send message")
  ),
  shinydashboardPlus::box(
    title = "Message history",
    icon = icon("message"),
    width = 12,
    status = "info",
    shinydashboardPlus::userMessages(
      width = 6,
      status = "danger",
      id = "pmmessages"
    )
  )
)