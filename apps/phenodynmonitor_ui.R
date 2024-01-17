pm.tab1 <-  tabItem("importdata",
  box(
    title = "Data import",
    icon = icon("file-import"),
    status = "info",
    width = 12,
    collapsible = TRUE,
    br(), h5("The first step is to upload the Excel file 'name.xlsx' 
             completed with the information of the position of the sensors
             and the duration of the measurements"), br(),
    fileInput("phenodynmonitordf", "Upload Excel file"),
    actionButton("pm.Submit", "Process data"),
    actionButton("pm.ReSubmit", "Resubmit data")
  ),
  box(
    title = "Information",
    icon = icon("circle-exclamation"),
    status = "success",
    width = 12,
    collapsible = TRUE,
    verbatimTextOutput("pm.Info")
  ),
  box(
    title = "Data table",
    icon = icon("table"),
    status = "warning",
    width = 12,
    collapsible = TRUE,
    DT::DTOutput("pm.df")
  ),
  box(
    title = "Generate files",
    icon = icon("file"),
    status = "primary",
    width = 12,
    collapsible = TRUE,
    actionButton("pm.Generate", "Generate DB files"),
    verbatimTextOutput("pm.GenInfo"), br(), br(),
    tabBox(id="dbfiles", width = 12,
      tabPanel(
        title = "Periode Measure Balance",
        DT::DTOutput("dbbalance")
      ),
      tabPanel(
        title = "Periode Measure Growth",
        DT::DTOutput("dbgrowth")
      ),
      tabPanel(
        title = "Periode Measure Thermocouple",
        DT::DTOutput("dbthermo")
      )
    )
  )
)

pm.tab2 <-  tabItem("exportdata",
  box(
    title = "Data export",
    icon = icon("file-export"),
    status = "info",
    width = 12,
    collapsible = TRUE,
    tabsetPanel(
      tabPanel(
        title = "Download",
        br(), 
        h5("The first step is to download the data from PhenodynDB and 
           process it using the PhenoDynDataCollector R package"), br(),
        actionButton("pm.DL", "Export data from PhenodynDB")
      ),
      tabPanel(
        title = "Data table"
      )
    )
  ),
  box(
    title = "Monitor results",
    icon = icon("chart-line"),
    status = "success",
    width = 12,
    collapsible = TRUE,
    h5("Here you can monitor the processed data")
  )
)
  
pm.tab3 <-  tabItem("messagedata",
  box(
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
  box(
    title = "Message history",
    icon = icon("message"),
    width = 12,
    status = "info",
    userMessages(
      width = 6,
      status = "danger",
      id = "pmmessages"
    )
  )
)