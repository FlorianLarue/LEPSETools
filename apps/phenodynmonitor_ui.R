tabItem("importdata",
  shinydashboardPlus::box(
    title = "Upload file",
    icon = icon("file-import"),
    status = "info",
    width = 12,
    h5("The first step is to upload the Excel file 'name.xlsx' 
       completed with the information of the position of the sensors 
       and the duration of the measurements")
  ),
  
  shinydashboardPlus::box(
    title = "Monitor sensors",
    icon = icon("chart-line"),
    status = "success",
    width = 12,
    h5("Here you can monitor sensor data based on the information provided
       above")
  ),
)
