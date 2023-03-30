currentMessages <- reactiveValues(mes=list())
excelFiles <- reactiveValues(workbook=NA, dateMesure=NA, notationMesure=NA)
csvFiles <- reactiveValues()

observeEvent(input$pm.send, {
  req(input$pm.name, input$pm.message)
  if(input$pm.type == "Info") {
    tmpType <- "received"
  } else {
    tmpType <- "sent"
  }
  shinydashboardPlus::updateUserMessages(
    id = "pmmessages",
    action = "add",
    content = list(
      author = input$pm.name,
      date = input$pm.date,
      image = "message.png",
      type = tmpType,
      text = input$pm.message
    )
  )
  
  currentMessages$mes[[length(currentMessages$mes)+1]] <- 
    c(input$pm.name, input$pm.date, input$pm.message, input$pm.type)
})

observeEvent(input$pm.Submit, {
  req(input$phenodynmonitordf)
  if(tail(strsplit(input$phenodynmonitordf$datapath, split=".", fixed=T)[[1]], 
     n=1) %in% c("xls","xlsx")) {
    excelFiles$workbook <-
      XLConnect::loadWorkbook(input$phenodynmonitordf$datapath)
    excelFiles$dateMesure <- 
      XLConnect::readWorksheet(excelFiles$workbook, "DateMesure", 3, 2)
    excelFiles$notationMesure <- 
      XLConnect::readWorksheet(excelFiles$workbook, "NotationMesure")

    output$pm.df <- DT::renderDT(
      excelFiles$notationMesure,
      options = list(scrollX=TRUE)
    )
      
      
    shinyalert("Excel file loaded", "Succesfully loaded the Excel file",
               type="success")
  } else {
    shinyalert("Incompatible file", "Please provide a .xls or .xlsx file",
               type="error")
  }
})
