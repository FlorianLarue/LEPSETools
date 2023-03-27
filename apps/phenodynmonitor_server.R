currentMessages <- reactiveValues(mes = list())
currentText <- reactiveVal()

observeEvent(input$pm.send, {
  currentText("Hakuna Matata")
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
