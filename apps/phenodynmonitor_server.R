# Variables
currentMessages <- reactiveValues(mes=list())
excelFiles <- reactiveValues(workbook=NA, dateMesure=NA, notationMesure=NA)
dfErrors <- reactiveVal(NA)

# Functions
check_errors <- function() {
  for(r in 1:nrow(excelFiles$notationMesure)) {
    if(!is.na(excelFiles$notationMesure[r, "RDT"])) {
      splt <- strsplit(excelFiles$notationMesure[r,"RDT"], split="")[[1]]
      newname <- c()
      firstdigit <- TRUE
      for(l in 1:length(splt)) {
        if(is.na(as.numeric(splt[l]))) {
          newname <- c(newname, tolower(splt[l]))
        } else {
          if(firstdigit && l == length(splt)) {
            newname <- c(newname, paste0("0",splt[l]))
          } else {
            newname <- c(newname, splt[l])
            firstdigit <- FALSE
          }
        }
      }
      excelFiles$notationMesure[r,"RDT"] <- paste(newname, collapse = "")
    }
  }
  
  errors <- list()
  
  if(length(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"]))]) > 0) {
    tmp <- c()
    for(s in 1:length(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"]))])) {
      errors[["Rdt"]] <- c(errors[["Rdt"]], unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$RDT), "RDT"]))][s])
    }
  }
  
  if(length(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"]))]) > 0) {
    tmp <- c()
    for(s in 1:length(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"]))])) {
      errors[["Tc"]] <- c(errors[["Tc"]], unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"])[!is.na(unique(excelFiles$notationMesure[duplicated(excelFiles$notationMesure$TC), "TC"]))][s])
    }
  }
  return(errors)
}

# Observers
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
    
    excelFiles$notationMesure$rowId <- rownames(excelFiles$notationMesure)
    
    errors <- check_errors()
    if(length(errors) == 0) {
      dfErrors(NA)
      
      output$pm.Info <- renderText({
        paste("No errors in file")
      })
      
      output$pm.df <- DT::renderDT(
        excelFiles$notationMesure,
        options = list(scrollX=TRUE),
        editable = FALSE
      )
      
      shinyalert("Excel file loaded", "Succesfully loaded the Excel file without errors",
                 type="success")
    } else {
      output$pm.Info <- renderText({
        paste("Duplicate RDT detected:", paste0(errors[["Rdt"]], collapse=", "), "",
              "Duplicate TC detected:", paste0(errors[["Tc"]], collapse=", "),
               sep="\n")
      })
      
      RDTrows <- subset(subset(excelFiles$notationMesure, duplicated(excelFiles$notationMesure$RDT) | 
                     duplicated(excelFiles$notationMesure$RDT, fromLast = TRUE)), !is.na(RDT))
      TCrows <- subset(subset(excelFiles$notationMesure, duplicated(excelFiles$notationMesure$TC) | 
                      duplicated(excelFiles$notationMesure$TC, fromLast = TRUE)), !is.na(TC))
      errorRows <- rbind(RDTrows, TCrows)
      errorRows <- errorRows[!duplicated(errorRows),]
      
      dfErrors(errorRows)
      
      output$pm.df <- DT::renderDT(
        errorRows,
        options = list(scrollX=TRUE),
        editable = TRUE
      )
      
      shinyalert("Excel file loaded", "Succesfully loaded the Excel file but with errors",
                 type="warning")
    }
    
  } else {
    shinyalert("Incompatible file", "Please provide a .xls or .xlsx file",
               type="error")
  }
})
observeEvent(input$pm.ReSubmit, {
  if(is.na(excelFiles$workbook)) {
    shinyalert("No data processed", "Please first process an excel file",
               type="error")
  } else if(length(dfErrors()) == 1 && is.na(dfErrors())) {
    shinyalert("No errors found", "No errors found, DB files can be generated without resubmitting data",
               type="error")
  } else {
    errors <- check_errors()
    if(length(errors) == 0) {
      dfErrors(NA)
      output$pm.Info <- renderText({
        paste("No errors in file")
      })
      
      output$pm.df <- DT::renderDT(
        excelFiles$notationMesure,
        options = list(scrollX=TRUE),
        editable = FALSE
      )
      
      shinyalert("Errors fixed", "Succesfully fixed errors",
                 type="success")
    } else {
      output$pm.Info <- renderText({
        paste("Duplicate RDT detected:", paste0(errors[["Rdt"]], collapse=", "), "",
              "Duplicate TC detected:", paste0(errors[["Tc"]], collapse=", "),
              sep="\n")
      })
      
      RDTrows <- subset(subset(excelFiles$notationMesure, duplicated(excelFiles$notationMesure$RDT) | 
                                 duplicated(excelFiles$notationMesure$RDT, fromLast = TRUE)), !is.na(RDT))
      TCrows <- subset(subset(excelFiles$notationMesure, duplicated(excelFiles$notationMesure$TC) | 
                                duplicated(excelFiles$notationMesure$TC, fromLast = TRUE)), !is.na(TC))
      errorRows <- rbind(RDTrows, TCrows)
      errorRows <- errorRows[!duplicated(errorRows),]
      
      dfErrors(errorRows)
      
      output$pm.df <- DT::renderDT(
        errorRows,
        options = list(scrollX=TRUE),
        editable = TRUE
      )
      
      shinyalert("Errors detected", "There are still errors left",
                 type="warning")
    }
  }
})
observeEvent(input$pm.Generate, {
  if(length(excelFiles$workbook) == 1 && is.na(excelFiles$workbook)) {
    shinyalert("No data processed", "Please first process an excel file",
               type="error")
  } else if(length(dfErrors()) > 1 || !is.na(dfErrors())) {
    shinyalert("Errors found", "Please fix errors in data and resubmit before generating DB files",
               type="error")
  }
})
observeEvent(input$pm.df_cell_edit, {
  rownb <- dfErrors()[input$pm.df_cell_edit$row, "rowId"]
  if(input$pm.df_cell_edit$value == "") {
    excelFiles$notationMesure[rownb, input$pm.df_cell_edit$col] <- NA
  } else {
    excelFiles$notationMesure[rownb, input$pm.df_cell_edit$col] <- input$pm.df_cell_edit$value
  }
})