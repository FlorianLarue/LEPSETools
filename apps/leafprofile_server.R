# Variables
df.lp.init <- reactiveVal(); df.lp.init.o <- reactiveVal()
df.lp <- reactiveVal(); df.changes <- reactiveVal(data.frame())
df.changes.f <- reactiveVal(data.frame())
lp.var.c <- reactiveVal(); lp.irrig.c <- reactiveVal()
lp.semis.c <- reactiveVal()
date.sel <- reactiveVal(NA);date.sel.p <- reactiveVal(NA)
max.plants <- reactiveVal(0); max.leaves <- reactiveVal(0)
col.names <- reactiveVal(
  c("Variety","Sowing","Irrigation","Date","Leafrank","Leafarea","Medianarea",
    "Meanarea","SDarea","PlantNb","LeafNb","MaxLeaves")
)
col.user <- reactiveVal()

# Functions
updateHistory <- function(v, s, i, d, p, r, a) {
  df.changes(rbind(df.changes(), data.frame(Variety = v, Sowing = s, 
                                            Irrigation = i, Date = d,
                                            Plantnumber = p, Leafrank = r, 
                                            Action = a)))
}
updateLeafarea <- function(df, v, i, s, d) {
  df.tmp <- df[which(df$Variety == v & df$Irrigation == i & df$Sowing == s &
                       df$Date == d),]
  for(l in unique(df.tmp$Leafrank)) {
    df.tmp[which(df.tmp$Leafrank == l),"Medianarea"] <- 
      median(df.tmp[which(df.tmp$Leafrank == l & df.tmp$ligulated == "x"), 
                    "Leafarea"], na.rm=T)
    df.tmp[which(df.tmp$Leafrank == l),"Meanarea"] <- 
      mean(df.tmp[which(df.tmp$Leafrank == l & df.tmp$ligulated == "x"), 
                  "Leafarea"], na.rm=T)
    df.tmp[which(df.tmp$Leafrank == l),"SDarea"] <- 
      sd(df.tmp[which(df.tmp$Leafrank == l & df.tmp$ligulated == "x"), 
                "Leafarea"], na.rm=T)
  }
  df[which(df$Variety == v & df$Irrigation == i & df$Sowing == s &
             df$Date == d),] <- df.tmp
  return(df)
}
incr <- function(x) {
  eval.parent(substitute(x <- x + 1))
}
decr <- function(x) {
  eval.parent(substitute(x <- x - 1))
}

# Listeners
listen.lp.sel <- reactive({
  list(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
})

# Observers
observeEvent(input$leafprofiledf,{
  df.input <- data.frame(fread(input$leafprofiledf$datapath))
  df.input$sel <- 16
  df.input$sel.p <- 16
  df.input$iso <- 1
  df.lp.init(df.input)
  df.lp.init.o(df.input)
  
  if(!all(col.names() %in% colnames(df.lp.init()))) {
    shinyalert("Warning", 
      paste0("Different column names detected ", 
             "don't forget to fill in the used column names below ",
             "and to hit the submit button"),
      type = "warning")
  } else {
    updateSelectizeInput(session, "leafprofilevar", 
                         choices = unique(df.lp.init.o()$Variety))
    updateSelectizeInput(session, "leafprofileirrig", 
                         choices = unique(df.lp.init.o()$Irrigation))
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing))
    max.plants(max(df.lp.init.o()$PlantNb, na.rm=T))
    max.leaves(max(df.lp.init.o()$Leafrank, na.rm=T))
  }
})

observeEvent(input$cn.Submit, {
  req(input$leafprofiledf)
  df.tmp <- df.lp.init()
  col.user(colnames(df.tmp))
  for(n in col.names()) {
    names(df.tmp)[names(df.tmp) == input[[paste0("cn.", n)]]] <- n
  }
  df.lp.init(df.tmp)
  df.lp.init.o(df.tmp)
  updateSelectizeInput(session, "leafprofilevar", 
                       choices = unique(df.lp.init.o()$Variety))
  updateSelectizeInput(session, "leafprofileirrig", 
                       choices = unique(df.lp.init.o()$Irrigation))
  updateSelectizeInput(session, "leafprofilesemis", 
                       choices = unique(df.lp.init.o()$Sowing))
  max.plants(max(df.lp.init.o()$PlantNb, na.rm=T))
  max.leaves(max(df.lp.init.o()$Leafrank, na.rm=T))
  shinyalert("Success", 
             paste0("Column names of your dataframe have been changed",
             "to match variables names used in this app. ", 
             "Upon saving your work, original column names will be restored"),
             type="success")
})

observeEvent(input$leafprofilevar, {
  lp.var.c(input$leafprofilevar)
})

observeEvent(input$leafprofileirrig, {
  lp.irrig.c(input$leafprofileirrig)
})

observeEvent(input$leafprofilesemis, {
  lp.semis.c(input$leafprofilesemis)
})

observeEvent(listen.lp.sel(), {
  output$pp.plot <- NULL
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.lp(df.lp.init()[which(df.lp.init()$Variety == input$leafprofilevar &
                             df.lp.init()$Irrigation == input$leafprofileirrig &
                             df.lp.init()$Sowing == input$leafprofilesemis),])
})

observeEvent(input$plot_click, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  
  # Extract the selected data
  df.tmp <- nearPoints(df.lp(), input$plot_click, allRows=TRUE)
  df.tmp.sel <- df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                               df.tmp$Irrigation == input$leafprofileirrig &
                               df.tmp$Sowing == input$leafprofilesemis),]
  # Change the status of selected data (identify in the "sel" column)
  for(d in unique(df.tmp.sel$Date)) {
    if(sum(df.tmp.sel[which(df.tmp.sel$Date == d),
                      "selected_"],na.rm=T) > 0) {
      if(df.tmp.sel[which(df.tmp.sel$Date == d),"sel"][1] == 13) {
        df.tmp.sel[which(df.tmp.sel$Date == d),"sel"] <- 16
        df.tmp.sel$sel.p <- 16
        if(13 %in% df.tmp.sel$sel) {
          if(d == date.sel()) {
            date.sel(date.sel.p())
          }
        } else {
          date.sel(NA)
          output$lp.selRank <- NULL
          output$lp.selPlant <- NULL
        }
      } else {
        if(13 %in% df.tmp.sel$sel) {
          date.sel.p(date.sel())
        } else {
          date.sel.p(NA)
        }
        df.tmp.sel[which(df.tmp.sel$Date == d),"sel"] <- 13
        date.sel(d)
      }
    }
  }
  df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                 df.tmp$Irrigation == input$leafprofileirrig &
                 df.tmp$Sowing == input$leafprofilesemis),] <- df.tmp.sel
  
  # Create output to allow for rank selection and deletion
  if(13 %in% df.tmp$sel) {
    output$lp.selRank <- renderUI({
      div(
        selectizeInput(inputId = "leafprofilerank", 
                       label = "Select a leaf rank", width = "100%",
                       choices = df.tmp[which(df.tmp$sel == 13 & 
                              !is.na(df.tmp$Leafarea) &
                              !is.na(df.tmp$Leafrank)),"Leafrank"]),
        actionButton("delrank", label="Delete rank", icon=icon("trash")))
    }) 
  }
  
  # Replace data in original dataframe
  df.tmp <- df.tmp[,-c(ncol(df.tmp))]
  df.lp(df.tmp)
  
  # Create output for per-plant plot
  if(!is.na(date.sel())) {
    output$pp.plot <- renderPlot({
      df.pp <- df.lp()
      df.pp <- df.pp[which(df.pp$Variety == input$leafprofilevar & 
                             df.pp$Irrigation == input$leafprofileirrig &
                             df.pp$Sowing == input$leafprofilesemis &
                             df.pp$Date == date.sel() & 
                             df.pp$iso == 1),]
      df.tmp2 <- df.pp %>%
        summarise(nbmax = mean(MaxLeaves, na.rm=T))
      ggplot(df.pp, aes(x = Leafrank, y = Leafarea, col = as.factor(PlantNb))) + 
      geom_line() + geom_point(shape=df.pp$sel.p) + 
      xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
      scale_x_continuous(breaks = function(x)
        unique(floor(pretty(seq(0, (max(df.tmp2$nbmax,
                                        na.rm=T)+1)*1.1))))) +
      geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                 linetype = "dashed", col="red") +
      guides(shape="none", col=guide_legend(nrow=2)) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size=6))
    }, height = 500, res = 150)
    
    # Create output to allow for plant selection and deletion
    output$lp.selPlant <- renderUI({
      div(selectizeInput(inputId = "leafprofileplant", 
                         label = "Select a plant number",
                         width = "100%",
                         choices = df.tmp[which(df.tmp$sel == 13 & 
                                                  !is.na(df.tmp$Leafarea) &
                                                  !is.na(df.tmp$Leafrank)),
                                          "PlantNb"]),
          actionButton("delplant", label="Delete plant", icon=icon("trash")))
    })
  } else {
    output$pp.plot <- NULL
  }
})

observeEvent(input$plot_click2, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  
  # Extract the selected data
  df.tmp <- nearPoints(df.lp(), input$plot_click2, allRows=TRUE)
  df.tmp.sel <- df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                               df.tmp$Irrigation == input$leafprofileirrig &
                               df.tmp$Sowing == input$leafprofilesemis &
                               df.tmp$Date == date.sel()),]
  
  # Change the status of selected data (identify in the "sel" column)
  for(p in unique(df.tmp.sel$PlantNb)) {
    if(sum(df.tmp.sel[which(df.tmp.sel$PlantNb == p),
                      "selected_"],na.rm=T) > 0) {
      if(df.tmp.sel[which(df.tmp.sel$PlantNb == p),"sel.p"][1] == 13) {
        df.tmp.sel[which(df.tmp.sel$PlantNb == p),"sel.p"] <- 16
      } else {
        df.tmp.sel[which(df.tmp.sel$PlantNb == p),"sel.p"] <- 13
      }
    }
  }
  
  df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                 df.tmp$Irrigation == input$leafprofileirrig &
                 df.tmp$Sowing == input$leafprofilesemis &
                 df.tmp$Date == date.sel()),] <- df.tmp.sel
  
  # Replace data in original dataframe
  df.tmp <- df.tmp[,-c(ncol(df.tmp))]
  df.lp(df.tmp)
})

observeEvent(input$leftshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel == 13),"Leafrank"] - 1
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                  input$leafprofileirrig, i, "all", "all", "Left shift")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(input$pp.leftshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel.p == 13),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel.p == 13),"Leafrank"] - 1
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig,
                           input$leafprofilesemis, date.sel())
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13),"PlantNb"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis,
                  input$leafprofileirrig, date.sel(), as.character(i), "all", 
                  "Left shift")
  }
  
  df.tmp[which(df.tmp$sel.p == 13),"sel.p"] <- 16
  df.lp(df.tmp)
})

observeEvent(input$rightshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel == 13),"Leafrank"] + 1
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                  input$leafprofileirrig, i, "all", "all","Right shift")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(input$pp.rightshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel.p == 13),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel.p == 13),"Leafrank"] + 1
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig,
                           input$leafprofilesemis, date.sel())
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13),"PlantNb"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis,
                  input$leafprofileirrig, date.sel(), as.character(i), "all", 
                  "Right shift")
  }
  
  df.tmp[which(df.tmp$sel.p == 13),"sel.p"] <- 16
  df.lp(df.tmp)
})

observeEvent(input$inverse, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date"])) {
    df.tmp.d <- df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),]
    for(j in unique(df.tmp.d$PlantNb)) {
      df.tmp.d[which(df.tmp.d$PlantNb == j), "Leafrank"] <-
        rev(df.tmp.d[which(df.tmp.d$PlantNb == j), "Leafrank"])
    }
    
    df.tmp.d <- updateLeafarea(df.tmp.d, input$leafprofilevar, 
                               input$leafprofileirrig, input$leafprofilesemis,i)
    
    df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),] <- df.tmp.d
    
    updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                  input$leafprofileirrig, i, "all", "all", "Inverse")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  output$pp.plot <- NULL
})

observeEvent(input$pp.inverse, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13), "PlantNb"])) {
    df.tmp[which(df.tmp$sel.p == 13 & df.tmp$PlantNb == i), "Leafrank"] <-
      rev(df.tmp[which(df.tmp$sel.p == 13 & df.tmp$PlantNb == i), "Leafrank"])
  }
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig, 
                           input$leafprofilesemis, date.sel())
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13),"PlantNb"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis,
                  input$leafprofileirrig, date.sel(), as.character(i), "all", 
                  "Inverse")
  }
  
  df.tmp[which(df.tmp$sel.p == 13),"sel.p"] <- 16
  df.lp(df.tmp)
})

observeEvent(input$delrank, {
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13 & df.tmp$Leafrank == input$leafprofilerank),
         c("Medianarea","Meanarea","SDarea","Leafarea")] <- c(NA,NA,NA,NA)
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date"])) {
    updateHistory(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),
                         "Variety"][1], 
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),
                         "Sowing"][1],
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),
                         "Irrigation"][1], i, "all",
                  as.character(input$leafprofilerank), "Delete")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
})

observeEvent(input$delplant, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis,
      input$leafprofileplant)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13 & df.tmp$PlantNb == input$leafprofileplant),
         c("Medianarea","Meanarea","SDarea","Leafarea")] <- c(NA,NA,NA,NA)
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig, 
                           input$leafprofilesemis, date.sel())
  updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                input$leafprofileirrig, date.sel(), 
                as.character(input$leafprofileplant), "all", "Delete")
  df.lp(df.tmp)
})

observeEvent(input$pp.isolate, {
  df.tmp <- df.lp()
  if(0 %in% df.tmp$iso) {
    df.tmp$iso <- 1
  } else {
    if(13 %in% df.tmp$sel.p) {
      df.tmp[which(df.tmp$sel.p == 16), "iso"] <- 0
      output$lp.selRank <- NULL
      output$lp.selPlant <- NULL
    }
  }
  df.lp(df.tmp)
})

observeEvent(df.lp(), {
  output$lp.df <- DT::renderDataTable({
    DT::datatable(
      df.lp()[,c("Date","PlantNb","LeafNb","Leafarea","Medianarea",
                 "Meanarea","SDarea","Leafrank","MaxLeaves")], 
      extensions = "FixedColumns",
      options=list(dom="t", scollX=TRUE, scrollY="400px",pageLength=-1, 
                   fixedColumns=list(leftColumns = 3),
                   columnDefs=list(list(className = 'dt-center', 
                                        targets = "_all"))),
      fillContainer = T,
      rownames = FALSE
    ) %>% formatRound(c(4:7), 2)
  })
  
  output$lp.plot <- renderPlot({
    df.tmp <- df.lp()
    df.tmp$Date <- as.Date(df.tmp$Date, "%d/%m/%Y")
    df.tmp$Date <- as.factor(df.tmp$Date)
    df.tmp2 <- df.tmp %>%
      summarise(nbmax = mean(MaxLeaves, na.rm=T))
    ggplot(df.lp(), aes(x = Leafrank, y = Medianarea, 
                        col = Date)) +
      geom_point(shape=df.lp()$sel) + geom_line() +
      xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
      scale_x_continuous(breaks = function(x)
        unique(floor(pretty(seq(0, (max(df.lp()$Leafrank,
                                        na.rm=T)+1)*1.1))))) +
      geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                 linetype = "dashed", col="red") +
      guides(shape="none", col=guide_legend(nrow=2)) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size=6))
  }, height = 500, res = 150)
})

observeEvent(df.changes(), {
  output$changes.df <- DT::renderDataTable({
    DT::datatable(
      df.changes(), 
      extensions = "FixedColumns",
      options=list(dom="t", scollX=TRUE, scrollY="400px",pageLength=-1, 
                   fixedColumns=list(leftColumns = 3),
                   columnDefs=list(list(className = 'dt-center', 
                                        targets = "_all"))),
      fillContainer = T,
      rownames = FALSE)
  })
})

observeEvent(input$saveChanges, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  output$lp.selPlant <- NULL
  output$pp.plot <- NULL
  df.org <- df.lp.init()
  df.org[which(df.org$Variety == input$leafprofilevar &
                 df.org$Irrigation == input$leafprofileirrig &
                 df.org$Sowing == input$leafprofilesemis),] <- df.lp()
  df.lp.init(df.org)
  df.changes.f(df.changes())
  updateSelectizeInput(session, "leafprofilevar", 
                       choices = unique(df.lp.init.o()$Variety),
                       selected = lp.var.c())
  updateSelectizeInput(session, "leafprofileirrig", 
                       choices =  unique(df.lp.init.o()$Irrigation),
                       selected = lp.irrig.c())
  updateSelectizeInput(session, "leafprofilesemis", 
                       choices = unique(df.lp.init.o()$Sowing),
                       selected = lp.semis.c())
})

observeEvent(input$reset, {
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  output$pp.plot <- NULL
  date.sel(NA)
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.lp(df.lp.init()[which(df.lp.init()$Variety == input$leafprofilevar &
                           df.lp.init()$Irrigation == input$leafprofileirrig &
                           df.lp.init()$Sowing == input$leafprofilesemis),])
  df.changes(df.changes.f())
})

observeEvent(input$resetall, {
  df.lp.init(df.lp.init.o())
  output$lp.df <- NULL
  output$lp.plot <- NULL
  output$pp.plot <- NULL
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  df.changes(data.frame())
  df.changes.f(data.frame())
})

observeEvent(input$nextS, {
  req(input$leafprofiledf, input$leafprofilevar, input$leafprofileirrig, 
      input$leafprofilesemis) 
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  if(input$leafprofilesemis == "S2") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S1")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S2")
    if(input$leafprofileirrig == "WD") { 
      updateSelectizeInput(session, "leafprofileirrig", 
                           choices = unique(df.lp.init.o()$Irrigation), 
                           selected = "WW")
    } else { # case 3 : change irrig, sowing and var
      currentVar <- match(input$leafprofilevar, unique(df.lp.init()$Variety))
      updateSelectizeInput(session, "leafprofilevar", 
        choices = unique(df.lp.init.o()$Variety), 
        selected = unique(df.lp.init.o()$Variety)[currentVar + 1])
      updateSelectizeInput(session, "leafprofileirrig", 
                           choices = unique(df.lp.init.o()$Irrigation), 
                           selected = "WD")
    }
  }
})

observeEvent(input$previousS, {
  req(input$leafprofiledf, input$leafprofilevar, input$leafprofileirrig, 
      input$leafprofilesemis)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  if(input$leafprofilesemis == "S1") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S2")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S1")
    if(input$leafprofileirrig == "WW") { 
      updateSelectizeInput(session, "leafprofileirrig", 
                           choices = unique(df.lp.init.o()$Irrigation), 
                           selected = "WD")
    } else { # case 3 : change irrig, sowing and var  
      currentVar <- match(input$leafprofilevar, unique(df.lp.init()$Variety))
      updateSelectizeInput(session, "leafprofilevar", 
        choices = unique(df.lp.init.o()$Variety), 
        selected = unique(df.lp.init.o()$Variety)[currentVar + - 1])
      updateSelectizeInput(session, "leafprofileirrig", 
                           choices = unique(df.lp.init.o()$Irrigation), 
                           selected = "WW")
    }
  }
})

observeEvent(input$revert, {
  req(input$changes.df_rows_selected)
  output$lp.selRank <- NULL
  df.tmp <- df.lp()
  for(r in input$changes.df_rows_selected) {
    v <- df.changes()[r, "Variety"]; s <- df.changes()[r, "Sowing"]
    i <- df.changes()[r, "Irrigation"]; d <- df.changes()[r, "Date"]
    p <- df.changes()[r, "Plantnumber"]; l <- df.changes()[r, "Leafrank"]
    if(p == "all") p <- 1:max.plants()
    if(l == "all") l <- 1:max.leaves()
    a <- df.changes()[r, "Action"]
    
    if(a == "Left shift") {
      incr(df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i & 
                   df.tmp$Sowing == s & df.tmp$Date == d & 
                   df.tmp$PlantNb %in% p), "Leafrank"])
    } else if(a == "Right shift") {
      decr(df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i & 
                     df.tmp$Sowing == s & df.tmp$Date == d & 
                     df.tmp$PlantNb %in% p), "Leafrank"])
    } else if(a == "Delete") {
      df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i & 
                   df.tmp$Sowing == s & df.tmp$Date == d &
                   df.tmp$PlantNb %in% p & df.tmp$Leafrank %in% l),] <-
      df.lp.init()[which(df.lp.init()$Variety == v & 
                         df.lp.init()$Irrigation == i & 
                         df.lp.init()$Sowing == s & 
                         df.lp.init()$Date == d &
                         df.lp.init()$PlantNb %in% p &
                         df.lp.init()$Leafrank %in% l),]
    } else if(a == "Inverse") {
      for(n in unique(df.tmp[which(df.tmp$Variety==v & df.tmp$Irrigation == i & 
                                   df.tmp$Sowing==s & df.tmp$Date == d &
                                   df.tmp$PlantNb %in% p), "PlantNb"])) {
        df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i & 
                     df.tmp$Sowing == s & df.tmp$Date == d &
                     df.tmp$PlantNb == n),] <-
        df.lp.init()[which(df.lp.init()$Variety == v & 
                           df.lp.init()$Irrigation == i & 
                           df.lp.init()$Sowing == s & 
                           df.lp.init()$Date == d & 
                           df.lp.init()$PlantNb == n),]
      }
    }
    
    df.tmp <- updateLeafarea(df.tmp, v, i, s, d)
  }
  df.changes(df.changes()[-input$changes.df_rows_selected,])
  df.lp(df.tmp)
})

# Outputs

output$downloadData <- downloadHandler(
  filename = function() {
    "leafprofile.csv"
  },
  content = function(file) {
    df.tmp <- df.lp.init()
    colnames(df.tmp) <- col.user()
    write.csv(df.tmp, file, row.names = FALSE)
  }
)

output$downloadHistory <- downloadHandler(
  filename = function() {
    "leafprofile_history.csv"
  },
  content = function(file) {
    write.csv(df.changes.f(), file, row.names = FALSE)
  }
)

output$downloadPlots <- downloadHandler(
  filename = function() {
    "leafprofilePlots.pdf"
  },
  content = function(file) {
    pdf(file)
    for(v in unique(df.lp.init()$Variety)) {
      df.tmp <- df.lp.init()[which(df.lp.init()$Variety == v),]
      plt <- ggplot(df.tmp, aes(x = Leafrank, y = Medianarea, 
                                col = Date)) +
        geom_line() + geom_point() + 
        facet_grid(Sowing ~ Irrigation) + 
        ggtitle(v)
      suppressWarnings(print(plt))
    }
    dev.off()
  }
)