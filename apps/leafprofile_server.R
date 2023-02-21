# Variables
df.lp.init <- reactiveVal()
df.lp.init.o <- reactiveVal()
df.lp <- reactiveVal()
df.changes <- reactiveVal(data.frame())
df.changes.f <- reactiveVal(data.frame())
lp.var.c <- reactiveVal()
lp.irrig.c <- reactiveVal()
lp.semis.c <- reactiveVal()
date.sel <- reactiveVal(NA)
date.sel.p <- reactiveVal(NA)

# Functions
updateHistory <- function(v, s, i, d, p, r, a) {
  df.changes(rbind(df.changes(), data.frame(Variety = v, Sowing = s, 
                                            Irrigation = i, Date = d,
                                            Plantnumber = p, Leafrank = r, 
                                            Action = a)))
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
  df.lp.init(df.input)
  df.lp.init.o(df.input)
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
                             df.lp.init()$Semis == input$leafprofilesemis),])
})

observeEvent(input$plot_click, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  
  # Extract the selected data
  df.tmp <- nearPoints(df.lp(), input$plot_click, allRows=TRUE)
  df.tmp.sel <- df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                               df.tmp$Irrigation == input$leafprofileirrig &
                               df.tmp$Semis == input$leafprofilesemis),]
  # Change the status of selected data (identify in the "sel" column)
  for(d in unique(df.tmp.sel$Date_de_plv)) {
    if(sum(df.tmp.sel[which(df.tmp.sel$Date_de_plv == d),
                      "selected_"],na.rm=T) > 0) {
      if(df.tmp.sel[which(df.tmp.sel$Date_de_plv == d),"sel"][1] == 13) {
        df.tmp.sel[which(df.tmp.sel$Date_de_plv == d),"sel"] <- 16
        df.tmp.sel$sel.p <- 16
        if(13 %in% df.tmp.sel$sel) {
          if(d == date.sel()) {
            date.sel(date.sel.p())
          }
        } else {
          date.sel(NA)
        }
      } else {
        if(13 %in% df.tmp.sel$sel) {
          date.sel.p(date.sel())
        } else {
          date.sel.p(NA)
        }
        df.tmp.sel[which(df.tmp.sel$Date_de_plv == d),"sel"] <- 13
        date.sel(d)
      }
    }
  }
  df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                 df.tmp$Irrigation == input$leafprofileirrig &
                 df.tmp$Semis == input$leafprofilesemis),] <- df.tmp.sel
  
  # Create output to allow for rank selection and deletion
  output$lp.selRank <- renderUI({
    div(selectizeInput(inputId = "leafprofilerank", 
                       label = "Select a leaf rank",
                       width = "100%",
                       choices = df.tmp[which(df.tmp$sel == 13 & 
                                                !is.na(df.tmp$areal) &
                                                !is.na(df.tmp$leafrank)),
                                        "leafrank"]),
        actionButton("delrank", label="Delete rank", icon=icon("trash")))
  })
  
  # Replace data in original dataframe
  df.tmp <- df.tmp[,-c(ncol(df.tmp))]
  df.lp(df.tmp)
  
  # Create output for per-plant plot
  if(!is.na(date.sel())) {
    output$pp.plot <- renderPlot({
      df.pp <- df.lp()
      df.pp <- df.pp[which(df.pp$Variety == input$leafprofilevar & 
                             df.pp$Irrigation == input$leafprofileirrig &
                             df.pp$Semis == input$leafprofilesemis &
                             df.pp$Date_de_plv == date.sel()),]
      df.tmp2 <- df.pp %>%
        summarise(nbmax = mean(nbmaxleaves, na.rm=T))
      ggplot(df.pp, aes(x = leafrank, y = areal, col = as.factor(N_Plante))) + 
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
                                                  !is.na(df.tmp$areal) &
                                                  !is.na(df.tmp$leafrank)),
                                          "N_Plante"]),
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
                               df.tmp$Semis == input$leafprofilesemis &
                               df.tmp$Date_de_plv == date.sel()),]
  
  # Change the status of selected data (identify in the "sel" column)
  for(p in unique(df.tmp.sel$N_Plante)) {
    if(sum(df.tmp.sel[which(df.tmp.sel$N_Plante == p),
                      "selected_"],na.rm=T) > 0) {
      if(df.tmp.sel[which(df.tmp.sel$N_Plante == p),"sel.p"][1] == 13) {
        df.tmp.sel[which(df.tmp.sel$N_Plante == p),"sel.p"] <- 16
      } else {
        df.tmp.sel[which(df.tmp.sel$N_Plante == p),"sel.p"] <- 13
      }
    }
  }
  
  df.tmp[which(df.tmp$Variety == input$leafprofilevar &
                 df.tmp$Irrigation == input$leafprofileirrig &
                 df.tmp$Semis == input$leafprofilesemis &
                 df.tmp$Date_de_plv == date.sel()),] <- df.tmp.sel
  
  # Replace data in original dataframe
  df.tmp <- df.tmp[,-c(ncol(df.tmp))]
  df.lp(df.tmp)
})

observeEvent(input$leftshift, {
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"leafrank"] <-  
    df.tmp[which(df.tmp$sel == 13),"leafrank"] - 1
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date_de_plv"])) {
    updateHistory(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Variety"][1], 
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Semis"][1],
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Irrigation"][1],
                  i, "all", "all", "Left shift")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(input$rightshift, {
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"leafrank"] <-  
    df.tmp[which(df.tmp$sel == 13),"leafrank"] + 1
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date_de_plv"])) {
    updateHistory(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Variety"][1], 
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Semis"][1],
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Irrigation"][1],
                  i, "all", "all","Right shift")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(input$inverse, {
  shinyalert("Be carefull, this will reverse the current order,
             without considering max number of leaves. This will also rerun the
             computation of mean, median and sd area. You can revert this change
             in the history panel below.", type = "warning")
  df.tmp <- df.lp()
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date_de_plv"])) {
    for(j in unique(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i), 
                           "N_Plante"])) {
      df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i &
                     df.tmp$N_Plante == j), "leafrank"] <-
        rev(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                           df.tmp$N_Plante == j), 
                   "leafrank"])
    }
    for(k in unique(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i), 
                           "leafrank"])) {
      df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                     df.tmp$leafrank == k), "meanareal"] <- 
        mean(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                            df.tmp$leafrank == k), "areal"], na.rm=T)
      df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                     df.tmp$leafrank == k), "sdareal"] <- 
        sd(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                            df.tmp$leafrank == k), "areal"], na.rm=T)
      df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                     df.tmp$leafrank == k), "medianareal"] <- 
        median(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i & 
                              df.tmp$leafrank == k), "areal"], na.rm=T)
    }
  }
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date_de_plv"])) {
    updateHistory(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Variety"][1], 
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Semis"][1],
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Irrigation"][1],
                  i, "all", "all", "Inverse")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(input$delrank, {
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13 & df.tmp$leafrank == input$leafprofilerank),
         c("medianareal","meanareal","sdareal","areal")] <- c(NA,NA,NA,NA)
  
  for(i in unique(df.tmp[which(df.tmp$sel == 13),"Date_de_plv"])) {
    updateHistory(df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Variety"][1], 
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Semis"][1],
                  df.tmp[which(df.tmp$sel == 13 & df.tmp$Date_de_plv == i),
                         "Irrigation"][1], i, "all",
                  as.character(input$leafprofilerank), "Delete")
  }
  
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  output$lp.selRank <- NULL
})

observeEvent(df.lp(), {
  output$lp.df <- DT::renderDataTable({
    DT::datatable(
      df.lp()[,c("Date_de_plv","N_Plante","N_Feuille","areal","medianareal",
                 "meanareal","sdareal","leafrank","nbmaxleaves")], 
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
    df.tmp$Date_de_plv <- as.Date(df.tmp$Date_de_plv, "%d/%m/%Y")
    df.tmp$Date_de_plv <- as.factor(df.tmp$Date_de_plv)
    df.tmp2 <- df.tmp %>%
      summarise(nbmax = mean(nbmaxleaves, na.rm=T))
    
    ggplot(df.lp(), aes(x = leafrank, y = medianareal, 
                        col = Date_de_plv)) +
      geom_point(shape=df.lp()$sel) + geom_line() +
      xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
      scale_x_continuous(breaks = function(x)
        unique(floor(pretty(seq(0, (max(df.lp()$leafrank,
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
  df.org <- df.lp.init()
  df.org[which(df.org$Variety == input$leafprofilevar &
                 df.org$Irrigation == input$leafprofileirrig &
                 df.org$Semis == input$leafprofilesemis),] <- df.lp()
  df.lp.init(df.org)
  df.changes.f(df.changes())
  updateSelectizeInput(session, "leafprofilevar", selected = lp.var.c())
  updateSelectizeInput(session, "leafprofileirrig", selected = lp.irrig.c())
  updateSelectizeInput(session, "leafprofilesemis", selected = lp.semis.c())
  
})

observeEvent(input$reset, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.lp(df.lp.init()[which(df.lp.init()$Variety == input$leafprofilevar &
                           df.lp.init()$Irrigation == input$leafprofileirrig &
                           df.lp.init()$Semis == input$leafprofilesemis),])
  df.changes(df.changes.f())
})

observeEvent(input$resetall, {
  df.lp.init(df.lp.init.o())
  output$lp.df <- NULL
  output$lp.plot <- NULL
  output$pp.plot <- NULL
  df.changes(data.frame())
  df.changes.f(data.frame())
})

observeEvent(input$nextS, {
  req(input$leafprofiledf, input$leafprofilevar, input$leafprofileirrig, 
      input$leafprofilesemis) 
  if(input$leafprofilesemis == "S2") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", selected = "S1")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", selected = "S2")
    if(input$leafprofileirrig == "WD") { 
      updateSelectizeInput(session, "leafprofileirrig", selected = "WW")
    } else { # case 3 : change irrig, sowing and var
      currentVar <- match(input$leafprofilevar, unique(df.lp.init()$Variety))
      updateSelectizeInput(session, "leafprofilevar", 
        selected = unique(df.lp.init()$Variety)[currentVar + 1])
      updateSelectizeInput(session, "leafprofileirrig", selected = "WD")
    }
  }
})

observeEvent(input$previousS, {
  req(input$leafprofiledf, input$leafprofilevar, input$leafprofileirrig, 
      input$leafprofilesemis) 
  if(input$leafprofilesemis == "S1") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", selected = "S2")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", selected = "S1")
    if(input$leafprofileirrig == "WW") { 
      updateSelectizeInput(session, "leafprofileirrig", selected = "WD")
    } else { # case 3 : change irrig, sowing and var  
      currentVar <- match(input$leafprofilevar, unique(df.lp.init()$Variety))
      updateSelectizeInput(session, "leafprofilevar", 
        selected = unique(df.lp.init()$Variety)[currentVar + - 1])
      updateSelectizeInput(session, "leafprofileirrig", selected = "WW")
    }
  }
})

observeEvent(input$revert, {
  req(input$changes.df_rows_selected)
  df.tmp <- df.lp()
  for(i in input$changes.df_rows_selected) {
    if(df.changes()[i,"Action"] == "Left shift") {
      df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                   df.tmp$Semis == df.changes()[i,"Sowing"] &
                   df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                   df.tmp$Date_de_plv == df.changes()[i,"Date"]),"leafrank"] <-
      df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                   df.tmp$Semis == df.changes()[i,"Sowing"] &
                   df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                   df.tmp$Date_de_plv == df.changes()[i,"Date"]),"leafrank"] + 1
    } else if(df.changes()[i,"Action"] == "Right shift") {
      df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                   df.tmp$Semis == df.changes()[i,"Sowing"] &
                   df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                   df.tmp$Date_de_plv == df.changes()[i,"Date"]),"leafrank"] <-
      df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                   df.tmp$Semis == df.changes()[i,"Sowing"] &
                   df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                   df.tmp$Date_de_plv == df.changes()[i,"Date"]),"leafrank"] -1
    } else if(df.changes()[i,"Action"] == "Delete") {
      df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                   df.tmp$Semis == df.changes()[i,"Sowing"] &
                   df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                   df.tmp$Date_de_plv == df.changes()[i,"Date"] & 
                   df.tmp$leafrank == df.changes()[i,"Leafrank"]),] <-
      df.lp.init()[which(df.lp.init()$Variety == df.changes()[i,"Variety"] &
                       df.lp.init()$Semis == df.changes()[i,"Sowing"] &
                       df.lp.init()$Irrigation == df.changes()[i,"Irrigation"] &
                       df.lp.init()$Date_de_plv == df.changes()[i,"Date"] & 
                       df.lp.init()$leafrank == df.changes()[i,"Leafrank"]),]
    } else if(df.changes()[i,"Action"] == "Inverse") {
      for(j in unique(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                     df.tmp$Semis == df.changes()[i,"Sowing"] &
                     df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                     df.tmp$Date_de_plv == df.changes()[i,"Date"]),
                     "N_Plante"])) {
        df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                       df.tmp$Semis == df.changes()[i,"Sowing"] &
                       df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                       df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                       df.tmp$N_Plante == j),"leafrank"] <- 
          rev(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                             df.tmp$Semis == df.changes()[i,"Sowing"] &
                             df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                             df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                             df.tmp$N_Plante == j),"leafrank"])
      }
      for(k in unique(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                      df.tmp$Semis == df.changes()[i,"Sowing"] &
                      df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                      df.tmp$Date_de_plv == df.changes()[i,"Date"]),
                      "leafrank"])) {
        df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                       df.tmp$Semis == df.changes()[i,"Sowing"] &
                       df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                       df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                       df.tmp$leafrank == k), "meanareal"] <-
          mean(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                            df.tmp$Semis == df.changes()[i,"Sowing"] &
                            df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                            df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                            df.tmp$leafrank == k), "areal"], na.rm=T)
        df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                       df.tmp$Semis == df.changes()[i,"Sowing"] &
                       df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                       df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                       df.tmp$leafrank == k), "medianareal"] <-
          median(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                  df.tmp$Semis == df.changes()[i,"Sowing"] &
                  df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                  df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                  df.tmp$leafrank == k), "areal"], na.rm=T)
        df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                       df.tmp$Semis == df.changes()[i,"Sowing"] &
                       df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                       df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                       df.tmp$leafrank == k), "sdareal"] <-
          sd(df.tmp[which(df.tmp$Variety == df.changes()[i,"Variety"] &
                          df.tmp$Semis == df.changes()[i,"Sowing"] &
                          df.tmp$Irrigation == df.changes()[i,"Irrigation"] &
                          df.tmp$Date_de_plv == df.changes()[i,"Date"] &
                          df.tmp$leafrank == k), "areal"], na.rm=T)
        
      }
    }
  }
  df.changes(df.changes()[-input$changes.df_rows_selected,])
  df.lp(df.tmp)
})

# Outputs

output$lp.selVar <- renderUI({
  selectizeInput(inputId = "leafprofilevar", label = "Select a variety",
                 choices = df.lp.init()$Variety)
})

output$lp.selIrrig <- renderUI({
  selectizeInput(inputId = "leafprofileirrig", label = "Select a irrigation",
                 choices = df.lp.init()$Irrigation)
})

output$lp.selSemis <- renderUI({
  selectizeInput(inputId = "leafprofilesemis", label = "Select a sowing",
                 choices = df.lp.init()$Semis)
})

output$downloadData <- downloadHandler(
  filename = function() {
    "leafprofile.csv"
  },
  content = function(file) {
    write.csv(df.lp(), file, row.names = FALSE)
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