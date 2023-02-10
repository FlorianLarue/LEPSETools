# Variables
df.lp.init <- reactiveVal()
df.lp.init.o <- reactiveVal()
df.lp <- reactiveVal()
df.changes <- reactiveVal(data.frame())
df.changes.f <- reactiveVal(data.frame())

# Functions
updateHistory <- function(v, s, i, d, r, a) {
  df.changes(rbind(df.changes(), data.frame(Variety = v, Sowing = s, 
                                            Irrigation = i, Date = d,
                                            Leafrank = r, Action = a)))
}

# Listeners
listen.lp.sel <- reactive({
  list(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
})

# Observers
observeEvent(input$leafprofiledf,{
  df.input <- data.frame(fread(input$leafprofiledf$datapath))
  df.input$sel <- 16
  df.lp.init(df.input)
  df.lp.init.o(df.input)
})

observeEvent(listen.lp.sel(), {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.lp(df.lp.init()[which(df.lp.init()$Variety == input$leafprofilevar &
                             df.lp.init()$Irrigation == input$leafprofileirrig &
                             df.lp.init()$Semis == input$leafprofilesemis),])
})

observeEvent(input$plot_click, {
  df.tmp <- nearPoints(df.lp(), input$plot_click, allRows=TRUE)
  for(v in unique(df.tmp$Variety)) {
    for(i in unique(df.tmp$Irrigation)) {
      for(s in unique(df.tmp$Semis)) {
        for(d in unique(df.tmp$Date_de_plv)) {
          if(sum(df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i &
                              df.tmp$Semis == s & df.tmp$Date_de_plv == d),
                        "selected_"],na.rm=T) > 0) {
            if(df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i &
                            df.tmp$Semis == s & df.tmp$Date_de_plv == d),
                      "sel"][1] == 13) {
              df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i &
                             df.tmp$Semis == s & df.tmp$Date_de_plv == d),
                     "sel"] <- 16
            } else {
              df.tmp[which(df.tmp$Variety == v & df.tmp$Irrigation == i &
                             df.tmp$Semis == s & df.tmp$Date_de_plv == d),
                     "sel"] <- 13
            }
          }
        }
      }
    }
  }
  
  output$lp.selRank <- renderUI({
    div(selectizeInput(inputId = "leafprofilerank", 
                       label = "Select a leaf rank",
                       width = "25%",
                       choices = df.tmp[which(df.tmp$sel == 13 & 
                                                !is.na(df.tmp$areal) &
                                                !is.na(df.tmp$leafrank)),
                                        "leafrank"]),
        actionButton("delrank", label="Delete rank", icon=icon("trash")))
  })
  
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
                  i, "all", "Left shift")
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
                  i, "all", "Right shift")
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
                  i, "all", "Inverse")
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
                         "Irrigation"][1],
                  i, as.character(input$leafprofilerank), "Delete")
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
      guides(shape = "none") + 
      theme(legend.position="bottom", legend.title = element_blank(),
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
  df.changes(data.frame())
  df.changes.f(data.frame())
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