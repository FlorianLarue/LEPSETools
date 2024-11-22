# Variables
df.lp.init <- reactiveVal() #initial dataframe that will be modified when changing leaf profiles
df.lp.init.o <- reactiveVal() #initial dataframe that will remain unchanged, used if everything needs to be reset
df.lp <- reactiveVal() #active dataframe for a given situation (Variety x Modalities)
df.changes <- reactiveVal(data.frame()) #dataframe used to track all changes made
df.sq <- reactiveVal(data.frame()) #dataframe of SQ parameter values, appended for each situation (Variety x Modalities)
df.changes.f <- reactiveVal(data.frame()) #snapshot of df.changes used in case of partial reset, used to save to file
df.lp.init.sq <- reactiveVal() #dataframe to store df.lp.init with modified point shapes according to SQ parameters
df.lp.sq <- reactiveVal(data.frame()) #dataframe to store df.lp with modified point shapes according to SQ parameters
lp.var.c <- reactiveVal(); lp.irrig.c <- reactiveVal(); lp.semis.c <- reactiveVal()
date.sel <- reactiveVal(NA); date.sel.p <- reactiveVal(NA)
max.plants <- reactiveVal(0); max.leaves <- reactiveVal(0)
sq.params <- reactiveValues(AreaPL = NA, AreaSL = NA, NLL = NA, RatioFLPL = NA, NLL_lm = NA, warn=0)
warn.sqpa <- reactiveVal()
nll.comp <- reactiveVal(FALSE); auto.sq <- reactiveVal(FALSE)
col.names <- reactiveVal(
  c("Variety","Sowing","Irrigation","Date","Leafrank","Leafarea","Medianarea",
    "Meanarea","SDarea","PlantNb","LeafNb","MaxLeaves", "Ligulated")
)
col.user <- reactiveVal()
path.save <- reactiveVal()
volumes <- getVolumes()()

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
      median(df.tmp[which(df.tmp$Leafrank == l & df.tmp$Ligulated == "x"), 
                    "Leafarea"], na.rm=T)
    df.tmp[which(df.tmp$Leafrank == l),"Meanarea"] <- 
      mean(df.tmp[which(df.tmp$Leafrank == l & df.tmp$Ligulated == "x"), 
                  "Leafarea"], na.rm=T)
    df.tmp[which(df.tmp$Leafrank == l),"SDarea"] <- 
      sd(df.tmp[which(df.tmp$Leafrank == l & df.tmp$Ligulated == "x"), 
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
observe({
  shinyDirChoose(input, id="savePath", roots=volumes, session = session)
  tmp.path <- parseDirPath(roots=volumes, input$savePath)
  if(!identical(tmp.path, character(0))) {
    path.save(paste0(tmp.path,"/", Sys.Date()))
  }
})
observeEvent(input$leafprofiledf,{
  df.input <- data.frame(fread(input$leafprofiledf$datapath))
  df.input$sel <- 16
  df.input$sel.p <- 16
  df.input$iso <- 1
  
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
    df.input <- df.input[order(df.input$Variety, df.input$Irrigation, 
                               df.input$Sowing),]
  }
  
  df.lp.init(df.input)
  df.lp.init.o(df.input)
})
observeEvent(input$cn.Submit, {
  req(input$leafprofiledf)
  df.tmp <- df.lp.init()
  col.user(colnames(df.tmp))
  for(n in col.names()) {
    names(df.tmp)[names(df.tmp) == input[[paste0("cn.", n)]]] <- n
  }
  df.tmp <- df.tmp[order(df.tmp$Variety, df.tmp$Irrigation, df.tmp$Sowing, df.tmp$Date, df.tmp$PlantNb, df.tmp$Leafrank),]
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
  df.lp.sq(data.frame())
  sq.params$AreaPL <- NA
  sq.params$AreaSL <- NA
  sq.params$NLL <- NA
  sq.params$RatioFLPL <- NA
  sq.params$NLL_lm <- NA
  output$sq.plot <- NULL
})
observeEvent(input$plot_click, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  if(!input$sqpar) {
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
      if(input$ppplot) {
        output$pp.plot <- renderPlot({
          df.pp <- df.lp()
          df.pp <- df.pp[which(df.pp$Variety == input$leafprofilevar & 
                                 df.pp$Irrigation == input$leafprofileirrig &
                                 df.pp$Sowing == input$leafprofilesemis &
                                 df.pp$Date == date.sel() & 
                                 df.pp$iso == 1 & df.pp$Ligulated == "x"),]
          df.tmp2 <- df.pp %>%
            summarise(nbmax = mean(MaxLeaves, na.rm=T))
          ggplot(df.pp, aes(x = Leafrank, y = Leafarea, 
                            col = as.factor(PlantNb))) + 
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
                             label = "Select a plant or leaf number",
                             width = "100%",
                             choices = sort(unique(unlist(
                               df.tmp[which(df.tmp$Date == date.sel() & 
                                              !is.na(df.tmp$Leafarea) &
                                              !is.na(df.tmp$Leafrank)),
                                      c("PlantNb","Leafrank")])))),
              actionButton("delplant", label="Delete plant", icon=icon("trash")),
              actionButton("delleaf", label="Delete leaf", icon=icon("trash")))
        })
      }
    } else {
      output$pp.plot <- NULL
    }
  } else {
    # (De)Select clicked points
    df.tmp <- nearPoints(df.lp(), input$plot_click, allRows=TRUE)
    for(m in unique(df.tmp[which(df.tmp$selected_ == TRUE), "Meanarea"])) {
      if(unique(df.tmp[which(df.tmp$Meanarea == m),"sel"]) == 16) {
        df.tmp[which(df.tmp$Meanarea == m),"sel"] <- 13
      } else {
        df.tmp[which(df.tmp$Meanarea == m),"sel"] <- 16
      }
    }
    # Replace data in original dataframe
    df.tmp <- df.tmp[,-c(ncol(df.tmp))]
    df.lp(df.tmp)
  }
})
observeEvent(input$sqpar.sel, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  df.lp(df.tmp)
  nll.comp(FALSE)
  auto.sq(FALSE)
  updateCheckboxInput(session, "sqparA", value = FALSE)
})
observeEvent(input$sqpar, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  for(i in 1:nrow(df.tmp)) {
    if(!is.na(df.tmp[i, "Leafrank"])) {
      df.tmp[i, "ConsensusArea"] <- mean(unique(df.tmp[which(df.tmp$Leafrank == df.tmp[i, "Leafrank"]), "Medianarea"]), na.rm=T)
    }
  }
  df.lp(df.tmp)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  output$pp.plot <- NULL
})
observeEvent(input$sqparA, {
  if(input$sqparA == TRUE) {
    auto.sq(TRUE)
    req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
    df.tmp <- df.lp()
    df.tmp$SQpar <- "Not Used"
    for(i in 1:nrow(df.tmp)) {
      if(!is.na(df.tmp[i, "Leafrank"])) {
        df.tmp[i, "ConsensusArea"] <- mean(unique(df.tmp[which(df.tmp$Leafrank == df.tmp[i, "Leafrank"]), "Medianarea"]), na.rm=T)
      }
    }
    penultRank <-  unique(df.tmp[which(df.tmp$Leafrank == max(df.tmp[which(!is.na(df.tmp$ConsensusArea)), "Leafrank"], na.rm=T) - 1), "Leafrank"], na.rm=T)
    
    # Change symbols for points and compute params
    df.tmp[which(df.tmp$Leafrank == penultRank), "SQpar"] <- "AreaPL/RatioFLPL"
    df.tmp[which(df.tmp$Leafrank == (penultRank+1)), "SQpar"] <- "AreaPL/RatioFLPL"
    sq.params$AreaPL <- unique(df.tmp[which(df.tmp$Leafrank == penultRank), "ConsensusArea"], na.rm=T)
    sq.params$RatioFLPL <- unique(df.tmp[which(df.tmp$Leafrank == max(df.tmp$Leafrank, na.rm=T)), "ConsensusArea"], na.rm=T) /
      unique(df.tmp[which(df.tmp$Leafrank == penultRank), "ConsensusArea"], na.rm=T)
    if(unique(df.tmp[which(df.tmp$Leafrank == penultRank), "ConsensusArea"], na.rm=T) < 
       unique(df.tmp[which(df.tmp$Leafrank == (penultRank+1)), "ConsensusArea"], na.rm=T)) {
      sq.params$warn <- 1
    }
    
    nbmax <- as.numeric(df.tmp %>%
                          summarise(nbmax = mean(MaxLeaves, na.rm=T)))
    nll_ranks <- c(penultRank, penultRank-1, penultRank-2, penultRank-3, penultRank-4)
    df.tmp[which(df.tmp$Leafrank %in% nll_ranks),"SQpar"] <- "NLL"
    lm.res <- lm(df.tmp[which(df.tmp$Leafrank %in% nll_ranks), "ConsensusArea"] ~ df.tmp[which(df.tmp$Leafrank %in% nll_ranks),"Leafrank"])
    sq.params$NLL <- nbmax+coef(lm.res)[1]/coef(lm.res)[2]
    sq.params$NLL_lm <- lm.res
    df.tmp$inter_nll <- coef(sq.params$NLL_lm)[1]
    df.tmp$slope_nll <- coef(sq.params$NLL_lm)[2]
    if(!is.nan(mean(unique(df.tmp[which(df.tmp$Leafrank < -(coef(lm.res)[1]/coef(lm.res)[2])), "ConsensusArea"], na.rm=T), na.rm=T))) {
      df.tmp[which(df.tmp$Leafrank < -(coef(lm.res)[1]/coef(lm.res)[2])),"SQpar"] <- "AreaSL"
      sq.params$AreaSL <- mean(unique(df.tmp[which(df.tmp$Leafrank < -(coef(lm.res)[1]/coef(lm.res)[2])), "ConsensusArea"], na.rm=T), na.rm=T)
    } else {
      warn.sqpa(paste0(warn.sqpa(),"\nAreaSL was not computed \n as no rank <= nbmax-NLL was found"))
    }
    
    df.lp.sq(df.tmp)
    df.lp(df.tmp)
    
    df.tmp.init <- df.lp.init()
    if(!("SQpar" %in% colnames(df.tmp.init))) {
      df.tmp.init$SQpar <- "Not Used"
      df.tmp.init$ConsensusArea <- NA 
      df.tmp.init$inter_nll <- NA
      df.tmp.init$slope_nll <- NA
      df.tmp.init$warn <- 0
    }
    df.tmp.init[which(df.tmp.init$Variety == input$leafprofilevar &
                        df.tmp.init$Irrigation == input$leafprofileirrig &
                        df.tmp.init$Sowing == input$leafprofilesemis),] <- df.tmp
    df.lp.init(df.tmp.init)
  }
})
observeEvent(input$sqpar.val, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp$SQpar <- "Not Used"
  nbmax <- as.numeric(df.tmp %>%
    summarise(nbmax = mean(MaxLeaves, na.rm=T)))
  if(nrow(df.tmp[which(df.tmp$sel == 13),]) == 0) {
    shinyalert("Error", 
               paste0("Please select points on the plot to compute SQ parameters"),
               type="error")
  } else {
    mod.rows <- rownames(df.tmp[which(df.tmp$sel == 13),])
    switch(input$sqpar.sel, 
      "AreaPL" = {
        sq.params$AreaPL <- mean(df.tmp[which(df.tmp$sel == 13),"ConsensusArea"], na.rm=T)
        df.tmp[which(df.tmp$sel == 13),"SQpar"] <- "AreaPL/RatioFLPL"
      },
      "AreaSL" = {
        sq.params$AreaSL <- mean(df.tmp[which(df.tmp$sel == 13),"ConsensusArea"], na.rm=T)
        df.tmp[which(df.tmp$sel == 13),"SQpar"] <- "AreaSL"
      },
      "NLL" = {
        lm.res <- lm(df.tmp[which(df.tmp$sel == 13),"ConsensusArea"] ~ df.tmp[which(df.tmp$sel == 13),"Leafrank"])
        sq.params$NLL <- nbmax+coef(lm.res)[1]/coef(lm.res)[2]
        sq.params$NLL_lm <- lm.res
        nll.comp(TRUE)
        df.tmp[which(df.tmp$sel == 13),"SQpar"] <- "NLL"
      },
      "RatioFLPL" = {
        penultRank <- min(unique(df.tmp[which(df.tmp$sel == 13), "Leafrank"]), na.rm=T)
        flagRank <- max(unique(df.tmp[which(df.tmp$sel == 13), "Leafrank"]), na.rm=T)
        sq.params$RatioFLPL <- unique(df.tmp[which(df.tmp$sel == 13 & 
          df.tmp$Leafrank == flagRank),"ConsensusArea"])/
          unique(df.tmp[which(df.tmp$sel == 13 & df.tmp$Leafrank == penultRank),
          "ConsensusArea"])
        df.tmp[which(df.tmp$sel == 13),"SQpar"] <- "AreaPL/RatioFLPL"
      }
    )
    if(length(df.lp.sq()) == 0) {
      df.lp.sq(df.tmp)
    } else {
      df.tmp2 <- df.lp.sq()
      df.tmp2[which(rownames(df.tmp2) %in% mod.rows),] <- df.tmp[which(df.tmp$sel %in% c(3,6,13,15)),]
      df.lp.sq(df.tmp2)
    }
  }
})
observeEvent(input$sqpar.save, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  id <- paste(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis, sep="_")
  df.tmp <- df.sq()
  nll.comp(FALSE)
  if(nrow(df.tmp) == 0) {
    df.tmp[1, "id"] <- id
    df.tmp[1, "AreaPL"] <- sq.params$AreaPL
    df.tmp[1, "AreaSL"] <- sq.params$AreaSL
    df.tmp[1, "NLL"] <- sq.params$NLL
    df.tmp[1, "RatioFLPL"] <- sq.params$RatioFLPL
    df.tmp[1, "Warn"] <- sq.params$warn
  } else {
    if(id %in% df.tmp$id) {
      df.tmp[which(df.tmp$id == id), "AreaPL"] <- sq.params$AreaPL
      df.tmp[which(df.tmp$id == id), "AreaSL"] <- sq.params$AreaSL
      df.tmp[which(df.tmp$id == id), "NLL"] <- sq.params$NLL
      df.tmp[which(df.tmp$id == id), "RatioFLPL"] <- sq.params$RatioFLPL
      df.tmp[which(df.tmp$id == id), "Warn"] <- sq.params$warn
    } else {
      df.tmp[nrow(df.tmp)+1, "id"] <- id
      df.tmp[nrow(df.tmp), "AreaPL"] <- sq.params$AreaPL
      df.tmp[nrow(df.tmp), "AreaSL"] <- sq.params$AreaSL
      df.tmp[nrow(df.tmp), "NLL"] <- sq.params$NLL
      df.tmp[nrow(df.tmp), "RatioFLPL"] <- sq.params$RatioFLPL
      df.tmp[nrow(df.tmp), "Warn"] <- sq.params$warn
    }
  }
  df.sq(df.tmp)
  
  output$sq.plot <- renderPlot({
    nameSQ <- c("AreaPL/RatioFLPL","AreaSL","NLL","Not Used")
    idSQ <- c(13,6,15,16)
    df.tmp <- df.lp.sq()
    df.tmp$Date <- as.Date(df.tmp$Date, "%d/%m/%Y")
    df.tmp$Date <- as.factor(df.tmp$Date)
    df.tmp2 <- df.tmp %>%
      summarise(nbmax = mean(MaxLeaves, na.rm=T))
    if(input$meanmedian == "Median") {
      plt <- ggplot(df.lp.sq(), aes(x = Leafrank, y = Medianarea, col = Date))
      if(input$sduse) {
        plt <- plt + geom_errorbar(aes(ymin=Medianarea-SDarea, 
                                       ymax=Medianarea+SDarea))
      }
    } else {
      plt <- ggplot(df.lp.sq(), aes(x = Leafrank, y = Meanarea, col = Date))
      if(input$sduse) {
        plt <- plt + geom_errorbar(aes(ymin=Meanarea-SDarea, 
                                       ymax=Meanarea+SDarea))
      }
    }
    plt <- plt + geom_point(aes(shape=as.factor(SQpar))) + geom_line() +
      xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
      scale_x_continuous(breaks = function(x)
        unique(floor(pretty(seq(0, (max(df.lp()$Leafrank,
                                        na.rm=T)+1)*1.1))))) +
      geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                 linetype = "dashed", col="red") +
      scale_shape_manual(name = "SQ Params", labels = levels(as.factor((df.lp.sq()$SQpar))), values = idSQ[match(levels(as.factor((df.lp.sq()$SQpar))), nameSQ)]) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_blank(),
            legend.text = element_text(size=6), legend.margin = margin())
    # if(!is.na(sq.params$NLL)) {
    #   plt <- plt + geom_abline(intercept = coef(sq.params$NLL_lm)[1], slope = coef(sq.params$NLL_lm)[2])
    # }
    #if(auto.sq() ||) {
      plt <- plt + geom_line(aes(x = Leafrank, y = ConsensusArea), col="black", size=0.9)
    #}
    plt
  }, height = 500, res = 150)
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
  
  if(!input$keepsel) {
    df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
    output$lp.selRank <- NULL
    output$lp.selPlant <- NULL
    output$pp.plot <- NULL
  } else {
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
  }
  df.lp(df.tmp)
})
observeEvent(input$pp.leftshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),"Leafrank"] - 1
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig,
                           input$leafprofilesemis, date.sel())
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),
                         "PlantNb"])) {
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
  
  if(!input$keepsel) {
    df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
    output$lp.selRank <- NULL
    output$lp.selPlant <- NULL
    output$pp.plot <- NULL
  } else {
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
  }
  df.lp(df.tmp)
})
observeEvent(input$pp.rightshift, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),"Leafrank"] <-  
    df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),"Leafrank"] + 1
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig,
                           input$leafprofilesemis, date.sel())
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),
                         "PlantNb"])) {
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
    df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i),] <- df.tmp.d
    
    df.tmp.d <- df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i & df.tmp$Ligulated == "x"),]
    df.tmp.d <- updateLeafarea(df.tmp.d, input$leafprofilevar, 
                               input$leafprofileirrig, input$leafprofilesemis,i)
    df.tmp[which(df.tmp$sel == 13 & df.tmp$Date == i & 
                   df.tmp$Ligulated == "x"),] <- df.tmp.d
    
    updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                  input$leafprofileirrig, i, "all", "all", "Inverse")
  }
  
  if(!input$keepsel) {
    df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
    output$lp.selRank <- NULL
    output$lp.selPlant <- NULL
    output$pp.plot <- NULL
  } else {
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
  }
  df.lp(df.tmp)
})
observeEvent(input$pp.inverse, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis)
  df.tmp <- df.lp()
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Ligulated == "x" & 
                               df.tmp$Date == date.sel()), 
                         "PlantNb"])) {
    df.tmp[which(df.tmp$sel.p == 13 & df.tmp$PlantNb == i & 
                 df.tmp$Ligulated == "x" & df.tmp$Date == date.sel()), 
           "Leafrank"] <- rev(df.tmp[which(df.tmp$sel.p == 13 & 
                                           df.tmp$PlantNb == i & 
                                           df.tmp$Ligulated == "x"), 
                                     "Leafrank"])
  }
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig, 
                           input$leafprofilesemis, date.sel())
  
  for(i in unique(df.tmp[which(df.tmp$sel.p == 13 & df.tmp$Date == date.sel()),
                         "PlantNb"])) {
    updateHistory(input$leafprofilevar, input$leafprofilesemis,
                  input$leafprofileirrig, date.sel(), as.character(i), "all", 
                  "Inverse")
  }
  
  if(!input$keepsel) {
    df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
  } else {
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
  }
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
  
  if(!input$keepsel) {
    df.tmp[which(df.tmp$sel == 13),"sel"] <- 16
    output$lp.selRank <- NULL
    output$lp.selPlant <- NULL
    output$pp.plot <- NULL
  } else {
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
  }
  df.lp(df.tmp)
})
observeEvent(input$delplant, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis,
      input$leafprofileplant)
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$Date == date.sel() & 
                 df.tmp$PlantNb == input$leafprofileplant),
         c("Medianarea","Meanarea","SDarea","Leafarea")] <- c(NA,NA,NA,NA)
  
  df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig, 
                           input$leafprofilesemis, date.sel())
  updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                input$leafprofileirrig, date.sel(), 
                as.character(input$leafprofileplant), "all", "Delete")
  df.lp(df.tmp)
})
observeEvent(input$delleaf, {
  req(input$leafprofilevar, input$leafprofileirrig, input$leafprofilesemis,
      input$leafprofileplant)
  df.tmp <- df.lp()
  if(nrow(df.tmp[which(df.tmp$Date == date.sel() & df.tmp$sel.p == 13 &
                       df.tmp$Leafrank == input$leafprofileplant & 
                       !is.na(df.tmp$Leafarea)),]) > 0) {
    df.tmp[which(df.tmp$Date == date.sel() & df.tmp$sel.p == 13 &
                   df.tmp$Leafrank == input$leafprofileplant),
           c("Medianarea","Meanarea","SDarea","Leafarea")] <- c(NA,NA,NA,NA)
    df.tmp <- updateLeafarea(df.tmp, input$leafprofilevar, input$leafprofileirrig, 
                             input$leafprofilesemis, date.sel())
    for(p in unique(df.tmp[which(df.tmp$sel.p == 13),"PlantNb"])) {
      updateHistory(input$leafprofilevar, input$leafprofilesemis, 
                    input$leafprofileirrig, date.sel(), 
                    p, as.character(input$leafprofileplant), "Delete") 
    }
    df.lp(df.tmp)
  }
})
observeEvent(input$pp.isolate, {
  df.tmp <- df.lp()
  if(0 %in% df.tmp$iso) {
    df.tmp$iso <- 1
  } else {
    if(13 %in% df.tmp$sel.p) {
      df.tmp[which(df.tmp$sel.p == 16), "iso"] <- 0
    }
  }
  df.lp(df.tmp)
})
observeEvent(df.lp(), {
  output$lp.df <- DT::renderDataTable({
    DT::datatable(
      df.lp()[,c("Date","PlantNb","LeafNb","Leafarea","Medianarea",
                 "Meanarea","SDarea","Leafrank","MaxLeaves", "Ligulated")], 
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
    if(input$meanmedian == "Median") {
      plt <- ggplot(df.lp()[which(df.lp()$Ligulated == "x"),], aes(x = Leafrank, y = Medianarea, col = Date))
      if(input$sduse) {
        plt <- plt + geom_errorbar(aes(ymin=Medianarea-SDarea, 
                                       ymax=Medianarea+SDarea))
      }
    } else {
      plt <- ggplot(df.lp()[which(df.lp()$Ligulated == "x"),], aes(x = Leafrank, y = Meanarea, col = Date))
      if(input$sduse) {
        plt <- plt + geom_errorbar(aes(ymin=Meanarea-SDarea, 
                                       ymax=Meanarea+SDarea))
      }
    }
    plt <- plt + geom_point(shape=df.lp()[which(df.lp()$Ligulated == "x"),]$sel) + geom_line() +
      xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
      scale_x_continuous(breaks = function(x)
        unique(floor(pretty(seq(0, (max(df.lp()[which(df.lp()$Ligulated == "x"),]$Leafrank,
                                        na.rm=T)+1)*1.1))))) +
      geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                 linetype = "dashed", col="red") +
      guides(shape="none", col=guide_legend(nrow=2)) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size=6))
    if(nll.comp() == TRUE) {
      plt <- plt + geom_abline(intercept = coef(sq.params$NLL_lm)[1], slope = coef(sq.params$NLL_lm)[2])
    }
    if(input$sqparA || "ConsensusArea" %in% colnames(df.lp())) {
      plt <- plt + geom_line(aes(x = Leafrank, y = ConsensusArea), col="black", size=1.2)
    }
    plt
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
  df.tmp <- df.lp()
  df.tmp[which(df.tmp$sel == 13), "sel"] <- 16
  df.tmp[which(df.tmp$sel.p == 13), "sel"] <- 16
  df.lp(df.tmp)
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
  if(is.character(path.save()) && length(path.save() == 1L && 
                                         !is.na(path.save()))) {
    fwrite(df.lp.init(), paste0(path.save(), "_tmp_save_",
                                input$leafprofiledf$name))
    fwrite(df.changes.f(), paste0(path.save(), "_tmp_history_", 
                                  input$leafprofiledf$name))
  }
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
  nll.comp(FALSE)
  auto.sq(FALSE)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  warn.sqpa("")
  updateCheckboxInput(session, "sqparA", value = FALSE)
  if(input$leafprofilesemis == "S1") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S2")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S1")
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
  nll.comp(FALSE)
  auto.sq(FALSE)
  output$lp.selRank <- NULL
  output$lp.selPlant <- NULL
  warn.sqpa("")
  updateCheckboxInput(session, "sqparA", value = FALSE)
  
  if(input$leafprofilesemis == "S2") { # case 1 : change only sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S1")
  } else { # case 2 : change irrig and sowing
    updateSelectizeInput(session, "leafprofilesemis", 
                         choices = unique(df.lp.init.o()$Sowing), 
                         selected = "S2")
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
output$sqpar.text <- renderText({
  paste0("Computed SQ parameter values:", "\n", "\n",
        "AreaPL = ", sq.params$AreaPL, "\n",
        "AreaSL = ", sq.params$AreaSL, "\n",
        "NLL = ", sq.params$NLL, "\n",
        "RatioFLPL = ", sq.params$RatioFLPL)  
})
output$sqparA.text <- renderText({
  paste0("Warning messages:", "\n", "\n",
         warn.sqpa())  
})
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
    if(auto.sq() == FALSE) {
      for(v in unique(df.lp.init()$Variety)) {
        df.tmp <- df.lp.init()[which(df.lp.init()$Variety == v),]
        df.tmp2 <- df.tmp %>%
          group_by(Sowing) %>%
          summarise(nbmax = mean(MaxLeaves, na.rm=T))
        plt <- ggplot(df.tmp, aes(x = Leafrank, y = Medianarea, 
                                  col = Date)) +
          geom_line() + geom_point() + 
          geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                     linetype = "dashed", col="red") +
          facet_grid(Sowing ~ Irrigation) + 
          ggtitle(v)
        suppressWarnings(print(plt))
      }
    } else {
      idSQ <- c(13,6,15,16)
      nameSQ <- c("AreaPL/RatioFLPL","AreaSL","NLL","Not Used")
      for(v in unique(df.lp.init()$Variety)) {
        df.tmp <- df.lp.init()[which(df.lp.init()$Variety == v),]
        df.tmp2 <- df.tmp %>%
          group_by(Sowing) %>%
          summarise(nbmax = mean(MaxLeaves, na.rm=T))
        plt <- ggplot(df.tmp, aes(x = Leafrank, y = Medianarea, 
                                  col = Date)) +
          geom_point(aes(x = Leafrank, y = ConsensusArea, shape=as.factor(SQpar)), col="black") + geom_line() +
          xlab("Estimated Leaf rank") + ylab("Leaf area (cm²)") +
          scale_x_continuous(breaks = function(x)
            unique(floor(pretty(seq(0, (max(df.lp()$Leafrank,
                                            na.rm=T)+1)*1.1))))) +
          geom_vline(data=df.tmp2, aes(xintercept=nbmax),
                     linetype = "dashed", col="red") +
          scale_shape_manual(name = "SQ Params", labels = levels(as.factor((df.lp.init()$SQpar))), values = idSQ[match(levels(as.factor((df.lp.init()$SQpar))), nameSQ)]) +
          theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_blank(),
                legend.text = element_text(size=6), legend.margin = margin()) +
          geom_line(aes(x = Leafrank, y = ConsensusArea), col="black", size=0.5) +
          geom_abline(aes(intercept = inter_nll, slope = slope_nll), data = df.tmp, col="red") +
          facet_grid(Sowing ~ Irrigation) + 
          ggtitle(v)
        suppressWarnings(print(plt))
      }
    }
    dev.off()
  }
)
output$downloadParams <- downloadHandler(
  filename = function() {
    "SQparameters.csv"
  },
  content = function(file) {
    write.csv(df.sq(), file, row.names = FALSE)
  }
)