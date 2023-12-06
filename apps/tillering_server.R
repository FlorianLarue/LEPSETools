# functions
getTillers <- function(n1, n2, n) {
  if(is.na(n2) && is.na(n1)) {
    return("0")
  } else if(is.na(n2) && !is.na(n1)) {
    return(as.character("1"))
  } else {
    solString <- NULL
    nSplit <- unlist(strsplit(n, ";"))
    # Add to n-1
    if(n1 != "") {
      n1Split <- unlist(strsplit(n1, ";"))
      for(n1s in n1Split) {
        n1sSplit <- unlist(strsplit(n1s, "-"))
        n1sParent <- paste(n1sSplit[1:(length(n1sSplit)-1)], collapse="-")
        if(n1sParent %in% nSplit || length(n1sSplit) == 1) {
          n1sSplit[length(n1sSplit)] <- as.character(as.numeric(n1sSplit[length(n1sSplit)]) + 1)
          if(is.null(solString)) {
            solString <- paste(n1sSplit, collapse="-")
          } else {
            solString <- paste(solString, paste(n1sSplit, collapse="-"), sep=";")
          }
        }
      }
    }
    
    # Increment to n-2
    if(n2 != "") {
      n2Split <- unlist(strsplit(n2, ";"))
      for(n2s in n2Split) {
        if(is.null(solString)) {
          solString <- paste(n2s, "-0", sep="")
        } else {
          solString <- paste(solString, paste(n2s, "-0", sep=""), sep=";")
        }
      }
    }
    
    if(is.null(solString)) {
      solString <- ""
    }
    return(solString)
  }
}
drawTiller <- function(tillerString, stepSize = 15) {
  colorList <- c("black","red","green","blue","orange","purple",
                 "yellow","lightblue")
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1, 
                 xscale = c(0, 100), yscale = c(0, 120), name = "VP")
  pushViewport(vp)
  grid.rect()
  root <- c(50, 5, 0, 1) # x, y, theta, color
  top <- list()
  top[[1]] <- root
  
  # First 2 phytomers
  grid.move.to(root[1], root[2], default.units = "native")
  x <- top[[1]][1] + (stepSize/3) * sin(top[[1]][3] * pi/180)
  y <- top[[1]][2] + (stepSize/3) * cos(top[[1]][3] * pi/180)
  grid.line.to(x, y, default.units = "native")
  grid.line.to(x+0.5, y, default.units = "native")
  top[[1]] <- c(x, y, 0, 1)
  
  grid.move.to(top[[1]][1], top[[1]][2], default.units = "native")
  x <- top[[1]][1] + (stepSize) * sin(top[[1]][3] * pi/180)
  y <- top[[1]][2] + (stepSize) * cos(top[[1]][3] * pi/180)
  grid.line.to(x, y, default.units = "native")
  grid.line.to(x+0.5, y, default.units = "native")
  top[[1]] <- c(x, y, 0, 1)

  if(length(tillerString) > 0) {
    for(n in 1:length(tillerString)) {
      # Create new phytomer at top of each tiller
      for(t in 1:length(top)) {
        grid.move.to(top[[t]][1], top[[t]][2], default.units = "native")
        x <- top[[t]][1] + stepSize * sin(top[[t]][3] * pi/180)
        y <- top[[t]][2] + stepSize * cos(top[[t]][3] * pi/180)
        grid.line.to(x, y, default.units = "native", 
                     gp = gpar(col = colorList[top[[t]][4]]))
        grid.line.to(x+0.5, y, default.units = "native")
        top[[t]] <- c(x, y, top[[t]][3], top[[t]][4])
      }
      
      stringn <- unlist(strsplit(tillerString[n], ";"))
      for(c in stringn) {
        stringc <- unlist(strsplit(c, "-"))
        # Move to phytomer = stringC[1]
        grid.move.to(root[1], root[2] + (stepSize/3) + 
                       (stepSize*(as.numeric(stringc[1]))), 
                     default.units = "native")
        if((as.numeric(stringc[1]) %% 2) == 0) {
          cp <- c(root[1], root[2] + (stepSize/3) + 
                    (stepSize*(as.numeric(stringc[1]))), -30)
        } else {
          cp <- c(root[1], root[2] + (stepSize/3) + 
                    (stepSize*(as.numeric(stringc[1]))), 30)
        }
        
        if(length(stringc) > 1) {
          for(k in 1:((length(stringc))-1)) {
            # Move to Tiller phytomer
            x <- cp[1] + (((as.numeric(stringc[k+1])) * stepSize) + 
                            (stepSize/3)) * sin(cp[3] * pi/180)
            y <- cp[2] + (((as.numeric(stringc[k+1])) * stepSize) + 
                            (stepSize/3)) * cos(cp[3] * pi/180)
            cp <- c(x, y, cp[3])
            grid.move.to(cp[1], cp[2], default.units = "native")
            if((as.numeric(stringc[k+1]) %% 2) == 0) {
              cp <- c(cp[1], cp[2], 1.5*cp[3])
            } else {
              cp <- c(cp[1], cp[2], cp[3]/2)
            }
          }
        }
        
        # Draw new tiller
        x <- cp[1] + (stepSize/3) * sin(cp[3] * pi/180)
        y <- cp[2] + (stepSize/3) * cos(cp[3] * pi/180)
        grid.line.to(x, y, default.units = "native", 
                     gp = gpar(col = colorList[n+1]))
        grid.line.to(x+0.5, y, default.units = "native")
        # Add new tiller to list
        top[[length(top)+1]] <- c(x, y, cp[3], n+1)
      }
    }
  }
}
drawTiller2 <- function(tillerString, realString, stepSize = 15) {
  colorList <- c("black","red","green","blue","orange","purple",
                 "yellow","lightblue")
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1, 
                 xscale = c(0, 100), yscale = c(0, 120), name = "VP")
  pushViewport(vp)
  grid.rect()
  root <- c(50, 5, 0, 1, 1) # x, y, theta, color, emerged
  top <- list()
  top[[1]] <- root
  
  # First 2 phytomers
  grid.move.to(root[1], root[2], default.units = "native")
  x <- top[[1]][1] + (stepSize/3) * sin(top[[1]][3] * pi/180)
  y <- top[[1]][2] + (stepSize/3) * cos(top[[1]][3] * pi/180)
  grid.line.to(x, y, default.units = "native")
  grid.line.to(x+0.5, y, default.units = "native")
  top[[1]] <- c(x, y, 0, 1, 1)
  
  grid.move.to(top[[1]][1], top[[1]][2], default.units = "native")
  x <- top[[1]][1] + (stepSize) * sin(top[[1]][3] * pi/180)
  y <- top[[1]][2] + (stepSize) * cos(top[[1]][3] * pi/180)
  grid.line.to(x, y, default.units = "native")
  grid.line.to(x+0.5, y, default.units = "native")
  top[[1]] <- c(x, y, 0, 1, 1)
  
  if(length(tillerString) > 0) {
    for(n in 1:length(tillerString)) {
      # Create new phytomer at top of each tiller
      for(t in 1:length(top)) {
        grid.move.to(top[[t]][1], top[[t]][2], default.units = "native")
        x <- top[[t]][1] + stepSize * sin(top[[t]][3] * pi/180)
        y <- top[[t]][2] + stepSize * cos(top[[t]][3] * pi/180)
        if(top[[t]][5] == 1) {
          grid.line.to(x, y, default.units = "native", 
                       gp = gpar(col = colorList[top[[t]][4]]))
          grid.line.to(x+0.5, y, default.units = "native")
        } else {
          grid.line.to(x, y, default.units = "native", 
                       gp = gpar(col = colorList[top[[t]][4]], lty = "dotted"))
          grid.line.to(x+0.5, y, default.units = "native")
        }
        top[[t]] <- c(x, y, top[[t]][3], top[[t]][4], top[[t]][5])
      }
      
      stringn <- unlist(strsplit(tillerString[n], ";"))
      stringr <- unlist(strsplit(realString[n], ";"))
      
      for(c in stringn) {
        stringc <- unlist(strsplit(c, "-"))
        # Move to phytomer = stringC[1]
        grid.move.to(root[1], root[2] + (stepSize/3) + 
                       (stepSize*(as.numeric(stringc[1]))), 
                     default.units = "native")
        if((as.numeric(stringc[1]) %% 2) == 0) {
          cp <- c(root[1], root[2] + (stepSize/3) + 
                    (stepSize*(as.numeric(stringc[1]))), -30)
        } else {
          cp <- c(root[1], root[2] + (stepSize/3) + 
                    (stepSize*(as.numeric(stringc[1]))), 30)
        }
        
        if(length(stringc) > 1) {
          for(k in 1:((length(stringc))-1)) {
            # Move to Tiller phytomer
            x <- cp[1] + (((as.numeric(stringc[k+1])) * stepSize) + 
                            (stepSize/3)) * sin(cp[3] * pi/180)
            y <- cp[2] + (((as.numeric(stringc[k+1])) * stepSize) + 
                            (stepSize/3)) * cos(cp[3] * pi/180)
            cp <- c(x, y, cp[3])
            grid.move.to(cp[1], cp[2], default.units = "native")
            if((as.numeric(stringc[k+1]) %% 2) == 0) {
              cp <- c(cp[1], cp[2], 1.5*cp[3])
            } else {
              cp <- c(cp[1], cp[2], cp[3]/2)
            }
          }
        }
        
        # Draw new tiller
        x <- cp[1] + (stepSize/3) * sin(cp[3] * pi/180)
        y <- cp[2] + (stepSize/3) * cos(cp[3] * pi/180)
        
        if(c %in% stringr) {
          grid.line.to(x, y, default.units = "native", 
                       gp = gpar(col = colorList[n+1]))
          grid.line.to(x+0.5, y, default.units = "native")
          top[[length(top)+1]] <- c(x, y, cp[3], n+1, 1)
        } else {
          grid.line.to(x, y, default.units = "native", 
                       gp = gpar(col = colorList[n+1], lty = "dotted"))
          top[[length(top)+1]] <- c(x, y, cp[3], n+1, 0)
        }
      }
    }
  }
}

sensiEquation <- function(SDi, USD, LSD) {
  return(max(c(0, min(c(1, (SDi - LSD)/(USD - LSD))))))
}
fiboTiller <- function(i) {
  return((((1+sqrt(5))/2)^i - ((1-sqrt(5))/2)^i)/sqrt(5))
}
newTiller <- function(i, SDi, USD, LSD) {
  return(round(fiboTiller(i-2) * sensiEquation(SDi, USD, LSD)))
}

# variables
tillerString <- reactiveVal(NULL)
realString <- reactiveVal(NULL)
df.til.sens <- reactiveVal()
df.til.till <- reactiveVal({
  df.tmp <- data.frame(Phyllochron = 1:8, PNT = c(0, 0, fiboTiller(1:6)), 
                       PTT = fiboTiller(1:8), 
                       SDi = c(1.8, 1.8, seq(1.8, 0.8, -0.2)), 
                       NT = rep(0, 8), TT = rep(1, 8))
  df.tmp
})
ls.til.grid <- reactiveVal()
ls.real.grid <- reactiveVal()

# Listeners
listen.til.param <- reactive({
  list(input$tilleringLSD, input$tilleringUSD)
})

# Observers
observeEvent(listen.til.param(), {
  # sensitivity df
  if(!is.na(input$tilleringUSD) && !is.na(input$tilleringLSD) &&
     as.numeric(input$tilleringUSD) > 0 && as.numeric(input$tilleringLSD > 0)) {
    df.tmp <- data.frame(SDi = seq(from=0.2, to = 2, by = 0.1))
    for(m in 1:nrow(df.tmp)) {
      df.tmp[m, "Sensitivity"] <- sensiEquation(df.tmp[m, "SDi"], 
                                                input$tilleringUSD, 
                                                input$tilleringLSD)
    }
    df.til.sens(df.tmp)
    
    # potential and real tiller number
    df.tmp <- df.til.till()
    for(n in 2:nrow(df.tmp)) {
      df.tmp[n, "NT"] <- newTiller(df.tmp[n, "Phyllochron"], df.tmp[n, "SDi"],
                                   input$tilleringUSD, input$tilleringLSD)
      df.tmp[n, "TT"] <- df.tmp[n-1, "TT"] + df.tmp[n, "NT"]
    }
    
    df.til.till(df.tmp)
  }
  
  # tiller diagram list
  list.tmp <- c(NA, NA)
  real.tmp <- c(NA, NA)
  for(i in 3:8) {
    rl <- getTillers(real.tmp[i-1], real.tmp[i-2], real.tmp)
    pt <- getTillers(real.tmp[i-1], list.tmp[i-2], list.tmp)
    sens <- sensiEquation(df.til.till()[i, "SDi"], input$tilleringUSD, input$tilleringLSD)
    pt.tmp <- unlist(strsplit(pt, ";"))
    pt.tmp <- pt.tmp[order(pt.tmp)]
    nt <- round(length(pt.tmp) * sens)
    pt.tmp <- tail(pt.tmp, nt)
    pt.tmp <- paste(pt.tmp, collapse=";")
    list.tmp <- c(list.tmp, pt.tmp)
    real.tmp <- c(real.tmp, rl)
  }
  ls.til.grid(list.tmp[3:8])
  ls.real.grid(real.tmp[3:8])
})
observeEvent(input$tilleringphyllo, {
  if(input$tilleringphyllo > 2) {
    tillerString(ls.til.grid()[1:((input$tilleringphyllo)-2)])
    realString(ls.real.grid()[1:((input$tilleringphyllo)-2)])
  } else {
    tillerString(NULL)
    realString(NULL)
  }
})
observeEvent(input$til.df.till_cell_edit, {
  df.tmp <- df.til.till()
  df.tmp$SDi <- input$til.df.till_cell_edit$value
  for(n in 2:nrow(df.tmp)) {
    df.tmp[n, "NT"] <- newTiller(df.tmp[n, "Phyllochron"], df.tmp[n, "SDi"],
                                 input$tilleringUSD, input$tilleringLSD)
    df.tmp[n, "TT"] <- df.tmp[n-1, "TT"] + df.tmp[n, "NT"]
  }
  df.til.till(df.tmp)
  
  list.tmp <- c(NA, NA)
  real.tmp <- c(NA, NA)
  for(i in 3:8) {
    rl <- getTillers(real.tmp[i-1], real.tmp[i-2], real.tmp)
    pt <- getTillers(real.tmp[i-1], list.tmp[i-2], list.tmp)
    sens <- sensiEquation(df.til.till()[i, "SDi"], input$tilleringUSD, input$tilleringLSD)
    pt.tmp <- unlist(strsplit(pt, ";"))
    pt.tmp <- pt.tmp[order(pt.tmp)]
    nt <- round(length(pt.tmp) * sens)
    pt.tmp <- tail(pt.tmp, nt)
    pt.tmp <- paste(pt.tmp, collapse=";")
    list.tmp <- c(list.tmp, pt.tmp)
    real.tmp <- c(real.tmp, rl)
  }
  ls.til.grid(list.tmp[3:8])
  ls.real.grid(real.tmp[3:8])
})

# Outputs
output$tilgrid <- renderPlot({
  if(input$til.showall) {
    drawTiller2(realString(), tillerString())
  } else {
    drawTiller(tillerString())
  }
})
output$til.df.sens <- DT::renderDataTable({
  DT::datatable(
    df.til.sens(),
    options=list(dom="t", pageLength=-1,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = 0:1))),
    fillContainer = T,
    rownames = FALSE
  ) %>% formatRound(c(1:2), 2)
})
output$til.plot.sens <- renderPlot({
  plot(df.til.sens()$SDi, df.til.sens()$Sensitivity, type="b", xlab="SDi", 
       ylab="Sensitivity")
})
output$til.df.till <- DT::renderDataTable({
  DT::datatable(
    df.til.till(),
    options=list(dom="t", pageLength=-1,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = 0:5))),
    editable = list(target = "column", disable = list(columns = c(0:2, 4:5))),
    selection = "none",
    fillContainer = T,
    rownames = FALSE
  ) %>% formatRound(c(1:3), 0) %>% 
    formatRound(c(4), 2) %>% 
    formatRound(c(5:6), 0)
})
output$til.plot.till <- renderPlot({
  plot(df.til.till()$Phyllochron, df.til.till()$PTT, type="b", col="red",
       xlab = "Phyllochron", ylab = "Total Tiller Number")
  lines(df.til.till()$Phyllochron, df.til.till()$TT, type="b")
})
