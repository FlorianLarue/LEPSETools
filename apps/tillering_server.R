# variables
tillerString <- reactiveVal(c(""))
df.til.sens <- reactiveVal()
df.til.till <- reactiveVal()
totalTil <- c("1-0","2-0","1-1;3-0","1-2;2-1;4-0", 
              "1-3;1-1-1;2-2;3-1;5-0", 
              "1-4;1-1-2;1-2-1;2-3;2-1-1;3-2;4-1;6-0")
tmptil <- reactiveVal(1)

# functions
drawTiller <- function(tillerString, stepSize = 10) {
  colorList <- c("black","red","green","blue","orange","purple",
                 "yellow","lightblue")
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1, 
                 xscale = c(0, 100), yscale = c(0, 100), name = "VP")
  pushViewport(vp)
  grid.rect()
  root <- c(50, 5, 0, 1) # x, y, theta
  top <- list()
  top[[1]] <- root
  
  # First phytomer
  grid.move.to(root[1], root[2], default.units = "native")
  x <- top[[1]][1] + stepSize * sin(top[[1]][3] * pi/180)
  y <- top[[1]][2] + stepSize * cos(top[[1]][3] * pi/180)
  grid.line.to(x, y, default.units = "native")
  grid.line.to(x+0.5, y, default.units = "native")
  top[[1]] <- c(x, y, 0, 1)
  
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
      grid.move.to(root[1], root[2] + (stepSize*(as.numeric(stringc[1])-1)), 
                   default.units = "native")
      if((as.numeric(stringc[1]) %% 2) == 0) {
        cp <- c(root[1], root[2] + (stepSize*(as.numeric(stringc[1])-1)), 30)
      } else {
        cp <- c(root[1], root[2] + (stepSize*(as.numeric(stringc[1])-1)), -30)
      }
      
      if(stringc[2] != 0) {
        for(k in 1:((length(stringc))-1)) {
          # Move to Tiller phytomer
          x <- cp[1] + (as.numeric(stringc[k+1])-1) * stepSize * 
            sin(cp[3] * pi/180)
          y <- cp[2] + (as.numeric(stringc[k+1])-1) * stepSize * 
            cos(cp[3] * pi/180)
          cp <- c(x, y, cp[3])
          grid.move.to(cp[1], cp[2], default.units = "native")
          if((as.numeric(stringc[k+1]) %% 2) == 0) {
            cp <- c(cp[1], cp[2], cp[3]/2)
          } else {
            cp <- c(cp[1], cp[2], 1.5*cp[3])
          }
        }
      }
      
      # Draw new tiller
      x <- cp[1] + stepSize * sin(cp[3] * pi/180)
      y <- cp[2] + stepSize * cos(cp[3] * pi/180)
      grid.line.to(x, y, default.units = "native", 
                   gp = gpar(col = colorList[n+1]))
      grid.line.to(x+0.5, y, default.units = "native")
      
      # Add new tiller to list
      top[[length(top)+1]] <- c(x, y, cp[3], n+1)
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

# Listeners
listen.til.param <- reactive({
  list(input$tilleringLSD, input$tilleringUSD)
})

# Observers
observeEvent(listen.til.param(), {
  df.tmp <- data.frame(SDi = seq(from=0.2, to = 2, by = 0.2))
  for(m in 1:nrow(df.tmp)) {
    df.tmp[m, "Sensitivity"] <- sensiEquation(df.tmp[m, "SDi"], 
                                              input$tilleringUSD, 
                                              input$tilleringLSD)
  }
  df.til.sens(df.tmp)
  
  df.tmp <- data.frame(Phyllochron = 1:8, PNT = c(0, 0, fiboTiller(1:6)), 
                       PTT = fiboTiller(1:8), 
                       SDi = c(1.8, 1.8, seq(1.8, 0.8, -0.2)))
  df.tmp$NT <- 0
  df.tmp$TT <- 1
  for(n in 2:nrow(df.tmp)) {
    df.tmp[n, "NT"] <- newTiller(df.tmp[n, "Phyllochron"], df.tmp[n, "SDi"],
                                 input$tilleringUSD, input$tilleringLSD)
    df.tmp[n, "TT"] <- df.tmp[n-1, "TT"] + df.tmp[n, "NT"]
  }
  
  df.til.till(df.tmp)
})

observeEvent(input$tilleringphyllo, {
  if(input$tilleringphyllo > 2) {
    tillerString(totalTil[1:((input$tilleringphyllo)-2)])
    output$tilgrid <- renderPlot({
      drawTiller(tillerString())
    })
  } else {
    tillerString("")
  }
})

# Outputs
output$tilgrid <- renderPlot({
  drawTiller(tillerString())
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

