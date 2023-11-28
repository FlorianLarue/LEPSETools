# variables

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

output$tilgrid <- renderPlot({
  testString <- c("1-0","2-0","1-1;3-0","1-2;2-1;4-0", "1-3;1-1-1;2-2;3-1;5-0", 
                  "1-4;1-1-2;1-2-1;2-3;2-1-1;3-2;4-1;6-0")
  drawTiller(testString)
})