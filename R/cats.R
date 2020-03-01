#catterplot test.

# I really want a graph of how angry my cat is right now

library(tidyverse)
#library(CatterPlots)
library(readxl)

catdata = read_excel("R/catdata.xlsx")
catdata = mutate(catdata, Action = factor(Action, levels = c("Not allowed to play with live mouse",
                                                             "Not allowed outside",
                                                             "Squirt medicine in mouth",
                                                             "Cone of embarrissment",
                                                             "Not allowed on the counter",
                                                             "New dog")))

labs = c("No live \n mice",
           "Kept inside",
           "Medicine",
           "Cone",
           "Get off \n the table",
           "New dog")

catplot(as.numeric(catdata$Action), catdata$Hate, 
        xlab = "What I did", ylab = "Cat Hate Index",
        main = "How much my cat hates me", labs = labs)


cathate = function (xs, ys, size = 0.1, cat = 1, catcolor = "#000000FF", 
          linecolor = 1, type = "justcats", canvas = c(0, 1.1, 
                                                       0, 1.1), ...) 
{
  args <- list(...)
  plot(x = xs, y = ys, col = 0, xaxt = "n", yaxt = "n", 
       ...)
  par(usr = canvas)
  img <- catdat[[cat]]
  scaledData <- scaleData(xs, ys, args)
  xscale <- scaledData$xscale
  yscale <- scaledData$yscale
  #xat = seq(min(xscale), max(xscale), length.out = length(xscale))
  yat = seq(min(yscale), max(yscale), length.out = length(yscale))
  #xaxtlab = round(seq(min(xs), max(xs), length.out = length(xat)),1)
  yaxtlab = round(seq(min(ys), max(ys), length.out = length(xat)), 
                  1)
  axis(side = 1, at = c(1:6), labels = levels(catdata$Action))
  axis(side = 2, at = yat, labels = yaxtlab)
  imgMod <- colorMod(img, catcolor)
  if (type == "line") {
    points(x = xscale, y = yscale, col = linecolor, type = "l")
  }
  rasterImage(imgMod, xscale - (size/2), yscale - (size/2), 
              xscale + (size/2), yscale + (size/2), interpolate = TRUE)
  list(xs = xs, ys = ys, args = args, canvas = canvas)
}


cathate(as.numeric(catdata$Action), catdata$Hate, 
        xlab = "What I did", ylab = "Cat Hate Index",
        main = "How much my cat hates me")
