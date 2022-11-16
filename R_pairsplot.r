setwd("C:/Users/HP/Downloads/Thierry/Datas")

data = read.csv2(file = file.choose(), sep = ',', dec = '.')

# load libraries ggplot2 and ggally
library(ggplot2)
library(GGally)

#1
ggpairs(data[1:8])

#2

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[iris$Species])
}

my_cols <- c("darkgreen", "#FC4E07")  
pairs(data[1:8],
      lower.panel = panel.cor,
      upper.panel = upper.panel)


#3
library(psych)
pairs.panels(data[1:8], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses)
             )
