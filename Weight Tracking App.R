#Function to cache entry of weight per day and display a graph

addweight <- function(weight){
  
  #caching input
  odata <- read.csv("C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Weight Tracking.csv",
                    header = TRUE)
  x <- length(odata$Date) + 1
  odata[x,1] <- as.character.Date(Sys.Date())
  odata[x,2] <- weight
  odata <- subset(odata, is.na(odata$Weight) == FALSE)
  write.csv(odata, file = "C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Weight Tracking.csv", 
            row.names = FALSE)
  
  #graphing complete history
  library(ggplot2)
  gdata <- read.csv("C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Weight Tracking.csv",
                    header = TRUE)
  gdata <- subset(gdata, is.na(gdata$Weight) == FALSE)
  ggplot(gdata, aes(x=Date, y=Weight)) +
    geom_point(shape=1) 
}




