#Random Insult Generator
#Author: Dean Mirabito
#Date: 6/28/2016
Insult_Gen <- function(){
  
  #Building library
  library(RCurl) #install if you error out here
  lib_raw <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Insult%20Library.csv")
  lib <- read.csv(text = lib_raw)
  
  #formatting
  adj <- as.character(lib$Adj)
  noun <- as.character(lib$Noun)
  xx <- as.numeric(length(adj))
  yy <- as.numeric(length(noun))

  #Generating insults
  print("Welcome to the Random Insult Generator!")
  continue <- "Y"

  while(continue == "Y"){
    level <- readline("What intensity of insult do you need? Type 1, 2 or 3: ")
    if(level == "1"){
      nkey <- as.numeric(sample(yy, 1))
      insult <- paste(noun[nkey], "!")
      print(insult)
    }
    if(level == "2"){
      akey <- as.numeric(sample(xx, 1))
      nkey <- as.numeric(sample(yy, 1))
      insult <- paste(adj[akey], noun[nkey], "!")
      print(insult)
    }
    if(level == "3"){
      akey <- as.numeric(sample(xx, 1))
      a2key <- as.numeric(sample(xx, 1))
      nkey <- as.numeric(sample(yy, 1))
      insult <- paste(adj[akey], adj[a2key], noun[nkey], "!")
      print(insult)
    }
    continue <- readline("How is that? Type Y to try again, type N to use that one: ")
  }#end while loop
  
  insult
  
}#end function
