#Random Insult Generator
#Author: Dean Mirabito
#Date: 6/28/2016
Insult_Gen <- function(){
  
  #Building libraries
  lib <- read.csv("C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Insult Library.csv")
  adj <- as.character(lib$Adj)
  noun <- as.character(subset(lib$Noun, lib$Noun != ""))
  xx <- as.numeric(length(adj))
  yy <- as.numeric(length(noun))

  #Generating insults
  print("Welcome to the Random Insult Generator!")
  continue <- "Y"

  while(continue == "Y"){
    level <- readline("What intensity of insult do you need? Type 1, 2 or 3: ")
    if(level == "1"){
      nkey <- as.numeric(sample(yy, 1))
      print(paste("How about:", noun[nkey], "!"))
    }
    if(level == "2"){
      akey <- as.numeric(sample(xx, 1))
      nkey <- as.numeric(sample(yy, 1))
      print(paste("How about:", adj[akey], noun[nkey], "!"))
    }
    if(level == "3"){
      akey <- as.numeric(sample(xx, 1))
      a2key <- as.numeric(sample(xx, 1))
      nkey <- as.numeric(sample(yy, 1))
      print(paste("How about:", adj[akey], adj[a2key], noun[nkey], "!"))
    }
    continue <- readline("Need another? Type Y or N: ")
  }#end while loop
  
  print("Thanks for using the Random Insult Generator! We hope you hurt someone's feelings.")
  
}#end function
