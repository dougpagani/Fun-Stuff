#Text Adventure
#Author: Dean Mirabito
#Date: 6/28/2016

Adventure1 <- function(){
  
  #user inputs
  #make sure that these point to the location of your functions and libraries
  source("C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Random Insult Generator.R")
  
  library(RCurl) #install if you error out here
  rid_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Riddle%20Library.csv")
  riddle_lib <- read.csv(text = rid_library)
  
  #prep work
  continue <- 1
  
  #randomising function
  dice <- function(sides = 6){
    xx <- 1:sides
    n <- sample(xx, 1)
    as.numeric(n)
  }
  
  #riddle libraries
  riddle <- as.character(riddle_lib$Riddle)
  answer <- as.character(riddle_lib$Answer)
  
  #start the game
  while(continue == 1){
    
    print("Welcome to Text Adventure 1: Explorers of the Unknown!")
    username <- readline("What is your name, adventurer? ")
    print(paste0("Welcome, ", username, "!"))
    print("You are an explorer who has just arrived on a previously undiscovered island in the Pacific.") 
    print("You step from your boat with your assistant at your side.")
    ass <- readline("What was their name again? ")
    print(paste0("Oh yes, that was their name... ", ass, "!"))
    print(paste0("You and ", ass, " stand next to the water, looking at the dense forest that waits ahead."))
    print("You can hear low noises coming from within the dark canopy.")
    c1 <- readline("What would you like to do? Type 1 to venture into the forest, type 2 to walk along the beach: ")
    
    #forest adventure
    if(c1 == "1"){
      print("The forest is thick and hard to move through.")
      print("After 2 hours of trailblazing, you are tired and hungry.")
      print(paste0("You and ", ass, " sit down and you notice a strange orange fruit underneath a bush."))
      
      #fruit choice
      fruit <- readline(paste0("What would you like to do? Type 1 to eat it, type 2 to make ", ass, " eat it: "))
      
      #eat it yourself
      if(fruit == "1"){
        f1 <- dice(10)
        if(f1 == 1){
          print("The fruit was poisonous! You die a horrible, painful death.")
          print(paste0(ass, " is left to wander the forest alone. GAME OVER, ", username, "!"))
          break
        } else {
          print("The fruit is quite tasty! You notice a trail of the fruit leads away to the east.")
        }
      }#end if fruit = 1
      
      #make your assistant eat it
      if(fruit == "2"){
        f1 <- dice(5)
        if(f1 == 1){
          print(paste0("The fruit was poisonous! ", ass, " throws up and dies a horrible, painful death."))
          print("You cannot bear to go on alone. You bite into the fruit and collapse.")
          print(paste0("GAME OVER, ", username, "!"))
          break
        } else {
          print(paste0(ass, " says the fruit is quite tasty! You notice a trail of the fruit leads away to the east."))
        }
      }#end if fruit = 2
      
      print("You follow the trail of fruit a half mile, arriving at a crude hut.")
      print("A strange man emerges from the hut. He is seven feet tall and wears bird feathers on his head.")
      print(paste0("You and ", ass, " stand transfixed."))
      
      #reacting to islander choice
      conv <- readline("What would you like to do? Type 1 to bow down, type 2 to insult him: ")
      
      #bow down
      if(conv == "1"){
        print(paste0("The giant man lumbers forward and lifts you and ", ass, " into the air."))
      }#end if conv = 1
      
      #insult
      if(conv == "2"){
        verb_attack <- Insult_Gen()
        print(paste0("You yell at the top of your lungs, You are a ", verb_attack))
        print("The giant man lumbers towards you.")
        f2 <- dice(2)
        if(f2 == 1){
          print(paste0("He raises his giant fists into the air and clobbers you and ", ass, "."))
          print(paste0("Your puny skulls are no match for the power of his strikes. GAME OVER, ", username, "!"))
          break
        } else {
          print(paste0("The giant man lifts you and ", ass, " into the air."))
        }
      }#end if conv = 2
      
      print("You are carried into the giant man's hut. A large black cauldron stands full of water.")
      print("The giant man speaks, I will cook your friend for my lunch if you cannot answer my riddle!")
      print(paste0(ass, " looks nervously across at you."))
      print("The giant man says, Okay here is my riddle.")
      
      #riddle time
      f3 <- dice(7)
      print(riddle[f3])
      user_answer <- readline("Think hard and type your answer in lowercase: ")
      real_answer <- answer[f3]
      correct_test <- grepl(real_answer, user_answer)
      
      #if correct
      if(correct_test == TRUE){
        print("The giant man says, I am very surprised by your intelligence! ")
      } else {
        print(paste0("The giant man yells, You idiot! The answer was ", real_answer, " ."))
        print(paste0("The giant man throws", ass, " into the cauldron and stokes the fire."))
        print("You are trapped in the giant man's grasp, forced to listen to the screams of your assistant.")
        print("The giant man turns his head towards you and says, And you'll be my dessert!")
        print(paste0("GAME OVER, ", username, "!"))
        break
      }#end riddle outcome
      
            
      
    }#end forest adventure
    
    #beach adventure
    if(c1 == "2"){
    
    }#end beach adventure
        
  }#end while loop
  
}#end function