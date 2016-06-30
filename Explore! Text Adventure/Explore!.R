#Text Adventure
#Author: Dean Mirabito
#Date: 6/28/2016

explore <- function(){
  
  ###########
  #Prep Work#
  ###########
  
  #source functions from github
  #error out here and inform user if RCurl not installed
  tryCatch({library(RCurl)}, 
           error = function(e) {stop("Please install the package RCurl to play this game.")})
  
  source_https <- function(url, ...) {
    
    #parse and evaluate each .R script
    sapply(c(url, ...), function(u) {
      eval(parse(text = getURL(u, followlocation = TRUE, 
                               cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), 
                               envir = .GlobalEnv)
    })
  }
  
  #source functions from Github
  source_https("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Random%20Insult%20Generator.R",
               "https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Search%20Game.R",
               "https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Dice%20Roll.R")
  
  #source riddle library from github
  rid_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Riddle%20Library.csv")
  riddle_lib <- read.csv(text = rid_library)
  riddle <- as.character(riddle_lib$Riddle)
  answer <- as.character(riddle_lib$Answer)
  
  #prep for while loop that will hold game
  continue <- 1
  
  #Make points visible to player
  Total.Points <<- 0
  
  #####################
  #Let's play the game#
  #####################
  
  #start the game
  while(continue == 1){
    
    print("Welcome to Explorers of the Unknown!")
    
    #username entry
    username <- readline("What is your name, adventurer? ")
    print(paste0("Welcome, ", username, "!"))
    readline()
    print("You are an explorer who has just arrived on a previously undiscovered island in the Pacific.")
    readline()
    print("You step from your boat with your assistant at your side.")
    readline()
    
    #assistant entry
    ass <- readline("What was your assistant's name again? ")
    print(paste0("Oh yes, that was their name... ", ass, "!"))
    readline()
    print(paste0("You and ", ass, " stand next to the water, looking at the dense forest that waits ahead."))
    readline()
    print("You can hear low noises coming from within the dark canopy.")
    readline()
    
    #first tree break - forest or ocean
    c1 <- readline("What would you like to do? Type 1 to venture into the forest, type 2 to walk along the beach: ")
    
    ##################
    #Forest Adventure#
    ##################
    
    if(c1 == "1"){
      print("The forest is thick and hard to move through.")
      readline()
      print("After 2 hours of trailblazing, you are tired and hungry.")
      readline()
      print(paste0("You and ", ass, " sit down and you notice a strange orange fruit underneath a bush."))
      readline()
      
      #fruit choice
      fruit <- readline(paste0("What would you like to do? Type 1 to eat it, type 2 to make ", ass, " eat it, type 3 to investigate: "))
      
      #eat it yourself
      if(fruit == "1"){
        f1 <- dice(10)
        if(f1 == 1){
          print("The fruit was poisonous! You die a horrible, painful death.")
          readline()
          print(paste0(ass, " is left to wander the forest alone."))
          readline()
          Total.Points <<- 0
          print(paste0("GAME OVER, ", username, "!"))
          break
        } else {
          print("You've earned 2 points for your digestive bravery!")
          Total.Points <<- Total.Points + 2
          readline()
          print("The fruit is quite tasty! You notice a trail of the fruit leads away to the east.")
          readline()
        }
      }#end if fruit = 1
      
      #make your assistant eat it
      if(fruit == "2"){
        f1 <- dice(5)
        if(f1 == 1){
          print(paste0("The fruit was poisonous! ", ass, " throws up and dies a horrible, painful death."))
          readline()
          print("You cannot bear to go on alone. You bite into the fruit and collapse.")
          readline()
          Total.Points <<- 0
          print(paste0("GAME OVER, ", username, "!"))
          readline()
          break
        } else {
          print("You've earned 1 point for your assistant's digestive bravery!")
          readline()
          Total.Points <<- Total.Points + 1
          print(paste0(ass, " says the fruit is quite tasty! You notice a trail of the fruit leads away to the east."))
          readline()
        }
      }#end if fruit = 2
      
      print("You follow the trail of fruit a half mile, arriving at a crude hut.")
      readline()
      print("A strange man emerges from the hut. He is seven feet tall and wears bird feathers on his head.")
      readline()
      print(paste0("You and ", ass, " stand transfixed."))
      readline()
      
      #reacting to islander choice
      conv <- readline("What would you like to do? Type 1 to bow down, type 2 to insult him: ")
      
      #bow down
      if(conv == "1"){
        print("You've earned 1 point for your show of respect!")
        readline()
        Total.Points <<- Total.Points + 1
        print(paste0("The giant man lumbers forward and lifts you and ", ass, " into the air."))
        readline()
      }#end if conv = 1
      
      #insult
      if(conv == "2"){
        verb_attack <- Insult_Gen()
        print(paste0("You yell at the top of your lungs, You are a ", verb_attack))
        readline()
        print("You've earned 5 points for your display of fearlessness!")
        readline()
        Total.Points <<- Total.Points + 5
        print("The giant man lumbers towards you.")
        readline()
        f2 <- dice(2)
        if(f2 == 1){
          print(paste0("He raises his giant fists into the air and clobbers you and ", ass, "."))
          readline()
          print("Your puny skulls are no match for the power of his strikes.")
          readline()
          Total.Points <<- 0
          print(paste0("GAME OVER, ", username, "!"))
          readline()
          break
        } else {
          print(paste0("The giant man lifts you and ", ass, " into the air."))
          readline()
        }
      }#end if conv = 2
      
      print("You are carried into the giant man's hut. A large black cauldron stands full of water.")
      readline()
      print("The giant man speaks, I will cook your friend for my lunch if you cannot answer my riddle!")
      readline()
      print(paste0(ass, " looks nervously across at you."))
      readline()
      print("The giant man says, Okay here is my riddle:")
      readline()
      
      #riddle time
      f3 <- dice(7)
      print(riddle[f3])
      user_answer <- readline("Think hard and type your answer in lowercase: ")
      real_answer <- answer[f3]
      correct_test <- grepl(real_answer, user_answer)
      
      #if correct
      if(correct_test == TRUE){
        print("You've earned 5 points for your quick thinking!")
        readline()
        Total.Points <<- Total.Points + 5
        print("The giant man says, I am very surprised by your intelligence! ")
        readline()
      } else {
        print(paste0("The giant man yells, You idiot! The answer was ", real_answer, "."))
        readline()
        print(paste0("The giant man throws ", ass, " into the cauldron and stokes the fire."))
        readline()
        print("You are trapped in the giant man's grasp, forced to listen to the screams of your assistant.")
        readline()
        print("The giant man turns his head towards you and says, You'll be my dessert!")
        readline()
        Total.Points <<- 0
        print(paste0("GAME OVER, ", username, "!"))
        readline()
        break
      }#end riddle outcome
      
      print("He continues, You are two explorers of the highest honor! Please, take this gift.")
      readline()
      print("He hands you a flat, dark object and says, This here is the finest chocolate in all the land!")
      readline()
      print("You take the chocolate, thank him graciously and return to the ship, satisfied with your adventuring for the day.")
      readline()
      continue <- readline("Congrats on surviving your adventure! Would you like to venture out again? Type 1 to replay, type 0 to quit: ")
      
    }#end forest adventure
    
    #################
    #Beach Adventure#
    #################
    
    if(c1 == "2"){
      print("You can feel the heat of the sand through the soles of your boots.")
      readline()
      
      #treasure hunt
      g1 <- dice(2)
      if(g1 == 1){
        print("As you look down the beach, you notice something shiny glimmer in the sand for a moment.")
        readline()
        digging <- readline("Would you like to try to dig for it? Type 1 to dig, type 2 to move on: ")
        
        #choose to dig
        if(digging == "1"){
          print(paste0("Okay, get your eyes ready, ", username, "! You will only have 25 seconds."))
          readline()
          print("Find the single LETTER that is mixed among the sand of numbers.")
          readline()
          
          #Digging game
          diff.search <- dice(8)
          search_game(diff.search)
          Total.Points <<- Total.Points
          
          break
          }#end if choose to dig
      }#end if dice = 1
      break
    }#end beach adventure
        
  }#end while loop
  
  #ending message
  print("Based on your total points: ")
  readline()
  
  if(Total.Points == 0){
    print("You are a POOR explorer!")
    readline()
  }
  if(Total.Points > 0 & Total.Points <= 6){
    print("You are an AMATEUR explorer!")
    readline()
  }
  if(Total.Points > 6 & Total.Points <= 11){
    print("You are an INTERMEDIATE explorer!")
    readline()
  }
  if(Total.Points > 11){
    print("You are a MASTER explorer!")
    readline()
  }
  
  print("Thanks for playing Explorers of the Unknown!")
  print("This game was developed by Dean Mirabito, June 2016.")
  
}#end function