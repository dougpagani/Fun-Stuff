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
  
  #source riddle library and word library from github
  rid_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Riddle%20Library.csv")
  riddle_lib <- read.csv(text = rid_library)
  riddle <- as.character(riddle_lib$Riddle)
  answer <- as.character(riddle_lib$Answer)
  word_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Explore!%20Text%20Adventure/Word%20Library.csv")
  word_lib <- read.csv(text = word_library, header = FALSE)
  
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
    Total.Points <<- 0
    readline()
    
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
      break
      
    }#end forest adventure
    
    #################
    #Beach Adventure#
    #################
    
    if(c1 == "2"){
      print("You can feel the heat of the sand through the soles of your boots.")
      readline()
      
      #treasure hunt
      g1 <- dice(1) #set to 1 so that it happens every time
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
          Total.Points <<- Total.Points + diff.search
          }#end if choose to dig
      }#end if dice = 1
      
      print("You continue your journey down the beach. The waves crash gently against the shore.")
      readline()
      print("You notice a strange apparition up ahead. Can it really be?")
      readline()
      print("It seems there is a mermaid basking in the sun on the beach!")
      readline()
      print("Try to be charming so that she does not run away!")
      readline()
      
      #mad libs style charmer
      adj1 <- readline("Enter an adjective: ")
      noun1 <- readline("Enter a noun: ")
      adj2 <- readline("Enter an adjective: ")
      bp1 <- readline("Enter a body part: ")
      print("You call out to her,")
      readline()
      print(paste0("Hello there, ", adj2, " mermaid! You have all the beauty of a ", noun1, ". I am especially impressed by your ", adj1, " ",bp1, "!"))
      readline()
      
      print(paste0("The mermaid turns to face you and ", ass, "."))
      readline()
      print("She says, You talk very strangely! But I am touched by your feelings.")
      readline()
      print("If you can solve my riddle, I have great wonders to show you!")
      readline()
      print("She continues, You must descramble the secret word! Your hint: It is related to the ocean.")
      readline()
      
      #Word descramble activity
      hhh <- as.numeric(length(word_lib$V1))
      iii <- dice(hhh)
      keyword <- as.character(word_lib[iii, 1])
      separatedword <- strsplit(keyword, split = character(0)) [[1]]
      scrambledword <- sample(separatedword, as.numeric(length(separatedword)))
      scrambledword <- paste(scrambledword, collapse = "")
      
      print("Press enter to see your word puzzle: ")
      readline()
      print(scrambledword)
      user_word <- readline("Descramble the word and enter it here: ")
      
      if(user_word == keyword){
        print("You have earned 5 points for your intellectual gymnastics!")
        readline()
        Total.Points <<- Total.Points + 5
      } else {
        print(paste0("The mermaid crys, You buffoon! The word was ", keyword, "!"))
        readline()
        print(paste0("The mermaid picks up a shell with a pointy tip and approaches you and ", ass, "."))
        readline()
        print("You are frozen with fear. You dig in your pockets to see if there is anything to defend yourself with.")
        readline()
        
        #if search game not played successfully...
        if(talisman != 1){
          print(paste0("There is nothing there! The mermaid is upon you. She stabs at you and ", ass, "."))
          readline()
          print("You are no match for her power and quickness!")
          readline()
          Total.Points <<- 0
          print(paste0("GAME OVER, ", username, "!"))
          readline()
          break
        } #end search unsuccessful
        
        if(talisman == 1){
          print("What is this? You pull from your pocket the talisman you found in the sand.")
          readline()
          print("It has a sharp edge! You raise it in front of you and you can see fear creep into the mermaid's eyes.")
          readline()
          print("It is time to do battle!")
          readline()
          print("Each time you see the mermaid's dagger --||----- press Enter quickly to defend yourself. You will only have a half second!")
          readline()
          readline("Ready? Press enter to battle: ")
          
          #battle game
          
          #1st iteration
          Sys.sleep(dice(5))
          attack.start <- proc.time()
          readline("--||-----")
          defense.time <- proc.time() - attack.start
            
          #too slow
          if(defense.time[3] > 0.5){
            print(paste0("You took ", defense.time[3], " seconds to defend yourself - it was too slow!"))
            readline()
            print("The mermaid dodges to the left of your counter-attack and stabs you through the heart.")
            readline()
            print(paste0("She grabs ", ass, " and drags them into the water, never to be seen again."))
            readline()
            Total.Points <<- 0
            print(paste0("GAME OVER, ", username, "!"))
            readline()
            break
            }
            
          #2nd iteration
          Sys.sleep(dice(5))
          attack.start <- proc.time()
          readline("--||-----")
          defense.time <- proc.time() - attack.start
          
          #too slow
          if(defense.time[3] > 0.5){
            print(paste0("You took ", defense.time[3], " seconds to defend yourself - it was too slow!"))
            readline()
            print("The mermaid dodges to the left of your counter-attack and stabs you through the heart.")
            readline()
            print(paste0("She grabs ", ass, " and drags them into the water, never to be seen again."))
            readline()
            Total.Points <<- 0
            print(paste0("GAME OVER, ", username, "!"))
            readline()
            break
          }
          
          #3rd iteration
          Sys.sleep(dice(5))
          attack.start <- proc.time()
          readline("--||-----")
          defense.time <- proc.time() - attack.start
          
          #too slow
          if(defense.time[3] > 0.5){
            print(paste0("You took ", defense.time[3], " seconds to defend yourself - it was too slow!"))
            readline()
            print("The mermaid dodges to the left of your counter-attack and stabs you through the heart.")
            readline()
            print(paste0("She grabs ", ass, " and drags them into the water, never to be seen again."))
            readline()
            Total.Points <<- 0
            print(paste0("GAME OVER, ", username, "!"))
            readline()
            break
          }
          #end battle game
          
          #successful defense against mermaid
          print("You've earned 5 points for defending yourself against all three attacks!")
          readline()
          Total.Points <<- Total.Points + 5
          print("The mermaid backs off and says, You are a formidable opponent!")
          readline()
          print("I did not expect you to possess such powerful fighting skills. Please, take this as an apology.")
          readline()
          print("She hands you a flat, dark object and says, This here is the finest chocolate in all the land!")
          readline()
          print("You take the chocolate, thank her graciously and return to the ship, satisfied with your adventuring for the day.")
          readline()
          break
      
        } #end defending against mermaid
      } #end failed descramble
      
      #successful descrambling
      print("The mermaid says, You are clearly a very intelligent human being! Let me show you something amazing.")
      readline()
      print(paste0("She takes you and ", ass, " by the hands and leads you into the water."))
      readline()
      print("The mermaid continues, As long you grasp my hand, you will be able to breathe underneath the water.")
      readline()
      print("She guides you through the coral reefs, past dolphins and down to the sea floor.")
      readline()
      print("A gleaming, metallic city shines through the water ahead, populated by merpeople!")
      readline()
      print(paste0("The mermaid says, I can turn you and ", ass, " into merpeople and you can live here with us forever! What do you think?"))
      merya <- readline("Type 1 to transform into a merperson, type 2 to ask to return to the surface: ")
      
      if(merya == "1"){
        print(paste0("The mermaid hands you and ", ass, " each a piece of kelp. You swallow it."))
        readline()
        print("You can feel gills sprout from your neck. You look down and your legs are gone! They have been replaced by a great, scaly tail.")
        readline()
        print(paste0("You let go of the mermaid's hand and the water is just like air! You and ", ass, " swim to the underwater city to live happily ever after."))
        readline()
        break
      }
      
      if(merya == "2"){
        print("The mermaid says, I'm sorry, you cannot return to the surface. We cannot risk our secret being exposed.")
        readline()
        print("The mermaid lets go of your hand. Your throat is immediately clogged with water. The surface of the water is hundreds of feet above you!")
        readline()
        print(paste0("As you and ", ass, " start to swim desperately upwards for air, the mermaid grabs you both by the ankles."))
        readline()
        print("She holds you there under the water. You struggle and thrash as the water overwhelms you, and everything becomes very quiet...")
        readline()
        Total.Points <<- 0
        print(paste0("GAME OVER, ", username, "!"))
        readline()
        break
      }
      
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