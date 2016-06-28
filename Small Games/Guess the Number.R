#Guess the Number
#Author: Dean Mirabito
#Date: 6/19/2016

guess_the_number <- function(){
  
  #generate master and key
  master <- as.numeric(1:10)
  key <- sample(master, 1)
  
  #start the game
  print("Let's play guess the number! It is between 1 and 10")
  guess <- as.numeric(readline("Your guess: "))
  
  #while loop to give you hints for each re-guess
  while(guess != key){
    if(guess - key > 0){
      print("Your guess is too high!")
    }
    if(guess - key < 0){
      print("Your guess is too low!")
    }
    guess <- as.numeric(readline("Guess again: "))
  }
  
  #confirmation upon exiting while loop
  print(paste0("You did it! The number was ", key))

}
        