#Search Game
#Author: Dean Mirabito
#Date: 6/28/2016

search_game <- function(difficulty = 5){
  
  master <- as.numeric(1:9)
  maze <- 1
  Total.Points <- 0
  
  #Generate sand to search through
  for(i in 1:difficulty){
    permute <- sample(master, 100, replace = TRUE)
    maze <- c(maze, permute)
  }#end for loop
  
  #Generate hidden letter
  yy <- as.numeric(length(maze))
  key_place <- as.numeric(sample(1:yy, 1))
  wow <- sample(LETTERS, 1)
  maze[key_place] <- wow
  
  readline("Press enter to start the search: ")
  
  #measure time elapsed during search
  print(maze)
  start.time <- proc.time()
  found.it <- readline("Enter your finding here in uppercase: ")
  time.elapsed <- proc.time() - start.time
  
  #results of digging
  if(time.elapsed[3] > 25){
    print(paste0("Just out of reach! You were a little too slow at ", time.elapsed[3], " seconds."))
    readline()
  }
  if(found.it != wow){
    print(paste0("Your eyes ain't as good as they used to be! There was a ", wow, " hiding in there!"))
    readline()
  }
  if(found.it == wow & time.elapsed[3] < 25){
    print(paste0("You found the letter ", wow, "! And it only took you ", time.elapsed[3], " seconds."))
    readline()
    print(paste0("You have earned ", difficulty, " points for your keen eye!"))
    readline()
    Total.Points <<- Total.Points + difficulty
  }
  
}#end function