#Search Game
#Author: Dean Mirabito
#Date: 6/28/2016

search_game <- function(difficulty = 5){
  
  master <- as.numeric(1:9)
  maze <- 1
  
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
  
  #make answer available to parent env
  wow <<- wow
  
  readline("Press enter to start the search: ")
  
  #measure time elapsed during search
  print(maze)
  start.time <- proc.time()
  found.it <- readline("Enter your finding here in uppercase: ")
  time.elapsed <- proc.time() - start.time
  
  #make finding and time.elapsed accessible to parent env
  time.elapsed <<- time.elapsed[3]
  found.it <<- found.it
  
}#end function