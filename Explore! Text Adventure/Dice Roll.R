#Function used to "roll dice"

dice <- function(sides = 6){
  xx <- 1:sides
  n <- sample(xx, 1)
  as.numeric(n)
}