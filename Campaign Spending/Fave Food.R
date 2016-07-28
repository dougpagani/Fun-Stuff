#Where does your favorite candidate like to eat?
#Function to analyze public campaign expenditure data and 
#determine where that candidate eats the most

#data available at: http://www.fec.gov/disclosurep/PDownload.do

#input: URL for raw csv dataset on github, candidates name 
#output: prints top three places the campaign has eaten at, dataframe showing all food expenses

fave_food <- function(Dataset_URL, Candidate_Name = "Your Candidate"){
  
  #Prep work
  library(RCurl)
  
  #Get that data
  cand_library <- getURL(Dataset_URL)
  campaign_expense <- read.csv(text = cand_library)
  
  #subset it
  campaign_expense$disb_desc <- as.character(campaign_expense$disb_desc)
  food_spending <- subset(campaign_expense, grepl("FOOD", campaign_expense$disb_desc) == TRUE)
  
  #aggregate spending by restaurant
  foodfood <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(foodfood) <- c("Place", "Amount.spent")
  
  cvb <- as.numeric(length(food_spending$recipient_nm))
  food_spending$recipient_nm <- as.character(food_spending$recipient_nm)
  
  for (i in 1:cvb){
    
    #subset food spending
    x <- food_spending[i, 4]
    y <- subset(food_spending, food_spending$recipient_nm == x)
    z <- sum(y$disb_amt)
    
    #assign to foodfood dataframe
    foodfood[i, 1] <- x
    foodfood[i, 2] <- z
    
  }
  
  #remove duplicates
  foodfood$dup <- duplicated(foodfood$Place)
  FinalFood <- subset(foodfood, foodfood$dup == FALSE)
  FinalFood$dup <- NULL
  
  #order by amount spent
  FinalFood <- FinalFood[order(FinalFood$Amount.spent, decreasing = TRUE),]
  
  #Make table available and display top three
  FoodSpending <<- FinalFood
  print(paste0(Candidate_Name, " likes to eat at: "))
  print(FinalFood[1:3,], row.names = FALSE)
  
}