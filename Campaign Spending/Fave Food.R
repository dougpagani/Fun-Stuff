#Where does your favorite candidate like to eat?
#Function to analyze public campaign expenditure data and 
#determine where that candidate eats the most

fave_food <- function(Candidate_Last_Name){
  
  #Data download -- if statement to cut runtime on successive iterations
  if(exists("data_download") == FALSE){
    
    #So you know that its working
    print("Please wait - compiling results...")
    
    #Prep work
    library(RCurl)
    
    #Get that data
    cand_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Campaign%20Spending/2016%20Campaign%20Spending.csv")
    campaign_expense <<- read.csv(text = cand_library)
    data_download <<- "Complete"

  }#end if data download incomplete
  
  #character swaps
  campaign_expense$disb_desc <- as.character(campaign_expense$disb_desc)
  Candidate_Last_Name <- as.character(Candidate_Last_Name)
  
  #subset for candidate of interest and food words
  VIP_spending <- subset(campaign_expense, grepl(Candidate_Last_Name, campaign_expense$cand_nm) == TRUE)
  food_spending <- subset(VIP_spending, grepl("FOOD", VIP_spending$disb_desc) == TRUE | 
                                        grepl("BEVERAGE", VIP_spending$disb_desc) == TRUE | 
                                        grepl("FOOD", VIP_spending$disb_desc) == TRUE | 
                                        grepl("FOOD", VIP_spending$disb_desc) == TRUE | 
                                        grepl("FOOD", VIP_spending$disb_desc) == TRUE | 
                                        grepl("FOOD", VIP_spending$disb_desc) == TRUE | 
                                        grepl("CATERING", VIP_spending$disb_desc) == TRUE)
  
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
  print(paste0(Candidate_Last_Name, " likes to eat at: "))
  print(FoodSpending)
  
  #source
  print("Data available at: http://www.fec.gov/disclosurep/PDownload.do")
  print("Data downloaded: July 29, 2016")
  
}