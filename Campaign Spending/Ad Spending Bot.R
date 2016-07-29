#Where do candidates buy advertising, and how much do they spend?
#Function to analyze public campaign expenditure data and 
#determine how that candidate spends $ on ads

Ad_Spend_Bot <- function(Candidate_Last_Name){
  
  #Check for proper user input
  if(is.character(Candidate_Last_Name) == FALSE){
    stop("Please enter the candidate's last name with quotation marks around it.")
  }
  
  #Data download -- if statement to cut runtime on successive iterations
  if(exists("data_download") == FALSE){
    
    #Prep work
    library(RCurl)
    
    #So you know that its working
    print("Please wait - downloading data...")
    
    #Get that data
    cand_library <- getURL("https://raw.githubusercontent.com/djmirabito/Fun-Stuff/master/Campaign%20Spending/2016%20Campaign%20Spending.csv")
    campaign_expense <<- read.csv(text = cand_library)
    data_download <<- "Complete"
    
  }#end if data download incomplete
  
  #character swaps
  campaign_expense$disb_desc <- as.character(campaign_expense$disb_desc)
  Candidate_Last_Name <- as.character(Candidate_Last_Name)
  
  #subset for candidate of interest and ad words
  VIP_spending <- subset(campaign_expense, grepl(Candidate_Last_Name, campaign_expense$cand_nm) == TRUE)
  ad_spending <- subset(VIP_spending, grepl("ADS", VIP_spending$disb_desc) == TRUE | 
                                      grepl("ADVERTISEMENT", VIP_spending$disb_desc) == TRUE |
                                      grepl("ADVERTISING", VIP_spending$disb_desc) == TRUE |
                                      grepl("ADVERTISMENT", VIP_spending$disb_desc) == TRUE |
                                      grepl("ADVERTISNG", VIP_spending$disb_desc) == TRUE
                          )#close subset for advertising
  
  #aggregate spending by ad seller
  adad <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(adad) <- c("Place", "Amount.spent")
  
  #prep for loop
  cvb <- as.numeric(length(ad_spending$recipient_nm))
  ad_spending$recipient_nm <- as.character(ad_spending$recipient_nm)
  
  for (i in 1:cvb){
    
    #subset ad spending
    x <- ad_spending[i, 4]
    y <- subset(ad_spending, ad_spending$recipient_nm == x)
    z <- sum(y$disb_amt)
    
    #assign to adad dataframe
    adad[i, 1] <- x
    adad[i, 2] <- z
    
  }
  
  #remove duplicates
  adad$dup <- duplicated(adad$Place)
  FinalAd <- subset(adad, adad$dup == FALSE)
  FinalAd$dup <- NULL
  
  #order by amount spent
  FinalAd <- FinalAd[order(FinalAd$Amount.spent, decreasing = TRUE),]
  
  #Make table available and calculate total spending
  AdSpending <<- FinalAd
  
  #prep for loop
  nbv <- as.numeric(length(AdSpending$Amount.spent))
  total <- 0
  
  for(i in 1:nbv){
    
    xx <- AdSpending[i, 2]
    total <- total + xx
    
  } #end for loop
  
  #Results
  print(AdSpending, row.names = FALSE)
  print(paste0(Candidate_Last_Name, " has spent $", total, " on advertising."))
  print("Data available at: http://www.fec.gov/disclosurep/PDownload.do")
  print("Data downloaded: 7/29//2016")
  
}