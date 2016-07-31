#Compare Trump and Hillary campaign spending according to given search terms
#Date: July 31, 2016

cand_comp <- function(search1 = 0, search2 = 0, search3 = 0){
  
  ###############
  #Data Download#
  ###############
  
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
  
  #####################
  #Reformat and subset#
  #####################
  
  #character swap
  campaign_expense$disb_desc <- as.character(campaign_expense$disb_desc)
  
  #table for each candidate, subset by search terms
  Trump_spending <- subset(campaign_expense, grepl("Trump", campaign_expense$cand_nm) == TRUE)
  Clinton_spending <- subset(campaign_expense, grepl("Clinton", campaign_expense$cand_nm) == TRUE)
  
  #one search term
  if(search1 != 0 & search2 == 0 & search3 == 0){
    
    T_sub_spend <- subset(Trump_spending, grepl(search1, Trump_spending$disb_desc) == TRUE )
    C_sub_spend <- subset(Clinton_spending, grepl(search1, Clinton_spending$disb_desc) == TRUE )
                            
  } #end if one search term
  
  
  #two search terms
  if(search1 != 0 & search2 != 0 & search3 == 0){
    
    T_sub_spend <- subset(Trump_spending, grepl(search1, Trump_spending$disb_desc) == TRUE |
                                          grepl(search2, Trump_spending$disb_desc) == TRUE )
    
    C_sub_spend <- subset(Clinton_spending, grepl(search1, Clinton_spending$disb_desc) == TRUE |
                                            grepl(search2, Clinton_spending$disb_desc) == TRUE )
    
  } #end if two search terms
  
  
  #three search terms
  if(search1 != 0 & search2 != 0 & search3 != 0){
  
  T_sub_spend <- subset(Trump_spending, grepl(search1, Trump_spending$disb_desc) == TRUE | 
                                        grepl(search2, Trump_spending$disb_desc) == TRUE | 
                                        grepl(search3, Trump_spending$disb_desc) == TRUE )
  
  C_sub_spend <- subset(Clinton_spending, grepl(search1, Clinton_spending$disb_desc) == TRUE | 
                                          grepl(search2, Clinton_spending$disb_desc) == TRUE | 
                                          grepl(search3, Clinton_spending$disb_desc) == TRUE )
  
  } #end if three search terms
  
  #################################################
  #Aggregate spending by who the money was paid to#
  #################################################
  
  ################
  #TRUMP Campaign#
  ################
  
  #aggregate spending by seller
  adad <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(adad) <- c("Party", "Amount.spent")
  
  #prep for loop
  cvb <- as.numeric(length(T_sub_spend$recipient_nm))
  T_sub_spend$recipient_nm <- as.character(T_sub_spend$recipient_nm)
  
  for (i in 1:cvb){
    
    #starting message
    if(i == 1){
      print("Compiling Trump campaign spending data according to search terms...")
    }
    
    #subset T_sub_spend
    x <- T_sub_spend[i, 4]
    y <- subset(T_sub_spend, T_sub_spend$recipient_nm == x)
    z <- sum(y$disb_amt)
    
    #assign to adad dataframe
    adad[i, 1] <- x
    adad[i, 2] <- z
    
    #print status 
    if(i %% 25 == 0){
      print(paste0("Aggregating row ", i, " of ", cvb, "..."))
    }
    
    #print completion
    if(i == cvb){
      print("Aggregation of Trump campaign spending complete.")
    }
    
  }
  
  #remove duplicates
  adad$dup <- duplicated(adad$Party)
  FinalTSpend <- subset(adad, adad$dup == FALSE)
  FinalTSpend$dup <- NULL
  
  #order by amount spent
  FinalTSpend <- FinalTSpend[order(FinalTSpend$Amount.spent, decreasing = TRUE),]
  
  ##################
  #CLINTON Campaign#
  ##################
  
  #aggregate spending by seller
  adad <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(adad) <- c("Party", "Amount.spent")
  
  #prep for loop
  cvb <- as.numeric(length(C_sub_spend$recipient_nm))
  C_sub_spend$recipient_nm <- as.character(C_sub_spend$recipient_nm)
  
  for (i in 1:cvb){
    
    #starting message
    if(i == 1){
      print("Compiling Clinton campaign spending data according to search terms...")
    }
    
    #subset C_sub_spend
    x <- C_sub_spend[i, 4]
    y <- subset(T_sub_spend, C_sub_spend$recipient_nm == x)
    z <- sum(y$disb_amt)
    
    #assign to adad dataframe
    adad[i, 1] <- x
    adad[i, 2] <- z
    
    #print status 
    if(i %% 25 == 0){
      print(paste0("Aggregating row ", i, " of ", cvb, "..."))
    }
    
    #print completion
    if(i == cvb){
      print("Aggregation of Clinton campaign spending complete.")
    }
    
  }
  
  #remove duplicates
  adad$dup <- duplicated(adad$Party)
  FinalCSpend <- subset(adad, adad$dup == FALSE)
  FinalCSpend$dup <- NULL
  
  #order by amount spent
  FinalCSpend <- FinalCSpend[order(FinalCSpend$Amount.spent, decreasing = TRUE),]
  
  #############################
  #Generate summary statistics#
  #############################
  
  #Make tables available 
  Trump <<- FinalTSpend[is.na(FinalTSpend$Amount.spent) == FALSE, ]
  Clinton <<- FinalCSpend[is.na(FinalCSpend$Amount.spent) == FALSE, ]
  
  #calculate total spending - TRUMP
  #prep for loop
  nbv <- as.numeric(length(Trump$Amount.spent))
  total_T <- 0
  
  for(i in 1:nbv){
    
    xx <- Trump[i, 2]
    total_T <- total_T + xx
    
  } #end for loop
  
  #calculate total spending - CLINTON
  #prep for loop
  nbv <- as.numeric(length(Clinton$Amount.spent))
  total_C <- 0
  
  for(i in 1:nbv){
    
    xx <- Clinton[i, 2]
    total_C <- total_C + xx
    
  } #end for loop
  
  #results
  fgh <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(fgh) <- c("Candidate", "$")
  fgh[1,1] <- "Trump"
  fgh[2,1] <- "Clinton"
  fgh[1,2] <- total_T
  fgh[2,2] <- total_C
  fgh$Candidate <- as.factor(fgh$Candidate)
  
  #printing....
  plot(fgh$Candidate, fgh$`$`, type = "h", ylab = "Spending ($)")
  print(paste0("Trump has spent $", total_T, " on goods/services associated with your search terms."))
  print(paste0("Clinton has spent $", total_C, " on goods/services associated with your search terms."))
  print("Data available at: http://www.fec.gov/disclosurep/PDownload.do")
  print("Data downloaded: 7/29//2016")
  
} #end function