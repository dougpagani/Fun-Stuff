#4pl Automater
#input needs to be in my_spinach format
#output: ED50 and plot of each row

#for testing:
file_location <- "C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Doug\\RAW_2.csv"

EC50_calc <- function(file_location){
  
  ###########
  #Prep Work#
  ###########
  
  #load package drc for model fitting - if error, install and then load
  tryCatch(library(drc), 
           error = function(e){
             install.packages("drc")
             library(drc)
           })
  
  #avoid auto-converting to factor when pulling in data frames
  options(stringsAsFactors = FALSE)
  
  #####################
  #Reading in raw data#
  #####################
  
  #pull .csv file from location specified by user
  raw <- read.csv(file_location)
  
  #keep only rows that are blank in the 1st and 2nd columns
  raw_sub1 <- subset(raw, raw$X..BLOCKS..9 == "" & raw$X == "")
  
  #keep only rows that contain real readings rather than the number labeling
  raw_sub2 <- subset(raw_sub1, raw_sub1$X.1 != 1.0000 & raw_sub1$X.2 != 2 & raw_sub1$X.3 != 3)
  
  #############################
  #Relabeling and reformatting#
  #############################
  
  #prep for loop
  raw_sub2_l <- as.numeric(length(raw_sub2$X..BLOCKS..9))
  plate_count <- 1
  index_count <- 1
  
  #loop through dataframe and label with plate and index numbers
  for(i in 1:raw_sub2_l){
    
    #label plate number
    raw_sub2[i, 1] <- plate_count
    
    #if 8 rows have been labeled, add one to plate count
    if(index_count %% 8 == 0){
      plate_count <- plate_count + 1
    }
    
    #label index number
    raw_sub2[i, 2] <- index_count
    
    #if 8 rows have been labeled, reset index to zero
    if(index_count %% 8 == 0){
      index_count <- 0
    }
    
    #add one to index each time loop completes
    index_count <- index_count + 1
    
  } #end plate and index number loop
  
  ###############
  #Getting Fancy#
  ###############
  
  #create a dataframe for each row of the matrix in order to fit a model to each one
  #first, shell dataframe
  shell <- data.frame(matrix(ncol = 4, nrow = 12))
  colnames(shell) <- c("Plate", "Index", "DF", "AU")
  shell$DF <- c(1000,	3000,	9000,	27000,	81000,	243000,	729000,	
                     2187000,	6561000,	19683000,	59049000,	177147000)
  
  #create a ton of dataframes to be fed to the model fitter
  for(i in 1:raw_sub2_l){
    
    #let's make the shell form available, but leave the original intact
    fshell <- shell
    
    #assign plate and row numbers
    fshell$Plate <- raw_sub2[i, 1]
    fshell$Index <- raw_sub2[i, 2]
    
    #convert the row of measurements to a column in the fshell
    msr_r <- raw_sub2[i, 3:14]
    msr_c <- as.data.frame(t(msr_r))
    
    for(j in 1:12){
      fshell[j, 4] <- msr_c[j, 1]
    }
    
    #create new dataframe object
    assign(paste0("obv", i), fshell)
    
  } #end dataframe-building loop
  
  ##########
  #Analysis#
  ##########
  
  #pull each dataframe and run it through the model fitter
  for(i in 1:raw_sub2_l){
    
    #get the dataframe and run it through the fitter
    booyah <- get(paste0("obv", i))
    boohoo <- drm(AU~DF, Index, data = booyah, fct = LL.4())
    
    #calculate EC50 and populate it to raw_sub2
    whoa <- ED(boohoo, 50)
    raw_sub2[i, 15] <- whoa[1]
    
    #create new model object for plotting
    assign(paste0("model", i), boohoo)
    
  } #end model fitter loop
  
} #end function



