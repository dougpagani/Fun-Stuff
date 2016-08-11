#Function to take raw data, fit a 4pl model for each row, calculate the EC50 and create a pdf of plots
#Arguments: read_location - provide the file path to the raw data (should always end in .csv)
#           save_location - provide file path to a folder where you would like the .csv and .pdf saved

#for testing:
read_location <- "C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Doug\\RAW_2.csv"
save_location <- "C:\\Users\\Dean\\Documents\\GitHub\\Fun-Stuff\\Doug"

EC50_calc <- function(read_location, save_location){
  
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
  
  #message to user
  print(paste0("Reading in the raw data from ", read_location))
  
  #pull .csv file from location specified by user
  raw <- read.csv(read_location)
  
  #message to user
  print("Raw data successfully loaded.")
  
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
  
  #report plate count to user
  print(paste0("Model fitting will be completed for ", (plate_count-1), " plates."))
  
  #####################################################################
  #Create dataframe objects in standard format to feed to model fitter#
  #####################################################################
  
  #create a dataframe for each row of the matrix in order to fit a model to each one
  #first, shell dataframe
  shell <- data.frame(matrix(ncol = 4, nrow = 12))
  colnames(shell) <- c("Plate", "Index", "DF", "AU")
  shell$DF <- c(1000, 3000, 9000, 27000, 81000, 243000, 729000, 
                2187000, 6561000, 19683000, 59049000, 177147000)
  
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
  
  print("Fitting models...")
  
  #pull each dataframe and run it through the model fitter
  for(i in 1:raw_sub2_l){
    
    #get the dataframe and run it through the fitter
    booyah <- get(paste0("obv", i))
    boohoo <- drm(AU ~ DF, Index, data = booyah, fct = LL.4())
    
    #calculate EC50 and populate it to raw_sub2
    whoa <- ED(boohoo, 50)
    raw_sub2[i, 15] <- whoa[1]
    
    #create new model object for plotting
    assign(paste0("model", i), boohoo)
    
    #report progress to user
    if(i %% 10 == 0){
      print(paste0("Fitting model for row ", i, " of ", raw_sub2_l))
    }
    
    #final progress report
    if(i == raw_sub2_l){
      print("Model fitting and EC50 calculation complete.")
    }
    
  } #end model fitter loop
  
  #######################
  #Final output creation#
  #######################
  
  #####
  #CSV#
  #####
  
  #Match multiple measurements to move EC50's back to raw data file
  for(i in 1:raw_sub2_l){
    
    #get three measurements to match to original file
    rrr <- raw_sub2[i, ]
    t1 <- rrr[1, 4]
    t2 <- rrr[1, 5]
    t3 <- rrr[1, 6]
    
    #what row in the original file has these same values?
    sol <- as.numeric(which(raw[,4] == t1 & raw[,5] == t2 & raw[,6] == t3))
    
    #EC50 calculated for row
    key <- raw_sub2[i, 15]
    
    #move it to raw
    raw[sol, 15] <- key
  }
  
  #make all NA values look blank for Excel sheet
  raw[is.na(raw)] <- " "
  
  #write it out
  write.csv(raw, paste0(save_location, "\\Output.csv"), row.names = FALSE)
  
  #####
  #PDF#
  #####
  
  #for titling each page, let's reset these variables
  plate_count <- 1
  index_count <- 1
  
  #open and develop the pdf
  pdf(file = paste0(save_location, "\\Plots.pdf"))
  for(i in 1:raw_sub2_l){
    
    #get a model
    vip <- get(paste0("model", i))
    
    #recalculate EC50 and add it to the title of the plot
    piv <- ED(vip, 50)
    title <- paste0("Plate ", plate_count, ", Row ", index_count, " (EC50 = ", piv[1], ")")
    plot(vip, main = title)
    
    #if 8 rows have been labeled, reset index to zero
    if(index_count %% 8 == 0){
      index_count <- 0
      plate_count <- plate_count + 1
    }
    
    #add one to index each time loop completes
    index_count <- index_count + 1
    
  }
  
  dev.off()
  
  #final message
  print(paste0("Function complete - files saved to ", save_location))
  
} #end function



