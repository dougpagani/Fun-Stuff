#4pl Automater
#input needs to be in my_spinach format
#output: ED50 and plot of each row


###########
#Prep Work#
###########
install.packages("drc")
library(drc)

#########################
#Creating the data table#
#########################

#sample (ideal) data
my_spinach <- data.frame(matrix(ncol = 4, nrow = 12))
colnames(my_spinach) <- c("Plate", "Row","DF", "AU")
my_spinach$AU <- c(1.8809,	1.8334,	1.8246,	1.686,	1.1934,	0.641,	
                   0.2614,	0.1327,	0.07,	0.0632,	0.076,	0.0663)
my_spinach$DF <- c(1000,	3000,	9000,	27000,	81000,	243000,	729000,	
                   2187000,	6561000,	19683000,	59049000,	177147000)
my_spinach[1:12,1] <- 1

#######################################
#Fit model and calculate EC50 and plot#
#######################################

model1 <- drm(AU~DF, Row, data = my_spinach, fct = LL.4())

ED(model1, 50)

plot(model1)

