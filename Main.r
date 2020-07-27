# INITIALIZATION

# number of hours in model run
size=500 

t_lines = list('203')
methods = list('') 

# MARS
# Linear regression
# Random forest
# kNN
# Support vector regression

input_file <- "Input.csv"
output_file <- "Output.csv"

#----------------------------------------------------------

# LIBRARIES
library(foreign)
library(lubridate)
library(caret)
library(Metrics)
library(ggplot2)
library(vip)
library(earth)
library(plyr)  
library(openxlsx)

source("functions.R")


#-----------------------------------

# PREPROCESSING

set.seed(123) 

# read_raw_data()

# load configuration data from input file
input_data = read_case_data(size, input_file)

# get input data and convert formats
covariates = input_data$covariates
covariates[,-1] = sapply(input_data$covariates[,-1], as.numeric)
lines = input_data$lines
lines[,-1] = sapply(input_data$lines[,-1], as.numeric)
#t_lines = input_data$t_lines
#methods = input_data$methods

# create empty dataframe 
results = data.frame(row.names = t_lines)

# --------------------------------------------------

# PREPARE DATA AND RUN MODELS

for (line in c(t_lines)){ 
    
    #add lagged line if not included in covariates and delete NAs
    covariates_l <- covariates
    covariates_l <- merge(covariates_l, lines[,c("Time",line)] ,by="Time")
    if (!(paste(line, ".lag", sep="") %in% names(covariates_l))){
        covariates_l <- merge(covariates_l, lagged(lines[,c("Time",line)]), by="Time")
    }
    covariates_l <- na.omit(covariates_l)
    
    # select training and test data
    trainingRowIndex <- sample(1:nrow(covariates_l), 0.7*nrow(covariates_l))  
    trainingData <- covariates_l[sample(trainingRowIndex, min(size, nrow(trainingRowIndex))), ]  
    testData  <- covariates_l[-trainingRowIndex, ] 
    
    # perform training and estimation
    for (method in methods){
        if (method == "MARS") {
            results[line, method] <- mars(line, trainingData, testData)
            
        } else if (method == "Linear regression") {
            results[line, method] <- lin_reg(line, trainingData, testData)
        
        } else if (method == "Random forest") {
            results[line, method] <- ran_forest(line, trainingData, testData)
            
        } else if (method == "Multiple linear regression") {
            print('Function is not implemented yet')
            
        } else if (method == "kNN") {
            results[line, method] <- kNN(line, trainingData, testData)
            
        } else if (method == "Support vector regression") {
            results[line, method] <- SVR(line, trainingData, testData)   
        }
    }
}


# ---------------------------------------

# OUTPUT PROCESSING

input <- read.csv(input_file, fileEncoding="UTF-8-BOM")

write.xlsx(
      input,
      input_file,
      sheetName = 'Input',
      col.names = TRUE,
      row.names = TRUE,
      append = TRUE,
    )

for (method in methods){
    write.xlsx(
      results[method],
      output_file,
      sheetName = String(method),
      col.names = TRUE,
      row.names = TRUE,
      append = TRUE,
    )
}


