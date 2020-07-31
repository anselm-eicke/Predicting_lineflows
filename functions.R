library(plyr)

# Preprocessing

#--------------------------
# preprocessline data
preprocess_line_data <- function(){
  
  start = 2016
  end <- 2020
  
  out = data.frame()
  for (year in (start:end)){
    for (semester in (1:2)){
      file = paste('Data/Netzlast_', year, '_', semester, '.csv', sep = "", collapse = NULL)
      print(file)
      print(file.exists(file))
      if (!file.exists(file)){next}
      raw <- read.csv(file, fileEncoding="UTF-8-BOM")
      
      raw$Zeit <- as.POSIXct(raw$Zeit, format = "%d.%m.%Y %H:%M")
      
      if (nrow(out) == 0){
        out <- raw
      }else{
        out <- rbind.fill(out, raw)
      }     
    }       
  }
  # for testing purposes
  # write.csv(out, "Data/Networkload_0.csv")
  
  
  new_names <- gsub("Leitung.Nr..", "Line.", colnames(out))
  new_names <- gsub("Zeit", "Time", new_names)
  
  names(out)<- new_names
  
  # for testing purposes
  #write.csv(out, "Data/Networkload_1.csv")
  
  for (c in col(out[1,])){
    for (r in (1:nrow(out))){
      out[r,c] <- gsub(" MW / grün", "", out[r,c])
      out[r,c] <- gsub(" MW / gelb", "", out[r,c])
      out[r,c] <- gsub(" MW / hoch", "", out[r,c])
    }
  }
  
  write.csv(out, "Data/Lineload.csv")
}


read_covariates <- function(){
  raw <- read.csv(file = 'Data/Raw Data/Forecasted_generation.csv', fileEncoding="UTF-8-BOM")
  
  raw$Datetime <- paste(raw$Datum, raw$Uhrzeit)
  raw$Datetime <- as.POSIXct(raw$Datetime, format = "%d.%m.%Y %H:%M")
  
  raw$Datetime2 <- trunc(raw$Datetime, units = "hours")
  
  newdata <- cast(md, formula, FUN)
  aggregated_data <- cast(raw, by = list(raw$Gesamt.MWh.), 
                               FUN=sum) 
}
  
library(dplyr)
library(lubridate)
  raw %>% group_by(hour=raw$Datetime2)  %>% summarize(Gesamt.MWh.=sum(Gesamt.MWh.))

  aggregated_data <- aggregate(raw, 
                               by = list(raw$Datetime2), 
                               FUN=mean) 
}

read_case_data <- function(size, input_file){
  #size - Maximal size of testdata
  
  # Load input file
  input <- read.csv(input_file, fileEncoding="UTF-8-BOM")
  
  # Bring data from input csv sheet into convenient form
  
  #1. Methods
  tmp <- input[,c('Methods', 'Methods.1')]
  methods <- tmp[which(tmp$Methods.1 > 0), c('Methods')]
  
  #2. Independent.variables
  tmp <- input[,c('Independent.variables', 'Independent.variables.1')]
  ind_var <- tmp[which(tmp$Independent.variables.1 > 0), c('Independent.variables')]
  
  #3. Tested lines
  tmp <- input[,c('Tested.lines', 'Tested.lines.1')]
  t_lines <- tmp[which(tmp$Tested.lines.1 > 0), c('Tested.lines')]
  
  
  # -----------------------
  
  #Predictor data (all but lineflows)
  predictors <-read.csv("Data/Predictors.csv", fileEncoding="UTF-8-BOM")
  predictors$Time <- as.POSIXct(predictors$Time, format = "%d/%m/%Y %H:%M")
  
  
  # -----------------------
  
  # Line data
  lines_raw <- read.csv("Data/Lineload.csv") #, fileEncoding="UTF-8-BOM"
  
  lines_raw$Time <- as.POSIXct(lines_raw$Time, format = "%Y-%m-%d %H:%M:%S")
  
  lines_raw = subset(lines_raw, select =-c(X))
  
  # Delete lines with more than 300 missing values
  mv = colSums(is.na(lines_raw))
  lines_with_missing_values = colnames(lines_raw)[which(mv > 400)]
  print('The following lines will be excluded because they have more than 400 missing values')
  print(lines_with_missing_values)
  lines <- lines_raw[, !(names(lines_raw) %in% lines_with_missing_values)]
  
  #Delete remaining missing values
  lines <- na.omit(lines)
  
  # -----------------------
  
  #Introducing lagged values
  covariates <- merge(predictors[, c('Time', ind_var)],lagged(lines),by="Time")
  covariates <- na.omit(covariates)
  covariates[ind_var] <- sapply(covariates[ind_var], as.numeric)
  
  results <- list("covariates" = covariates, "lines" = lines, "t_lines" = t_lines, "methods" = methods)
  
  return(results)
}


#---------------------------------------------

# Models

mars <- function(line, trainingData, testData){
  
  #Build MARS model
  marsMod <- train(
    form = reformulate(".", line),
    data = trainingData,
    method = "earth",
    preProcess= c("center", "scale")
  )
  
  #Make predictions
  Pred <- predict(marsMod, testData)
  
  #Evaluate predictions
  results = rmse(Pred, testData[ ,line])
  return(results)
}

lin_reg <- function(line, trainingData, testData){
  
  #Build linear regression model
  lmMod <- train(
    form = reformulate(".", line),
    data = trainingData,
    method = "lm",
    preProcess= c("center", "scale")
  )
  
  #Make predictions
  Pred <- predict(lmMod, testData)
  
  #Evaluate predictions
  results = rmse(Pred, testData[ ,line])
  return(results)
}

ran_forest <- function(line, trainingData, testData){
  
  #Build random forest model
  rfMod <- train(
    form = reformulate(".", line),
    data = trainingData,
    method = "ranger",
    importance = 'impurity'
  )
  
  #Make predictions
  Pred <- predict(rfMod, testData)
  
  #Evaluate predictions
  results = rmse(Pred, testData[ ,line])
  return(results)
}

kNN <- function(line, trainingData, testData){
  
  #Build kNN model
  knnMod <- train(
    form = reformulate(".", line),
    data = trainingData,
    method = "knn",
    preProcess= c("center", "scale")
  )
  
  #Make predictions
  Pred <- predict(knnMod, testData)
  
  #Evaluate predictions
  results = rmse(Pred, testData[ ,line])
  return(results)
}

SVR <- function(line, trainingData, testData){
  
  #Build MARS model
  svrMod <- train(
    form = reformulate(".", line),
    data = trainingData
    # method = , # seems to be missing here!
    
  )
  
  #Make predictions
  Pred <- predict(svrMod, testData)
  
  #Evaluate predictions
  results = rmse(Pred, testData[ ,line])
  return(results)
}


#Determining lagged values (default is 24h time lag)
lagged <- function(df, lag=24){
  # lag needs to be specified in hours timelag
  lagged_data <- df
  colnames(lagged_data) <- paste(colnames(lagged_data), ".lag", sep="")
  names(lagged_data)[names(lagged_data) == "Time.lag"] <- "Time"
  lagged_data$Time <- lagged_data$Time + 60*60 * lag
  return(lagged_data)
}