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



# fill in other methods here...



#----------------------------------
#help functions

#Introducing lagged values (default is 24h time lag)
lagged <- function(df, lag=24){
    # lag needs to be specified in hours timelag
    lagged_data <- df
    colnames(lagged_data) <- paste(colnames(lagged_data), ".lag", sep="")
    names(lagged_data)[names(lagged_data) == "Time.lag"] <- "Time"
    lagged_data$Time <- lagged_data$Time + 60*60 * lag
    return(lagged_data)
}
