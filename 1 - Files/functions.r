read_raw_data <- function(){
    
    start = 2016
    end <- 2020
    
    out = data.frame()
    for (year in (start:end)){
        for (semester in (1:2)){
            file = paste('2 - Data/Netzlast_', year, '_', semester, '.csv', sep = "", collapse = NULL)
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
    write.csv(out, "2 - Data/Networkload_0.csv")
    
    
    new_names <- gsub("Leitung.Nr..", "Line.", colnames(out))
    new_names <- gsub("Zeit", "Time", new_names)

    names(out)<- new_names

    write.csv(out, "2 - Data/Networkload_1.csv")
       
    for (c in col(out[1,])){
        for (r in (1:nrow(out))){
              out[r,c] <- gsub(" MW / grün", "", out[r,c])
              out[r,c] <- gsub(" MW / gelb", "", out[r,c])
              out[r,c] <- gsub(" MW / hoch", "", out[r,c])
        }
    }

    write.csv(out, "2 - Data/Lineload.csv")
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
    predictors <-read.csv("2 - Data/Predictors.csv", fileEncoding="UTF-8-BOM")
    predictors$Time <- as.POSIXct(predictors$Time, format = "%d/%m/%Y %H:%M")


    # -----------------------

    # Line data
    lines_raw <- read.csv("2 - Data/Lineload.csv", fileEncoding="UTF-8-BOM")

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
