if(!is.element("dplyr", installed.packages()[,1])) {
    #cat("installing dplyr...")
    install.packages("dplyr")
    library(dplyr)
} else {
    #cat("dplyr already installed.")
    require(dplyr)  # loads dplyr if it hasn't been loaded already
}

best <- function(state, outcome) {
    ## Reads outcome data, checks that state and outcome are valid, and
    ## returns a hospital name in that state with the lowest 30-day death rate.
    ## Ties are broken lexicographically: e.g. "Awesome Hospital" would come
    ## before "Fantastic Hospital"
    ##
    ## If state or outcome are invalid, "invalid state" or "invalid outcome"
    ## are returned respectively and funtion terminates.  If both are invalid,
    ## the "invalid state" error will be reported.
    
    # cache the unique states in the file to global env
    if(!exists("unique.states", mode = "character")) {
        unique.states <<- getUniqueStates()
    }
    
    if(!(state %in% unique.states)) {
        stop("invalid state")
    }
    
    if(!isValidOutcome(outcome)) {
        stop("invalid outcome")
    }
    
    data <- getRelevantData(state, outcome)
    # http://www.markhneedham.com/blog/2013/01/23/
    # r-ordering-rows-in-a-data-frame-by-multiple-columns/
    # to sort desc, can wrap items passed to order with rev(...)
    data <- data[order(data[,3], data[,1]), ]
    
    return(data[1, 1])
}

getRelevantData <- function(state = CO, outcome = "heart attack") {
    ## Returns a dataframe that is a subset of the data read from the file
    ## outcome-of-care-measures.csv for a particular state and outcome
    ## (specified in the input parameters). Data that is not available is
    ## filtered out of the returned dataframe.
    
    relevant.columns <- c("Hospital.Name", "State")  # only char cols needed
    dataColName <- getDataColName(outcome)
    relevant.columns <- c(relevant.columns, dataColName)  # add data col
    
    options(warn = 1)  # deal with warnings generated from NAs in data
    
    # start by reading in the two needed character fields
    dataCharacters <- read.csv("outcome-of-care-measures.csv",
                               colClasses = "character")[relevant.columns[1:2]]
    # read in the numeric col, read as characters initially because these
    # columns contain "Not Available" text for missing values
    dataNumeric <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")[relevant.columns[3]]
    v <- vector(mode = "numeric", length = nrow(dataNumeric))
    for(i in seq_along(dataNumeric[[1]])) {
        # populate vector v with the character data converted to numeric
        # (as.double call) then wrap with suppressWarning to avoid getting
        # "Warning: NAs introduced by coercion" messages in the console
        v[i] <- suppressWarnings(as.double(dataNumeric[i, 1]))
    }
    #data <- data.frame(dataCharacters, v)  # append numeric col w/o dplyr
    data <- mutate(dataCharacters, v)  # append numeric data col w/ dplyr
    names(data)[names(data)=="v"] <- dataColName  # rename appended data col
    
    # next 3 line filter by state and remove NA values w/o dplyr
#     data <- data[data$State == state, ]
#     goodRows <- !is.na(data[[3]])
#     goodData <- data[goodRows, ]
    # next lines does the same thing but using dplyr and a data pipeline
    good <- data %>% na.omit() %>% filter(State == state, !is.na(dataColName))

    options(warn = 0)
    
    return(good)  # return data with NA values removed
}

getUniqueStates <- function() {
    ## Reads the data file, grabs all the state entries and returns a vector
    ## of unique states
    allStates <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[["State"]]
    return(unique(allStates))
}

isValidOutcome <- function(outcome) {
    ## Returns TRUE if outcome is: "heart attack", "heart failure", or
    ## "pneumonia". Returns FALSE otherwise.
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    return(outcome %in% validOutcomes)
}

getDataColName <- function(outcome) {
    ## Returns the column name in the dataframe created from reading the file
    ## outcome-of-care-measures.csv corresponding to the value of outcome
    ## which can be one of 3 values:
    ## 'heart attack', 'heart failure', or 'pneumonia'
    colName <- NA
    # add the numeric column related to outcome
    if(outcome == 'heart attack') {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if(outcome == 'heart failure') {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else {
        # must be "pneumonia"
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    return(colName)
}