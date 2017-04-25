if(!is.element("dplyr", installed.packages()[,1])) {
    install.packages("dplyr")
    library(dplyr)
} else {
    require(dplyr)  # loads dplyr if it hasn't been loaded already
}

rankall <- function(outcome, num = 'best') {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## For each state, find the hospital of the given rank
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # cache the unique states in the file to global env
    if(!exists("unique.states", mode = "character")) {
        unique.states <<- getUniqueStates("ASC")
    }
    # create the return data frame which we'll populate later
    hospitals <- rep(NA, length(unique.states))
    # create df instance that will be returned
    hospitalsByState <- data.frame(hospital = hospitals,
                                   state = unique.states,
                                   row.names = unique.states)
    dataOutcomeAllStates <- getAllDataByOutcome(outcome)
    dataColName <- getDataColName(outcome)
    for(i in seq_along(unique.states)) {
        stateVal <- unique.states[i]
        hospital <- NA
        # get data for just the state of interest
        data <- filter(dataOutcomeAllStates, State == stateVal)
        colNames <- c("Hospital.Name", "State", dataColName)
        dataSortedByRank <- data %>% arrange_(colNames[3], colNames[1])
        invalidRankMsg <- "invalid rank: num be 'best', 'worst' or whole number"
        if(is.numeric(num)) {
            # test for + integer: http://stackoverflow.com/questions/3476782/
            if(num %% 1 == 0) {
                if(num > nrow(dataSortedByRank)) {
                    next  # leave hospital value NA
                }
                else {
                    hospital <- dataSortedByRank[num, colNames[1]]
                }
            }
            else {
                stop(invalidRankMsg)
            }
        }
        if(num == "best") {
            hospital <- dataSortedByRank[1, colNames[1]]
        }
        if(num == "worst") {
            hospital <- dataSortedByRank[nrow(dataSortedByRank), colNames[1]]
        }
        hospitalsByState[i, "hospital"] <- hospital  # populate requested item
    }
    
    hospitalsByStateOrdered <- arrange(hospitalsByState, state)
    row.names(hospitalsByStateOrdered) <- hospitalsByStateOrdered$state
    return(hospitalsByStateOrdered)
}


getAllDataByOutcome <- function(outcome = "heart attack") {
    # read data file to df, select 3 relevant col's, convert data col
    # to numeric, filter out NAs, arrange (sort) by data col, and
    # then returns the df
    relevant.columns <- c("Hospital.Name", "State")  # only char cols needed
    
    options(warn = 1)  # deal with warnings generated from NAs in data
    
    dataCharacters <- read.csv("outcome-of-care-measures.csv",
                               colClasses = "character")[relevant.columns[1:2]]
    dataColName <- getDataColName(outcome)
    relevant.columns <- c(relevant.columns, dataColName)  # add data col
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
    
    # filter by state and remove NA values using dplyr and a data pipeline
    good <- data %>% na.omit() %>% filter(!is.na(dataColName))
    
    options(warn = 0)
    
    return(good)  # return data with NA values removed
}

getUniqueStates <- function(order = "NONE") {
    ## Reads the data file, grabs all the state entries and returns a vector
    ## of unique states that is either sorted or not depending on the value
    ## of the order parameter.  The order parameter has 3 valid values:
    ## NONE (default, vector is unsorted), "ASC" (sorted ascending), and
    ## "DESC" (sorted descending)
    allStates <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[["State"]]
    uniqueStates <- unique(allStates)
    if(order == "NONE") {
        return(uniqueStates)
    }
    if(order == "ASC") {
        return(uniqueStates[order(uniqueStates)])
    }
    if(order == "DESC") {
        return(uniqueStates[rev(order(uniqueStates))])
    }
    else {
        stop("invalid order: must be 'NONE' (default), 'ASC', or 'DESC'")
    }
}

isValidOutcome <- function(outcome) {
    ## Returns TRUE if outcome is: "heart attack", "heart failure", or
    ## "pneumonia". Returns FALSE otherwise
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    return(outcome %in% validOutcomes)
}

getDataColName <- function(outcome) {
    ## Returns the column name in the dataframe created from reading the file
    ## outcome-of-care-measures.csv corresponding to the value of outcome
    ## which can be one of three values:
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