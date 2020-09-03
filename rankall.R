rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", 
                   na.string="Not Available", stringsAsFactors=FALSE)
    
    ## Setup named vector to extract outcome column and check validity
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    ## Check that state and outcome are valid
    if(!(outcome %in% names(outcomes))) stop("invalid outcome")
    
    ## Subset data column 2, 7 and the outcome column using named vector
    df <- df[, c(2, 7, outcomes[outcome])]
    
    ## Rename the column name for df and remove NA data
    names(df) <- c("hospital", "state", "outcome")
    my_data <- na.omit(df)
    
    ## Sort my_data by state, outcome and hospital columns
    rankedData <- arrange(my_data, state, outcome, hospital)
    
    ## Split sorted data by state; Get 1st row of each state to list
    rankByState <- split(rankedData, rankedData$state)
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    rankHospitalFunction <- function(x) {
        if (num == "best") {
            rank <- 1
        } else if (num == "worst") {
            rank <- dim(x)[1]
        } else {
            rank <- as.numeric(num)
        }
        hospital <- x[rank,1:2]
        if (is.na(hospital$state))
            hospital <- c(NA, x[1,2])
        return (hospital)
    }
    result <- lapply(rankByState, rankHospitalFunction)
    return(do.call(rbind, result))
}