best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", 
                   na.string="Not Available", stringsAsFactors=FALSE)
    
    ## Setup named vector to extract outcome column and check validity
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    ## Check that state and outcome are valid
    if(!(state %in% df$State)) stop("invalid state")
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

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    result <- lapply(rankByState, function(x) { x[1,1] })
    return(result[state])
}
