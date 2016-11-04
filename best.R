best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    ## Also assign the column number of the outcome if the state and 
    ## outcome pass an integrity test
    outcome_options <- c("heart attack", "heart failure", "pneumonia")
    if((state %in% data$State) == FALSE)
        { stop("invalid state") }
    else if ((outcome %in% outcome_options) == FALSE)
        {stop("invalid outcome")}
    else{
        if (outcome == "heart attack")
            {o_col <- 11} 
        else if (outcome == "heart failure")
            {o_col <- 17}
        else
            {o_col <- 23}
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    ##is.na() returned errors but was able to filter out "Not Available"
    hospitals <- subset(data
                    , data$State == state & data[, o_col] != "Not Available")
    h <- hospitals[, c(2, o_col)]
    h_sort <- h[order(as.numeric(h[,2]), h[,1]),]
    h_sort[1,1]
}