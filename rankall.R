rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcome_options <- c("heart attack", "heart failure", "pneumonia")
    if ((outcome %in% outcome_options) == FALSE)
        {stop("invalid outcome")}
    else{
        if (outcome == "heart attack")
            {o_col <- 11} 
        else if (outcome == "heart failure")
            {o_col <- 17}
        else
            {o_col <- 23}
    }
    
    
    ## For each state, find the hospital of the given rank
    hospitals <- subset(data, data[, o_col] != "Not Available")
    h <- split(hospitals[, c(2, 7, o_col)], hospitals$State)
    h_sort <- lapply(h, function(x){
        x <- x[order(as.numeric(x[,3]), x$Hospital.Name),]
        
        #Translate string entered in num param to numeric
        if(as.character(num) == "best")
        {n = 1}
        else if (as.character(num) == "worst")
        {n = nrow(x)}
        else
        {n = as.numeric(num)}
        
        # Get first 2 columns of row n for each state
        x <- x[n, 1:2]
    })
    

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    do.call(rbind, h_sort)
}
