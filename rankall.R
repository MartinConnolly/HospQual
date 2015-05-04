rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        source('C:/Users/martin.connolly/Documents/GitHub/HospQual/build_state_ranking.R')
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_validation <- c("heart attack", "heart failure", "pneumonia")
        output <- NA
        # Check if this is a valid outcome
        # Check if this is a valid outcome
        if(!(outcome %in% outcome_validation)) {
                stop("invalid outcome")
        }
        if(outcome == "heart attack") {
                # Convert column 11 to numeric and supress warnings
                suppressWarnings(x[, 11] <- as.numeric(x[, 11]))
                # Subset dataframe, just columns for Hospital Name, State and 
                # mortality for heart attack
                y <- x[, c(2, 7, 11)]
                names(y) <- c("hospital", "state", "Mortality.Rate.Heart.Attack")
                output <- build_state_ranking(y, num)
        }
        if(outcome == "heart failure") {
                # Convert column 17 to numeric and supress warnings
                suppressWarnings(x[, 17] <- as.numeric(x[, 17]))
                # Subset dataframe, just columns for Hospital Name, State and 
                # mortality for heart failure
                y <- x[, c(2, 7, 17)]
                names(y) <- c("hospital", "state", "Mortality.Rate.Heart.Attack")
                output <- build_state_ranking(y, num)
        }
        if(outcome == "pneumonia") {
                # Convert column 23 to numeric and supress warnings
                suppressWarnings(x[, 23] <- as.numeric(x[, 23]))
                # Subset dataframe, just columns for Hospital Name, State and 
                # mortality for heart failure
                y <- x[, c(2, 7, 23)]
                names(y) <- c("hospital", "state", "Mortality.Rate.Heart.Attack")
                output <- build_state_ranking(y, num)
        }
        row.names(output) <- output$state
        output
}