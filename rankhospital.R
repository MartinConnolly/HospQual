rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_validation <- c("heart attack", "heart failure", "pneumonia")
        # Check if this is a valid state, i.e. one listed in the data.frame
        if(!(state %in% x$State)) {
                stop("invalid state")
        }
        # Check if this is a valid outcome
        if(!(outcome %in% outcome_validation)) {
                stop("invalid outcome")
        }
        if(outcome == "heart attack") {
                y <- x[x$State == state, ] # Subsets data based on state
                # Convert column 11 to numeric and supress warnings
                suppressWarnings(y[, 11] <- as.numeric(y[, 11]))
                # Order data.frame by column 11 then 2 ascending with na values last
                y <- y[order(y[, 11], y[, 2], na.last = TRUE), ]
                y <- y[!is.na(y[, 11]), ]
                if(num == "best") {y$Hospital.Name[1]}
                else {
                        if(num == "worst") {y$Hospital.Name[length(y$Hospital.Name)]}
                        else {
                                if(num > length(y$Hospital.Name)) {NA}
                                else {y$Hospital.Name[num]}
                        }
                }
        }
}