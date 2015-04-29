best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
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
        }
        if(outcome == "heart failure") {
                y <- x[x$State == state, ] # Subsets data based on state
                # Convert column 17 to numeric and supress warnings
                suppressWarnings(y[, 17] <- as.numeric(y[, 17]))
                # Order data.frame by column 17 then 2 ascending with na values last
                y <- y[order(y[, 17], y[, 2], na.last = TRUE), ]
        }
        if (outcome == "pneumonia") {
                y <- x[x$State == state, ] # Subsets data based on state
                # Convert column 23 to numeric and supress warnings
                suppressWarnings(y[, 23] <- as.numeric(y[, 23]))
                # Order data.frame by column 23 then 2 ascending with na values last
                y <- y[order(y[, 23], y[, 2], na.last = TRUE), ]
        }
        # Return the top ranked hospital name
        y$Hospital.Name[1]
}