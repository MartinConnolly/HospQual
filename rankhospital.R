rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        ## Note that I'm using column numbers because the names of the columns
        ## are way too long with the exception of Hospital.Name column
        ## For info
        ## col 11 is "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        ## col 17 is "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        ## col 23 is "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        ##
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_validation <- c("heart attack", "heart failure", "pneumonia")
        output <- NA
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
                # Remove rows where column 11 contains NA
                y <- y[!is.na(y[, 11]), ]
                if(num == "best") {
                        output <- y$Hospital.Name[1]
                } else {
                        if(num == "worst") {
                                output <- y$Hospital.Name[length(y$Hospital.Name)]
                        } else {
                                if(num > length(y$Hospital.Name)) {
                                        output <-NA
                                } else {output <- y$Hospital.Name[num]}
                        }
                }
        }
        if(outcome == "heart failure") {
                y <- x[x$State == state, ] # Subsets data based on state
                # Convert column 17 to numeric and supress warnings
                suppressWarnings(y[, 17] <- as.numeric(y[, 17]))
                # Order data.frame by column 17 then 2 ascending with na values last
                y <- y[order(y[, 17], y[, 2], na.last = TRUE), ]
                # Remove rows where column 17 contains NA
                y <- y[!is.na(y[, 17]), ]
                if(num == "best") {
                        output <- y$Hospital.Name[1]
                } else {
                        if(num == "worst") {
                                output <- y$Hospital.Name[length(y$Hospital.Name)]
                        } else {
                                if(num > length(y$Hospital.Name)) {
                                        output <-NA
                                } else {output <- y$Hospital.Name[num]}
                        }
                }
        }
        if(outcome == "pneumonia") {
                y <- x[x$State == state, ] # Subsets data based on state
                # Convert column 23 to numeric and supress warnings
                suppressWarnings(y[, 23] <- as.numeric(y[, 23]))
                # Order data.frame by column 23 then 2 ascending with na values last
                y <- y[order(y[, 23], y[, 2], na.last = TRUE), ]
                # Remove rows where column 23 contains NA
                y <- y[!is.na(y[, 23]), ]
                if(num == "best") {
                        output <- y$Hospital.Name[1]
                } else {
                        if(num == "worst") {
                                output <- y$Hospital.Name[length(y$Hospital.Name)]
                        } else {
                                if(num > length(y$Hospital.Name)) {
                                        output <-NA
                                } else {output <- y$Hospital.Name[num]}
                        }
                }
        }
        output
}