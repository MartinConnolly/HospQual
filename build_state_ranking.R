build_state_ranking <- function(dataframe, rank) {
        states <- levels(factor(dataframe$state))
        n <- length(states)
        output <- data.frame(matrix(NA, nrow = length(states), ncol = 2))
        names(output) <- c("hospital", "state")
        for(i in 1:n) {
                state_code <- states[i]
                y <- dataframe[dataframe$state == state_code, ]
                y <- y[order(y[, 3], y[, 1], na.last = TRUE), ]
                # Remove rows where column 3 contains NA
                y <- y[!is.na(y[, 3]), ]
                 if(rank == "best") {
                        output[i, ] <- y[1, 1:2]
                        } else {
                                if(rank == "worst") {
                                        output[i, ] <- y[length(y$hospital), 1:2]
                                        } else {
                                                if(rank > length(y$hospital)) {
                                                        output[i, ] <- c(NA, state_code)
                                                        } else {
                                                                output[i, ] <- y[rank, 1:2]
                                                                }
                                                }
                                }           
                }
        # names(output) <- c("hostpital", "state")
        output
        }