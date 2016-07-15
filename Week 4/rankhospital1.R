rankhospital <- function(state, outcome, num = "best") {
        data_frame <-
                read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        if (!any(state == unique(data_frame[, 7])))
                stop("invalid state")
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia")))
                stop("invalid outcome")
        state_set <- data_frame[data_frame[, 7] == state ,]
        if (outcome == "heart attack")
                col <- 11
        if (outcome == "heart failure")
                col <- 17
        if (outcome == "pneumonia")
                col <- 23
        state_set[, col] <- as.numeric(state_set[, col])
        rate <- state_set[!is.na(state_set[, col]), ]
        if (is.numeric(num) == TRUE) {
                if (length(rate[, 2]) < num)
                        return(NA)
        }
        col_name <- names(rate)[col]
        hosp <- names(rate)[2]
        ordered <-
                rate[with(rate, order(rate[col_name], rate[hosp])),]
        if (is.character(num) == TRUE) {
                if (num == "best")
                        num <- 1
                else if (num == "worst")
                        num <- length(ordered[, col])
        }
        return(as.character(ordered[num, 2]))
}