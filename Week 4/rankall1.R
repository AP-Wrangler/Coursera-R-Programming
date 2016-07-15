rankall <- function(outcome, num = "best") {
        data <-
                read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia")))
                stop("invalid outcome")
        if (FALSE) {
                if (outcome == "heart attack")
                        col <- 11
                if (outcome == "heart failure")
                        col <- 17
                if (outcome == "pneumonia")
                        col <- 23
        }
        states <- unique(data[, 7])
        hosp_name <- character()
        state_name <- character()
        for (i in seq_len(length(states))) {
                hosp_name <- c(hosp_name,
                               rankhospital(as.character(states[i]), outcome, num))
                state_name <- c(state_name, as.character(states[i]))
                if (FALSE) {
                        state_data <- data[data[, 7] == states[i], ]
                        state_data[, col] <-
                                as.numeric(state_data[, col])
                        rate <-
                                state_data[!is.na(state_data[, col]), ]
                        if (is.numeric(num)) {
                                if (length(rate[, 2]) < num)
                                        return(NA)
                        }
                        col_name <- names(rate)[col]
                        hosp <- names(rate)[2]
                        ordered <-
                                rate[with(rate, order(rate[col], rate[hosp])), ]
                        if (is.character(num) == TRUE) {
                                if (num == "best")
                                        num <- 1
                                else if (num == "worst")
                                        num <-
                                                length(ordered[, col])
                        }
                        hosp_name <-
                                c(hosp_name, as.character(ordered[num, 2]))
                }
        }
        frame <-
                data.frame(Hospital = hosp_name, State = state_name)
        return(frame)
}