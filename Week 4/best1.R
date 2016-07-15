best <- function(state, outcome) {
        data_frame <-
                read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        if (!any(state == unique(data_frame[, 7])))
                stop("invalid state")
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia")))
                stop("invalid outcome")
        state_set <- data_frame[data_frame[, 7] == state , ]
        if (outcome == "heart attack")
                final <- as.numeric(state_set[, 11])
        if (outcome == "heart failure")
                final <- as.numeric(state_set[, 17])
        if (outcome == "pneumonia")
                final <- as.numeric(state_set[, 23])
        min <- min(final, na.rm = TRUE)
        result <- as.character(state_set[which(final == min), 2])
        if (length(result) > 1)
                return(result[1])
        else
                return(result)
}