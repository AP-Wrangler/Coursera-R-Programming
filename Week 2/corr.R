corr <- function(directory, threeshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        frames <- complete(directory)
        vect <- frames$id[frames$nobs > threeshold]
        j <- 1
        result <- numeric()
        for (index in vect) {
                data <- read.csv(files[index])
                v1 <-
                        data$sulfate[!is.na(data$sulfate) &
                                             !is.na(data$nitrate)]
                v2 <-
                        data$nitrate[!is.na(data$sulfate) &
                                             !is.na(data$nitrate)]
                result[j] <- cor(v1, v2)
                j <- j + 1
        }
        return(result)
}

