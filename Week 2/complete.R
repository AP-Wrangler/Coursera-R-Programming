complete <- function(directory, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        i <- 1
        v <- numeric(length(id))
        for (index in id)
        {
                data <- read.csv(files[index])
                v[i] <-
                        length(data$sulfate[!is.na(data$sulfate) &
                                                    !is.na(data$nitrate)])
                i <- i + 1
        }
        frames <- data.frame(id = id, nobs = v)
        return(frames)
}