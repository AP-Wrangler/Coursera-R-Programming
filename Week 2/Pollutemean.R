pollutantmean <- function(directory, pollutant, id = 1:332) {
        means <- numeric()
        files <- list.files(directory, full.names = TRUE)
        i <- 1
        if (pollutant == "sulfate") {
                for (index in id) {
                        data <- read.csv(files[index])
                        means <-
                                c(means,data$sulfate[!is.na(data$sulfate)])
                }
        }
        else if (pollutant == "nitrate") {
                for (index in id) {
                        data <- read.csv(files[index])
                        means <-
                                c(means,data$nitrate[!is.na(data$nitrate)])
                }
        }
        mean(means)
}