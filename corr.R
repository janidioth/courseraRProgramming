corr <- function(directory, threshold = 0) {
    wd <- paste(getwd(), "/", directory, sep="")
    my_files <- list.files(path=wd, pattern="*.csv", full.names = TRUE)
    
    id_range <- 1:332
    corr_vect <- c(NA)
    
    for (index in id_range) {
        dat <- read.csv(my_files[index])
        complete_obs <- sum(complete.cases(dat))
        
        if (complete_obs > threshold) {
            corr_value <- cor(dat$sulfate, dat$nitrate, use = "complete.obs")
            corr_vect <- c(corr_vect, corr_value)
        }
    }
    corr_vect <- corr_vect[!is.na(corr_vect)]
    return(corr_vect)
}