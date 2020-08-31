complete <- function(directory, id = 1:332) {
    wd <- paste(getwd(), "/", directory, sep="")
    my_files <- list.files(path=wd, pattern="*.csv", full.names = TRUE)
    
    id_vect <- c(NA)
    nobs_vect <- c(NA)
    
    for (index in id) {
        dat <- read.csv(my_files[index])
        id_vect <- c(id_vect, index)
        nobs_vect <- c(nobs_vect, sum(complete.cases(dat)))
    }  
    id_vect <- id_vect[!is.na(id_vect)]
    nobs_vect <- nobs_vect[!is.na(nobs_vect)]
    
    df <- setNames(data.frame(id_vect, nobs_vect), c("id", "nobs"))
    return(df)
}