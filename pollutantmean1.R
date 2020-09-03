pollutantmean1 <- function(directory, pollutant, id = 1:332) {
    wd <- paste("./", directory, sep="")
    my_files <- list.files(path=wd, pattern="*.csv", full.names = TRUE)[id]
    dat_csv <- lapply(my_files,function(x) read.csv(x)[[pollutant]])
    
    pollutant_dat <- unlist(dat_csv)
    mean(pollutant_dat, na.rm=TRUE)
}