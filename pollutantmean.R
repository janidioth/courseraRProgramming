pollutantmean <- function(directory, pollutant, id = 1:332) {
    wd <- paste(getwd(), "/", directory, sep="")
    my_files <- list.files(path=wd, pattern="*.csv", full.names = TRUE)
    dat_csv <- ldply(my_files[id], read_csv)
    
    if (pollutant == "sulfate") {
        mean(dat_csv$sulfate, na.rm=TRUE)
    } else if (pollutant == "nitrate") {
        mean(dat_csv$nitrate, na.rm=TRUE)
    }
}

