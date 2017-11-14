pollutantmean <- function(directory, pollutant, id = 1:332) {
      
      fn <- dir('specdata', full.names = TRUE)
      
      data <- numeric()
      
      for (i in id) {
            
            temp = read.csv(fn[i])[[pollutant]]
            
            data <- c(data, temp)
      }
      
      return(mean(data, na.rm = TRUE))
}


pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)
