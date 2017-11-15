corr <- function(directory, threshold = 0){
      
      fn <- dir(directory, full.names = TRUE)
      
      data <- numeric()
      
      for (i in 1:332){
            temp <- read.csv(fn[i])
            temp <- temp[complete.cases(temp), c('sulfate','nitrate')]
            if (nrow(temp) > threshold){
                  cn <- cor(temp[1], temp[2])
                  data <- c(data, cn)
            } 
      }
      return(data)            
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)
