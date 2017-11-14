complete <- function( directory, id = 1:332){
      
      fn <- dir(directory, full.names = TRUE)
      
      data <- data.frame()
      
      for (i in id){
            
            num <- sum(complete.cases(read.csv(fn[i]))/1)
            
            data <- rbind(data, cbind(i, num))
      }
      
      return(data)
      
}

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
