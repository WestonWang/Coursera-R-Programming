rankhospital <- function (state, outcome, num = 'best'){
      
      temp <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')[, c(2,7,11,17,23)]
      colnames(temp) <- c('name', 'state','heart attack', 'heart failure', 'pneumonia')
      
      if (!state %in% temp$state) {stop ("Invalid state")}
      if (!outcome %in% names(temp)) {stop ("Invalid outcome")}
      
      t1 <- temp[temp$state == state, c('name', outcome)]
      t1[[outcome]] <- as.numeric(t1[[outcome]])
      
      t2 <- t1[complete.cases(t1) , ]
      t2 <- t2[order(t2[[outcome]], t2[['name']]), ]
      t3 <- cbind(t2, Rank = 1:nrow(t2))
      
      if (num == "best") { 
            ranking <- 1
      }else if (num == "worst"){
            ranking <- nrow(t2)
      }else {
            ranking <- as.numeric(num)
            if (ranking > nrow(t2)) {
                  return( NA )
            }
      }
      
      t3[ranking, 1]
             
}

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)
