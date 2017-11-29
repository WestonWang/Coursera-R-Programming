rankall <- function(outcome, num = "best") {
      dd <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")[, c(2,7,11,17,23)]
      names(dd)<- c('name', 'state','heart attack', 'heart failure', 'pneumonia')
      
      if (!outcome %in% names(dd)) {stop ("Invalid outcome")}
      
      t1 <- dd[ , c("name" , "state" , outcome)]
      
      t1$state <- as.factor(t1$state)
      t1[ , outcome] <- suppressWarnings(as.numeric(t1[ , outcome]))
      t1 <- t1[complete.cases(t1[ , outcome]) , ]
      fa<- data.frame(hospital = numeric(0), state = numeric(0))
      
      for( i in levels(t1$state) ){
            
            c <- t1[t1$state == i, ]
            c1 <- c[order(c[, outcome], c[, "name"]) , ]
            ranking <- 0
            if (num == "best") { 
                  ranking <- 1
            }else if (num == "worst"){
                  ranking <- as.numeric(nrow(c))
            }else {
                  ranking <- as.numeric(num)
            }
            
            fa <- rbind(fa, cbind(hospital = c1$name[ranking] , state = i))
            
      }
      for ( i in 1:54) {
            row.names(fa)[i] <- levels(t1$state)[i] ## change row names one row by one row
      }
      
      fa
}

head(rankall("heart attack", 20) , 10)

tail(rankall("heart failure") , 10)

tail(rankall("pneumonia", "worst") , 3)
