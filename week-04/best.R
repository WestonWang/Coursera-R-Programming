best <- function (state, outcome){
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      temp <- read.csv("outcome-of-care-measures.csv")[c(2,7,11,17,23)]
      colnames(temp) <- c('name','state','heart attack','heart failure','pneumonia')
      
      if (!state %in% temp$state ) {stop('Invalid State')}
      if (!outcome %in% names(temp)) {stop('Invalid Outcome')}

      t1 <- temp[temp$state == state, ][c('name', 'state', outcome)] 
      
      t1[[outcome]] <- suppressWarnings(as.numeric(as.character(t1[[outcome]])))
      
      t1 <- t1[complete.cases(t1), ]
      
      t2 <- t1[t1[[outcome]] == min(t1[[outcome]]), ]
      
      t2 <- t2[order(t2[[1]]), ]

      return(t2[1])
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


best <- function(state, outcome) {
      data <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")[, c(2,7,11,17,23)]
      names(data)<- c('name', 'state','heart attack', 'heart failure', 'pneumonia')
      
      
      if (!state %in% data$state) {stop ("Invalid state")}
      if (!outcome %in% names(data)) {stop ("Invalid outcome")}
      
      
      if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia"  ) {
            
            ## find the death rate for every hospital for a given state
            ## convert character to numbers
            ## find the minimum death rate and then convert back to character (1 decimal) 
            ## find list of hospitals and choose hospital names base alphabetical order
            
            
            temp = suppressWarnings(as.numeric(data[data$state == state, outcome]))
            
            temp1 = min(temp, na.rm = TRUE)
            
            mdRate = sprintf("%.1f", temp1)
            
            hName = sort(data[data$state == state & data[outcome] == mdRate, "name"])[1]
      }
      
      
      hName
}
