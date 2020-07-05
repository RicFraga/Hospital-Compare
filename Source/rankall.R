# This file contains functions to rank a hospital in all states given an
# outcome and a num for rank

rankall <- function(outcome, num = 'best') {
     
     # First we read the data
     data <- read_file('care measures')
     
     # Getting the name of the states
     states <- sort(unique(data[, 7]))
     
     # Positions of the outcomes
     ha <- 11
     hf <- 17
     pn <- 23
     
     # Empty vector to build the matrix to convert it into a data frame
     build <- c()
     
     # Going through each state
     for(state in states) {
          
          # If it is the best hospital we are looking for
          if(num == 'best') {
               
               # We build the vector to bind in build
               build <- rbind(build, c(best(state, outcome), state))
          }
          
          else if( num == 'worst') {
               build <- rbind(build, c(worst(state, outcome), state))
          }
          
          else {
               hosp <- rankhospital(state, outcome, num)
               
               if(length(hosp) == 0)
                    hosp <- NA
               
               build <- rbind(build, c(hosp, state))
          }
     }
     
     dimnames(build) <- list(states, list('hospital', 'state'))
     return(data.frame(build))
}