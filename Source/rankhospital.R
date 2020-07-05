# This file contains functions to rank hospitals of a given state of a given
# outcome, it returns the num-th best

# This function receives the abbreviated name of state, the outcome on which
# you wanna rate the hospital and a parameter num to get the position of the
# rank
rankhospital <- function(state, outcome, num = 'best') {
     
     # Read the data
     data <- read_file('care measures')
     
     # Checking if the state name is valid
     if(!(state %in% unique(data[, 7]))) {
          stop('invalid state')
     }
     
     # Get the rows where the state is the given by the user
     state_index <- which(data[, 7] == state)
     
     if(outcome == 'heart attack') {
          
          # If it is the best hospital we're looking for
          if(num == 'best') {
               result <- best(state, outcome)
          }
          
          # If it is the worst hospital we're looking for
          else if(num == 'worst') {
               result <- worst(state, outcome)
          }
          
          # If it is another position in the rank we're looking for
          else {
               
               # Heart attack is on the  11th column
               outcome_data <- as.numeric(data[state_index, 11])
               
               # We get the coincidence for the outcome value we are looking for
               coincidence <- sort(outcome_data)[num]
               
               hit_rows <- which(data[, 11] == coincidence)
               
               if(length(hit_rows) == 1) {
                    result <- data[hit_rows, 2]
               }
               
               else {
                    # Heart attack is on the  11th column
                    outcome_data <- as.numeric(data[state_index, 11])
                    
                    # What is the num-th position of our outcome data?
                    coincidence <- sort(outcome_data)[num]
                    
                    # In which rows does it check in the global data?
                    hit_rows <- which(as.numeric(data[, 11]) == coincidence)
                    
                    # Where does the state check in the global data?
                    hit <- which(data[hit_rows, 7] == state)
                    
                    # Now, we access the hit position(s) in hit rows, that way
                    # we are sure it passed the two filters (value check and
                    # state check)
                    
                    # If we only have one row we return the name of the hospital
                    if(length(hit_rows[hit]) == 1) {
                         result <- data[hit_rows[hit], 2]
                    }
                    
                    # If we have multiple rows we sort them and pick the one which
                    # ends up in the position
                    else {
                         result <- sort(data[hit_rows[hit], 2])[length(hit_rows[hit])]
                    }
               }
          }
     }
     
     else if(outcome == 'heart failure') {
          
          # If it is the best hospital we're looking for
          if(num == 'best') {
               result <- best(state, outcome)
          }
          
          # If it is the worst hospital we're looking for
          else if(num == 'worst') {
               result <- worst(state, outcome)
          }
          
          # If it is another position in the rank we're looking for
          else {
               
               # Heart failure is on the  17th column
               outcome_data <- as.numeric(data[state_index, 17])
               
               coincidence <- sort(outcome_data)[num]
               
               hit_rows <- which(as.numeric(data[, 17]) == coincidence)
               
               hit <- which(data[hit_rows, 7] == state)
               
               if(length(hit_rows[hit]) == 1) {
                    result <- data[hit_rows[hit], 2]
               }
               
               else {
                    result <- sort(data[hit_rows[hit], 2])[length(hit_rows[hit])]
               }
               
          }
          
     }
     
     else if(outcome == 'pneumonia') {
          
          # If it is the best hospital we're looking for
          if(num == 'best') {
               result <- best(state, outcome)
          }
          
          # If it is the worst hospital we're looking for
          else if(num == 'worst') {
               result <- worst(state, outcome)
          }
          
          # If it is another position in the rank we're looking for
          else {
               
               # Pneumonia is on the  23th column
               outcome_data <- as.numeric(data[state_index, 23])
               
               coincidence <- sort(outcome_data)[num]
               
               hit_rows <- which(data[, 23] == coincidence)
               
               if(length(hit_rows) == 1) {
                    result <- data[hit_rows, 2]
               }
               
               else {
                    # Heart attack is on the  11th column
                    outcome_data <- as.numeric(data[state_index, 23])
                    
                    coincidence <- sort(outcome_data)[num]
                    
                    hit_rows <- which(as.numeric(data[, 23]) == coincidence)
                    
                    hit <- which(data[hit_rows, 7] == state)

                    if(length(hit_rows[hit]) == 1) {
                         result <- data[hit_rows[hit], 2]
                    }
                    
                    else {
                         result <- sort(data[hit_rows[hit], 2])[length(hit_rows[hit])]
                    }
               }
          }
     }
     
     # If the outcome is invalid
     else {
          stop('invalid outcome')
     }
     
     return(result)
}