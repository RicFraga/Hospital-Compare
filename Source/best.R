# This file contains a function to find the best hospital in a state

# This function receives the abbreviated name of a state and outputs the
# best hospital of a state based on the lowest 30-day mortality rates for
# heart attack, heart failure or pneumonia
best <- function(name_of_state, condition) {
     
     # Getting the data from the csv
     data <- read_file('care measures')
     
     # Getting the valid states (7th column)
     states <- unique(data[, 7])
     
     # Check if the state is valid
     if(!(name_of_state %in% states)) {
          stop('invalid state')
     }
     
     # Get the rows where the state is the given by the user
     state_index <- which(data[, 7] == name_of_state)
     
     if(condition == 'heart attack') {
          
          # Heart attack is on the  11th column
          outcome_data <- as.numeric(data[state_index, 11])

          # Get the lowest value for mortality
          lowest <- outcome_data[which.min(outcome_data)]
          
          # Now that we have the lowest value we are going to search in the
          # data, it has to coincide with the lowest value in the given state
          
          # Indices of the total data that match the lowest value obtained
          a <- which(as.numeric(data[, 11]) == lowest)
          
          # Indices where the given state matches
          b <- which(data[a, 7] == name_of_state)
          
          results <- data[a[b], 2]
          
          # If there is no tie between the lowest scores we select the name
          # of the winner (2nd column)
          if(length(results) == 1) {
               winner <- results
          }
          
          # If there is a tie we select all winners and sort them by
          # alphabetical order
          
          else {
          
               # We sort them
               winner <- sort(results)[1]
          }
     }
     
     else if(condition == 'heart failure') {
          
          # Heart failure is on the  17th column
          outcome_data <- as.numeric(data[state_index, 17])
          
          # Get the lowest value for mortality
          lowest <- outcome_data[which.min(outcome_data)]
          
          # Now that we have the lowest value we are going to search in the
          # data, it has to coincide with the lowest value in the given state
          
          # Indices of the total data that match the lowest value obtained
          a <- which(as.numeric(data[, 17]) == lowest)
          
          # Indices where the given state matches
          b <- which(data[a, 7] == name_of_state)
          
          results <- data[a[b], 2]
          
          # If there is no tie between the lowest scores we select the name
          # of the winner (2nd column)
          if(length(results) == 1) {
               winner <- results
          }
          
          # If there is a tie we select all winners and sort them by
          # alphabetical order
          
          else {
               
               # We sort them
               winner <- sort(results)[1]
          }
     }
     
     else if(condition == 'pneumonia') {
          
          # Pneumonia is on the  23th column
          outcome_data <- as.numeric(data[state_index, 23])
          
          # Get the lowest value for mortality
          lowest <- outcome_data[which.min(outcome_data)]
          
          # Now that we have the lowest value we are going to search in the
          # data, it has to coincide with the lowest value in the given state
          
          # Indices of the total data that match the lowest value obtained
          a <- which(as.numeric(data[, 23]) == lowest)
          
          # Indices where the given state matches
          b <- which(data[a, 7] == name_of_state)
          
          results <- data[a[b], 2]
          
          # If there is no tie between the lowest scores we select the name
          # of the winner (2nd column)
          if(length(results) == 1) {
               winner <- results
          }
          
          # If there is a tie we select all winners and sort them by
          # alphabetical order
          
          else {
               
               # We sort them
               winner <- sort(results)[1]
          }
     }
     
     # If the outcome is invalid
     else {
          stop('invalid outcome')
     }
     
     return(winner)
}