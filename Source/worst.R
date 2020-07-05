# This file contains a function to find the worst hospital in a state

# This function receives the abbreviated name of a state and outputs the
# best hospital of a state based on the lowest 30-day mortality rates for
# heart attack, heart failure or pneumonia
worst <- function(name_of_state, condition) {
     
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
          
          # Get the highest value for mortality
          highest <- outcome_data[which.max(outcome_data)]
          
          # Now that we have the highest value we are going to search in the
          # data, it has to coincide with the highest value in the given state
          
          # Indices of the total data that match the highest value obtained
          a <- which(as.numeric(data[, 11]) == highest)
     }
     
     else if(condition == 'heart failure') {
          
          # Heart failure is on the  17th column
          outcome_data <- as.numeric(data[state_index, 17])
          
          # Get the highest value for mortality
          highest <- outcome_data[which.max(outcome_data)]
          
          # Now that we have the highest value we are going to search in the
          # data, it has to coincide with the highest value in the given state
          
          # Indices of the total data that match the highest value obtained
          a <- which(as.numeric(data[, 17]) == highest)
     }
     
     else if(condition == 'pneumonia') {
          
          # Pneumonia is on the  23th column
          outcome_data <- as.numeric(data[state_index, 23])
          
          # Get the highest value for mortality
          highest <- outcome_data[which.max(outcome_data)]
          
          # Now that we have the highest value we are going to search in the
          # data, it has to coincide with the highest value in the given state
          
          # Indices of the total data that match the highest value obtained
          a <- which(as.numeric(data[, 23]) == highest)
     }
     
     # If the outcome is invalid
     else {
          stop('invalid outcome')
     }
     
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
          winner <- sort(results)[length(results)]
     }
     
     return(winner)
}