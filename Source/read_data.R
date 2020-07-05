# This file contains functions to read and process data in order to
# use it in other functions of other files of the project

read_file <- function(file_name) {
     if(file_name == 'hospital data') {
          data <- read.csv('Data/rprog_data_ProgAssignment3-data/hospital-data.csv', colClasses = 'character')
     }
     
     else if(file_name == 'care measures') {
          data <- read.csv('Data/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
     }
     
     else {
          stop('invalid file')
     }
     
     return(data)
}