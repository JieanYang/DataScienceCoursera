# Question 3
corr <- function(directory = "specdata", threshold = 0){
  # data_return <- data.frame(matrix(ncol = 2, nrow = 0))
  # colnames(data_return) <- c("sulfate", "nitrate")
  data_return <- vector("numeric", length = 0)
  id = 1:332
  
  for(i in seq_len(length(id))){
    sulfate <- retrieveData("specdata","sulfate", id = id[i])
    nitrate <- retrieveData("specdata","nitrate", id = id[i])
    
    data_two <- cbind(sulfate,nitrate)
    data_two <- data_two[(!is.na(data_two[,1]) &
                            !is.na(data_two[,2])),]
    
    
    if(threshold < length(data_two[,1])) {
      data_return <- c(data_return,
                           cor(data_two[,1],
                               data_two[,2]))
    }
  }
  
  data_return
}



# Question 2
complete <- function(directory = "specdata", id = 1:332){
  data_return <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(data_return) <- c("id", "nobs")
  j = 1;
  
  for(i in seq_len(length(id))){
    data_return[j,] <- data.frame(id = id[i], nobs = complete_single_file(id=id[i]))
    j <- j+1
  }
  
  print(data_return)
}

complete_single_file <- function(directory = "specdata", id = 1){
    data_sulfate <- retrieveData(directory, "sulfate", id)
    data_nitrate <- retrieveData(directory, "nitrate", id)
    data_complete <- complete.cases(data_sulfate, data_nitrate)
    
    length(data_complete[data_complete=="TRUE"])
}

retrieveData <-function(directory = "specdata",pollutant = "sulfate", id = 1) {
  
  fileName <- formatC(c(id), flag=0, width = 3)
  for(i in seq_len(length(fileName))) {
    fileName[i] <- paste(directory, "/",
                         fileName[i], ".csv", sep="")
  }
  
  data_all <- c()
  for(i in fileName){
    data_single_file <- read.csv(i)
    # take the pollutant of the file
    # then move into data_all
    data_all <- c(data_all, data_single_file[[pollutant]])
  }
  
  data_all
}


# Question 1
pollutantmean <- function(directory = "specdata", 
                          pollutant = "sulfate", id = 1:332) {
  
  fileName <- formatC(c(id), flag=0, width = 3)
  for(i in seq_len(length(fileName))) {
    fileName[i] <- paste(directory, "/",
                         fileName[i], ".csv", sep="")
  }
  
  data_all <- c()
  for(i in fileName){
    data_single_file <- read.csv(i)
    # take the pollutant of the file
    # then move into data_all
    data_all <- c(data_all, 
        data_single_file[[pollutant]]
        [!is.na(data_single_file[[pollutant]])])
  }
  
  mean(data_all)

}