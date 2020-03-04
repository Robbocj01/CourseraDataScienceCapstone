library(dplyr)
library(stringr)
library(data.table)
library(quanteda)

result_N <- 1

funct_Str <- function(str1, n = 4){
  
  if (n >= 4) {
    str_value <- word(str1, n - 2, -1)
  } else {
    str_value <- word(str1, 2, -1)
  }
  
  return(str_value)
}

funct_LW <- function(x){
  return(tail(strsplit(toString(x),split=" ")[[1]],1))
}

funct_Predict <- function(x) {

  
  result_LW <- " "
  
  
  if (length(x) >=1) {
    
    str1 <- tolower(x)
    str1 <- gsub("[[:punct:][:blank:]]+", " ", x)
    str1_len <- sapply(strsplit(str1, " "), length)
    str1 <- funct_Str(str1, str1_len)
    
    if(str1_len >= 4)
    {
      result <- head(Fourgram_P[Fourgram_P$PriorWord == str1,], n = result_N)
      
      if (nrow(result) < result_N) {
        str1 <- funct_Str(str1, str1_len)
        result_2 <- head(Threegram_P[Threegram_P$PriorWord == str1,], n = result_N)
        result_2$ProbScore <- result_2$ProbScore * 0.4
        result <- rbind(result, result_2)
      }
      
      if (nrow(result) < result_N) {
        str1 <- funct_Str(str1, str1_len)
        result_3 <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = result_N)
        result_3$ProbScore <- result_3$ProbScore * 0.4 *0.4
        result <- rbind(result, result_3)
      }
      
      if (nrow(result) < result_N) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- head(result$NGram,n=result_N)
      
    } else if (str1_len == 3){
      result <- head(Threegram_P[Threegram_P$PriorWord == str1,], n = result_N)
      
      if (nrow(result) < result_N) {
        str1 <- funct_Str(str1, str1_len)
        result_3 <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = result_N)
        result_3$ProbScore <- result_3$ProbScore * 0.4 *0.4
        result <- rbind(result, result_3)
      }
      
      if (nrow(result) < result_N) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- head(result$NGram,n=result_N)
      
    } else if (str1_len == 2){
      
      result <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = result_N)
      
      
      if (nrow(result) < result_N) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- head(result$NGram,n=result_N)
      
    } else if (str1_len == 1){
      result <- head(Onegram_P, n = result_N)
      result_LW <- head(result$NGram,n=result_N)
      
    }
    
  }
  
  for (i in 1:length(result_LW)) {
    result_LW[[i]] <- funct_LW(result_LW[[i]])
    
  }
  return(result_LW)
  
}


