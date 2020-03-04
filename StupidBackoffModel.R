#####################################################################
# Coursera Data Science Capstone - Stupid Backoff Model
#
# Chris Robertson Feb 2020
#####################################################################

#################################
# 0 - Set Folder Path and Options
#################################

# Remove any residual lists and set working directory
rm(list = ls())
gc()

# Load libraries
library(quanteda)
library(ggplot2)
library(caret)
library(ngram)
library(wordcloud)
library(dplyr)
library(stringr)
library(data.table)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Set fixed notation over exponential notation 
options("scipen"=7)
quanteda_options((threads = 3))

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

#################################
# 1 - Load Data
#################################

Onegram_P  <-  fread(file="NGrams/Onegram_P.txt", nThread = 3)
Twogram_P <-  fread(file="NGrams/Twogram_P.txt", nThread = 3)
Threegram_P <-  fread(file="NGrams/Threegram_P.txt", nThread = 3)
Fourgram_P <-  fread(file="NGrams/Fourgram_P.txt", nThread = 3)

##################################
# 2 - Create Predictive Text Model
##################################

str1 <- "hello how are you"

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
  ##################################
  # 2 - Predictive Text Model
  ##################################
  
  result_LW <- " "
  
  
  if (length(x) >=1) {
    
    str1 <- tolower(x)
    str1_len <- sapply(strsplit(str1, " "), length)
    str1 <- funct_Str(str1, str1_len)
    
    
    
    if(str1_len >= 4)
    {
      result <- head(Fourgram_P[Fourgram_P$PriorWord == str1,], n = 5)
      
      if (nrow(result) < 5) {
        str1 <- funct_Str(str1, str1_len)
        result_2 <- head(Threegram_P[Threegram_P$PriorWord == str1,], n = 5)
        result_2$ProbScore <- result_2$ProbScore * 0.4
        result <- rbind(result, result_2)
      }
      
      if (nrow(result) < 5) {
        str1 <- funct_Str(str1, str1_len)
        result_3 <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = 5)
        result_3$ProbScore <- result_3$ProbScore * 0.4 *0.4
        result <- rbind(result, result_3)
      }
      
      if (nrow(result) < 5) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- funct_LW(head(result$NGram,n=1))
      
    } else if (str1_len == 3){
      result <- head(Threegram_P[Threegram_P$PriorWord == str1,], n = 5)
      
      if (nrow(result) < 5) {
        str1 <- funct_Str(str1, str1_len)
        result_3 <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = 5)
        result_3$ProbScore <- result_3$ProbScore * 0.4 *0.4
        result <- rbind(result, result_3)
      }
      
      if (nrow(result) < 5) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- funct_LW(head(result$NGram,n=1))
      
    } else if (str1_len == 2){
      
      result <- head(Twogram_P[Twogram_P$PriorWord == str1,], n = 5)
      
      
      if (nrow(result) < 5) {
        result_4 <- head(Onegram_P)
        result_4$ProbScore <- result_4$ProbScore * 0.4 *0.4*0.4
        result <- rbind(result, result_4)
      }
      
      result_LW <- funct_LW(head(result$NGram,n=1))
      
    } else if (str1_len == 1){
      result <- head(Onegram_P, n = 5)
      result_LW <- funct_LW(head(result$NGram,n=1))
    }
    
  }
  return(result_LW)
  
}

funct_Predict(str1)
