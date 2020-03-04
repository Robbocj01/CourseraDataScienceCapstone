#####################################################################
# Coursera Data Science Capstone
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

#################################
# 1 - Load Data
#################################

Onegram <-  fread(file="NGrams/Onegram.txt", nThread = 3)
Twogram <-  fread(file="NGrams/Twogram.txt", nThread = 3)
Threegram <-  fread(file="NGrams/Threegram.txt", nThread = 3)
Fourgram <-  fread(file="NGrams/Fourgram.txt", nThread = 3)

############################################
# 2 - Functions
############################################
# Data processing functions
# Source last word
funct_LW <- function(x){
  return(tail(strsplit(toString(x),split=" ")[[1]],1))
}

# Source previous words
funct_PW <- function(x){
  return(gsub("\\s*\\w*$","",x))
}

# Calculate Probability Score
funct_probScore <- function(NGram_1,NGram_2){
  
  NGram_P  <- left_join(NGram_1,NGram_2, by = c("PriorWord" = "NGram"))
  NGram_P <- NGram_P  %>% mutate(ProbScore = Frequency.x / Frequency.y) %>% 
    select(NGram = NGram, Frequency = Frequency.x, PriorWord = PriorWord, ProbScore = ProbScore)
  
  
  return(NGram_P)
}

############################################
# 3 - Calculate Stupid Backoff Probabilities
############################################

Onegram$PriorWord <- Onegram$NGram
Twogram$PriorWord <- Twogram[,funct_PW(NGram), by = seq_len(nrow(Twogram))][[2]]
Threegram$PriorWord <- Threegram[,funct_PW(NGram), by = seq_len(nrow(Threegram))][[2]]
Fourgram$PriorWord <- Fourgram[,funct_PW(NGram), by = seq_len(nrow(Fourgram))][[2]]

Fourgram_P <- funct_probScore(Fourgram, Threegram)
Threegram_P <- funct_probScore(Threegram, Twogram)
Twogram_P <- funct_probScore(Twogram, Onegram)

Onegram_P <- Onegram
Onegram_P$ProbScore <- Onegram_P$Frequency / sum(Onegram_P$Frequency)

############################################
# 4 - Save Stupid Backoff Probabilities
############################################


write.table(Threegram_P,file="NGrams/Threegram_P.txt", row.names = FALSE)
write.table(Fourgram_P,file="NGrams/Fourgram_P.txt", row.names = FALSE)
write.table(Twogram_P,file="NGrams/Twogram_P.txt", row.names = FALSE)
write.table(Onegram_P,file="NGrams/Onegram_P.txt", row.names = FALSE)
