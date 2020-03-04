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

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Set fixed notation over exponential notation 
options("scipen"=7)

#################################
# 1 - Download Data
#################################

# Download training data file. Check to ensure it has not been already downloaded
if (!file.exists("Data/Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                destfile = "Data/Coursera-SwiftKey.zip", method = "auto", mode = "wb")
}

#################################
# 2 - Unzip Data
#################################
#---------- unzip the data files ---------- 
if(!file.exists("Data/final/en_US/en_US.blogs.txt")| !file.exists("Data/final/en_US/en_US.twitter.txt")| !file.exists("Data/final/en_US/en_US.news.txt")) {
  unzip("Data/Coursera-SwiftKey.zip", exdir = "Data", overwrite = TRUE)
}

##############################
# 3 - Import Files
##############################
#Blog Data
blogsFileName <- "Data/final/en_US/en_US.blogs.txt"
con <- file(blogsFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

# News
newsFileName <- "Data/final/en_US/en_US.news.txt"
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

# Twitter
twitterFileName <- "Data/final/en_US/en_US.twitter.txt"
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

##############################
# 4 - Save to a RDS
##############################
saveRDS(blogs,file="Data/final/en_US/blogs_data.RDS")
saveRDS(news,file="Data/final/en_US/news_data.RDS")
saveRDS(twitter,file="Data/final/en_US/twitter_data.RDS")