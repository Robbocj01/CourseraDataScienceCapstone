require(stringi)
require(data.table)

setwd("C:/Users/rober/OneDrive/Github/CourseraDataScienceCapstone/")
source('C:/Users/rober/OneDrive/Github/CourseraDataScienceCapstone/StupidBackoffPredictionFunction.R')

Fourgram_P_Sample <- data.table(sample_n(Fourgram_P, 1000)) 
Threegram_P_Sample <- data.table(sample_n(Threegram_P, 1000))

benchmark_Sample <- rbind(Fourgram_P_Sample, Threegram_P_Sample)

benchmark_Sample[, TestWord := sub('.*?(\\w+( \\w+){0})$', '\\1', NGram)]

benchmark_Sample$Prediction <- benchmark_Sample[,funct_Predict(PriorWord), by = seq_len(nrow(benchmark_Sample))][[2]]

benchmark_Sample$Match <- ifelse(benchmark_Sample$TestWord==benchmark_Sample$Prediction,1,0)

sum(benchmark_Sample$Match) / nrow(benchmark_Sample)
