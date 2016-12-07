library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
setwd('/Users/Potalora/Desktop')

NC2016 <- fread('/Users/Potalora/Downloads/ncvoter_Statewide.txt')
Early2016 <- read.csv('/Users/Potalora/Downloads/absentee11xx08xx2016-10.csv')
Early2016less <- subset(Early2016, select = -c(14:21, 25:26, 34))
NC2016less <- subset(NC2016, select = -c(1:8, 14:26, 28:68, 70:71))
Early2016Joined <- left_join(Early2016less, NC2016less, by = 'ncid')
Early2016Joined %<>%
  unite(race_ethnicity, race, ethnic_code, sep = "_", remove = FALSE)
write.csv(Early2016Joined, 'Early2016Ethnicity116.csv', row.names = FALSE)