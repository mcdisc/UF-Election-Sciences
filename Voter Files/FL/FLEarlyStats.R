library(data.table)
library(dplyr)
library(tidyr)
library(magrittr)
library(multidplyr)

FLVoterFile <- fread('/Users/Potalora/Downloads/20170110_VoterDetail/FLASTATEWIDE20170110.csv', colClasses = c("Voter ID" = "character"))
FLVoteHis <- fread('/Users/Potalora/Downloads/20170110_VoterHistory/FLAHIS20170110.csv', colClasses = c("Voter ID" = "character"))

FLVoterFileLess <- subset(FLVoterFile, select = -c(7:19, 23, 26:28, 30:38))

FLVote2016 <- filter(FLVoteHis, `Election Date` == '11/08/2016', `Election Type` == 'GEN')

FLVote2012 <- filter(FLVoteHis, `Election Date` == '11/06/2012', `Election Type` == 'GEN')

FLVote2016$Early <- 0
FLVote2016$Absentee <- 0
FLVote2016$AbsenteeRej <- 0
FLVote2016$NoVote <- 0
FLVote2016$ProvRej <- 0
FLVote2016$EleDay <- 0

# Recode History Code Types

FLVote2016$Absentee[FLVote2016$`History Code` == 'A'] <- 1
FLVote2016$AbsenteeRej[FLVote2016$`History Code` == 'B'] <- 1
FLVote2016$Early[FLVote2016$`History Code` == 'E'] <- 1
FLVote2016$NoVote[FLVote2016$`History Code` == 'N'] <- 1
FLVote2016$ProvRej[FLVote2016$`History Code` == 'P'] <- 1
FLVote2016$EleDay[FLVote2016$`History Code` == 'Y'] <- 1

# Collapse History Code Types

FLTest2016 <- FLVote2016 %>% 
  group_by(`Voter ID`) %>% 
  summarise_each(funs(sum), Absentee, AbsenteeRej, Early, NoVote, ProvRej, EleDay)


# Get rid of duplicates but keep data

FLTest2016$TotalEarly <- 0
FLTest2016$TotalEarly[FLTest2016$Early >= 1] <- 1
FLTest2016$TotalEleDay <- 0
FLTest2016$TotalEleDay[FLTest2016$EleDay >= 1] <- 1
FLTest2016$TotalAbsentee <- 0
FLTest2016$TotalAbsentee[FLTest2016$Absentee >= 1] <- 1

# Get total votes not accounting for duplicates

FLTest2016$Total <- 0
FLTest2016$Total <- FLTest2016$Absentee +  + FLTest2016$Early + FLTest2016$EleDay

# Get total without duplicates

FLTest2016$TotalActual <- 0
FLTest2016$TotalActual[FLTest2016$Total >= 1] <- 1

FLTest2016$TotalEarlyAll <- 0
FLTest2016$TotalEarlyAll[(FLTest2016$TotalEarly + FLTest2016$TotalAbsentee) >= 1] <- 1

# Do the same with 2012

FLVote2012$Early <- 0
FLVote2012$Absentee <- 0
FLVote2012$AbsenteeRej <- 0
FLVote2012$NoVote <- 0
FLVote2012$ProvRej <- 0
FLVote2012$EleDay <- 0

FLVote2012$Absentee[FLVote2012$`History Code` == 'A'] <- 1
FLVote2012$AbsenteeRej[FLVote2012$`History Code` == 'B'] <- 1
FLVote2012$Early[FLVote2012$`History Code` == 'E'] <- 1
FLVote2012$NoVote[FLVote2012$`History Code` == 'N'] <- 1
FLVote2012$ProvRej[FLVote2012$`History Code` == 'P'] <- 1
FLVote2012$EleDay[FLVote2012$`History Code` == 'Y'] <- 1

FLTest2012 <- FLVote2012 %>% 
  group_by(`Voter ID`) %>% 
  summarise_each(funs(sum), Absentee, AbsenteeRej, Early, NoVote, ProvRej, EleDay)


FLTest2012$Total <- FLTest2012$Absentee + FLTest2012$Early + FLTest2012$EleDay

FLTest2012$TotalEarly <- 0
FLTest2012$TotalEarly[FLTest2012$Early >= 1] <- 1
FLTest2012$TotalEleDay <- 0
FLTest2012$TotalEleDay[FLTest2012$EleDay >= 1] <- 1
FLTest2012$TotalAbsentee <- 0
FLTest2012$TotalAbsentee[FLTest2012$Absentee >= 1] <- 1

FLTest2012$TotalActual <- 0
FLTest2012$TotalActual[FLTest2012$Total >= 1] <- 1

# Join 2016 voters with 2012 without losing any observations

FL1216 <- full_join(FLTest2016, FLTest2012, by = 'Voter ID')

# Join 2016 voters with voter file for race

FL2016joined <- left_join(FLTest2016, FLVoterFileLess, by = 'Voter ID')

write.csv(FL2016joined, 'FLVotHis2016Clean.csv', row.names = FALSE)

FL2016joined <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FLVotHis2016Clean.csv')

# Black voters by County

FLFIPS <- fread('/Users/Potalora/Desktop/FLFIPS.csv')


processethnic <- function(df, fips, ethniccode, name){
  ethnicdf <- df %>%
    filter(Race == ethniccode) %>%
    group_by(`County Code`) %>%
    summarise_each(funs(sum), TotalEarly, TotalAbsentee, TotalActual)
  
  # Simple early vote ratio
  
  ethnicdf$EarlyRatio <- ethnicdf$TotalEarly/ethnicdf$TotalActual
  ethnicdf$AbsenteeRatio <- ethnicdf$TotalAbsentee/ethnicdf$TotalActual
  ethnicdf <- left_join(ethnicdf, FLFIPS, by = "County Code")
  write.csv(ethnicdf, paste(name, ".csv", sep = ''), row.names = FALSE)
  ethnicdf
}


ethnictotbycounty <- function(){
  df <- FLVoterFileLess %>%
    partition(`County Code`) %>%
    spread(Race) %>%
    group_by(`County Code`)
}

df <- ethnictotbycounty()

FL2016BlackbyCounty <- processethnic(FL2016joined, FLFIPS, 3, 'FL2016BlackEarly')

FL2016WhitebyCounty <- processethnic(FL2016joined, FLFIPS, 5, 'FL2016WhiteEarly')

FL2016HispanicbyCounty <- processethnic(FL2016joined, FLFIPS, 4, 'FL2016HispanicEarly')


allethnicdf <- FL2016joined %>%
  group_by(`County Code`) %>%
  summarise_each(funs(sum), TotalEarly, TotalAbsentee, TotalActual)

# Simple early vote ratio

allethnicdf$EarlyRatio <- allethnicdf$TotalEarly/allethnicdf$TotalActual
allethnicdf$AbsenteeRatio <- allethnicdf$TotalAbsentee/allethnicdf$TotalActual
allethnicdf <- left_join(allethnicdf, FLFIPS, by = "County Code")
write.csv(allethnicdf, paste('FL2016AllEarly', ".csv", sep = ''), row.names = FALSE)



