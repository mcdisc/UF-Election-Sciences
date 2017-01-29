library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)

NC2017hisall <- fread('/Users/Potalora/Downloads/ncvhis_Statewide.txt')
NC2017votfile <- fread('/Users/Potalora/Downloads/ncvoter_Statewide-2.txt')

NC2018hisallless <- subset(NC2017hisall, select = -c(1, 4, 7, 9, 13, 14))
NC2017less <- subset(NC2017votfile, select = -c(1:8, 14:25, 29:68, 70:71))

NC2017test2016 <- subset(NC2018hisallless, election_desc == '11/08/2016 GENERAL')

NC2017test2012 <- subset(NC2018hisallless, election_desc == '11/06/2012 GENERAL')

NC2017_1216 <- full_join(NC2017test2016, NC2017test2012, by = 'ncid')

NC2017_1216 <- subset(NC2017_1216, select = -c(6,8,9,15,16,17))

NCjoined <- left_join(NC2017_1216, NC2017less, by = 'ncid')


NCtable <- NCjoined %>%
  filter(race_code == 'B') %>%
  group_by(county_desc.x) %>% 
  count(voting_method.x)
nctable1 <- spread(NCtable, voting_method.x, n)
nctable1 <- subset(nctable1, select = -c(9))
nctable1 <- nctable1[1:100,]
nctable1[is.na(nctable1)] <- 0
nctable1$total <- nctable1$`ABSENTEE BY MAIL` + nctable1$`ABSENTEE CURBSIDE` + nctable1$`ABSENTEE ONESTOP` + nctable1$CURBSIDE + nctable1$`IN-PERSON` + nctable1$PROVISIONAL + nctable1$TRANSFER
nctable1$early <- nctable1$`ABSENTEE BY MAIL` + nctable1$`ABSENTEE CURBSIDE` + nctable1$`ABSENTEE ONESTOP`
nctable1$percent_early <- nctable1$early/nctable1$total

ncfips <- fread("/Users/Potalora/Documents/UF_Elections/NC_Early_Voting/NCFIPS.csv")
nctable1 <- left_join(nctable1, ncfips, by = 'county_desc.x')
write.csv(nctable1, 'Black_Early_NC.csv', row.names = FALSE)

NC2012voters <- filter(NCjoined, election_desc.y != "N/A")
NCtable2 <- NC2012voters %>%
  filter(race_code == 'B') %>%
  group_by(county_desc.y) %>% 
  count(voting_method.y)
NCtable2 <- spread(NCtable2, voting_method.y, n)
NCtable2 <- subset(NCtable2, select = -c(9))
NCtable2 <- NCtable2[1:100,]
NCtable2[is.na(NCtable2)] <- 0
NCtable2$total <- NCtable2$`ABSENTEE BY MAIL` + NCtable2$`ABSENTEE` + NCtable2$`ABSENTEE ONESTOP` + NCtable2$CURBSIDE + NCtable2$`IN-PERSON` + NCtable2$PROVISIONAL + NCtable2$TRANSFER
NCtable2$early <- NCtable2$`ABSENTEE BY MAIL` + NCtable2$`ABSENTEE` + NCtable2$`ABSENTEE ONESTOP`
NCtable2$percent_early <- NCtable2$early/NCtable2$total


NCtable3 <- NC2012voters %>%
  filter(race_code == 'B') %>%
  group_by(county_desc.x) %>% 
  count(voting_method.x)
NCtable3 <- spread(NCtable3, voting_method.x, n)
NCtable3 <- subset(NCtable3, select = -c(9))
NCtable3 <- NCtable3[1:100,]
NCtable3[is.na(NCtable3)] <- 0
NCtable3$total <- NCtable3$`ABSENTEE BY MAIL` + NCtable3$`ABSENTEE CURBSIDE` + NCtable3$`ABSENTEE ONESTOP` + NCtable3$CURBSIDE + NCtable3$`IN-PERSON` + NCtable3$PROVISIONAL + NCtable3$TRANSFER
NCtable3$early <- NCtable3$`ABSENTEE BY MAIL` + NCtable3$`ABSENTEE CURBSIDE` + NCtable3$`ABSENTEE ONESTOP`

nctable4 <- left_join(NCtable2, NCtable3, by = c('county_desc.y' = 'county_desc.x'))
nctable4$repeatratio <- nctable4$early.y/nctable4$early.x

nctable4 <- left_join(nctable4, ncfips, by = c('county_desc.y'='county_desc.x'))
write.csv(nctable4, 'Black_repeat1.csv', row.names = FALSE)
