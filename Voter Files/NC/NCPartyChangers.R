library(dplyr)
setwd('/Users/Potalora/Desktop')

#Read in voter file snapshot and early vote file
NC2013 <- read.delim('/Users/Potalora/Desktop/VR_20130101.txt', quote = "", header = TRUE, sep = '\t')
Early2016 <- read.csv('/Users/Potalora/Downloads/absentee11xx08xx2016-9.csv')

#Remove useless columns
NC2013less <- subset(NC2013, select = -c(1,4,6,8,10:11,16:37,39,41:45,47:90))

#Remove rows that the voter file says are 'DUPLICATE'- might want to investigate these rows in the future
NC2013clean <- subset(NC2013less, voter_status_reason_desc != 'DUPLICATE')

#Remove 'REMOVED' and 'DENIED'
NC2013clean <- subset(NC2013clean, voter_status_desc != 'REMOVED')
NC2013clean <- subset(NC2013clean, voter_status_desc != 'DENIED')

#Remove useless columns
Early2016less <- subset(Early2016, select = -c(14:21, 25:26, 34))

#Get 'REMOVED' Voters
NC2013r <- subset(NC2013less, voter_status_desc == 'REMOVED')

#Get 'DENIED' Voters
NC2013d <- subset(NC2013less, voter_status_desc == 'DENIED')

#Bind REMOVED and DENIED Voters
NC2013rd <- bind_rows(NC2013r, NC2013d)

#Find REMOVED and DENIED voters that do not have corresponding "clean" rows
NC2013rdnot <- anti_join(NC2013rd, NC2013clean, by = 'ncid')

#Remove duplicates
NC2013rdnot <- NC2013rdnot %>% distinct(ncid, .keep_all = TRUE)

#Add these voters to clean file, 
#these voters are unique and should be preserved as they could return to voter file
#as we will see in 2016 early voting and current voter file
NC2013clean <- bind_rows(NC2013clean, NC2013rdnot)

#Get rid of factors
NC2013clean %>% mutate_if(is.factor, as.character) -> NC2013less
Early2016less %>% mutate_if(is.factor, as.character) -> Early2016less

#Join cleaned up 2013 NC file
JoinedEarly2016 <- left_join(Early2016less, NC2013clean, by = "ncid")

#Write out joined file with old party affiliation
write.csv(JoinedEarly2016, 'JoinedEarly1162016.csv', row.names = FALSE)

#Stats on Accepted and Party Switchers
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', ballot_request_party != party_cd))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'REP', ballot_request_party == 'DEM'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'DEM', ballot_request_party == 'REP'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party == 'DEM'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party == 'REP'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party != 'UNA'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd != 'UNA', ballot_request_party == 'UNA'))


