library(dplyr)
setwd('/Users/Potalora/Desktop')
NC2013 <- read.delim('/Users/Potalora/Desktop/VR_20130101.txt', quote = "", header = TRUE, sep = '\t')
Early2016 <- read.csv('/Users/Potalora/Downloads/absentee11xx08xx2016-9.csv')
NC2013 <- subset(NC2013, voter_status_reason_desc != 'DUPLICATE')
NC2013 <- subset(NC2013, voter_status_desc != 'REMOVED')
NC2013 <- subset(NC2013, voter_status_desc != 'DENIED')
NC2013less <- subset(NC2013, select = -c(1,4,6, 8:11,16:37,39, 41:45, 47:90))
Early2016less <- subset(Early2016, select = -c(14:21, 25:26, 34))
NC2013less %>% mutate_if(is.factor, as.character) -> NC2013less
Early2016less %>% mutate_if(is.factor, as.character) -> Early2016less
JoinedEarly2016 <- left_join(Early2016less, NC2013less, by = "ncid")
write.csv(JoinedEarly2016, 'JoinedEarly1162016.csv', row.names = FALSE)
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', ballot_request_party != party_cd))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'REP', ballot_request_party == 'DEM'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'DEM', ballot_request_party == 'REP'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party == 'DEM'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party == 'REP'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd == 'UNA', ballot_request_party != 'UNA'))
count(filter(JoinedEarly2016, ballot_rtn_status == 'ACCEPTED', party_cd != 'UNA', ballot_request_party == 'UNA'))

NC2013less <- NC2013less %>% distinct(ncid, .keep_all = TRUE)
count(inner_join(NC2013, Early2016less, by = 'ncid'))
NC2013 %>% distinct(voter_status_desc)
duplicates <- (NC2013test %>% 
                            group_by(ncid) %>% 
                            filter(n()>1))
