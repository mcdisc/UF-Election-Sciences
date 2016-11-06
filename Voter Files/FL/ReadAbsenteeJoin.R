library(dplyr)
library(tidyr)
library(magrittr)
absentee <- read.delim('/Users/Potalora/Downloads/ABL_10282_10022016_072435186.txt', quote = "", header = FALSE, sep = '\t')
namesabs <- c("RecordType",
              "CountyId",
              "FVRSVoterIdNumber",
              "FVRSElectionNumber",
              "ElectionDate",
              "ElectionName",
              "LastAbsRecordChangeDate",
              "AbsenteeRequestStatus",
              "AbsReqDate",
              "AbsDelivery Date",
              "AbsReturnDate",
              "AbsReqCanceledDate",
              "AbsMilitary",
              "AbsOverseasFlag",
              "AbsMilitary Dependent",
              "Precinct",
              "PrecinctSplit",
              "CongressionalDistrict",
              "SenateDistrict",
              "HouseDistrict",
              "CountyCommissionDistrict",
              "SchoolBoardDistrict",
              "OtherDistricts",
              "Abs Party",
              "Voter Name",
              "AbsReqMailingAddressLine 1",
              "AbsReqMailingAddressLine 2",
              "AbsReqMailingAddressLine 3",
              "AbsReqMailingAddress City",
              "AbsReqMailingAddress State",
              "AbsReqMailingAddressZip",
              "AbsReqMailingAddressCountry",
              "AbsReqE-mailAddress",
              "AbsReqFaxnumber")
colnames(absentee) <- namesabs
florida <- subset(florida, select = -c(8:11, 13:19, 26:28, 33:38))
absentee <- absentee[-c(21:23, 26:34)]
FIPS <- read.csv('/Users/Potalora/Desktop/FLFIPS.csv')
absenteemerged <- left_join(absentee, florida, by = c('FVRSVoterIdNumber' = 'Voter ID', 'CountyId' = 'County Code'))
absenteemerged <- left_join(absenteemerged, FIPS, by = c('CountyId' = 'County.Code'))
setwd('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting')
absenteemerged %<>%
  unite(UniqueID, CountyId, FVRSVoterIdNumber, remove = FALSE)
write.csv(absenteemerged, 'Absentee10022016.csv', row.names = FALSE)

