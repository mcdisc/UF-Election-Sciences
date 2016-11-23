setwd("~/Documents/Data Projects/National Voter Data File/Florida_Absentee")

absentee <- read.delim2("ABL0922.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"09/22" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_09/22"

absentee1 <- read.delim2("ABL1002.txt", header = FALSE)
names(absentee1) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee1$"10/02" <- "x"
absentee1 <- absentee1[ c (3, 8, 35)]
colnames(absentee1)[2] <- "Type_10/02"

total <- merge(absentee, absentee1, by = "VoterIDNumber", all = TRUE)
rm(absentee)
rm(absentee1)

absentee <- read.delim2("ABL1009.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"10/09" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_10/09"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)

absentee <- read.delim2("ABL1016.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"10/16" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_10/16"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)

absentee <- read.delim2("ABL1024.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"10/24" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_10/24"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)


absentee <- read.delim2("ABL1030.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"10/30" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_10/30"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)

absentee <- read.delim2("ABL1105.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"11/05" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_11/05"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)

absentee <- read.delim2("ABL1110.txt", header = FALSE)
names(absentee) <- c("RecordType", "CountyID", "VoterIDNumber", "ElectionNumber", "ElectionDate", "LastAbsRecordChangeDate", "AbsenteeRequestStatus", "AbsReqDate", "AbsDeliveryDate", "AbsReturnDate", "AbsReqCanceledDate", "AbsMilitary", "AbsOverseasFlag", "AbsMilitaryDependent", "Precinct", "PrecintSplit", "CongressionalDistrict", "SenateDistrict", "HouseDistrict", "CountyCommissionDistrict", "SchoolBoardDistrict", "SchoolBoardDistrict", "OtherDistricts", "Abs Party", "Voter Name", "AbsReqMailingAddressLine1", "AbsReqMailingAddressLine2", "AbsReqMailingAddressLine3", "AbsReqMailingAddressCity", "AbsReqMailingAddressState", "AbsReqMailingAddressZip", "AbsReqMailingAddressCountry", "AbsReqE-mailAddress", "AbsReqFaxNumber")
absentee$"11/10" <- "x"
absentee <- absentee[ c (3, 8, 35)]
colnames(absentee)[2] <- "Type_11/10"
total <- merge(total, absentee, by = "VoterIDNumber", all = TRUE)
rm(absentee)

total$total <- paste(total$`Type_09/22`, total$`Type_10/02`, total$`Type_10/09`, total$`Type_10/16`, total$`Type_10/24`, total$`Type_10/30`, total$`Type_11/05`, total$`Type_11/10`)
names(total) <- c("index", "VoterIDNumber", "Type0922", "0922", "Type1002", "1002", "Type1009", "1009", "Type1016", "1016", "Type1024", "1024", "Type1030", "1030", "Type1105", "1105", "Type1110", "1110", "Vector") 




write.csv(total, "absenteereturns_final.csv")
total <- absentee_returns_final
rm(absentee_returns_final)

total$total1 <- paste(total$Type1002, total$Type1009, total$Type1016, total$Type1024, total$Type1030, total$Type1105, total$Type1110)
total$total2 <- paste(total$Type1009, total$Type1016, total$Type1024, total$Type1030, total$Type1105, total$Type1110)
total$total3 <- paste(total$Type1016, total$Type1024, total$Type1030, total$Type1105, total$Type1110)
total$total4 <- paste(total$Type1024, total$Type1030, total$Type1105, total$Type1110)
total$total5 <- paste(total$Type1030, total$Type1105, total$Type1110)
total$total6 <- paste(total$Type1105, total$Type1110)

tostudy2 <- total[which((grepl("C", total$total2) | grepl("E", total$total2) | grepl("N", total$total2) | grepl("P", total$total2) | grepl("R", total$total2) | grepl("S", total$total2) | grepl("U", total$total2) | grepl("X", total$total2)) & total$Type1002 == "V" & !grepl("NA", total$total2)),]
tostudy3 <- total[which(total$Type1009 == "V" & !grepl("NA", total$total3) & (grepl("C", total$total3) | grepl("E", total$total3) | grepl("N", total$total3) | grepl("P", total$total3) | grepl("R", total$total3) | grepl("S", total$total3) | grepl("U", total$total3) | grepl("X", total$total3))) ,]
tostudy4 <- total[which(total$Type1016 == "V" & !grepl("NA", total$total4) & (grepl("C", total$total4) | grepl("E", total$total4) | grepl("N", total$total4) | grepl("P", total$total4) | grepl("R", total$total4) | grepl("S", total$total4) | grepl("U", total$total4) | grepl("X", total$total4))) ,]
tostudy5 <- total[which(total$Type1024 == "V" & !grepl("NA", total$total5) & (grepl("C", total$total5) | grepl("E", total$total5) | grepl("N", total$total5) | grepl("P", total$total5) | grepl("R", total$total5) | grepl("S", total$total5) | grepl("U", total$total5) | grepl("X", total$total5))) ,]
tostudy6 <- total[which(total$Type1030 == "V" & !grepl("NA", total$total6) & (grepl("C", total$total6) | grepl("E", total$total6) | grepl("N", total$total6) | grepl("P", total$total6) | grepl("R", total$total6) | grepl("S", total$total6) | grepl("U", total$total6) | grepl("X", total$total6))) ,]
final <- rbind(tostudy2, tostudy3, tostudy4, tostudy5, tostudy6)

final <- final[!duplicated(final$VoterIDNumber), ]
final$Vector <- paste(final$Type0922, final$Type1002, final$Type1009, final$Type1016, final$Type1024, final$Type1030, final$Type1105, final$Type1110)

readcolumns = c("character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL")

VoterFile <- read.csv("Florida_Oct_22.txt", header = FALSE, sep = "\t", dec = ".", fill = TRUE, colClasses = readcolumns,quote="")

colnames(VoterFile)<- c("CountyAbbrv", "VoterIDNumber", "address.line1", "address.line2","address.city","address.state","address.zip","gender","race","date.birth","date.reg","party","prec","prec.group","prec.split","prec.suffix","status")


final <- final[c(2, 19)]
final1 <- merge(final, VoterFile, by = "VoterIDNumber")
write.csv(final1, "DanSmith.csv")
