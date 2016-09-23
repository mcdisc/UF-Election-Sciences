#Scrape the North Carolina Statewide Absentee Ballot File to a local directory and Analyze
#
#########
# SETUP #
#########
#
# Load required packages

library(sqldf)
library(maptools)
library(RColorBrewer)
library(ggmap)
library(ggplot2)
library(rgeos)
library(png)
library(gridGraphics)
library(plyr)

#########
# Setup #
#########
#

# Working Directory

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/NC/"

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

#########################
# Get current absentees #
#########################

url <- "http://dl.ncsbe.gov.s3.amazonaws.com/ENRS/absentee11xx08xx2016.zip"
destfile <- "D:/Research/Turnout/Voter Files/Analyze/NC/absentee11xx08xx2016.zip"

download.file(url, destfile, mode = "wb")

# unzip file

# set destination directory for zip file

current.file <- paste(working.dir,"absentee11xx08xx2016.csv", sep="")

# file.remove(current.week.file)
# need automate rename current week to last week

if (file.exists(current.file)) {
  file.remove(current.file)
}

zipdest = "D:/Research/Turnout/Voter Files/Analyze/NC"

unzip(destfile, exdir=zipdest, overwrite = TRUE)

# Read current file

readcolumns = c("character", "character", "character", "NULL", "NULL", "NULL", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "character", "character", "NULL")
currentabs <- read.csv(current.file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, colClasses = readcolumns, stringsAsFactors = FALSE)

# Need to remove duplicates for multiple ballot requests from same voter

dup <- sqldf("SELECT ncid, Count(ncid) AS count FROM currentabs GROUP BY ncid")

currentabs <- merge(x = currentabs, y = dup, by = "ncid", all=TRUE, sort=FALSE)

query <- sqldf("SELECT voter_party_code, Count(voter_party_code) AS req FROM currentabs WHERE ((count=1) OR ((count>1) AND ((ballot_send_dt !='') AND ((ballot_rtn_status ='') Or (ballot_rtn_status='ACCEPTED'))))) GROUP BY voter_party_code")

sum(query$req)

query$voter_party_code <- as.character(query$voter_party_code)

query[1,1] <- "Dem 2016"
query[2,1] <- "Lib 2016"
query[3,1] <- "Rep 2016"
query[4,1] <- "Una 2016"

currentstat<-data.frame()

currentstat[1,1] <- query[1,2] 
currentstat[1,2] <- query[3,2]
currentstat[1,3] <- query[2,2]
currentstat[1,4] <- query[4,2]

# 2012 Relative Counts

party_2012_file <- "D:/Research/Turnout/Voter Files/Analyze/NC/2012_requests_party.csv"
party_2012 <- read.csv(party_2012_file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)

party_2012$req_dt <- as.Date(party_2012$ballot_req_dt,format="%m/%d/%Y")

# Calculate cumulative requests by party

party_2012 <- ddply(party_2012, .(voter_party_code), transform, Cumulative.Sum = cumsum(req))

# Select date rage

party_2012 <- party_2012[party_2012$req_dt>as.Date("2012-09-05") & party_2012$req_dt<as.Date("2012-11-06"),]

# Compute days from election

party_2012$countdown12 <- as.Date("2012-11-06") - party_2012$req_dt
party_2012

Current.relative.day <- as.Date("2016-11-08") - as.Date(Sys.Date())
Current.relative.day

abs.2012 <- party_2012[party_2012[,"countdown12"]==Current.relative.day,]
abs.2012

keepvars <- c("voter_party_code","Cumulative.Sum")
abs.2012 <- abs.2012[keepvars]

colnames(abs.2012)[2] = "req"
abs.2012

levels(abs.2012$voter_party_code)[levels(abs.2012$voter_party_code)=="DEM"] <- "Dem 2012"
levels(abs.2012$voter_party_code)[levels(abs.2012$voter_party_code)=="LIB"] <- "Lib 2012"
levels(abs.2012$voter_party_code)[levels(abs.2012$voter_party_code)=="REP"] <- "Rep 2012"
levels(abs.2012$voter_party_code)[levels(abs.2012$voter_party_code)=="UNA"] <- "Una 2012"

combined <- rbind(query,abs.2012)
combined
combined$voter_party_code <- as.factor(combined$voter_party_code)

ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=11))

bar.party.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))


# Total absentee requests
# Read in voter file to do race analysis (this step requires scraping the file first by running Scrape NC.R)
# This is a large file that can overwhelm some computers

current.voter.file <- paste(working.dir,"ncvoter_Statewide.txt", sep="")

readcolumns = c(NA, "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, NA)
currentvoters <- read.csv(current.voter.file, header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, colClasses = readcolumns)

keepvars <- c("status_cd","ethnic_code","race_code","ncid","county_id","birth_state")

currentvoters <- currentvoters[keepvars]

currentabs <- merge(x = currentabs, y = currentvoters, by = "ncid", all.x = TRUE, sort=FALSE)

currentabs$NC[currentabs$birth_state == "NC"] <- "NC"
currentabs$NC[currentabs$birth_state != "NC"] <- "Elsewhere"

query1 <- sqldf("SELECT county_id, age, ncid, race, race_code, ethnic_code, voter_party_code, gender, NC FROM currentabs WHERE ((count=1) OR ((count>1) AND ((ballot_send_dt !='') AND ((ballot_rtn_status ='') Or (ballot_rtn_status='ACCEPTED')))))")
query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 GROUP BY county_id")

reqs.T <- sum(query2$reqs)
reqs.T

# Race Analysis 
#
# NH White

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE ((race='WHITE') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

reqs.W <- sum(query2$reqs)
reqs.W

currentstat[1,5] <- sum(query2$reqs)

# NH Black

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE ((race='BLACK or AFRICAN AMERICAN') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

reqs.B <- sum(query2$reqs)
reqs.B

currentstat[1,6] <- sum(query2$reqs)

# NH Asian

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE ((race='ASIAN') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

reqs.A <- sum(query2$reqs)
reqs.A

# Hispanics

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (ethnic_code='HL') GROUP BY county_id")

reqs.H <- sum(query2$reqs)
reqs.H

currentstat[1,7] <- sum(query2$reqs)

# Unknown (for Associated Press)

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE ((race='UNDESIGNATED') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

reqs.U <- sum(query2$reqs)

# Other

reqs.O <- reqs.T - reqs.B - reqs.W - reqs.A - reqs.H

# Other (for Associated Press)

currentstat[1,8] <- reqs.T - reqs.W - reqs.B - reqs.H - reqs.U
currentstat[1,9] <- reqs.U
currentstat

abs.race <- data.frame(race=c("Black","White","Asian","Hisp","Oth"),reqs = c(reqs.B, reqs.W, reqs.A, reqs.H, reqs.O))

bar.race.grob <- ggplotGrob(ggplot(data = abs.race,aes(x=race, y = reqs, fill=race)) + 
 scale_fill_manual(values=c("thistle1","grey20","gold","coral3","antiquewhite1")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Gender

# Women

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (gender='F') GROUP BY county_id")
reqs.Women <- sum(query2$reqs)

currentstat[1,10] <- reqs.Women

# Men

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (gender='M') GROUP BY county_id")
reqs.Men <- sum(query2$reqs)

currentstat[1,11] <- reqs.Men

reqs.Unk <- reqs.T - reqs.Women - reqs.Men

currentstat[1,12] <- reqs.Unk
currentstat
abs.gender <- data.frame(gender=c("Women","Men","Unk"),reqs = c(reqs.Women, reqs.Men, reqs.Unk))

bar.gender.grob <- ggplotGrob(ggplot(data = abs.gender,aes(x=gender, y = reqs, fill=gender)) + 
 scale_fill_manual(values=c("darkolivegreen","gray","coral")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Age

query2 <- sqldf("SELECT age, Count(age) AS requests FROM query1 GROUP BY age")

query2$age<-as.numeric(query2$age)

bar.age.grob <- ggplotGrob(ggplot(query2, aes(x=age, y= requests, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  labs(y = "Ballot Requests", x = "Age")+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=c(25,50,75,100)) +
  theme(axis.title.x = element_text(size=10)))

# Age 4 categories (for Associated Press)

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age<30) GROUP BY county_id")
reqs.age1829 <- sum(query2$reqs)

currentstat[1,13] <- reqs.age1829

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>29 AND age<45) GROUP BY county_id")
reqs.age3044 <- sum(query2$reqs)

currentstat[1,14] <- reqs.age3044

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>44 AND age<60) GROUP BY county_id")
reqs.age4459 <- sum(query2$reqs)

currentstat[1,15] <- reqs.age4459

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>59) GROUP BY county_id")
reqs.age60plus <- sum(query2$reqs)

currentstat[1,16] <- reqs.age60plus
currentstat

#####################
# Accepted ballots  #
#####################

query3 <- sqldf("SELECT voter_party_code, Count(voter_party_code) AS accepted FROM currentabs WHERE ballot_rtn_status='ACCEPTED' GROUP BY voter_party_code")

sum(query3$accepted)

query3$voter_party_code <- as.character(query$voter_party_code)

query3[1,1] <- "Dem 2016"
query3[2,1] <- "Lib 2016"
query3[3,1] <- "Rep 2016"
query3[4,1] <- "Una 2016"

currentstat[2,1] <- query3[1,2] 
currentstat[2,2] <- query3[3,2]
currentstat[2,3] <- query3[2,2]
currentstat[2,4] <- query3[4,2]
currentstat

accepted_party_2012_file <- "D:/Research/Turnout/Voter Files/Analyze/NC/2012_accepted_party.csv"
accepted_party_2012 <- read.csv(accepted_party_2012_file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)

accepted_party_2012$rtn_dt <- as.Date(accepted_party_2012$ballot_rtn_dt,format="%m/%d/%Y")

# Fill in missing return dates/party (have to do this for Libertarians)

rtn_dt <- rep(seq(as.Date("2012/09/06"), as.Date("2012/11/08"), by = "day"),  each=4)
rtn_dt <- as.character(rtn_dt)

voter_party_code <- c("DEM","LIB","REP","UNA")
blank_table <- as.data.frame(cbind(rtn_dt, voter_party_code))
colnames(blank_table) <- c("rtn_dt","voter_party_code")

temp <- merge(x = accepted_party_2012, y = blank_table, by = c("rtn_dt","voter_party_code"), all=TRUE)
temp<-as.data.frame(temp)

temp <- temp[order(temp$voter_party_code, temp$rtn_dt),]
temp$Accepted[is.na(temp$Accepted)] <- 0

# Calculate cumulative requests by party

accepted_party_2012 <- ddply(temp, .(voter_party_code), transform, Cumulative.Sum = cumsum(Accepted))

# Select date rage

# Compute days from election

accepted_party_2012$countdown12 <- as.Date("2012-11-06") - accepted_party_2012$rtn_dt
accepted_party_2012

Current.relative.day <- as.Date("2016-11-08") - as.Date(Sys.Date())
Current.relative.day

accept_2012 <- accepted_party_2012[accepted_party_2012[,"countdown12"]==Current.relative.day,]
accept_2012

keepvars <- c("voter_party_code","Cumulative.Sum")
accept_2012 <- accept_2012[keepvars]

colnames(accept_2012)[2] = "accepted"
accept_2012

levels(accept_2012$voter_party_code)[levels(accept_2012$voter_party_code)=="DEM"] <- "Dem 2012"
levels(accept_2012$voter_party_code)[levels(accept_2012$voter_party_code)=="LIB"] <- "Lib 2012"
levels(accept_2012$voter_party_code)[levels(accept_2012$voter_party_code)=="REP"] <- "Rep 2012"
levels(accept_2012$voter_party_code)[levels(accept_2012$voter_party_code)=="UNA"] <- "Una 2012"

combined <- rbind(query3,accept_2012)

combined$voter_party_code <- as.factor(combined$voter_party_code)

combined

bar.party.accept.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = accepted, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Accepted Ballots", x = "") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# Demographics

query1 <- sqldf("SELECT county_id, ncid, age, race_code, ethnic_code, voter_party_code, race, gender FROM currentabs WHERE ballot_rtn_status='ACCEPTED'")
query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 GROUP BY county_id")

acc.T <- sum(query2$acc)
acc.T

# Race Analysis 
#
# Read in voter file to do race analysis (this step requires scraping the file first by running Scrape NC.R)
# This is a large file that can overwhelm some computers

# NH White

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE ((race_code='W') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

acc.W <- sum(query2$acc)
acc.W

currentstat[2,5] <- sum(query2$acc)

# NH Black

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE ((race_code='B') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

acc.B <- sum(query2$acc)
acc.B

currentstat[2,6] <- sum(query2$acc)

# NH Asian

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE ((race_code='A') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

acc.A <- sum(query2$acc)
acc.A

# Hispanics

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE (ethnic_code='HL') GROUP BY county_id")

acc.H <- sum(query2$acc)
acc.H

currentstat[2,7] <- sum(query2$acc)

# Unknown (for Associated Press)

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE ((race='UNDESIGNATED') AND (ethnic_code='NL' OR ethnic_code='UN')) GROUP BY county_id")

acc.U <- sum(query2$acc)

acc.O <- acc.T - acc.B - acc.W - acc.A - acc.H

# Other (for Associated Press)

currentstat[2,8] <- acc.T - acc.W - acc.B - acc.H - acc.U
currentstat[2,9] <- acc.U
currentstat

abs.acc.race <- data.frame(race=c("Black","White","Asian","Hisp","Oth"),acc = c(acc.B, acc.W, acc.A, acc.H, acc.O))

bar.race.accept.grob <- ggplotGrob(ggplot(data = abs.acc.race,aes(x=race, y = acc, fill=race)) + 
 scale_fill_manual(values=c("thistle1","grey20","gold","coral3","antiquewhite1")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Accepted Ballots", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Gender

# Women

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE (gender='F') GROUP BY county_id")
acc.Women <- sum(query2$acc)

currentstat[2,10] <- acc.Women

# Men

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS acc FROM query1 WHERE (gender='M') GROUP BY county_id")
acc.Men <- sum(query2$acc)

currentstat[2,11] <- acc.Men

acc.Unk <- acc.T - acc.Women - acc.Men

currentstat[2,12] <- acc.Unk
currentstat

abs.acc.gender <- data.frame(gender=c("Women","Men","Unk"),acc = c(acc.Women, acc.Men, acc.Unk))

bar.gender.accept.grob <- ggplotGrob(ggplot(data = abs.acc.gender,aes(x=gender, y = acc, fill=gender)) + 
 scale_fill_manual(values=c("darkolivegreen","gray","coral")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Acceptd Ballots", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Age

query3 <- sqldf("SELECT age, Count(age) AS accepted FROM currentabs WHERE ballot_rtn_status='ACCEPTED' GROUP BY age")
query3

query3$age<-as.numeric(query3$age)

# Age 4 categories (for Associated Press)

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age<30) GROUP BY county_id")
reqs.age1829 <- sum(query2$reqs)

currentstat[2,13] <- reqs.age1829

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>29 AND age<45) GROUP BY county_id")
reqs.age3044 <- sum(query2$reqs)

currentstat[2,14] <- reqs.age3044

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>44 AND age<60) GROUP BY county_id")
reqs.age4459 <- sum(query2$reqs)

currentstat[2,15] <- reqs.age4459

query2 <- sqldf("SELECT county_id, COUNT(ncid) AS reqs FROM query1 WHERE (age>59) GROUP BY county_id")
reqs.age60plus <- sum(query2$reqs)

currentstat[2,16] <- reqs.age60plus

bar.age.accept.grob <- ggplotGrob(ggplot(query3, aes(x=age, y= accepted, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Accepted Ballots", x = "Age")+
  scale_x_continuous(breaks=c(25,50,75,100)) +
  theme(axis.title.x = element_text(size=10)))

#####################
# Combine All Plots #
#####################

accepted.label <- paste("Accepted Ballots: ", formatC(sum(query3$accepted), format = "d", big.mark=','))
accepted.label

requested.label <- paste("Requested Ballots: ",as.character(formatC(sum(reqs.T), format = "d", big.mark=',')))
requested.label

requested.label

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(logo.grob, xmin=.5, xmax=2, ymin=0, ymax=.6) +
  annotate("text",x=1.3,y=.1,label="www.electproject.org", size=2.5) +
  annotation_custom(bar.party.accept.grob, xmin=1.2, xmax=5.3, ymin=3.4, ymax=4.9) +
  annotation_custom(bar.party.grob, xmin=5.5, xmax=9.9, ymin=3.4, ymax=4.9) +
  annotation_custom(bar.race.accept.grob, xmin=1.2, xmax=5.3, ymin=2.4, ymax=3.65) +
  annotation_custom(bar.race.grob, xmin=5.5, xmax=9.9, ymin=2.4, ymax=3.65) +
  annotation_custom(bar.age.accept.grob, xmin=1.2, xmax=5.3, ymin=1.1, ymax=2.6) +
  annotation_custom(bar.age.grob, xmin=5.5, xmax=9.9, ymin=1.1, ymax=2.6) +
  annotation_custom(bar.gender.accept.grob, xmin=1.2, xmax=5.3, ymin=0, ymax=1.2) +
  annotation_custom(bar.gender.grob, xmin=5.5, xmax=9.9, ymin=0, ymax=1.2) +
  annotate("text",x=3.5,y=5,label=accepted.label) +
  annotate("text",x=8,y=5,label=requested.label)

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/NC/NC_abs_0921.jpg", sep="")
ggsave(save.image.file, device = "jpeg")

reqs.T

currentstat
write.table(currentstat, "D:/Research/Turnout/Voter Files/Analyze/NC/current_stats.csv", sep = ",", quote = TRUE, dec = ".", col.names = TRUE, row.names = FALSE)

# Cumulative Ballot Requests
#
# Still experimenting with this

query <- sqldf("SELECT ballot_req_dt, Count(voter_party_code) AS req FROM currentabs WHERE ((count=1) OR ((count>1) AND ((ballot_send_dt !='') AND ((ballot_rtn_status ='') Or (ballot_rtn_status='ACCEPTED'))))) GROUP BY ballot_req_dt")
query$ballot_req_dt <- as.Date(query$ballot_req_dt, format="%m/%d/%Y")
query <- query[order(query$ballot_req_dt),]
query$cum_req <- cumsum(query$req)

query$countdown16 <- as.Date("2016-11-08") - query$ballot_req_dt
query

temp <- query[query$ballot_req_dt>as.Date("2016-09-07") & query$ballot_req_dt<Sys.Date(),]
temp

days <- seq(as.Date("2016/09/08"), as.Date("2016/11/08"), by = "day")
days <- as.data.frame(days)
days$countdown16.all <- as.Date("2016-11-08") - days$days

temp <- merge(x = temp, y = days, by.x = "ballot_req_dt", by.y = "days", all.y = TRUE, sort=FALSE)
temp

lastabs.file <- paste(working.dir,"2012_requests.txt", sep="")
lastabs <- read.csv(lastabs.file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors = FALSE)

lastabs$cum_reqs_2012 <- cumsum(lastabs$reqs_2012)
lastabs$ballot_req_dt <- as.Date(lastabs$ballot_req_dt, format="%m/%d/%Y")
lastabs <- lastabs[order(lastabs$ballot_req_dt),]
lastabs

lastabs$countdown12 <- as.Date("2012-11-06") - lastabs$ballot_req_dt

temp2 <- lastabs[lastabs$ballot_req_dt>as.Date("2012-09-05") & lastabs$ballot_req_dt<as.Date("2012-11-07"),]
temp2

abs_12_16 <- merge(x = temp, y = temp2, by.x = "countdown16.all", by.y = "countdown12", all = TRUE, sort=FALSE)
abs_12_16

ggplot(abs_12_16, aes(x = ballot_req_dt.x, y = cum_reqs_2012)) +
 scale_y_continuous(breaks=seq(0,3000000,by=100000)) +
 scale_y_continuous(labels = scales::comma) +
 geom_line(colour="firebrick", linetype="dashed") +
 ggtitle("2016 NC Absentee Ballot Requests with 2012 Comparison") +
 labs(y = "Ballot Requests", x = "Date") +
 theme_minimal() +
 geom_line(data = abs_12_16, aes(x = ballot_req_dt.x, y = cum_req))

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/NC/NC_abreq_0913.jpg", sep="")
ggsave(save.image.file, device = "jpeg")



