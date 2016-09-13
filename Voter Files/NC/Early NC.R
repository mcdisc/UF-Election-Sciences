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

# Working Directory

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/NC/"

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

# readcolumns = c(NA, "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, NA)

current <- read.csv(current.file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)

dup <- sqldf("SELECT ncid, Count(ncid) AS count FROM current GROUP BY ncid")

query

temp <- merge(x = current, y = dup, by = "ncid", all=TRUE, sort=FALSE)

query <- sqldf("SELECT voter_party_code, Count(voter_party_code) AS req FROM temp WHERE ((count=1) OR ((count>1) AND ((ballot_send_dt !='') AND ((ballot_rtn_status ='') Or (ballot_rtn_status='ACCEPTED'))))) GROUP BY voter_party_code")

ggplot(data = query,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("darkblue","green","red","yellow")) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "", title="NC Absentee Ballot Requests as of 9/12") +
 guides(fill=FALSE) +
 theme_minimal()

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/NC/NC_abreq_0912.jpg", sep="")
ggsave(save.image.file, device = "jpeg")

