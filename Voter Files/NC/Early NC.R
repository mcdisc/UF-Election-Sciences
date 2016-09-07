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


