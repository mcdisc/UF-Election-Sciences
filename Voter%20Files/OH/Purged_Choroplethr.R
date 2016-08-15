library(sqldf)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(gridGraphics)

#upload your file into current week, and then use the sql function to count the occurances from each county

location = "file location"
currentweek <- read.csv(location)
query <- sqldf("SELECT COUNTY_NUMBER_week1, Count(COUNTY_NUMBER_week1) AS filldata FROM currentweek GROUP BY COUNTY_NUMBER_week1")

#upload the County Codes file

Ohio_County_Codes <- read.csv("filelocation")
countycodes <- Ohio_County_Codes

#often when you load csv, it drops the leading zeros. this bit puts them back
countycodes$FIPS.Code = str_pad(countycodes$FIPS.Code, 3, pad = "0")

#changes the name ofthe column to something that makes it easy to merge the two datasets
colnames(query)[colnames(query)=="COUNTY_NUMBER_week1"] <- "Number"

#and here is the merge
total <- merge(query, countycodes, by="Number")

#the choroplethr function needs a column named region and one named value, so we'll switch it up here
colnames(total)[colnames(total)=="FIPS.Code"] <- "region"
colnames(total)[colnames(total)=="filldata"] <- "value"

#the region value must also have the state FP number at the beginning, which is why the leading 0s are important
#this is pretty clearly a lazy way of doing this but it's fast and easy
total$region = str_pad(total$region, 4, pad = "9")
total$region = str_pad(total$region, 5, pad = "3")
total$region = as.numeric(total$region)


#and finally, the visualization part
county_choropleth(total, state_zoom = "ohio", legend = "Purged Voters")