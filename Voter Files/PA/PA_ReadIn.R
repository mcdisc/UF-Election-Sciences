library(data.table)
library(dplyr)
options(nwarnings = 10000) 

#For the purposes of this script I manually made two directories- one with the county voter files
#and one with the county election map files

#Load in voter files as a list of data frames

path <- '/Users/Potalora/Documents/UF_Elections/Voter_Files/Voter_Files/PA/Voter_Files_20170906'

files <- list.files(path=path)

list <- list(for(file in files)
{
  assign(file, fread(file, header = F))
  
}
)
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

rm(list=names(Filter(is.data.frame, mget(ls(all=T)))))

#Load in county election map files as a list of data frames

path <- '/Users/Potalora/Documents/UF_Elections/Voter_Files/Voter_Files/PA/Election_Map_20170906'

files <- list.files(path=path)

list <- list(for(file in files)
{
  assign(file, fread(file, header = F))
  
}
)
Election_Maps <- Filter(function(x) is(x, "data.frame"), mget(ls()))

rm(list=names(Filter(is.data.frame, mget(ls(all=T)))))

#Ensure that all elements of the list are data frames

for (i in 1:length(Election_Maps)){
  Election_Maps[[i]] <- as.data.frame(Election_Maps[[i]])
}

#Read in voter file schema and assign it to county voter files

schema <- read.csv('/Users/Potalora/Documents/UF_Elections/Voter_Files/Voter_Files/PA/Schema.csv', header = F, stringsAsFactors = F)

for(i in 1:length(dfs)){
  colnames(dfs[[i]]) <- schema$V1
}

cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

#The voter file schema includes placeholders for elections, which have columns for vote method and party
#This loop creates a column in each county election map file that will then be the names of columns for elections

for (y in 1:length(Election_Maps)) {
  ticker <- 1
  elections <- c()
  for (z in 1:nrow(Election_Maps[[y]])) {
    elections[ticker] <- paste0(Election_Maps[[y]]$V3[z], '_VoteMethod')
    elections[ticker + 1] <- paste0(Election_Maps[[y]]$V3[z], '_Party')
    ticker<-ticker + 2
  }
  Election_Maps[[y]] <- cbind.fill(Election_Maps[[y]], elections)
  Election_Maps[[y]] <- as.data.frame(Election_Maps[[y]])
}

#This loop 'splices' the previously made vector into the corresponding placeholders in the county voter files
#Each county has different elections and # of elections-that is why the number of columns being input is different per county

for(i in 1:length(dfs)) {
  setnames(dfs[[i]], old = c(71:(70 + nrow(Election_Maps[[i]]))), new = as.character(Election_Maps[[i]]$V5))
}

#Select desirable columns-regex is needed b/c counties have different names for same elections

selecteddfs <- list()

for(i in 1:length(dfs)){
  selecteddfs[[i]] <- subset(dfs[[i]], select = c('ID Number', 
                                                  'Last Name',
                                                  'First Name', 
                                                  'Middle Name', 
                                                  'Gender',
                                                  'DOB', 
                                                  'Registration Date',
                                                  'Party Code',
                                                  'House Number',
                                                  'House Number Suffix',
                                                  'Street Name',
                                                  'Apartment Number',
                                                  'Address Line 2',
                                                  'City',
                                                  'State',
                                                  'Zip',
                                                  'Mail Address 1',
                                                  'Mail Address 2',
                                                  'Mail City',
                                                  'Mail State',
                                                  'Mail Zip',
                                                  #Counties have very very weird names for 2008-very hard to make regex
                                                  #grep('^2008 GENERAL ELECTION.*\\_Party', colnames((dfs[[i]])), value = T),
                                                  #grep('^2008 GENERAL ELECTION.*\\_VoteMethod', colnames(dfs[[i]]), value = T),
                                                  grep('^2010 GENERAL ELECTION.*\\_Party', colnames(dfs[[i]]), value = T),
                                                  grep('^2010 GENERAL ELECTION.*\\_VoteMethod', colnames(dfs[[i]]), value = T),
                                                  grep('^2012 GENERAL ELECTION.*\\_Party', colnames(dfs[[i]]), value = T),
                                                  grep('^2012 GENERAL ELECTION.*\\_VoteMethod', colnames(dfs[[i]]), value = T),
                                                  grep('^2014 GENERAL ELECTION.*\\_Party', colnames(dfs[[i]]), value = T),
                                                  grep('^2014 GENERAL ELECTION.*\\_VoteMethod', colnames(dfs[[i]]), value = T),
                                                  grep('^2016 GENERAL ELECTION.*\\_Party', colnames(dfs[[i]]), value = T),
                                                  grep('^2016 GENERAL ELECTION_.*\\VoteMethod', colnames(dfs[[i]]), value = T)))
}

#Bind county data frames into single data frame

PA <- rbind_list(selecteddfs)
