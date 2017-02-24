library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)


FLBlack2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016BlackEarly.csv')
FLBlack2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Black.csv')
FLHispanic2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Hispanic.csv')
FLWhite2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012White.csv')
FLHispanic2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016HispanicEarly.csv')
FLWhite2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016WhiteEarly.csv')


FL2016EarlyHours <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/2016EarlyVotingFLHours.csv', colClasses = list(integer = 2:6))
FL2012EarlyHours <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Hours.csv')

FL2016EarlyHours <- FL2016EarlyHours[1:67,]
FL2012EarlyHours <- FL2012EarlyHours[1:67,]


processearly <- function(df, dfnew, name){
  df$TotalEarly2012 <- df$EarlyInPerson
  df$TotalAbsentee2012 <- df$VBM
  df$TotalEthnicTurnout2012 <- df$VBM + df$EarlyInPerson + df$ElectionDay
  df$EarlyRatio2012 <- df$TotalEarly2012/df$TotalEthnicTurnout2012
  df$AbsenteeRatio2012 <- df$TotalAbsentee2012/df$TotalEthnicTurnout2012
  dfjoined <- left_join(dfnew, df, by = c('County Code' = 'County'))
  dfjoined$PercentDiffEarly <- (dfjoined$EarlyRatio - dfjoined$EarlyRatio2012)/dfjoined$EarlyRatio
  dfjoined2012hours <- left_join(dfjoined, FL2012EarlyHours, by = c('County Code' = 'CountyAbbr'))
  dfjoined1216hours <- left_join(dfjoined2012hours, FL2016EarlyHours, by = 'County')
  dfjoined1216hours$Total2012Hours <- dfjoined1216hours$`2012 EV Locations` * dfjoined1216hours$TotHours
  dfjoined1216hours$Total2016Hours <- dfjoined1216hours$`Total Locations` * dfjoined1216hours$`Total Hours`
  dfjoined1216hours$PercentChange <- (dfjoined1216hours$Total2016Hours- dfjoined1216hours$Total2012Hours)/dfjoined1216hours$Total2012Hours
  write.csv(dfjoined1216hours, name, row.names = FALSE)
  dfjoined1216hours
}


dfnames <- c('FLB1216Hours', 'FLH1216Hours', 'FLW1216Hours')

df2012 <- list(FLBlack2012, FLHispanic2012, FLWhite2012)

df2016 <- list(FLBlack2016, FLHispanic2016, FLWhite2016)

names <- c('FLB1216Hours.csv', 'FLH1216Hours.csv', 'FLW1216Hours.csv')

result <- vector("list", 3)

for (i in 1:3){
   result[[i]] <- processearly(df2012[[i]], df2016[[i]], names[i])
}

for (i in 1:3){
  assign(dfnames[i], (as.data.frame(result[[i]])))
}


visualization <- function(df){
  model <- lm(PercentDiffEarly ~ PercentChange, data = df)
  watup <- ggplot(data = df, aes(PercentChange, PercentDiffEarly)) +
    geom_point(aes(size = TotalEarly)) +
    geom_smooth(method = lm) +
    geom_text(aes(label = `County Code`), position = position_nudge(y = 0.02))
  watup
}

visualizationtest <- function(df){
  model <- lm(PercentDiff ~ PercentChange, data = df)
  watup <- ggplot(data = df, aes(PercentChange, PercentDiff)) +
    geom_point(aes(size = TotalEarly)) +
    geom_smooth(method = lm) +
    geom_text(aes(label = `County Code`), position = position_nudge(y = 0.02)) +
    xlim(-0.5,2.5) +
    ylim(-0.3,1.78)
  watup
}


Black <- visualization(FLB1216Hours)
Hispanic <- visualization(FLH1216Hours)
White <- visualization(FLW1216Hours)


