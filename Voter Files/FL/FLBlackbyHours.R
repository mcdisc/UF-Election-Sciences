library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(broom)

setwd('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/')

FLBlack2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016BlackEarly.csv')
FLBlack2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Black.csv')
FLHispanic2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Hispanic.csv')
FLWhite2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012White.csv')
FLHispanic2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016HispanicEarly.csv')
FLWhite2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016WhiteEarly.csv')
FLAll2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2016AllEarly.csv')
FLAll2012 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012AllEarly.csv')
FLAll2016 <- FLAll2016[1:67,]
FLAll2012 <- FLAll2012[1:67, 1:9]

FL2016EarlyHours <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/2016EarlyVotingFLHours.csv', colClasses = list(integer = 2:6))
FL2012EarlyHours <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/FL2012Hours.csv')

FL2016EarlyHours <- FL2016EarlyHours[1:67,]
FL2012EarlyHours <- FL2012EarlyHours[1:67,]


ethnicregtotal2016 <- fread('/Users/Potalora/Documents/UF_Elections/FL_Early_Voting/ethnicregtotal2016.csv')

processearly <- function(df, dfnew, name, race){
  df$TotalEarly2012 <- df$EarlyInPerson
  df$TotalAbsentee2012 <- df$VBM
  df$TotalEthnicTurnout2012 <- df$VBM + df$EarlyInPerson + df$ElectionDay
  df$EarlyRatio2012 <- df$TotalEarly2012/df$TotalEthnicTurnout2012
  df$AbsenteeRatio2012 <- df$TotalAbsentee2012/df$TotalEthnicTurnout2012
  dfjoined <- left_join(dfnew, df, by = c('County Code' = 'County'))
  dfjoined$PercentDiffEarly <- (dfjoined$EarlyRatio - dfjoined$EarlyRatio2012)/dfjoined$EarlyRatio
  dfjoined <- left_join(dfjoined, ethnicregtotal2016, by = c('County Code' = 'County'))
  dfjoined$EarlyRatioTurn2012 <- dfjoined$EarlyInPerson/dfjoined$Total
  dfjoined$EarlyRatioTurn2016 <- dfjoined$TotalEarly/dfjoined[[race]]
  dfjoined$PercentDiffTurn <- (dfjoined$EarlyRatioTurn2016 - dfjoined$EarlyRatioTurn2012)/dfjoined$EarlyRatioTurn2012
  dfjoined2012hours <- left_join(dfjoined, FL2012EarlyHours, by = c('County Code' = 'CountyAbbr'))
  dfjoined1216hours <- left_join(dfjoined2012hours, FL2016EarlyHours, by = c('County.x' = 'County'))
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

race <- c('3','4','5')

result <- vector("list", 3)

for (i in 1:3){
   result[[i]] <- processearly(df2012[[i]], df2016[[i]], names[i], race[i])
}

for (i in 1:3){
  assign(dfnames[i], (as.data.frame(result[[i]])))
}


visualization <- function(df, title){
  model <- lm(PercentDiffEarly ~ PercentChange, data = df)
  watup <- ggplot(data = df, aes(100*PercentChange, 100*PercentDiffEarly)) +
    geom_point(aes(size = TotalEarly)) +
    geom_smooth(method = lm) +
    geom_text(aes(label = `County Code`), position = position_nudge(y = 2)) +
    labs(y = 'Percentage Difference in Early Vote Turnout', x = 'Percentage Change in Total Hours', title = title)
  watup
}



model1 <- lm(PercentDiffEarly ~ PercentChange, data = FLH1216Hours)
FLHTest <- augment(model1, FLH1216Hours)

model2 <- lm(PercentDiffEarly ~ PercentChange, data = FLW1216Hours)
FLWTest <- augment(model2, FLW1216Hours)

model3 <- lm(PercentDiffEarly ~ PercentChange, data = FLB1216Hours)
FLBTest <- augment(model3, FLB1216Hours)

p1 <- FLBTest %>%
  plot_ly(x = ~PercentChange*100,
          showlegend = FALSE) %>%
  add_markers(x = ~PercentChange*100, y = ~PercentDiffEarly*100, marker = list(opacity = 0.5, sizemode = 'diameter'), 
              text = ~paste('County:', County.x, '<br>Early Turnout:', TotalEarly),
              size = ~TotalEarly,
              sizes = c(10,50),
              hoverinfo = 'text') %>%
  add_markers(x = FLHTest$PercentChange*100, y = FLHTest$PercentDiffEarly*100, marker = list(opacity = 0.5, sizemode = 'diameter'), 
              text = paste('County:', FLHTest$County.x, '<br>Early Turnout:', FLHTest$TotalEarly),
              size = FLHTest$TotalEarly,
              sizes = c(10,50),
              hoverinfo = 'text',
              visible = FALSE) %>%
  add_markers(x = FLWTest$PercentChange*100, y = FLWTest$PercentDiffEarly*100, marker = list(opacity = 0.5, sizemode = 'diameter'), 
              text = paste('County:', FLWTest$County.x, '<br>Early Turnout:', FLWTest$TotalEarly),
              size = FLWTest$TotalEarly,
              sizes = c(10,50),
              hoverinfo = 'text',
              visible = FALSE) %>%
  add_ribbons(ymin = ~.fitted*100 - 1.96 * .se.fit*100, 
              ymax = ~.fitted*100 + 1.96 * .se.fit*100, line = list(width = 0.5), color = I('gray80'), mode = 'lines',
              hoverinfo = 'none') %>%
  add_trace(y = ~.fitted*100, line = list(width = 0.5), color = I('steelblue'), mode = 'lines',
            hoverinfo = 'none') %>%
  add_ribbons(ymin = FLHTest$.fitted*100 - 1.96 * FLHTest$.se.fit*100, 
              ymax = FLHTest$.fitted*100 + 1.96 * FLHTest$.se.fit*100, line = list(width = 0.5), color = I('gray80'), mode = 'lines',
              hoverinfo = 'none',
              visible = FALSE) %>%
  add_trace(y = FLHTest$.fitted*100, line = list(width = 0.5), color = I('steelblue'), mode = 'lines',
            hoverinfo = 'none',
            visible = FALSE) %>%
  add_ribbons(ymin = FLWTest$.fitted*100 - 1.96 * FLWTest$.se.fit*100, 
              ymax = FLWTest$.fitted*100 + 1.96 * FLWTest$.se.fit*100, line = list(width = 0.5), color = I('gray80'), mode = 'lines',
              hoverinfo = 'none',
              visible = FALSE) %>%
  add_trace(y = FLWTest$.fitted*100, line = list(width = 0.5), color = I('steelblue'), mode = 'lines',
            hoverinfo = 'none',
            visible = FALSE) 
  


p1 <- p1 %>%
  layout(yaxis = list(range = c(-30,90)),
         title = 'Change in Ethnic Turnout as Proportion of Total Turnout',
    updatemenus = list(
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
               label = "Black"),
          
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)),
               label = "Hispanic"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)),
               label= 'White')))
    )
  )

plotly_POST(p1, 'test')



Black <- visualization(FLB1216Hours, 'Black')
Hispanic <- visualization(FLH1216Hours, 'Hispanic')
White <- visualization(FLW1216Hours, 'White')
All <- visualization(FLAll1216Hours, 'All Ethnicities')

processearlyall <- function(df, dfnew, name){
  df$TotalEarly2012 <- df$EIP + df$`EIP Provisional Accept`
  df$TotalAbsentee2012 <- df$ABS
  df$TotalEthnicTurnout2012 <- df$`ABS` + df$EIP + df$`EIP Provisional Accept` + df$`Election Day` + df$`ED Provisional Accept`
  df$EarlyRatio2012 <- df$TotalEarly2012/df$TotalEthnicTurnout2012
  df$AbsenteeRatio2012 <- df$TotalAbsentee2012/df$TotalEthnicTurnout2012
  dfjoined <- left_join(dfnew, df, by = c('County Code' = 'County'))
  dfjoined$PercentDiffEarly <- (dfjoined$EarlyRatio - dfjoined$EarlyRatio2012)/dfjoined$EarlyRatio
  dfjoined <- left_join(dfjoined, ethnicregtotal2016, by = c('County Code' = 'County'))
  dfjoined2012hours <- left_join(dfjoined, FL2012EarlyHours, by = c('County Code' = 'CountyAbbr'))
  dfjoined1216hours <- left_join(dfjoined2012hours, FL2016EarlyHours, by = c('County.x' = 'County'))
  dfjoined1216hours$Total2012Hours <- dfjoined1216hours$`2012 EV Locations` * dfjoined1216hours$TotHours
  dfjoined1216hours$Total2016Hours <- dfjoined1216hours$`Total Locations` * dfjoined1216hours$`Total Hours`
  dfjoined1216hours$PercentChange <- (dfjoined1216hours$Total2016Hours- dfjoined1216hours$Total2012Hours)/dfjoined1216hours$Total2012Hours
  write.csv(dfjoined1216hours, name, row.names = FALSE)
  dfjoined1216hours
}

FLAll1216Hours <- processearlyall(FLAll2012, FLAll2016, 'FLAll1216Hours.csv')
Black
ggsave('Black1216Hours.png',width = 600, height = 500, units = 'mm', scale = 0.4)
White
ggsave('White1216Hours.png',width = 600, height = 500, units = 'mm', scale = 0.4)
Hispanic
ggsave('Hispanic1216Hours.png',width = 600, height = 500, units = 'mm', scale = 0.4)
All
ggsave('All1216Hours.png',width = 600, height = 500, units = 'mm', scale = 0.4)

