library(ggplot2)
library(tidyverse)
library(readxl)

mlb <- read.csv("D:MLBTeamStats.csv")
str(mlb)


#GraphForAverageRank
plot1 <- ggplot(data = mlb, aes(y = Wins, x = AverageRank)) + geom_point() + geom_smooth(method="lm")
print(plot1)

model3 <- lm(Wins ~ AverageRank, data=mlb)
sm1<-summary(model3)
print(sm1)

print(confint(model3,level=0.95))

#GraphForEarnedRunsAverage
plot <- ggplot(data = mlb, aes(y = Wins, x = EarnedRunsAverage)) + geom_point() + geom_smooth(method="lm")
print(plot)

model <- lm(Wins ~ EarnedRunsAverage, data=mlb)
sm<-summary(model)
print(sm)

print(confint(model,level=0.95))

#GraphForTriples
plot2 <- ggplot(data = mlb, aes(y = Wins, x = Triples)) + geom_point() + geom_smooth(method="lm")
print(plot2)

model2 <- lm(Wins ~ Triples, data=mlb)
sm2<-summary(model2)
print(sm2)

print(confint(model2,level=0.95))

#GraphForRuns
plot4 <- ggplot(data = mlb, aes(y = Wins, x = Runs)) + geom_point() + geom_smooth(method="lm")
print(plot4)

model4 <- lm(Wins ~ Runs, data=mlb)
sm4<-summary(model4)
print(sm4)

print(confint(model4,level=0.95))

#GraphForHits
plot5 <- ggplot(data = mlb, aes(y = Wins, x = Hits)) + geom_point() + geom_smooth(method="lm")
print(plot5)

model5 <- lm(Wins ~ Hits, data=mlb)
sm5<-summary(model5)
print(sm5)

print(confint(model5,level=0.95))

#GraphForDoubles
plot6 <- ggplot(data = mlb, aes(y = Wins, x = Doubles)) + geom_point() + geom_smooth(method="lm")
print(plot6)

model6 <- lm(Wins ~ Doubles, data=mlb)
sm6<-summary(model6)
print(sm6)

print(confint(model6,level=0.95))

#GraphForHomeruns
plot7 <- ggplot(data = mlb, aes(y = Wins, x = Homeruns)) + geom_point() + geom_smooth(method="lm")
print(plot7)

model7 <- lm(Wins ~ Homeruns, data=mlb)
sm7<-summary(model7)
print(sm7)

print(confint(model7,level=0.95))

#GraphForRunsBattedIn
plot8 <- ggplot(data = mlb, aes(y = Wins, x = RunsBattedIn)) + geom_point() + geom_smooth(method="lm")
print(plot8)

model8 <- lm(Wins ~ RunsBattedIn, data=mlb)
sm8<-summary(model8)
print(sm8)

print(confint(model8,level=0.95))

#GraphForQualityStarts
plot9 <- ggplot(data = mlb, aes(y = Wins, x = QualityStarts)) + geom_point() + geom_smooth(method="lm")
print(plot9)

model9 <- lm(Wins ~ QualityStarts, data=mlb)
sm9<-summary(model9)
print(sm9)

print(confint(model9,level=0.95))

#GraphForWalks
plot10 <- ggplot(data = mlb, aes(y = Wins, x = Walks)) + geom_point() + geom_smooth(method="lm")
print(plot10)

model10 <- lm(Wins ~ Walks, data=mlb)
sm10<-summary(model10)
print(sm10)

print(confint(model10,level=0.95))

#GraphForStrikeoutsBatting
plot11 <- ggplot(data = mlb, aes(y = Wins, x = StrikeoutsBatting)) + geom_point() + geom_smooth(method="lm")
print(plot11)

model11 <- lm(Wins ~ StrikeoutsBatting, data=mlb)
sm11<-summary(model11)
print(sm11)

print(confint(model11,level=0.95))


#GraphForStolenBases
plot12 <- ggplot(data = mlb, aes(y = Wins, x = StolenBases)) + geom_point() + geom_smooth(method="lm")
print(plot12)

model12 <- lm(Wins ~ StolenBases, data=mlb)
sm12<-summary(model12)
print(sm12)

print(confint(model12,level=0.95))

#GraphForBattingAverage
plot13 <- ggplot(data = mlb, aes(y = Wins, x = BattingAverage)) + geom_point() + geom_smooth(method="lm")
print(plot13)

model13 <- lm(Wins ~ BattingAverage, data=mlb)
sm13<-summary(model13)
print(sm13)

print(confint(model13,level=0.95))

#GraphForOnBasePercentage
plot14 <- ggplot(data = mlb, aes(y = Wins, x = OnBasePercentage)) + geom_point() + geom_smooth(method="lm")
print(plot14)

model14 <- lm(Wins ~ OnBasePercentage, data=mlb)
sm14<-summary(model14)
print(sm14)

print(confint(model14,level=0.95))

#GraphForSlugging
plot15 <- ggplot(data = mlb, aes(y = Wins, x = Slugging)) + geom_point() + geom_smooth(method="lm")
print(plot15)

model15 <- lm(Wins ~ Slugging, data=mlb)
sm15<-summary(model15)
print(sm15)

print(confint(model15,level=0.95))

#GraphForSaves
plot16 <- ggplot(data = mlb, aes(y = Wins, x = Saves)) + geom_point() + geom_smooth(method="lm")
print(plot16)

model16 <- lm(Wins ~ Saves, data=mlb)
sm16<-summary(model16)
print(sm16)

print(confint(model16,level=0.95))

#GraphForHolds
plot17 <- ggplot(data = mlb, aes(y = Wins, x = Holds)) + geom_point() + geom_smooth(method="lm")
print(plot17)

model17 <- lm(Wins ~ Holds, data=mlb)
sm17<-summary(model17)
print(sm17)

print(confint(model17,level=0.95))

#GraphForShutouts
plot18 <- ggplot(data = mlb, aes(y = Wins, x = Shutouts)) + geom_point() + geom_smooth(method="lm")
print(plot18)

model18 <- lm(Wins ~ Shutouts, data=mlb)
sm18<-summary(model18)
print(sm18)

print(confint(model18,level=0.95))

#GraphForHitsAllowed
plot19 <- ggplot(data = mlb, aes(y = Wins, x = HitsAllowed)) + geom_point() + geom_smooth(method="lm")
print(plot19)

model19 <- lm(Wins ~ HitsAllowed, data=mlb)
sm19<-summary(model19)
print(sm19)

print(confint(model19,level=0.95))

#GraphForHomerunsAllowed
plot20 <- ggplot(data = mlb, aes(y = Wins, x = HomerunsAllowed)) + geom_point() + geom_smooth(method="lm")
print(plot20)

model20 <- lm(Wins ~ HomerunsAllowed, data=mlb)
sm20<-summary(model20)
print(sm20)

print(confint(model20,level=0.95))

#GraphForWalksAllowed
plot21 <- ggplot(data = mlb, aes(y = Wins, x = WalksAllowed)) + geom_point() + geom_smooth(method="lm")
print(plot21)

model21 <- lm(Wins ~ WalksAllowed, data=mlb)
sm21<-summary(model21)
print(sm21)

print(confint(model21,level=0.95))

#GraphForStrikeoutsPitching
plot22 <- ggplot(data = mlb, aes(y = Wins, x = StrikeoutsPitching)) + geom_point() + geom_smooth(method="lm")
print(plot22)

model22 <- lm(Wins ~ StrikeoutsPitching, data=mlb)
sm22<-summary(model22)
print(sm22)

print(confint(model22,level=0.95))

#GraphForFieldingPercentage
plot23 <- ggplot(data = mlb, aes(y = Wins, x = FieldingPercent)) + geom_point() + geom_smooth(method="lm")
print(plot23)

model23 <- lm(Wins ~ FieldingPercent, data=mlb)
sm23<-summary(model23)
print(sm23)

print(confint(model23,level=0.95))