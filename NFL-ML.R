#lets try to build a general model for all teams. Lets start with a middle of the road
#team. Maybe that will give better predictability for ALL teams when compared to 
#training a model for only one team. 
#last season, the Atlanta falcons went 8-8. They also have a points scored and
#points against that is only -6. The lowest of any team in the NFL last season. 
#PCR has given the best sort of results. So lets drag, drop, and analyze.
rm(list = ls())
library(ISLR)
library(MASS)
library(boot)
library(tree)
library(glmnet)
library(leaps)
library(gam)
library(pls)
library(nflscrapR)
library(caret)
library(kernlab)
library(e1071)
library(randomForest)
library(SparseM)
set.seed(49)

setwd("C:/Users/Nathan/Downloads/NFL")
NFL= data.frame(read.csv('NFLPlaybyPlay2015.csv',stringsAsFactors = FALSE))
attach(NFL)

team.all = which(posteam =="DEN" | DefensiveTeam == "DEN")
team.all = NFL[team.all,]
team.off = which(posteam =="DEN")
team.Def = which(DefensiveTeam == "DEN")
NFL.team = team.all
NFL.team.off = NFL[team.off,]

team.score.offense = NFL.team.off[c('yrdln','yrdline100','ydsnet',
                                'Yards.Gained','Touchdown','sp',
                                'AbsScoreDiff','Drive',
                                'TimeSecs','PosTeamScore','GoalToGo')]
print(cov(team.score.offense))
detach(NFL)
#end = which(qtr == 4 & TimeSecs == 0) - 1
#pdf('Histdrive.pdf')
#histogram(NFL$Drive)
#dev.off()
#var = rep(0,464)
#var[1:232] = NFL$PosTeamScore[end]
#var[233:464] = NFL$DefTeamScore[end]
#jpeg('Pointdistribution.jpeg')
#densityplot(var,xlab = "Actual score of the game")
#dev.off()



#############################################################
ctrl = trainControl(method = "repeatedcv",
                    number = 3,
                    repeats = 2,
                    search = "random")
team.tree =train(PosTeamScore~.,data = team.score.offense,
               method = "rf",
               tuneLength = 15,
               trControl = ctrl)
#Look like mtry is 9!
team.treefit = tree(PosTeamScore~.,data = team.score.offense)
team.rf = randomForest(PosTeamScore~.,data = team.score.offense,
                      mtry = 9,ntree = 500)
team.rfpred = predict(team.rf,newdata = team.score.offense)
print(mean((team.rfpred - team.score.offense$PosTeamScore)^2))

team.svm = svm(PosTeamScore~.,data = team.score.offense,kernel = "radial")
team.svmpred = predict(team.svm,newdata = team.score.offense)
team.tune = tune(svm,PosTeamScore~.,data = team.score.offense,
                ranges = list(epsilon = seq(.05,.4,0.050),
                              cost = seq(from = 1, to = 40,by = 1)))
#We now have the best SVM model with our parameters. 
#Now we can use the best model to make predictions.
team.tunebest = team.tune$best.model
team.tunepred = predict(team.tunebest,newdata = team.score.offense)
#print(mean((team.svmpred - team.score.offense$PosTeamScore)^2))
print(mean((team.tunepred - team.score.offense$PosTeamScore)^2))

ctrl = trainControl(method = "boot",
                    repeats = 10)
team.plsfit = train(PosTeamScore~.,data = team.score.offense,
                   method = "rpart",
                   tuneLength = 10,trControl = ctrl)
team.trainplspred = predict(team.plsfit,newdata = team.score.offense)
print(mean((team.trainplspred - team.score.offense$PosTeamScore)^2))
pls.fit = plsr(PosTeamScore~.,data = team.score.offense,ncomp = 10,
               validation = "CV",scale = TRUE)

# Lets pull current NFL data to test our models against it for an "average MSE" 
# across the season so far to see how our models are doing. 
NFL2016 = nflscrapR::season_play_by_play(2016,Weeks = 12) #Pulling 2016 season up to week 12
nadown = which(is.na(NEWNFL$down))
NFL.na = NFL2016[-nadown,]
NFL.svmpred = predict(team.tunebest,newdata = NFL.na)
NFL.rfpred = predict(team.rf,newdata = NFL.na)
NFL.plspred = predict(team.plsfit,newdata = NFL.na)

print(mean((NFL.svmpred - NFL.na$PosTeamScore)^2))
print(mean((NFL.rfpred - NFL.na$PosTeamScore)^2))
print(mean((NFL.plspred - NFL.na$PosTeamScore)^2))
#NFL.offense = NFL.na[c('yrdln','yrdline100','ydsnet',
#                                    'Yards.Gained','Touchdown','sp',
#                                    'AbsScoreDiff','Drive',
#                                    'TimeSecs','PosTeamScore','GoalToGo')]
#NFL.pred = predict(team.tunebest,newdata = NFL.offense)
#print(mean((NFL.pred - NFL.na$PosTeamScore)^2))

# GAME ID for Miami versus SF November 27,2016
# END SCORE 31-24 MIAMI
# nflscrapR::game_play_by_play(2016112706)

# Game ID for Dallas versus SF October 2, 2016
# END SCORE 24-17 Dallas
# nflscrapR::game_play_by_play(2016100211) -> SF2016
# 2015 Superbowl(2016020700)
# 2013 Superbowl(2014020200)
# 2012 Superbowl(2013020300)
# 2016112001
# 2017 Superbowl(2017020500)
NFLGame = function(gameid){
  team.game = nflscrapR::game_play_by_play(gameid)
  #SVM TREE PLS FOR TEAM 1
  team.off = which(team.game$posteam == unique(team.game$posteam)[1])
  game.predictme = team.game[team.off,]
  team.svmpred = predict(team.tunebest,newdata = game.predictme)
  team.treepred = predict(team.rf,newdata = game.predictme)
  team.plspred = predict(team.plsfit,newdata = game.predictme)
  print(unique(team.game$posteam)[1])
  print('SVM of team in this game mean squared error')
  print(mean((game.predictme$PosTeamScore - team.svmpred)^2))
  print('Tree model of team in this game mean squared error')
  print(mean((game.predictme$PosTeamScore - team.treepred)^2))
  print('PLS of team in this game mean squarederror')
  dev.new()
  #jpeg('Ten1vsIndOct232016.jpeg')
  print(mean((game.predictme$PosTeamScore - team.plspred)^2))
  plot(game.predictme$PosTeamScore,type = 'l',lwd = 3)
  points(team.svmpred,col = "green",lwd = 3)
  points(team.treepred,col = "red",lwd = 3)
  points(team.plspred,col = "blue",lwd = 3)
  legend('topleft',c("Actual score","SVM","Tree","PLS"),lty= c(1,3,3,3),
         lwd = c(3,3,3,3),col = c("Black","green","red","blue"))
  title('TEAM 1 SCORE PREDICTION')
  #dev.off()
  print('Actual end score')
  print(max(na.omit(game.predictme$PosTeamScore)))
  print('~~~~~~~~~~~~~~~~~~~~')
 
  #SVM TREE PLS FOR TEAM 2
  team.off = which(team.game$posteam == unique(team.game$posteam)[2])
  game.predictme = team.game[team.off,]
  team.svmpred = predict(team.tunebest,newdata = game.predictme)
  team.treepred = predict(team.rf,newdata = game.predictme)
  team.plspred = predict(team.plsfit,newdata = game.predictme)
  print(unique(team.game$posteam)[2])
  print('SVM of team in this game mean squared error')
  print(mean((game.predictme$PosTeamScore - team.svmpred)^2))
  print('Tree model of team in this game mean squared error')
  print(mean((game.predictme$PosTeamScore - team.treepred)^2))
  print('PLS of team in this game mean squared error')
  dev.new()
  #jpeg('TenvsInd1Oct232016.jpeg')
  print(mean((game.predictme$PosTeamScore - team.plspred)^2))
  plot(game.predictme$PosTeamScore,type = 'l',lwd = 3)
  points(team.svmpred,col = "green",lwd = 3)
  points(team.treepred,col = "red",lwd = 3)
  points(team.plspred,col = "blue",lwd = 3)
  legend('topleft',c("Actual score","SVM","Tree","PLS"),lty= c(1,3,3,3),
         lwd = c(3,3,3,3),col = c("Black","green","red","blue"))
  title('TEAM 2 SCORE PREDICTION')

  #dev.off()
  print('Actual end score')
  print(max(na.omit(game.predictme$PosTeamScore)))
  print('~~~~~~~~~~~~~~~~~~~~')
}









