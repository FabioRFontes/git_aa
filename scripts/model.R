# ---------------------------------------------------------------------------- #
                                                                               
#   Script com métodos utilizados na resolução do projeto

# ---------------------------------------------------------------------------- #
# Libraries

library(data.table)
library(AlgDesign)
library(dplyr)
library(randomForest)

# ---------------------------------------------------------------------------- #


setwd("/home/fabio/Desktop/UMinho_4_Ano/gits/git_aa/datasets/")

training <- fread("train_V2.csv", header = T)
test <- fread("test_V2.csv", header = T)


attach(training)
attach(test)
names(training)

length(Id[is.na(Id)])
length(headshotKills[is.na(headshotKills)])
length(groupId[is.na(groupId)])
length(matchId[is.na(matchId)])
length(assists[is.na(assists)])
length(boosts[is.na(boosts)])
length(damageDealt[is.na(damageDealt)])
length(DBNOs[is.na(DBNOs)])
length(heals[is.na(heals)])
length(killPlace[is.na(killPlace)])
length(killPoints[is.na(killPoints)])
length(kills[is.na(kills)])
length(killStreaks[is.na(killStreaks)])
length(longestKill[is.na(longestKill)])
length(matchDuration[is.na(matchDuration)])
length(matchType[is.na(matchType)])
length(maxPlace[is.na(maxPlace)])
length(numGroups[is.na(numGroups)])
length(rankPoints[is.na(rankPoints)])
length(revives[is.na(revives)])
length(rideDistance[is.na(rideDistance)])
length(roadKills[is.na(roadKills)])
length(swimDistance[is.na(swimDistance)])
length(teamKills[is.na(teamKills)])
length(vehicleDestroys[is.na(vehicleDestroys)])
length(walkDistance[is.na(walkDistance)])
length(weaponsAcquired[is.na(weaponsAcquired)])
length(winPoints[is.na(winPoints)])
length(winPlacePerc[is.na(winPlacePerc)])




# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff <- winPlacePerc ~ . - Id - matchId - groupId - matchType
ff2 <- winPlacePerc ~ .
ff3 <- winPlacePerc ~ assists
ff3 <- winPlacePerc ~ kills
train$Id = NULL
train$groupId = NULL

train <- train[!is.na(train$winPlacePerc),]
is.na(training$winPlacePerc)

boxplot(ff3, data=training)
boxplot(assists)
boxplot(walkDistance)
summary(as.factor(assists))

cor(training, method="pearson")

model <- randomForest(ff2, data=train)
model <- lm(ff2, data=train)
summary(model)
p <- predict(model, test)

p[c(1,2,2,3)]
?rm(model)
gc()

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff2 <- winPlacePerc ~ . - Id - matchId - groupId + (killPoints * rankPoints)

model2 <- lm(ff2, data=training)
summary(model2)

rm(model2)
gc()

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff3 <- winPlacePerc ~ killPoints * rankPoints

model3 <- lm(ff3, data=training)
summary(model3)

rm(model3)
gc()

# Permite obter o vetor dos t values
s <- summary(model)
coef(s)[, "t value"]

rm(test)
gc()


# ---------------------------------------------------------------------------- #
#### Primeira submissão no kagglelibrary(data.table)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("train_V2.csv", header = T)
train$Id = NULL
train$groupId = NULL
train$matchId = NULL
train$longestKill = NULL
train$matchType = NULL
cor(train, method="pearson")
cor(train, method="spearman")
cor(train, method="kendall")

ff3 <- winPlacePerc ~ . + winPoints * rankPoints
model <- lm(ff3,data=train)

test<-fread("../input/test_V2.csv", header = T)
test$Id = NULL
test$groupId = NULL
test$matchId = NULL
test$longestKill = NULL
p <- predict(model,test)
names(test)
test<- fread("../input/test_V2.csv", header = T)
submission = cbind(Id=test$Id,winPlacePerc=p)
write.csv(submission,'sample_submission.csv',row.names = FALSE)


#####################
### Segunda submissão
library(data.table)
library(dplyr)
library(randomForest)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("../input/train_V2.csv", header = T)
train <- train %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace),
            winPlacePerc = (mean(winPlacePerc)))
train$groupId = NULL
train$longestKill = NULL
train<-train[!is.na(train$winPlacePerc),]
y<-train$winPlacePerc
ff3 <- winPlacePerc ~ .
train$winPlacePerc=NULL
model <- randomForest(train,y,nodesize=20,n_jobs=4,maxnodes=30,data=train)
summary(model)
test<-fread("../input/test_V2.csv", header = T)
test <- test %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace))
groupIdVector <- test$groupId
test$groupId = NULL
test$longestKill = NULL
p <- predict(model,test) 

results <- cbind(groupIdVector,p)

test<- fread("../input/test_V2.csv", header = T)
ab<-match(test$groupId,results[,1])
results <- p[ab]
noUnderZero<-function(x){if(x<0){return (x=0)}else{return(x=x)}}
noOverOne<-function(x){if(x>1){return (x=1)}else{return(x=x)}}
#test<- cbind(test,winPlacePerc=results)
results<- lapply(results, noUnderZero)
results<- lapply(results,noOverOne)
results<- unlist(results)
submission <- cbind(Id=test$Id, winPlacePerc=results)
#submission[,2]<-lapply(submission[,2],noUnderZero)
#submission<-lapply(submission$winPlacePerc,noOverOne)
write.csv(submission,'sample_submission.csv',row.names = FALSE)


##################
### 3º Submissão - randomForests
library(data.table)
library(dplyr)
library(randomForest)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("../input/train_V2.csv", header = T)
train <- train %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace),
            winPlacePerc = (mean(winPlacePerc)))
train$groupId = NULL
train$longestKill = NULL
train<-train[!is.na(train$winPlacePerc),]
y<-train$winPlacePerc
ff3 <- winPlacePerc ~ .
train$winPlacePerc=NULL
model <- randomForest(train,y,nodesize=20,n_jobs=4,maxnodes=30,data=train)
summary(model)
test<-fread("../input/test_V2.csv", header = T)
test <- test %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace))
groupIdVector <- test$groupId
test$groupId = NULL
test$longestKill = NULL
p <- predict(model,test) 

results <- cbind(groupIdVector,p)

test<- fread("../input/test_V2.csv", header = T)
ab<-match(test$groupId,results[,1])
results <- p[ab]
noUnderZero<-function(x){if(x<0){return (x=0)}else{return(x=x)}}
noOverOne<-function(x){if(x>1){return (x=1)}else{return(x=x)}}
#test<- cbind(test,winPlacePerc=results)
results<- lapply(results, noUnderZero)
results<- lapply(results,noOverOne)
results<- unlist(results)
submission <- cbind(Id=test$Id, winPlacePerc=results)
#submission[,2]<-lapply(submission[,2],noUnderZero)
#submission<-lapply(submission$winPlacePerc,noOverOne)
write.csv(submission,'sample_submission.csv',row.names = FALSE)






####################################
## Random Forests - almost maxed out options

library(data.table)
library(dplyr)
library(randomForest)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("../input/train_V2.csv", header = T)
train <- train %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace),
            winPlacePerc = (mean(winPlacePerc)))
train$groupId = NULL
train$longestKill = NULL
train<-train[!is.na(train$winPlacePerc),]
y<-train$winPlacePerc
ff3 <- winPlacePerc ~ .
train$winPlacePerc=NULL
model <- randomForest(train,y,nodesize=4,n_jobs=4,maxnodes=2250,data=train)
summary(model)



test<-fread("../input/test_V2.csv", header = T)
test <- test %>% 
  group_by(groupId) %>%
  summarise(Id = length(Id),
            assists = sum(assists),
            DBNOs = sum(DBNOs),
            boosts = sum(boosts),
            damageDealt = sum(damageDealt),
            headshotKills = sum(headshotKills),
            heals = sum(heals),
            killPlace= mean(killPlace),
            killPoints = mean(killPoints),
            killStreaks = max(killStreaks),
            kills = sum(kills),
            longestKill = max(longestKill),
            matchDuration = mean(matchDuration),
            rankPoints = mean(rankPoints),
            revives = sum(revives),
            rideDistance = max(rideDistance),
            roadKills = sum(roadKills),
            swimDistance = max(swimDistance),
            teamKills = sum(teamKills),
            vehicleDestroys = sum(vehicleDestroys),
            walkDistance = max(walkDistance),
            weaponsAcquired = sum(weaponsAcquired),
            winPoints = min(winPoints),
            numGroups = mean(numGroups),
            maxPlace = mean(maxPlace))
groupIdVector <- test$groupId
test$groupId = NULL
test$longestKill = NULL
p <- predict(model,test) 

results <- cbind(groupIdVector,p)

test<- fread("../input/test_V2.csv", header = T)
ab<-match(test$groupId,results[,1])
results <- p[ab]
noUnderZero<-function(x){if(x<0){return (x=0)}else{return(x=x)}}
noOverOne<-function(x){if(x>1){return (x=1)}else{return(x=x)}}
#test<- cbind(test,winPlacePerc=results)
results<- lapply(results, noUnderZero)
results<- lapply(results,noOverOne)
results<- unlist(results)
submission <- cbind(Id=test$Id, winPlacePerc=results)
#submission[,2]<-lapply(submission[,2],noUnderZero)
#submission<-lapply(submission$winPlacePerc,noOverOne)
write.csv(submission,'sample_submission.csv',row.names = FALSE)

