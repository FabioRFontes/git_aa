# ---------------------------------------------------------------------------- #
                                                                               
#   Script com métodos utilizados na resolução do projeto

# ---------------------------------------------------------------------------- #
# Libraries

library(data.table)
library(AlgDesign)
library(dplyr)

# ---------------------------------------------------------------------------- #

setwd("/home/fabio/Desktop/UMinho_4_Ano/gits/git_aa/datasets/")

training <- fread("train_V2.csv", header = T)
trest <- fread("test_V2.csv", header = T)

attach(training)
names(training)

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff <- winPlacePerc ~ . - Id - matchId - groupId - matchType
ff2 <- winPlacePerc ~ . - groupId - Id
model <- lm(ff2, data=train)
summary(model)

rm(model)
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

attach(model)
names(model)
class(coefficients)
names(coefficients)
effects
s <- summary(model)
coef(s)[, "t value"]

# ---------------------------------------------------------------------------- #
#### Primeira submissão no kagglelibrary(data.table)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("../input/train_V2.csv", header = T)
train$Id = NULL
train$groupId = NULL
train$matchId = NULL
train$longestKill = NULL
ff3 <- winPlacePerc ~ . + winPoints * rankPoints
model <- lm(ff3,data=train)

test<-fread("../input/test_V2.csv", header = T)
test$Id = NULL
test$groupId = NULL
test$matchId = NULL
test$longestKill = NULL
p <- predict(model,test) 

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

