# ---------------------------------------------------------------------------- #

# R Script with various models for the dataset.

# ---------------------------------------------------------------------------- #
# Libraries

library(data.table)
library(randomForest)
# library(AlgDesign)
# library(dplyr)

# ---------------------------------------------------------------------------- #
# Data Load

setwd("/home/fabio/Desktop/UMinho_4_Ano/gits/git_aa/datasets/")

train <- fread("train_V2.csv", header = T)
test <- fread("test_V2.csv", header = T)

names(train)
names(test)

# ---------------------------------------------------------------------------- #
# Data processing.

# Train Dataset ---------------------- #

attach(train)

# Search for any missing value in any of the train dataset fields.
length(Id[is.na(Id)]) # 0
length(headshotKills[is.na(headshotKills)]) # 0
length(groupId[is.na(groupId)]) # 0
length(matchId[is.na(matchId)]) # 0
length(assists[is.na(assists)]) # 0
length(boosts[is.na(boosts)]) # 0
length(damageDealt[is.na(damageDealt)]) # 0
length(DBNOs[is.na(DBNOs)]) # 0
length(heals[is.na(heals)]) # 0
length(killPlace[is.na(killPlace)]) # 0
length(killPoints[is.na(killPoints)]) # 0
length(kills[is.na(kills)]) # 0
length(killStreaks[is.na(killStreaks)]) # 0
length(longestKill[is.na(longestKill)]) # 0
length(matchDuration[is.na(matchDuration)]) # 0
length(matchType[is.na(matchType)]) # 0
length(maxPlace[is.na(maxPlace)]) # 0
length(numGroups[is.na(numGroups)]) # 0
length(rankPoints[is.na(rankPoints)]) # 0
length(revives[is.na(revives)]) # 0
length(rideDistance[is.na(rideDistance)]) # 0
length(roadKills[is.na(roadKills)]) # 0
length(swimDistance[is.na(swimDistance)]) # 0
length(teamKills[is.na(teamKills)]) # 0
length(vehicleDestroys[is.na(vehicleDestroys)]) # 0
length(walkDistance[is.na(walkDistance)]) # 0
length(weaponsAcquired[is.na(weaponsAcquired)]) # 0
length(winPoints[is.na(winPoints)]) # 0
length(winPlacePerc[is.na(winPlacePerc)]) # 1
# The only field with missing values is the winPlacePerc that has 1 missing
# value. 

# Removal of the row with the missing value in the field winPlacePerc, from
# the train dataset.
train <- train[!is.na(train$winPlacePerc)]

# Train dataset without the Ids and the variable dim to be the least
# influential in the determination of winPlacePerc.
train2 <- train
train2$Id = NULL
train2$groupId = NULL
train2$matchId = NULL
train2$longestKill = NULL

# Train dataset without the Ids and the variable dim to be the least
# influential in the determincation of the winPlacePerc. In this dataset,
# the stats of group members, in matches that are played in squads or duos,
# are join together, through the use of operations such as sum or mean,
# this way all the members of a group are combine in a single player.
train3 <- training
train3 <- train3 %>% 
  group_by(groupId) %>%
  summarise(gElements = length(Id),
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
            # longestKill = max(longestKill),
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
train3$groupId = NULL
# train3$longestKill = NULL



# Test Dataset ---------------------- #

attach(test)

# Search for any missing value in any of the test dataset fields.
length(Id[is.na(Id)]) # 0
length(headshotKills[is.na(headshotKills)]) # 0
length(groupId[is.na(groupId)]) # 0
length(matchId[is.na(matchId)]) # 0
length(assists[is.na(assists)]) # 0
length(boosts[is.na(boosts)]) # 0
length(damageDealt[is.na(damageDealt)]) # 0
length(DBNOs[is.na(DBNOs)]) # 0
length(heals[is.na(heals)]) # 0
length(killPlace[is.na(killPlace)]) # 0
length(killPoints[is.na(killPoints)]) # 0
length(kills[is.na(kills)]) # 0
length(killStreaks[is.na(killStreaks)]) # 0
length(longestKill[is.na(longestKill)]) # 0
length(matchDuration[is.na(matchDuration)]) # 0
length(matchType[is.na(matchType)]) # 0
length(maxPlace[is.na(maxPlace)]) # 0
length(numGroups[is.na(numGroups)]) # 0
length(rankPoints[is.na(rankPoints)]) # 0
length(revives[is.na(revives)]) # 0
length(rideDistance[is.na(rideDistance)]) # 0
length(roadKills[is.na(roadKills)]) # 0
length(swimDistance[is.na(swimDistance)]) # 0
length(teamKills[is.na(teamKills)]) # 0
length(vehicleDestroys[is.na(vehicleDestroys)]) # 0
length(walkDistance[is.na(walkDistance)]) # 0
length(weaponsAcquired[is.na(weaponsAcquired)]) # 0
length(winPoints[is.na(winPoints)]) # 0
# There are no missing values in any of the data fields in the test dataset. 

# Test dataset without the Ids and the variable dim to be the least
# influential in the determination of winPlacePerc.
test2 <- test
test2$Id = NULL
test2$groupId = NULL
test2$matchId = NULL
test2$longestKill = NULL

# Test dataset without the Ids and the variable dim to be the least
# influential in the determination of the value of winPlacePerc. In this 
# dataset, the stats of group members, in matches that are played
# in squads or duos, are join together, through the use of operations
# such as sum or mean, this way all the members of a group are 
# combine in a single player.
test3 <- test
test3 <- test3 %>% 
  group_by(groupId) %>%
  summarise(gElements = length(Id),
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
            # longestKill = max(longestKill),
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
test3$groupId = NULL
# test3$longestKill = NULL


# ---------------------------------------------------------------------------- #
# Formulas

fm1 <- winPlacePerc ~ .
fm2 <- winPlacePerc ~ . - Id - matchId - groupId . matchType
fm3 <- winPlacePerc ~ . + winPoints * rankPoints
fm4 <- winPlacePerc ~ . + killPoints * rankPoints

cor1 <- cor(train2)
cor2 <- cor(train3)

# ---------------------------------------------------------------------------- #
# Models

# Linear Models ---------------------- #
# It's not pratical to do the model with all the variables as predictors,
# because variables such as Id, which are categorical variables with
# to many values.
model1 <- lm(fm1, data=train)
summary(model1)

# Model done without any categorical variable.
model2 <- lm(fm2, data=train)
summary(model2)

# --------------------------------------------- #
# Models build with the dataset train1 as a basis.

model3 <- lm(fm1, data=train2)
summary(model3)

model4 <- lm(fm3, data=train2)
summary(model4)

model5 <- lm(fm4, data=train2)
summary(model5)

# --------------------------------------------- #
# Models build with the dataset train2 as a basis.

model6 <- lm(fm1, data=train3)
summary(model6)

model7 <- lm(fm3, data=train3)
summary(model7)

model8 <- lm(fm4, data=train3)
summary(model8)

# Random Forest ---------------------- #


# ---------------------------------------------------------------------------- #
# Prevision and submition of data

p1 <- predict(model2, test)

p2 <- predict(model3, test2)
p3 <- predict(model4, test2)
p4 <- predict(model5, test2)

p5 <- predict(model6, test3)
p6 <- predict(model7, test3)
p7 <- predict(model8, test3)

# ---------------------------------------------------------------------------- #
# Data cleaning

rm(train1)
rm(train2)

rm(fm1)
rm(fm2)
rm(fm3)
rm(fm4)

rm(model1)
rm(model2)
rm(model3)
rm(model4)

gc()

# ---------------------------------------------------------------------------- #
