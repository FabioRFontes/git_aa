
library(data.table)
library(biglm)

setwd("~/Downloads/train")
train <- fread("train_V2.csv", header = T)
chunck1.crashfpp <- train[matchType=="crashfpp", ]
chunck2.crashtpp <- train[matchType=="crashtpp", ]
chunck3.duo <- train[matchType=="duo", ]
chunck4.duo_fpp <- train[matchType=="duo-fpp", ]
chunck5.flarefpp <- train[matchType=="flarefpp", ]
chunck6.flaretpp <- train[matchType=="flaretpp", ]
chunck7.normal_duo <- train[matchType=="normal-duo", ]
chunck8.normal_duo_fpp <- train[matchType=="normal-duo-fpp", ]
chunck9.normal_solo <- train[matchType=="normal-solo", ]
chunck10.normal_solo_fpp <- train[matchType=="normal-solo-fpp", ]
chunck11.normal_squad <- train[matchType=="normal-squad", ]
chunck12.normal_squad_fpp <- train[matchType=="normal-squad-fpp", ]
chunck13.solo <- train[matchType=="solo", ]
chunck14.solo_fpp <- train[matchType=="solo-fpp", ]
chunck15.squad <- train[matchType=="squad", ]
chunck16.squad_fpp <- train[matchType=="squad-fpp", ]

attach(train)
train[matchType=="squad-fpp" & groupId=="4d4b580de459be",]
matchType=="squad-fpp" & groupId=="4d4b580de459be"
summary(as.factor(matchType))
ff <- winPlacePerc ~ . - Id - matchId - groupId - matchType
ff <- assists ~ kills + killStreaks + longestKill + maxPlace + rankPoints + revives + roadKills
model <- biglm(ff, data=train)
model <- lm(ff, data=chunck16.squad_fpp)
summary(model)

library(leaps)
regfit.fwd=regsubsets(ff,data=train,nvmax=39,method="forward")
summary(regfit.fwd)

class(regfit.fwd)
