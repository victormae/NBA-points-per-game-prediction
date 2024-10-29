
# Title:  'Predicting Points per game in an NBA game using NBA 2022/2023 datset'
# Author: "Victor Oche"

library(tidyverse)
library(corrplot)
library(psych)

nba <- read.csv("NBA_Player_stats.csv", sep=";")
head(nba)

summary(nba)

colSums(is.na(nba))

#we'll divide the dataset into team and player stats
nbaF <- nba[nba1$Tm != "TOT", ]

nba_plyrstat <- nbaF[c(3:4, 6:8, 12:13, 15:16, 19:20, 22:23, 25:30)]

head(nba_plyrstat)

nba_tmstat <- nbaF[ -c(1, 2, 5, 9, 18, 24)]

head(nba_tmstat)

summary(nba_plyrstat)

summary(nba_tmstat)

duplicate_rows <- nba_plyrstat[duplicated(nba_plyrstat) | duplicated(nba_plyrstat, fromLast = TRUE), ] #checking for duplicate rows
print(duplicate_rows)

summary(nba_plyrstat)

duplicate_rows2 <- nba_tmstat[duplicated(nba_tmstat) | duplicated(nba_tmstat, fromLast = TRUE), ]
print(duplicate_rows2)

#on the nba player stat
head(nba_plyrstat)

#Create dummy
library(fastDummies)
nba_plyrstat2 <- dummy_cols(nba_plyrstat, select_columns = "Pos")

head(nba_plyrstat2)

pairs.panels(nba_plyrstat[,c(-1)]) #scatterplot

##Transforming the variables
library(dplyr)
nba_plyrstat3 <- nba_plyrstat2 %>%
  mutate_at(vars(GS), ~log(1 + .)) %>% mutate_at(vars(X2P ), ~log(1 + .)) %>% 
  mutate_at(vars(X3P), ~log(1 + .)) %>% mutate_at(vars(X3PA), ~log(1 + .)) %>% 
  mutate_at(vars(X2PA), ~log(1 + .)) %>% mutate_at(vars(FT), ~log(1 + .)) %>% 
  mutate_at(vars(FTA), ~log(1 + .)) %>% mutate_at(vars(ORB), ~log(1 + .)) %>% 
  mutate_at(vars(DRB), ~log(1 + .)) %>% mutate_at(vars(AST), ~log(1 + .)) %>% 
  mutate_at(vars(STL), ~log(1 + .)) %>% mutate_at(vars(BLK), ~log(1 + .)) %>% 
  mutate_at(vars(TOV), ~log(1 + .)) %>% mutate_at(vars(PTS), ~log(1 + .))


head(nba_plyrstat3)
#guys do the same for the nba_tmstat dataset
library(ggplot2)

g1 <- ggplot(nba_plyrstat3, aes(x= GS)) + geom_histogram()
g2 <- ggplot(nba_plyrstat3, aes(x= G)) + geom_histogram()
g3 <- ggplot(nba_plyrstat3, aes(x= X3P)) + geom_histogram()
g4 <- ggplot(nba_plyrstat3, aes(x= X2P)) + geom_histogram()
g5 <- ggplot(nba_plyrstat3, aes(x= FT)) + geom_histogram()
g6 <- ggplot(nba_plyrstat3, aes(x= FTA)) + geom_histogram()
g7 <- ggplot(nba_plyrstat3, aes(x= ORB)) + geom_histogram()
g8 <- ggplot(nba_plyrstat3, aes(x= DRB)) + geom_histogram()
g9 <- ggplot(nba_plyrstat3, aes(x= AST)) + geom_histogram()
g10 <- ggplot(nba_plyrstat3, aes(x= STL)) + geom_histogram()
g11 <- ggplot(nba_plyrstat3, aes(x= BLK)) + geom_histogram()
g12 <- ggplot(nba_plyrstat3, aes(x= TOV)) + geom_histogram()
g13 <- ggplot(nba_plyrstat3, aes(x= PTS)) + geom_histogram()

library(gridExtra)

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow = 4)

#we check correlations
head(nba_plyrstat3)

nba_plyrstat4 <- nba_plyrstat3[, -c(1)]
head(nba_plyrstat4)

cor1 = cor(nba_plyrstat4) #correlation
corrplot(cor1,order="AOE")

 

head(nba_plyrstat4)

KMO(nba_plyrstat4)

#all have 0.5. We investigate further.
#From our cor plot, we can see that X3PA, X3P, X2PA, X2P, BLK, FT and FTa, ORB have very similar correlation. 
#we take them out.

#so we take one out leaving the other. we leave the not so obvious.
nba_plyrstatrd <- nba_plyrstat4[, -c(5, 7, 9, 11)] #so we take one out leaving the other. we leave the not so obvious
head(nba_plyrstatrd)

KMO(nba_plyrstatrd)
#Each of them still gives us 0.5
MCorrTest = corr.test(nba_plyrstatrd, adjust="none")

M = MCorrTest$p
round(M, 2)
MTest = ifelse(M < .01, T, F)
MTest

colSums(MTest) - 1

#DRB will be its own factor, Also let Age be it's own factor for now*

#we run an initial OLS to see if there is still heavy multi-collinearity
fit2 = lm(PTS ~., nba_plyrstatrd)
summary(fit2)

library(car)
vif(fit2)


#dribble may have to be its own factor
head(nba_plyrstatrd)

library(psych)
nba_plyrd <- nba_plyrstatrd[, -c(1, 8, 14)] # response var and highly correlated DRB and AGE vars since it is its own factor

head(nba_plyrd)
str(nba_plyrd)

pc = prcomp(nba_plyrd, scale=T)
summary(pc)
plot(pc)
abline(1, 0, col="red")

fa.parallel(nba_plyrd)
# components

#nba_plyrd$DRB <- nba_plyrstatrd$DRB
head(nba_plyrd)
#nba_plyrd <- nba_plyrd[,-c(18)]
pf1 = principal(nba_plyrd, rotate="varimax",  nfactors=5 )
summary(pf1)
print(pf1$loadings,cutoff=.4,sort=T)

scores = as.data.frame(pf1$scores)
head(scores)

names(scores) = c("actions_in_game",  "3pattemps_by_SG&BLK_by_C", "synergy_btw_PG$SG", "synergy_btw_SG&PF_intbyPF", "synergy_btw_SG&SF_intbySF")
scores$PTS = nba_plyrstatrd$PTS
scores$DRB <- nba_plyrstatrd$DRB
scores$Age <- nba_plyrstatrd$Age
head(scores)

#running an OLS regression

fitR1 = lm(PTS ~., data=scores)
summary(fitR1)

#vif

vif(fitR1)

#Doing for factor Analysis
head(nba_plyrstatrd)

nba_factor <- nba_plyrstatrd[, -c(1, 8, 14, 15, 16, 17, 18, 19)]#removing Age, PTS, DRB and dummys.

head(nba_factor)
pfc = prcomp(nba_factor, scale=T)
summary(pfc)
plot(pfc)
abline(1, 0, col="red")

fa.parallel(nba_factor)

fitnba_ply1 = factanal(nba_factor, factors=2)
print(fitnba_ply1)
print(fitnba_ply1$loadings, cutoff=.4, sort=T)

fact_loadings <- fitnba_ply1$loadings 

scores3 <- as.data.frame(as.matrix(nba_factor) %*% fact_loadings)

head(scores3)

names(scores3) = c("Actions in games played including blocks - 3PA(c)",  "Actions in games with 3pa - blocks(SG)")
scores3$PTS = nba_plyrstatrd$PTS
scores3$DRB <- nba_plyrstatrd$DRB
scores3$Age <- nba_plyrstatrd$Age
#adding back the position dummies vars
scores3$Pos_C <- nba_plyrstatrd$Pos_C
scores3$Pos_PF <- nba_plyrstatrd$Pos_PF
scores3$Pos_PG <- nba_plyrstatrd$Pos_PG
scores3$Pos_SF <- nba_plyrstatrd$Pos_SF
scores3$Pos_SG <- nba_plyrstatrd$Pos_SG

head(scores3)

#run an OLS reg

fft1 = lm(PTS ~., data=scores3)
summary(fft1)
#=======================================================================================================
#We run PCA and PFA and FA on the data without transforming it to see.

head(nba_plyrstat2)

nba_pstatpt2 <- nba_plyrstat2[, -c(1)]
head(nba_pstatpt2)

cor3 = cor(nba_pstatpt2)
corrplot(cor3, order ="AOE")

nba_pstatpt3 <- nba_pstatpt2[, -c(5, 7, 9, 11)]#removing equally correlated vars from the corplot and the response var

head(nba_pstatpt3)

MCorrTest3 = corr.test(nba_pstatpt2, adjust="none")

M3 = MCorrTest3$p
round(M3, 2)
MTest1 = ifelse(M3 < .01, T, F)
MTest1
colSums(MTest1) - 1

#DRB has to be it's own factor and age as well
head(nba_pstatpt2)

nba_pstatpfa <- nba_pstatpt2[, -c(1, 18, 12)]
head(nba_pstatpfa)

#doing PFA on the full untransformed data without removing highly corr to see

put = prcomp(nba_pstatpfa, scale=T) #p untransformed
summary(put)
plot(put)
abline(1, 0, col="red")

fa.parallel(nba_pstatpfa)


head(nba_pstatpfa)

ptpf2 = principal(nba_pstatpfa, rotate="varimax",  nfactors=6 )
summary(ptpf2)
print(ptpf2$loadings, cutoff=.4, sort=T)

scores4 = as.data.frame(ptpf2$scores)
head(scores4)
names(scores4) = c("2Pt_2Pa_FreeT&FreeTA_ASt&ToV_in_GS&MP", "AST_TOV_3P_3PA_STL&PF in Games", "Personal_fouls_ORB_BLK_on 3p & 3PA_by_C", "Synergy_btw_PG&SG_in_Ast", "PF", "synergy_btw_SG&SF")

head(scores4)
scores4$PTS = nba_pstatpt2$PTS
scores4$DRB <- nba_pstatpt2$DRB
scores4$Age <- nba_pstatpt2$Age
head(scores4)

#run a regression
ftu1 = lm(PTS ~., data=scores4)
summary(ftu1)

vif(ftu1)

#removing those with high vif scores

ftu =lm(PTS ~ `AST_TOV_3P_3PA_STL&PF in Games`+`Personal_fouls_ORB_BLK_on 3p & 3PA_by_C`+ `Synergy_btw_PG&SG_in_Ast`+ PF + `synergy_btw_SG&SF` + DRB  +  Age, data= scores4)
summary(ftu)

vif(ftu)
summary(ftu)


head(nba_pstatpfa)


nba_pstatpfa2 <- nba_pstatpfa[, -c(16, 17, 18, 19, 20)] #takiing out dummies for CFA Untransformed

v = prcomp(nba_pstatpfa2, scale=T) #p untransformed
summary(v)
plot(v)
abline(1, 0, col="red")

fa.parallel(nba_pstatpfa2)

ftpfa = factanal(nba_pstatpfa2, factors=2)#Factanal did not work with the untransformed dataset
print(ftpfa)
print(ftpfa$loadings, cutoff=.4)
ftpfa_loadings <- ftpfa$loadings

scores7 <- as.data.frame(as.matrix(nba_pstatpfa2) %*% ftpfa_loadings)

head(scores7)

names(scores7) = c("Actions_in_games_played_including blocks_minus_3PA,3p&STLs", "Actions_in_Games_Including_3PA&3PA -2P,ORB,BLK&PF")
scores7$PTS = nba_pstatpt2$PTS
scores7$DRB <- nba_pstatpt2$DRB
scores3$Age <- nba_pstatpt2$Age
#adding back the position dummies vars
scores7$Pos_C <- nba_pstatpt2$Pos_C
scores7$Pos_PF <- nba_pstatpt2$Pos_PF
scores7$Pos_PG <- nba_pstatpt2$Pos_PG
scores7$Pos_SF <- nba_pstatpt2$Pos_SF
scores7$Pos_SG <- nba_pstatpt2$Pos_SG
head(scores7)

#running OLS in the CFA 2(untransformed) scores 
utfit <- lm(PTS ~., data=scores7)

summary(utfit)
#=======================================================================================================
#untransformed taking highly correlated out
head(nba_pstatpt3)
KMO(nba_pstatpt3)
MCorrTest4 = corr.test(nba_pstatpt3, adjust="none")

M4 = MCorrTest4$p
round(M4, 2)
MTest2 = ifelse(M4 < .01, T, F)
MTest2
colSums(MTest2) - 1

#again DRB and Age will be their own factor
head(nba_pstatpt3)
nba_pstatpt4 <-nba_pstatpt3[, -c(1, 8, 14)] 

head(nba_pstatpt4)


b = prcomp(nba_pstatpt4, scale=T) #p untransformed 2
summary(b)
plot(b)
abline(1, 0, col="red")

fa.parallel(nba_pstatpt4)
#5 components

head(nba_pstatpt4)



pnt = principal(nba_pstatpt4, rotate="varimax",  nfactors=5)
summary(pnt)
print(pnt$loadings, cutoff=.4, sort=T)

scores5 = as.data.frame(pnt$scores)
head(scores5)
names(scores5) = c("actions_in_game_played",  "3pattemps_by_SG&BLK_by_C", "synergy_btw_PG$SG_in_Ast", "synergy_btw_PF&SG_inTbyPF", "synergy_btw_SF&SG")
scores5$PTS = nba_pstatpt3$PTS # Adding back PTS, DRB and Age
scores5$DRB <- nba_pstatpt3$DRB
scores5$Age <- nba_pstatpt3$Age
head(scores5)

#fit the ols model

ftut <- lm(PTS ~., data= scores5)
summary(ftut)

vif(ftut)

#CFA with untransformed data and with Aliasing removed
head(nba_pstatpt4)

nba_statpt7 <- nba_pstatpt4[, -c(12,13,14,15,16)]

head(nba_statpt7)
#Factanal with the untransformed data
c1 = prcomp(nba_statpt7, scale=T) #p untransformed 3 with dummys removed
summary(c1)
plot(c1)
abline(1, 0, col="red")

fa.parallel(nba_statpt7)

head(nba_statpt7)

#Running CFA3

faut = factanal(nba_statpt7, factors =2)
print(faut)
print(faut$loadings, cutoff=.4, sort=T)

fact_loadingsut <- faut$loadings 

scores6 <- as.data.frame(as.matrix(nba_statpt7) %*% fact_loadingsut)

head(scores6)

names(scores6) = c("Actions_in_ Game - 2PA_FTA&BLK", "Actions_in_games_started - 3PA_STL_PF&BLKS")

head(nba_pstatpt3)
#Adding back PTS, DRB, Age and Position dummy variables
scores6$PTS = nba_pstatpt3$PTS
scores6$DRB <- nba_pstatpt3$DRB
scores6$Age <- nba_pstatpt3$Age
#adding back the position dummies vars
scores6$Pos_C <- nba_pstatpt3$Pos_C
scores6$Pos_PF <- nba_pstatpt3$Pos_PF
scores6$Pos_PG <- nba_pstatpt3$Pos_PG
scores6$Pos_SF <- nba_pstatpt3$Pos_SF
scores6$Pos_SG <- nba_pstatpt3$Pos_SG

head(scores6)

#fit reg model

fitunt = lm(PTS ~., data = scores6)
summary(fitunt)

vif(fitunt)
#removing high var with high vif score

#ftut4 = lm(PTS ~.-`3pA_in _MP`-`2pa_FreeTA_Ast&Tov_in_MP&Gs`- `2Pa_FreeTA_BLKs_PF_STL_ingames`, data=scores6) 
#summary(ftut4)

#vif(ftut4)

#head(scores)
#===========================================================================================================================#
# All subsets for validating the models

#PFA 1 Scores
#CFA1 scores3
install.packages("leaps")
library(leaps)
PFA1Subsets = regsubsets(PTS ~ ., data=scores)

plot(PFA1Subsets, scale="bic")

#validating the model for CFA1 using allsubsets
CFA1Subsets = regsubsets(PTS ~ ., data=scores3)
plot(CFA1Subsets, scale="adjr2")

#fiiting it in an ols gives 
#"Actions in games played including blocks - 3PA(c)",  "Actions in games with 3pa - blocks(SG)" DRB, Age, Pos_c, Pos_SG
CFA1bestfit <- lm(PTS ~. -Pos_PF - Pos_PG - Pos_SF, data=scores3)
summary(CFA1bestfit)

#we check how much the significance of Age

agecheck <- lm(PTS~ Age, data= scores3)
summary(agecheck)
