install.packages("readstata13")
install.packages("mlogit")
install.packages("sandwich")

library(readstata13)
library(tidyverse)
library(sandwich)
library(mlogit)

setwd("D:/GitHub/Research-Projects/FLCovid_2020/")
data_raw <- read.dta13("data/BEBR_final.dta")


##Helper Recode Functions
#NAing all negative values
to_na <- function(x){
  ifelse(x < 0, NA, x )
  
}

#0/1 dummies.
make_dum <- function(x){
  ifelse(x == 2, 0 , x)
}

data_cleaned <- data_raw %>%
  #Sets all values less than zero to NA
  mutate(across(.cols = everything(), to_na),
         #Dummies Sex(1=Male), Voter(1=Registered), and Hispanic(1=Hispanic)
         across(.cols = c(ISEX, VOTER, HISPAN), make_dum),
         #Racial Dummies (Other and mixed together; intended reference category)
         white = ifelse(data_raw$RRACE==1 & HISPAN !=1,1,0),
         black = ifelse(data_raw$RRACE==2 & HISPAN !=1,1,0),
         asian = ifelse(data_raw$RRACE==3 & HISPAN !=1,1,0),
         native = ifelse(data_raw$RRACE==4 & HISPAN !=1,1,0),
         other = ifelse(data_raw$RRACE>4& HISPAN !=1,1,0),
         #PID dummies (Other/nopref reference)
         REP = ifelse(data_cleaned$PID==2,1,0),
         DEM = ifelse(data_cleaned$PID==1,1,0)) %>%
  #Renaming our variables
  rename(HealthAnxious = MCD1,
         FinanceAnxious = MCD2,
         UnderControl = MCD3,
         RaceImport = MCD4)




#Take a look at distribution of answers on the DV of interest

#Data labels
whatdo<-c("1" ="Getting the virus\nunder control is\n essential before\nwe open up\nthe economy",
          "2" = "We should open\nthe economy so that\n people can get\nback to their\n jobs and\nnormal lives",
          "3" = "Both/somewhere\nin between \n[Volunteered]")

(p1<-ggplot()+geom_bar(data = filter(data_cleaned, !is.na(UnderControl)),
                       aes(x=as.factor(UnderControl)), fill = "#84CEFF" )+theme_minimal()+
  scale_x_discrete(labels = whatdo, name = "")+
    labs(y = "Number of\nRespondents", 
         title = " Floridians' Thoughts on Whether\nto Prioritize Virus vs. Economy")+
    theme(axis.title.y = element_text(hjust = 1, angle = 0, size = 8),
          plot.title = element_text(hjust = .5)) )


#Loading in the library for multinomial logistic regression.
library(nnet)


#Turn it into a factor and relevel so 3 is base

#Gotta prune NAs or else they become their own factor -.-

data_pruned <- data_cleaned[!is.na(data_cleaned$UnderControl),]

data_pruned$UnderControl<-as.factor(data_pruned$UnderControl)
data_pruned$UnderControl<-relevel(data_pruned$UnderControl,ref = "3")

#Setting the Multinomial Model
model1<- multinom(UnderControl ~ FinanceAnxious + HealthAnxious + 
                    INCOM2 + EDUCAT + AGE + HISPAN +
                    REP + DEM + white + black + asian ,data = data_pruned)


y<-as.data.frame(vcov(model1))

z <- summary(model1)$coefficients / summary(model1)$standard.errors

summary(model1)


#Base as 2 now

data_pruned$UnderControl<-relevel(data_pruned$UnderControl,ref = "2")

#Setting the Multinomial Model
model1<- multinom(UnderControl ~ FinanceAnxious + HealthAnxious + 
                    INCOM2 + EDUCAT + AGE + HISPAN +
                    REP+ DEM+ white + black + asian ,data = data_pruned)


y<-as.data.frame(vcov(model1))

z <- summary(model1)$coefficients / summary(model1)$standard.errors

summary(model1)


z1<-t(as.data.frame(z))
mod1<-t(as.data.frame(summary(model1)$coefficients))


###Interaction models

data_pruned_ints<-data_pruned %>%
  mutate(RaceImport = -1*(RaceImport-1),
         wrace = white * RaceImport,
         brace = black * RaceImport,
         hrace = HISPAN * RaceImport)


data_pruned_ints <- data_pruned_ints[!is.na(data_pruned_ints$UnderControl),]
data_pruned_ints$UnderControl<-as.factor(data_pruned_ints$UnderControl)
data_pruned_ints$UnderControl<-relevel(data_pruned_ints$UnderControl,ref = "2")

#Setting the Multinomial Model
model2<- multinom(UnderControl ~ wrace+brace +hrace  + RaceImport+ FinanceAnxious +   
                    HealthAnxious + INCOM2 + EDUCAT + AGE + REP + DEM + HISPAN +white + 
                    black + asian ,data = data_pruned_ints)

summary(model2)
z <- summary(model2)$coefficients / summary(model2)$standard.errors

dim(data_pruned_ints)

data_pruned_ints$UnderControl<-relevel(data_pruned_ints$UnderControl,ref = "3")

#Setting the Multinomial Model
model2<- multinom(UnderControl ~ wrace  + RaceImport+ FinanceAnxious +  
                    HealthAnxious + INCOM2 + EDUCAT + AGE + REP + DEM + white + 
                    black + asian ,data = data_pruned_ints)

summary(model2)
z <- summary(model2)$coefficients / summary(model2)$standard.errors

sum(data_pruned_ints$white, na.rm = T)



y<-vcov(model2)