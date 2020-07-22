install.packages("readstata13")
install.packages("nnet")

library(readstata13)
library(tidyverse)

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
         white = ifelse(data_raw$RRACE==1,1,0),
         black = ifelse(data_raw$RRACE==2,1,0),
         asian = ifelse(data_raw$RRACE==3,1,0),
         native = ifelse(data_raw$RRACE==4,1,0),
         other = ifelse(data_raw$RRACE>4,1,0)) %>%
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
         title = " Floridians Thoughts on Whether\nto Prioritize Virus vs. Economy")+
    theme(axis.title.y = element_text(hjust = 1, angle = 0, size = 8),
          title = element_text(hjust = .5)) )


#Loading in the library for multinomial logistic regression.
library(nnet)


#Turn it into a factor and relevel so 3 is base

#Gotta prune NAs or else they become their own factor -.-

data_pruned <- data_cleaned[!is.na(data_cleaned$UnderControl),]

data_pruned$UnderControl<-as.factor(data_pruned$UnderControl)
data_pruned$UnderControl<-relevel(data_pruned$UnderControl,ref = "3")

#Setting the Multinomial Model
model1<- multinom(UnderControl ~ FinanceAnxious + HealthAnxious + 
                    INCOM2 + EDUCAT + AGE + 
                    PID2 + white + black + asian ,data = data_pruned)

z <- summary(model1)$coefficients / summary(model1)$standard.errors

summary(model1)
z