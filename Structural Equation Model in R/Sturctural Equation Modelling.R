rm(list = ls())

library(readxl)

analysis <- read_excel("dataset/analysis.xlsx")

#structural equation modelling (SEM Anaysis)



######Necessary Packages#####
install.packages("pysch")
install.packages("GPArotation")
install.packages("lavaan")
install.packages("semPlot")
install.packages("MVN")
install.packages("semTools")

######## Library#############
library(openxlsx)
library(psych) # for psychometric analysis
library(GPArotation)## for rotation in EFA
library (lavaan) ## for SEM
library(dplyr)
library (semPlot)# fr sem plot
library (MVN) ##for multivariate normality check
library(semTools) ## measurement invariance check 

##############Anlysis of the data####################3
table(data$gender)
describe(data$gender) ### this comes from psych library

distinct_count <- analysis %>%
  group_by(G) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(analysis)) * 100)
count2 <- Dataset %>%
  group_by(Education) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
count3 <- Dataset %>%
  group_by(Occupation) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

INDL_income <- Dataset %>%
  group_by(Individual_Income) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

Household_Member <- Dataset %>%
  group_by(Household_Member) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

MC <- Dataset %>%
  group_by(MC) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
TP <- Dataset %>%
  group_by(TP) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
DF <- Dataset %>%
  group_by(DF) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)\
HU <- Dataset %>%
  group_by(HU) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
RAT <- Dataset %>%
  group_by(RAT) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
SP <- Dataset %>%
  group_by(SP) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

HM <- Dataset %>%
  group_by(Music) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

os<- Dataset %>%
  group_by(OS) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

accidental_exp<- Dataset %>%
  group_by(`ACCIdental experience`) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
Perceived <- Dataset %>%
  group_by(Perceived) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)
Consnet <- Dataset %>%
  group_by(Consnet) %>%
  summarize(count = n()) %>%
  mutate(percentage= (count / nrow(Dataset)) * 100)

########## descriptive statistics#########

correlation <- newdataset %>% 
  select(Age,Education,,Household_member,MO, TP,DF,HU,RAT,SP,HM,OS,AE,PRA,WTP1,WTP,WTP_new)
desciptive <- mvn(correlation)
correlation#### by this command we get the normality test along with t
############################the desrciptive stattistics but here no variable can remain in ordinal form

desciptive

#################Correlation pyrramid##########

cmat <- round(cor(correlation),5)
cmat[upper.tri(cmat)] <- " "
cmat
cmat <- as.data.frame(cmat)
cmat
head(dataset)
write.xlsx(cmat,file="12.xlsx")

###########################################
correlation_2 <- analysis %>% 
  select(G,A,E1,E2,E3,O1,O2,O3,O4,I1,I2,I3,I4,H2,H3,MC,DF,HU,RAT,SP,HM,OS,PAE,FAE,PRI,RR,WTP)




cmat_2 <- round(cor(correlation_2))
cmat_2 [upper.tri(cmat_2 ),] <- " "
cmat_2 
cmat_2  <- as.data.frame(cmat_2 )
cmat_2 
write.xlsx(cmat,file="22.xlsx")

############################CFA MDODEL########################
head(analysis)
Cfamodel1 <- 'Socio =~ E1 + E2+    E3+    O1  +  O2 + O3+ O4 +I1 + I2+ I3+I4

GOOD_DRIVING=~ TP+DF+HU+RAT+ SP+ HM+OS
RISK_PERCEPTION =~  PAE+ FAE+RR +PRI

'

fit.cfamodel <- cfa(Cfamodel1,data=analysis, estimator="MLR" ,mimic="Mplus")

summary(fit.cfamodel, fit.measures=TRUE, standardized= TRUE, rsq= TRUE)

df <- df %>% 
  mutate(WTP_new = ifelse(WTP >= 0 & WTP <= 1000, 1, 0))

X2 <- read_excel("C:/Desktop/2.xlsx")
df <- X2 %>%
  mutate(WTP_new = case_when(
    WTP >= 0 & WTP <= 5000 ~ 1,
    WTP > 5000 & WTP <= 10000 ~ 2,
    WTP > 10000 & WTP <= 15000 ~ 3,
    WTP > 15000 ~ 4,
    TRUE ~ NA_integer_
  ))



filepath= "C:/Desktop/new.xlsx"




#####################Cronbach's alpha  test###################


alpha(newdataset[,c(18,19)])

alpha(newdataset[,c(11,12,13,14)])

alpha(newdataset[,c(15,16)])



########################## Sturctural Eqution Model####################


model <- 'socio=~ Intention_1 + Intention_2 + Intention_3 +Intention_4
good_driving=~ PBC_1 +PBC_2+PBC_3
risk_perception =~ Attitude_1 + Attitude_2+Attitude_3 +Attitude_4
WTP =~ WTP+
#regression path
WTP~ socio+ good_driving + risk_perception
'
semmodel <- sem(model,data= dataset,estimator="ML", mimic="Mplus")
summary(semmodel,fit.measures=TRUE, standardized=TRUE, rsq=TRUE)


newprobit$WTP_new <- factor(newprobit$WTP_new)

summary(model.oprobit <- polr(WTP_new ~ G+ Age+ Education+Occupation+ Income+Member+DF+Helmet+RAT+SP+Music+OS+Accident+ PRI, 
                              method = "probit", data= newprobit ))


