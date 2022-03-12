library(lme4)
library(orcutt)
library(lattice)
library(lavaan)
library(tidySEM)
library(readxl)
library(tidyverse)

# myth 1
# table 1.1
growcurve<-data.frame(id=c("A","B","C","D","E","F"),
                      start=c(50,40,30,20,10,0), # starting level
                      asymptote=c(80,70,60,50,40,30), 
                      curvature=c(0.25,0.22,0.19,0.16,0.13,0.10)) # the curvature parameter
# table 1.2
growcurve$delta_t0_t1<-with(growcurve,((asymptote-(asymptote-start)*exp(-curvature*1))-start)%>%round(2))

growcurve$delta_t4_t5<-with(growcurve,((asymptote-(asymptote-start)*exp(-curvature*5))-
                                       (asymptote-(asymptote-start)*exp(-curvature*4)))%>%round(2))

growcurve$delta_t10_t11<-with(growcurve,((asymptote-(asymptote-start)*exp(-curvature*11))-
                                         (asymptote-(asymptote-start)*exp(-curvature*10)))%>%round(2))


growcurve[,5:7] # results for individuals B-F different from the table

# remarks
# the rate of change differs across different specific time intervals within an individual
# (the ranking of) the rate of change differs across individuals within the same specific time interval

# myth 2
# table 1.3
tab1<-data.frame(
  reliability_constant=c(0.7,0.8,0.9,0.7,0.8,0.9,0.7,0.8,0.9,NA,0.8,0.9,NA,NA,0.9),
  cor_obs=c(0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,NA,0.8,0.8,NA,NA,0.9),
  reliability_diff=c(0.4,0.6,0.8,0.25,0.5,0.75,0,0.33,0.67,NA,0,0.5,NA,NA,0)
)

# true-score correlation: rho (xi1 xi2)
# rho (xi1 xi2) = rho (x1 x2) / rho (x)
tab1$cor_true<-with(tab1,cor_obs/reliability_constant)%>%round(2)
matrix(tab1$cor_true,byrow=T,nrow=5)
# Noted that the true-score correlation is 1 along the diagonal of 0 reliability for the difference score.
# i.e., low reliability of diff score, high true-score correlation


# table 1.4 (a slightly different tabulation)
tab<-data.frame(
  cor_true=rep(c(0.4,0.6,0.8),each=3), # true-score correlation
  reliability_t1=rep(c(0.6,0.7,0.8),3), # reliability of observed scores at time 1: rho (X1)
  reliability_t2=rep(0.9,9), # reliability of observed scores at time 2: rho (X2)
  # rho (X2) > rho (X1) maintains approximately equal error variances at time 1 and time 2
  reliability_diff1=c(0.79,0.82,0.85,0.65,0.71,0.76,0.4,0.48,0.57), 
  # reliability of the difference score, i.e., the true change: rho (D)
  cor_change_initial=rep(0,9)
  # correlation between the true change and the true initial status: rho (xi1 delta) is set to 0
)


# calculate rho (D) by hand
# reference to rogosa & willet (1983), appendix
tab$R1<-with(tab,(1-reliability_t1)/reliability_t1)
tab$R2<-with(tab,(1-reliability_t2)/reliability_t2)
rho.d<-with(tab,(1-(cor_true)^2)/(R1*(cor_true)^2+R2+1-(cor_true)^2))%>%round(2)
# compare rho.d to reliability_diff1
rho.d==tab$reliability_diff1

# a slightly different tabulation of the reliability of the difference score: 
# rho (D) / mean(rho (X1) + rho (X2)) 
tab$reliability_diff2<-with(tab,reliability_diff1/((reliability_t1+reliability_t2)/2))

round(tab$reliability_diff2,2)

# remarks:
# A moderate time-1, time-2 correlation corresponds to numerous crossings of the growth curves 
# and considerable individual differences in change.


# myth 8
# figure 1.6
ramus <- read_excel("ramus.xlsx")
ramus_long<-ramus%>%gather(key="age",value="ramus.mm",-ID)
names(ramus_long)<-c("subject","age","ramus.mm")
ramus_long$age<-recode(ramus_long$age,'t1'=8,'t2'=8.5,'t3'=9,'t4'=9.5)
str(ramus_long)
ggplot(ramus_long,aes(x=age,y=ramus.mm,group=as.factor(subject)))+
  geom_smooth(method="lm",se=F,size=0.5,color="black")

# supplemental questions
# exhibit 1
# 40 cases, x1, x3, x5 are ture scores, w for the exogenous variable 
ex1 <- read_excel("exhibit1.xlsx")

summary(ex1[,2:5]) # data description

cor(ex1[,2:5])%>%round(3) # correlations

# scatter plots
pairs(~x1+x3+x5+w,data=ex1)


# true scores
lm(x5~x1+x3,data=ex1)%>%summary()
# observed scores
lm(obs5~obs1+obs3,data=ex1)%>%summary()

# misleading consequences of standard structural model:
# two waves with an exogenous var
# in the ppl there is no association between w and individual RATE of change theta
ex1$theta<-with(ex1,(x5-x1)/4)
cor.test(ex1$theta,ex1$w)

# x3 as the initial value
lm(x5~w+x3,data=ex1)%>%summary()

# x1 as the initial value
lm(x5~w+x1,data=ex1)%>%summary()

# example of ramus data
mod<-lmList(ramus.mm ~ age |subject, data = ramus_long) #fit straight-line to each subject
mod2<-lmList(ramus.mm ~ age + I(age^2) |subject, data = ramus_long) #fit straight-line to each subject

# rate of change
ramus$theta<-coef(mod)$age
ramus$theta%>%summary()%>%round(3)

# table 1.16
ramus$theta # column 1
summary(mod)$r.squared # column 2
summary(mod2)$r.squared-summary(mod)$r.squared # column 3
# figure 1.9
stem(ramus$theta)
boxplot(ramus$theta)
# figure 1.10
ggplot(ramus,aes(x=t1,y=theta))+geom_point()

# table 1.17
summary(mod) # Residual standard error: 0.4398295 
cor(coef(mod)$age,coef(mod)[,1]) # correlation between intercept and slope

var(coef(mod)$age)

# center at age 8 (initial status)
modlmerc<-lmer(ramus.mm ~ I(age-8) + (age|subject), data = ramus_long)
summary(modlmerc) # attention to correlation between initial status and rate 

sqrt(var(coef(modlmerc)$subject[,2]))
# average error variance (msr)
sigma(modlmerc)^2
# maximum likelihood estimation for variance of theta
var(coef(mod)$age)-sigma(modlmerc)^2/1.25
# reliability coefficient for rate of change/reliability of theta
(var(coef(mod)$age)-sigma(modlmerc)^2/1.25)/var(coef(mod)$age)

# figure 1.11
path<-'t2~t1
t3~t2
t4~t3
t4~t1
t3~t1
t4~t2
'
w4path<-sem(path, data=ramus)
summary(w4path)



# North Caroline data
# 277 females grade 1-8
nc_wide<-read.delim("nc8wide_data.txt",sep = "")
nc_long<-read.delim("ncLong_data.txt",sep = "")

nc_mod<-lmList(Y~time|ID,data=nc_long)
nc_wide$theta<-coef(nc_mod)$time
nclmer<-lmer(Y ~ time + (time|ID), data = nc_long)

# rate of change
summary(nc_wide$theta)
# alternative
summary(coef(nclmer)$ID[,2])
# correlation between theta and w
cor(nc_wide$theta,nc_wide$Z) 
# figure 1.12
ggplot(nc_wide,aes(x=Z,y=theta))+geom_point()

# table 1.18
mean(nc_wide$theta)
median(nc_wide$theta)
var(nc_wide$theta)

nclmerc<-lmer(Y ~ I(time-1) + (time|ID), data = nc_long)
# correlation between theta and initial status
cor(coef(nclmerc)$ID[,1],coef(nclmerc)$ID[,2])

# two-wave data
# table 1.19
w2<-read_excel("two-wave.xls")
# figure 1.13
stem(w2$d)
boxplot(w2$d)

summary(w2[,2:5])

lm(d~w,data=w2)%>%summary()

# figure 1.14
ggplot(w2,aes(x=x1,y=d))+geom_point()
# figure 1.15
ggplot(w2,aes(x=w,y=d))+geom_point()

cor(w2[,3:5])
pairs(w2[,3:5])

lm(x2~x1+w,data=w2)%>%summary()

tf<-data.frame(
  true3=c(41.93,27.02,42.37,47.82,41.02,51.06,44.81,47.38,30.68,31.93,42.53,45.5,35.43,40.91,48.28),
  true7=c(57.27,36.49,70.52,73.36,54.89,86.68,63.33,80.69,46.52,41.15,76.04,61.4,44.59,75.2,77.5),
  obs3=c(40.11,29.05,41.15,52.09,41.97,50.67,47.36,48.7,35.04,31.24,51.05,51.97,29.4,40.05,56.36),
  obs7=c(54.88,40.23,73.72,72.19,62.37,82.19,69.74,83.13,51.35,49.51,69.02,53.52,31.12,78.89,77.66)
)

# mean of ppl: 40 for t3 and 60 for t7
sum(abs(tf$true3-40)>abs(tf$true7-60))








