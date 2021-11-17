library(readxl)
library(tidyverse)
library(ggforce)
library(lme4)
library(sjstats)
library(cowplot)
library(lmerTest)
# options(scipen=999)

# import data
df1<- read_excel("centering_data.xlsx",sheet="Sheet1")
# get an overview
str(df1)
# transform cluster to a factor var
df1$cluster<-factor(df1$cluster)
# generate grand-mean centered l1 predictor: hours_cgm
df1$hours_cgm<-df1$hours-mean(df1$hours)
# generate group-mean centered l1 predictor: hours_cwc
df1<-df1%>%mutate(hours_grpm = ave(hours, cluster),
                  hours_cwc=hours-hours_grpm) 

# intra-class correlation
m1<-lmer(wellbeing~hours+(1|cluster),df1)
summary(m1)
icc(m1)
# null model
m0<-lmer(wellbeing~(1|cluster),df1) # by default REML=T
icc(m0)


###############figure 1
# fiure 1 top
fg1a<-ggplot(df1,aes(x=hours,y=wellbeing,shape=cluster))+
  geom_point()+
  scale_shape_manual(values = c(2,3,1))+
  geom_mark_ellipse(aes(group=cluster),
                    expand = unit(2,"mm"))+ # draw ellipses for clusters
  scale_x_continuous(limits = c(30, 70))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hours",
       y="Well-Being")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))


# figure 1 bottom
# prepare the data
df_fg1b<-df1%>%group_by(cluster)%>%summarise(x_m=mean(hours),
                                             y_m=mean(wellbeing))

fg1b<-ggplot(df_fg1b,aes(x=x_m,y=y_m,shape=cluster))+
  geom_point()+
  scale_shape_manual(values = c(2,3,1))+
  scale_x_continuous(limits = c(30, 70))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hour Cluster Means",
       y="Well-Being Cluster Means")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))
  

plot_grid(fg1a, fg1b, labels = "AUTO",nrow = 2)

##################figure 2
# figure 2 top
fg2a<-ggplot(df1,aes(x=hours_cgm,y=wellbeing,shape=cluster))+
  geom_point()+
  scale_shape_manual(values = c(2,3,1))+
  scale_x_continuous(limits = c(-20, 20))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hours (CGM)",
       y="Well-Being")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))

# figure 2 bottom
fg2b<-ggplot(df1,aes(x=hours_cwc,y=wellbeing,shape=cluster))+
  geom_point()+
  scale_shape_manual(values = c(2,3,1))+
  scale_x_continuous(limits = c(-20, 20))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hours (CWC)",
       y="Well-Being")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))


plot_grid(fg2a, fg2b, labels = " ",nrow = 2)


#######table 1
# table 1 top
df1%>%group_by(cluster)%>%summarise(
  c1=mean(wellbeing),
  c2=mean(hours),
  c3=mean(hours_cgm)
)

# table 1 bottom
df1<-df1%>%mutate(wellbeing_grpm=ave(wellbeing,cluster)) # group mean of wellbeing
df_tb1<-df1%>%select(wellbeing,hours,hours_cgm,hours_cwc,hours_grpm,wellbeing_grpm,size)
round(cor(df_tb1),2)



#############figure 3
# figure 3 top
df_fg3a<-df1%>%group_by(cluster)%>%summarise(x_m=mean(hours_cgm),
                                            y_m=mean(wellbeing))
fg3a<-ggplot(df1,aes(x=hours_cgm,y=wellbeing))+
  geom_point(aes(shape=cluster))+
  scale_shape_manual(values = c(2,3,1))+
  scale_x_continuous(limits = c(-20, 20))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hours (CGM)",
       y="Well-Being")+
  geom_smooth(formula='y ~ x',method='lm', se = FALSE,fullrange = T,linetype = "dashed",color="black",size=0.3)+
  geom_smooth(data=df_fg3a,aes(x=x_m,y=y_m),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "solid",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==1),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==2),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==3),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))
  

# figure 3 bottom
df_fg3b<-df1%>%group_by(cluster)%>%summarise(x_m=mean(hours_cwc), # group means are all equal to zero
                                             y_m=mean(wellbeing))

fg3b<-ggplot(df1,aes(x=hours_cwc,y=wellbeing))+
  geom_point(aes(shape=cluster))+
  scale_shape_manual(values = c(2,3,1))+
  scale_x_continuous(limits = c(-20, 20))+
  scale_y_continuous(limits = c(40, 80))+
  guides(shape="none")+
  labs(x="Work Hours (CWC)",
       y="Well-Being")+
  geom_smooth(formula='y ~ x',method='lm', se = FALSE,fullrange = T,linetype = "dashed",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==1),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==2),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  geom_smooth(data=subset(df1,cluster==3),formula='y ~ x',method='lm', se = FALSE,fullrange = T, linetype = "dotted",color="black",size=0.3)+
  theme(panel.border = element_rect(linetype = "solid", fill = NA,size=0.5),
        panel.background = element_rect(fill = "white"))

plot_grid(fg3a, fg3b, labels = " ",nrow = 2)


######## table 2
# prepare the data
df2<- read_excel("centering_data.xlsx",sheet = "Sheet2")
str(df2)
df2$cluster<-factor(df2$cluster)
# grand-mean centered hours
df2$hours_cgm<-df2$hours-mean(df2$hours)
# group-mean centered hours
df2<-df2%>%mutate(hours_grpm = ave(hours, cluster),
                            hours_cwc=hours-hours_grpm)
df2$size_cgm<-df2$size-mean(df2$size)
# group means of (grand-mean centered hours)
df2<-df2%>%mutate(hours_cgm_grpm=ave(hours_cgm,cluster))  
  
# fit MLMs  
# null model: cgm
mod0<-lmer(wellbeing~(1|cluster),df2,REML=F)
summary(mod0)

# with one l1 predictor
# gcm
mod1_cgm<-lmer(wellbeing~hours_cgm+(1|cluster),df2,REML=F)
summary(mod1_cgm)

# cwc
mod1_cwc<-lmer(wellbeing~hours_cwc+(1|cluster),df2,REML=F)
summary(mod1_cwc)
# coefficient of l1 predictor is larger in cgm than in cwc
# individual-level variances are roughly equal
# group-level variances are smaller in cgm than in cwc

# with both l1 & l2 predictors
# cgm
mod2_cgm<-lmer(wellbeing~hours_cgm+size_cgm+(1|cluster),df2,REML=F)
summary(mod2_cgm)
# cwc
mod2_cwc<-lmer(wellbeing~hours_cwc+size_cgm+(1|cluster),df2,REML=F)
summary(mod2_cwc)
# coefficient of l1 predictor is larger in cgm than in cwc
# coefficient of l2 predictor is larger in cgm than in cwc
# individual-level variances are roughly equal
# group-level variances are smaller in cgm than in cwc


# table 3
# grand mean center
model_cgm<-lmer(wellbeing~hours_cgm_grpm+hours_cgm+(1|cluster),df2,REML=F)
summary(model_cgm)
# group mean center:ok
model_cwc<-lmer(wellbeing~hours_cgm_grpm+hours_cwc+(1|cluster),df2,REML=F)
summary(model_cwc)
# cgm and cwc show same estimate of coef at l1
# intercepts (and variances at l2) are roughly equal

# table 4
# prepare the data
df3<- read_excel("centering_data.xlsx",sheet = "Sheet3")
str(df3)
df3$cluster<-factor(df3$cluster)
# cgm of hours
df3$hours_cgm<-df3$hours-mean(df3$hours)
# cwc of hours
df3<-df3%>%mutate(hours_grpm=ave(hours,cluster),
                  hours_cwc=hours-hours_grpm)
# cluster means
df3<-df3%>%mutate(hours_cgm_grpm=ave(hours_cgm,cluster))
df3$size_cgm<-df3$size-mean(df3$size)

# one cross-level intercation effect
m1_cgm<-lmer(wellbeing~size_cgm+hours_cgm+size_cgm*hours_cgm+(1|cluster),df3,REML=F)
summary(m1_cgm)

m1_cwc<-lmer(wellbeing~size_cgm+hours_cwc+size_cgm*hours_cwc+(1|cluster),df3,REML=F)
summary(m1_cwc)
# cwc is preferred in cross-level interaction

# two interaction effects: one cross-level and one level 2
m2_cgm<-lmer(wellbeing~hours_cgm_grpm+size_cgm+hours_cgm_grpm*size_cgm+hours_cgm+size_cgm*hours_cgm+(1|cluster),df3,REML=F)
summary(m2_cgm)

m2_cwc<-lmer(wellbeing~hours_cgm_grpm+size_cgm+hours_cgm_grpm*size_cgm+hours_cwc+size_cgm*hours_cwc+(1|cluster),df3,REML=F)
summary(m2_cwc)
# cwc is preferred






