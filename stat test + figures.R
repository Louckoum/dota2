

#stat tests

pt <- read.csv("testdata.csv")

pmotivation<-read.csv("testmotdata.csv")

library(nlme)
library(lme4)
library(car)

class(pt$BPpe)
pt$BPpe<-as.factor(pt$BPpe)
class(pt$group)
pt$group<-as.factor(pt$group)
class(pt$player)
pt$player<-as.character(pt$player)


class(pt$date)
pt$date<-as.Date.character(pt$date)
pts<-pt

pts[which(pts$group==2&pts$BPpe==1),]
pts$BP[which(pts$group==2&pts$BPpe==1)]<-TRUE
pts[which(pts$group==3&pts$BPpe==2),]
pts$BP[which(pts$group==3&pts$BPpe==2)]<-TRUE

pts[which(pts$group==4&pts$BPpe==1),]
pts$BP[which(pts$group==4&pts$BPpe==1)]<-TRUE
pts[which(pts$group==4&pts$BPpe==2),]
pts$BP[which(pts$group==4&pts$BPpe==2)]<-TRUE

pts1<-pts[-which(pts$player=="1046337243"),]
pt2<-pts1
pt2$group<-replace(pt2$group,pt2$group==3,2)
pt2$group<-replace(pt2$group,pt2$group==4,3)
pt2$group<-droplevels(pt2$group)
which(pt2$group==1)
which(pt2$group==2)
which(pt2$group==3)
which(pt2$group==4)
head(pt2$group)
pts1<-pt2

library(ggplot2)

ggplot(pts1,aes(x=date,y=duration,colour=BP))+
  geom_point()+ylab("daily play time in secondes")
ggplot(pts1,aes(x=date,y=duration,colour=group))+
  geom_point()+ylab("daily play time in secondes")


#0 inflated


library(glmmTMB)



model6 <- glmmTMB(duration ~ BP + (1+BP|player), # Hurdle zero-inflated Poisson data set glmm 
                  zi=~BP,
                  family=truncated_poisson, pts1)

model7 <- glmmTMB(duration ~ BP + (1|player), # Hurdle zero-inflated Poisson
                  zi=~BP,
                  family=truncated_poisson, pts1)

model8 <- glmmTMB(duration ~ BP + (1+BP|player), #poisson convergence singular
                  zi=~BP,
                  family=poisson, data=pts1)

model9 <- glmmTMB(duration ~ BP + (1|player),
                  zi=~BP,family=poisson, data=pts1)

model10 <- glmmTMB(duration ~ BP + (1|player), #negative binomial
                   zi=~BP,family=nbinom2, data=pts1)
model10.1 <- glmmTMB(duration ~ BP + (1|player)*(1|date), #negative binomial
                     zi=~BP,family=nbinom2, data=pts1)
model11 <- glmmTMB(duration ~ BP + (1+BP|player), #negative binomial slope 
                   zi=~BP,family=nbinom2, data=pts1)
model11.1 <- glmmTMB(duration ~ BP*player + (1+BP*player|player), #negative binomial slope 
                     zi=~BP,family=nbinom2, data=pts1)
model11.2 <- glmmTMB(duration ~ BP*player + (1+|player)*(1|date), #negative binomial slope 
                     zi=~BP,family=nbinom2, data=pts1)

model11.3 <- glmmTMB(duration ~ BP + (1+BP|player)*(1|date), #negative binomial slope 
                     zi=~BP,family=nbinom2, data=pts1)




model13 <- glmmTMB(duration ~ BP + (1+BP|player), #negative hurdle + slope model model convergence singular
                   zi=~BP,family=truncated_nbinom1, data=pts1)
model14 <- glmmTMB(duration ~ BP + (1|player), #negative hudrle model inite value in x 
                   zi=~BP,family=truncated_nbinom1, data=pts1)

model15 <- glmmTMB(duration ~ group*BPpe + (1|player), #negative hudrle model
                   zi=~BP,family=truncated_nbinom1, data=pts1)
model16 <- glmmTMB(duration ~ group*BPpe + (1+group*BPpe|player), #negative hudrle model convergence issue
                   zi=~BP,family=truncated_nbinom1, data=pts1)
model17 <- glmmTMB(duration ~ group*BPpe + (1|player), #negative poisson
                   zi=~BP,family=nbinom2, data=pts1)
model17.1 <- glmmTMB(duration ~ group*BPpe + (1|player)*(1|date), #negative poisson
                     zi=~BP,family=nbinom2, data=pts1)
model18 <- glmmTMB(duration ~ group*BPpe + (1+group*BPpe|player), #negative poisson with slope #non positive definite hessian matric
                   zi=~BP,family=nbinom2, data=pts1)


anova(model6,model7,model8,model9,model10,model10.1,model11,model13,model15,model16,model17,model17.1,model18)
anova(model10,model10.1,model11,model11.1,model11.2, model11.3,model17,model17.1,model18)

library(DHARMa)
summary(model11)
simulateResiduals(model11, plot=T)
simulateResiduals(model17, plot=T)

#glmm gamma distr


#glmm
library(lme4)
pt2[-which(pt$duration==0),]
pt3<-pt2[-which(pt2$duration==0),]
pt3[which(pt3$duration==0),]
pt3$logdu<-log(pt3$duration)
pt3$sqrtdu<-sqrt(pt3$duration)
pt3$sqdu<-pt3$duration^2

u1<-glmer(duration~group*BPpe+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
          glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #ok
u12<-glmer(logdu~group*BPpe+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #ok
u13<-glmer(sqrtdu~group*BPpe+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #ok
u14<-glmer(sqdu~group*BPpe+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #ok
u15<-glmer(sqdu~group*BPpe+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #ok

simulateResiduals(u1, plot=T)
simulateResiduals(u14, plot=T)
simulateResiduals(u12, plot=T)

u11<-glmer(sqdu~group*BPpe+(1+group*BPpe|player)*(1|date),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #singular
u111<-glmer(sqdu~group*BPpe+(1+group*BPpe|player)+(1|date),family = Gamma(link = "log"),data = pt3,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #singular

u2<-glmer(sqdu~group*BPpe+(1|player),family = Gamma(link = "log"),
          data = pt3,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) #worked 



u21<-glmer(sqdu~group*BPpe+(1+group*BPpe|player),family = Gamma(link = "log"),
           data = pt3,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))#bundary fit


u3<-glmer(sqdu~BP+(1|player),family = Gamma(link = "log"),data = pt3,
          glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))#worked
u31<-glmer(sqdu~BP+(1+BP|player),family = Gamma(link = "log"),data = pt3,
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))#ok

u311<-glmer(sqdu~BP+(1|player)*(1|date),family = Gamma(link = "log"),data = pt3,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
u312<-glmer(sqdu~BP+(1+BP|player)*(1|date),family = Gamma(link = "log"),data = pt3,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))#ok


u4<-glm(sqdu~group*BPpe,family = Gamma,data = pt3)

anova(u14,u11,u111,u2,u21,u3,u31,u311,u312,u4)

library(DHARMa)
summary(u312)
summary(u14)

plot(u1)

simulateResiduals(u312, plot=T)
simulateResiduals(u14, plot=T)
simulateResiduals(u31, plot=T)
?simulateResiduals
hist(residuals(u21))

summary(u211)
plot(u211)
simulateResiduals(u211, plot=T)


#model is not acurate across value , unreliable either increase complexity to account for it , simplify the data 

#stats

nrow(pts1[pts1$player==381624637,])
18*254



su21<-summary(u21)#good one

write.table(su22,"C:/Users/lucad/OneDrive/Bureau/master/data/dota/summglmm.csv")
su21
#all stat test table
table(su22)

library(sjPlot)

library(sjmisc)
sjt.glmer()
devtools::install_cran("sjPlot", force = TRUE, dependencies = TRUE)


pt3<-pt2
pt3$BPpe<- replace(pt2$BPpe,pt2$BPpe==2,1)


nrow(pt2[pt2$duration==1&pt2$group==1,])
nrow(pt2[pt2$duration==1&pt2$group==2,])
nrow(pt2[pt2$duration==1&pt2$group==3,])
nrow(pt2[pt2$duration==1&pt2$group==1&pt2$BPpe==0,])
nrow(pt2[pt2$duration==1&pt2$group==2&pt2$BPpe==0,])
nrow(pt2[pt2$duration==1&pt2$group==3&pt2$BPpe==0,])
nrow(pt2[pt2$duration==1&pt2$group==1&pt2$BPpe==1,])
nrow(pt2[pt2$duration==1&pt2$group==2&pt2$BPpe==1,])
nrow(pt2[pt2$duration==1&pt2$group==3&pt2$BPpe==1,])
nrow(pt2[pt2$duration==1&pt2$group==1&pt2$BPpe==2,])
nrow(pt2[pt2$duration==1&pt2$group==2&pt2$BPpe==2,])
nrow(pt2[pt2$duration==1&pt2$group==3&pt2$BPpe==2,])

pt2[pt2$duration==1&pt2$group==1,]

#graphs
library(ggplot2)
ggplot(pt,aes(x=date,y=duration,colour=player))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(pt, aes(x=BPpe,y=logdu,fill=group))+
  geom_bar(stat = "summary",fun.y="mean")
ggplot(pt, aes(x=BPpe,y=duration,fill=group))+
  geom_bar(stat = "summary",fun.y="mean")


ggplot(pt2, aes(x=BPpe,y=logdu,fill=group))+
  geom_bar(stat = "summary",fun.y="mean")+xlab("battlepass periode")+ylab("log of daily play time")
ggplot(pt2, aes(x=BPpe,y=duration,fill=group))+
  geom_bar(stat = "summary")+xlab("battlepass periode")+ylab("daily play time in secondes")
ggplot(pt3, aes(x=BPpe,y=duration,fill=group))+
  geom_bar(stat = "summary")+xlab("battlepass periode")+ylab("daily play time in secondes")

ggplot(pts,aes(x=date,y=duration,colour=BP))+
  geom_point()+ylab("daily play time in secondes")

dfg<-pt2[pt2$player==102570160,]


#motivation test

mlml<-lm(cbind(EXT,INJ,IDE,INT,IMO)~nbbp,data = pmotivation)
summary(mlml)
Anova(mlml)

library(GGally)
ggcorr(pmotivation)

extm<-lm(EXT~nbbp,data = pmotivation)
extm
injm<-lm(INJ~nbbp,data = pmotivation)
injm
idem<-lm(IDE~nbbp,data = pmotivation)
idem
intm<-lm(INT~nbbp,data = pmotivation)
intm
imom<-lm(IMO~nbbp,data = pmotivation)
imom1<-lm(log(IMO)~nbbp,data = pmotivation)
imom

summary(extm)
summary(injm)
summary(idem)
summary(intm)
summary(imom)
summary(imom1)
plot(imom)
plot(imom1)



sigma(imom)*100/mean(pmotivation$IMO)


dependent_vars <- cbind(pmotivation$EXT,pmotivation$INJ,pmotivation$IDE,pmotivation$INT,pmotivation$IMO)
independent_var <- pmotivation$group
manm<-manova(cbind(EXT,INJ,IDE,INT,IMO)~group,data = pmotivation)
manm<-manova(dependent_vars ~ independent_var, data = pmotivation)
summary(manm)
manlda<-lda(independent_var ~ dependent_vars, CV = F)
manlda


#graphs
library(ggplot2)
a<-ggplot(pmotivation, aes(x=nbbp, y=EXT))+
  geom_point() +
  stat_smooth(method = lm)+labs(x="NB of battlepasses", y="external regulation")
b<-ggplot(pmotivation, aes(x=nbbp, y=INJ))+
  geom_point() +
  stat_smooth(method = lm)+labs(x="NB of battlepasses", y="introjected regulation")
c<-ggplot(pmotivation, aes(x=nbbp, y=IDE))+
  geom_point() +
  stat_smooth(method = lm)+labs(x="NB of battlepasses", y="identified regulation")
d<-ggplot(pmotivation, aes(x=nbbp, y=INT))+
  geom_point() +
  stat_smooth(method = lm)+labs(x="NB of battlepasses", y="integrated regulation")
e<-ggplot(pmotivation, aes(x=nbbp, y=IMO))+
  geom_point() +
  stat_smooth(method = lm)+labs(x="NB of battlepasses", y="intrinsic motivation")

library(ggpubr)
ggarrange(a,b ,
          labels = c("EXT","INJ"))
ggarrange(c,d,labels = c("IDE","INT"))
ggarrange(e,labels = "IMO")


#html table

library(htmlTable)
library(magrittr)
output <- matrix(c("sqdu~BP+(1|player)","sqdu~BP+(1|player)*(1|date)",
                   "sqdu~BP+(1+BP|player)","sqdu~group*BPpe+(1|player)",
                   "sqdu~group*BPpe+(1|player)*(1|date)",
                   "sqdu~group*BPpe+(1+group*BPpe|player)",
                   "sqdu~group*BPpe+(1+group*BPpe|player)*(1|date)",
                   "sqdu~group*BPpe+(1+group*BPpe|player)+(1|date)",
                   69455,69444,69445,69442,69429,69471,69457,69457),nrow = 8,
                 ncol = 2,
                 dimnames = list(c(paste("Model",1:8)),
                                 c("formula","AIC")))

addHtmlTableStyle(output,align = "l",css.tspanner.sep = TRUE)
table1<-htmlTable(output,
                  ctable = c("solid", "double"),
                  caption = "Anova of the tested models")





