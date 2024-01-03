rm(list=ls())
library(ROpenDota)
library(rjson)
library(httr)
library(jsonlite)
library(opendotaR)
library(plyr)
library(readr)
library(lutz)
library(Rcpp)


#this code is not cleaned do not use except if you are me 




BP_motivation_final <- read.csv("BP+motivation final.csv")


#data cleaning
BP_motivationX <- read.csv("BP+motivation final.csv")

#not finished ones
BP_motivation<- BP_motivationX[-c(which(BP_motivationX$Finished=="False")),]
#dups
if (length(which(duplicated(na.omit(BP_motivation$`Steam ID`))))==0){
  print("no dup")
} else {BP_motivation <- BP_motivation[-c(which(duplicated(na.omit(BP_motivation$`Steam ID`)))),]}

AMO<-c()
EXT<-c()
INJ<-c()
IDE<-c()
INT<-c()
IMO<-c()
IDm<-c()
group<-c()
bp_mot<-BP_motivation[-1,]
bp_mot<-bp_mot[-1,]#3 times

bp_mot$AgeÂ.<-as.numeric(bp_mot$AgeÂ.)
mean(bp_mot$AgeÂ.)
sd(bp_mot$AgeÂ.)





bp_mot$A1s<-as.numeric(mapvalues(bp_mot$A1, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$A2s<-as.numeric(mapvalues(bp_mot$A2, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$A3s<-as.numeric(mapvalues(bp_mot$A3, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$E1s<-as.numeric(mapvalues(bp_mot$E1, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$E2s<-as.numeric(mapvalues(bp_mot$E3, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$E3s<-as.numeric(mapvalues(bp_mot$E3, 
                                 from = c("Strongly agree", "Agree","Somewhat agree",
                                          "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                 to = c(1,2,3,4,5,6,7)))
bp_mot$Intr1s<-as.numeric(mapvalues(bp_mot$Intr1, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Intr2s<-as.numeric(mapvalues(bp_mot$Intr2, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Intr3s<-as.numeric(mapvalues(bp_mot$Intr3, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Id1s<-as.numeric(mapvalues(bp_mot$Id1, 
                                  from = c("Strongly agree", "Agree","Somewhat agree",
                                           "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                  to = c(1,2,3,4,5,6,7)))
bp_mot$Id2s<-as.numeric(mapvalues(bp_mot$Id2, 
                                  from = c("Strongly agree", "Agree","Somewhat agree",
                                           "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                  to = c(1,2,3,4,5,6,7)))
bp_mot$Id3s<-as.numeric(mapvalues(bp_mot$Id3, 
                                  from = c("Strongly agree", "Agree","Somewhat agree",
                                           "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                  to = c(1,2,3,4,5,6,7)))
bp_mot$Inte1s<-as.numeric(mapvalues(bp_mot$Inte1, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Inte2s<-as.numeric(mapvalues(bp_mot$Inte2, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Inte3s<-as.numeric(mapvalues(bp_mot$Inte3, 
                                    from = c("Strongly agree", "Agree","Somewhat agree",
                                             "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                    to = c(1,2,3,4,5,6,7)))
bp_mot$Intri1s<-as.numeric(mapvalues(bp_mot$Intri1, 
                                     from = c("Strongly agree", "Agree","Somewhat agree",
                                              "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                     to = c(1,2,3,4,5,6,7)))
bp_mot$Intri2s<-as.numeric(mapvalues(bp_mot$Intri2, 
                                     from = c("Strongly agree", "Agree","Somewhat agree",
                                              "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                     to = c(1,2,3,4,5,6,7)))
bp_mot$Intri3s<-as.numeric(mapvalues(bp_mot$Intri3, 
                                     from = c("Strongly agree", "Agree","Somewhat agree",
                                              "Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree"),
                                     to = c(1,2,3,4,5,6,7)))
bp_mot$BP1<-as.numeric(mapvalues(bp_mot$BP1, 
                                 from = c("yes","no"),
                                 to = c(1,0)))
bp_mot$BP2<-as.numeric(mapvalues(bp_mot$BP2, 
                                 from = c("yes","no"),
                                 to = c(1,0)))



#groups
for (x in 1:nrow(bp_mot)) {
  if (bp_mot$BP1==1&bp_mot$BP2[x]==1) {group[x]<-c(1)}
  
  if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==0) {group[x]<-c(4)}
  if (bp_mot$BP1[x]==1&bp_mot$BP2[x]==0) {group[x]<-c(2)}
  if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==1) {group[x]<-c(3)}
}

nbbp<-c()

#mean motivation
for (x in 1:nrow(bp_mot)) { IDm[x]<-bp_mot$`Steam ID`[x]
AMO[x]<-mean(c(bp_mot$A1s[x],bp_mot$A2s[x],bp_mot$A3s[x]))
EXT[x]<-mean(c(bp_mot$E1s[x],bp_mot$E2s[x],bp_mot$E3s[x]))
INJ[x]<-mean(c(bp_mot$Intr1s[x],bp_mot$Intr2s[x],bp_mot$Intr3s[x]))
IDE[x]<-mean(c(bp_mot$Id1s[x],bp_mot$Id2s[x],bp_mot$Id3s[x]))
INT[x]<-mean(c(bp_mot$Inte1s[x],bp_mot$Inte2s[x],bp_mot$Inte3s[x]))  
IMO[x]<-mean(c(bp_mot$Intri1s[x],bp_mot$Intri2s[x],bp_mot$Intri3s[x]))

if (bp_mot$BP1[x]==1&bp_mot$BP2[x]==1) {group[x]<-c("bp-1+2")}

if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==0) {group[x]<-c("no-bp")}
if (bp_mot$BP1[x]==1&bp_mot$BP2[x]==0) {group[x]<-c("bp-1")}
if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==1) {group[x]<-c("bp-2")}

if (bp_mot$BP1[x]==1&bp_mot$BP2[x]==1) {nbbp[x]<-c(2)}

if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==0) {nbbp[x]<-c(0)}
if (bp_mot$BP1[x]==1&bp_mot$BP2[x]==0) {nbbp[x]<-c(1)}
if (bp_mot$BP1[x]==0&bp_mot$BP2[x]==1) {nbbp[x]<-c(1)}
}

pmotivation<-data.frame(IDm,AMO,EXT,INJ,IDE,INT,IMO,group,nbbp)
pmotivation


#time played
z<-BP_motivation_final
z$Progress<-as.numeric(z$Progress)

which(z$Progress>30)
which(z$Progress>100)


y<-z[-which(z$Progress<30),]
y<-y[-which(y$Status != "IP Address"),]
y$AgeÂ.<-as.numeric(y$AgeÂ.)
mean(y$AgeÂ.)
sd(y$AgeÂ.)

groupp<-c()
y$BP1<-as.numeric(mapvalues(y$BP1, 
                            from = c("yes","no"),
                            to = c(1,0)))
y$BP2<-as.numeric(mapvalues(y$BP2, 
                            from = c("yes","no"),
                            to = c(1,0)))
groupp<-c()
group<-c()

for (x in 1:nrow(y)) {
  if (y$BP1[x]==1&y$BP2[x]==1) {groupp[x]<-c(4)}
  
  if (y$BP1[x]==0&y$BP2[x]==0) {groupp[x]<-c(1)}
  if (y$BP1[x]==1&y$BP2[x]==0) {groupp[x]<-c(2)}
  if (y$BP1[x]==0&y$BP2[x]==1) {groupp[x]<-c(3)}
  
  if (y$BP1[x]==1&y$BP2[x]==1) {group[x]<-c("bp-1+2")}
  
  if (y$BP1[x]==0&y$BP2[x]==0) {group[x]<-c("no-bp")}
  if (y$BP1[x]==1&y$BP2[x]==0) {group[x]<-c("bp-1")}
  if (y$BP1[x]==0&y$BP2[x]==1) {group[x]<-c("bp-2")}
}
eng22<-y

IDm<-y$Steam.ID
ptime<-data.frame(IDm,groupp)

ptime$IDm[7]<-c("125318167")



ptime<-ptime[-21,]
ptime<-ptime[-11,]
ptime<-ptime[-1,]
eng22<-eng22[-21,]
eng22<-eng22[-11,]
eng22<-eng22[-11,]
eng22$`Steam ID`[7]<-c("125318167")


pdata<-list()
nodata<-c()
dele<-as.numeric(c())


which(eng22$`Steam ID`!=ptime$IDm)



#play data


for (x in 1:nrow(ptime)) {
  ID<-ptime$IDm[x]
  g<-get_matches(ID,limit = 1000)
  g$start_time<-as.Date(as.POSIXct(g$start_time, origin="1970-01-01"))#unix to date
  y<-data.frame(matchID=g$match_id,
                playerID=ID,date=g$start_time,duration=g$duration)
  yag<-aggregate(y[c("duration")],by=list(date=g$start_time), FUN = sum)#aggragate by date
  yag$player<-ID
  yag$group<-ptime$group[x]
  pdata[[x]]<-yag
  if (length(g)==0){
    nodata[x]<-ID
    dele[x]<-x
    print(paste0("no data"," ",ID))}}

#to complexe a bit usless (do not use)
dele
dele<-na.omit(dele)
pdata<-pdata[-c(dele)]
pdata[2]


ptall<-data.frame()
for (i in 1:length(pdata)) {
  ptall<-rbind(ptall,pdata[[i]])
}

pdata[[1]]
pt<-ptall
#write.csv(pt,"C:/Users/lucad/OneDrive/Bureau/master/data/dota/allplaydata.csv")
pt<-read.csv("allplaydata.csv")
#pt<-pt[-1]

#cleaning
pt$dateU<-as.numeric(as.POSIXct(pt$date, format="%Y-%m-%d"))
#bp date 
a<-as.numeric(as.POSIXct("2021-06-24", format="%Y-%m-%d"))#nemestice s
b<-as.numeric(as.POSIXct("2021-08-17", format="%Y-%m-%d"))#e
c<-as.numeric(as.POSIXct("2021-12-14", format="%Y-%m-%d"))#agah s
d<-as.numeric(as.POSIXct("2022-02-23", format="%Y-%m-%d"))#e
sd<-as.numeric(as.POSIXct("2021-06-19", format="%Y-%m-%d"))#start date
sdd<-as.Date("2021-06-19", format="%Y-%m-%d")
ed<-as.numeric(as.POSIXct("2022-02-27", format="%Y-%m-%d"))#end date
edd<-as.Date("2022-02-27", format="%Y-%m-%d")

pt<-pt[-c(which(pt$dateU<sd)),]#dele match that started prior startdate
pt<-pt[-c(which(pt$dateU>ed)),]


#mettre les autre jours en 0 add the days with no played time 


library(dplyr)
hhh<-list()

for (i in 1:nrow(ptime)) {
  gh<-pt[pt$player==ptime$IDm[i],]
  date<-seq(as.Date(sdd),as.Date(edd),by="days")
  fg<-as.data.frame(date)
  tt<-full_join(gh,fg)
  tt$player<-ptime$IDm[i]
  tt$group<-ptime$groupp[i]
  tt$duration[is.na(tt$duration)]<-0
  
  hhh[[i]]<-tt
  
}


pta<-data.frame()
for (i in 1:length(hhh)) {
  pta<-rbind(pta,hhh[[i]])
}
ptb<-pt
pt<-pta

#periode
BPpe<-vector(mode = "integer", length = nrow(pt))
pt$dateU<-as.numeric(as.POSIXct(pt$date, format="%Y-%m-%d"))
BPpe[which(pt$dateU>a&pt$dateU<b)]<- 1 #nemestice
BPpe[which(pt$dateU>c&pt$dateU<d)]<- 2 #agah
pt$BPpe<-as.factor(BPpe)

#bpossesion 
BP<-c()
BP<-vector(mode = "logical", length = nrow(pt))
for (i in 1:nrow(eng22)) {
  if (eng22$BP1[i]==1){
    BP[which(pt$dateU>eng22$BP1D[i]&pt$dateU<b&pt$player==eng22$`Steam ID`[i])]<-TRUE
  }
  if (eng22$BP2[i]==1){
    BP[which(pt$dateU>eng22$BP2DU[2]&pt$player==eng22$`Steam ID`[2]&pt$dateU<d)]<-TRUE
  }
}
pt$BP<-BP

#bpposs binary
BP<-vector(mode = "logical", length = nrow(pt))

for (i in 1:nrow(ptime)) {
  if (ptime$groupp[i]==2){
    BP[which(pt$dateU>a&pt$dateU<b&pt$player==ptime$IDm[i])]<-TRUE
  }
  if (ptime$groupp[i]==3){
    BP[which(pt$dateU>c&pt$dateU<d&pt$player==ptime$IDm[i])]<-TRUE
  }
  if (ptime$groupp[i]==4){
    BP[which(pt$dateU>c&pt$dateU<d&pt$player==ptime$IDm[i])]<-TRUE
    BP[which(pt$dateU>a&pt$dateU<b&pt$player==ptime$IDm[i])]<-TRUE
  }
}
pt$BP<-BP


which(pt$dateU>a&pt$dateU<b&pt$player==ptime$IDm[1])

