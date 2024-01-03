rm(list = ls())
require(dplyr)
require(data.table)
require(jsonlite)
library(httr)

playerlist<-c("9JJPJYJ9R","CGQYJCJP","YVVRCU8UJ","8GY2VPP9","8VCLLLR0G")
my_data<-list()
#Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6Ijk2ZWU4YjhhLTQwOTUtNGFlMC1iMDg1LTAwZTBiZTA3N2VmMiIsImlhdCI6MTY0NzQzNzEzNSwic3ViIjoiZGV2ZWxvcGVyL2JiMGYzODk3LWUzYjMtNTBlNy04ZmFiLWFmZWE5NTQ0OGI3ZSIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyIxNzguMTk3LjIwMC44OSJdLCJ0eXBlIjoiY2xpZW50In1dfQ.hqnEvlDqm1XXkfrluGZlmB0SoUM75jhjKkpJpWPTxbxfJwpci-mzqpSOrjigTxcu-i3Nu7Qg-9W60WWC1cjvNg"))
#"Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImE1Yzg3NjMwLWMxNDUtNDFkOS04ZTQ1LTViZmFiMjU4ZDI4OSIsImlhdCI6MTY0Njc0NTIzNCwic3ViIjoiZGV2ZWxvcGVyL2JiMGYzODk3LWUzYjMtNTBlNy04ZmFiLWFmZWE5NTQ0OGI3ZSIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI4NS4yLjI0MS4yMzAiXSwidHlwZSI6ImNsaWVudCJ9XX0.5Ula5bsTRofuVSjiuJR9jLIboSQGDA-uuzJiIuIByssMTK9OdrmWCfdGtw4C_SIXIk1Gv2Q0osydHHjEs97StQ "))


for (z in 1:length(playerlist)) {
  player<-paste0("%23",playerlist[z])
  pp<-playerlist[z]
  ur<-modify_url("https://api.clashroyale.com/v1/players/",path = c("v1","players",player,"battlelog"))
  v<-GET(ur,
         add_headers(Authorization="Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImE1Yzg3NjMwLWMxNDUtNDFkOS04ZTQ1LTViZmFiMjU4ZDI4OSIsImlhdCI6MTY0Njc0NTIzNCwic3ViIjoiZGV2ZWxvcGVyL2JiMGYzODk3LWUzYjMtNTBlNy04ZmFiLWFmZWE5NTQ0OGI3ZSIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI4NS4yLjI0MS4yMzAiXSwidHlwZSI6ImNsaWVudCJ9XX0.5Ula5bsTRofuVSjiuJR9jLIboSQGDA-uuzJiIuIByssMTK9OdrmWCfdGtw4C_SIXIk1Gv2Q0osydHHjEs97StQ "))
  #print(rawToChar(v$content))
  Btime<-c()
  crowns<-c()
  v1<-fromJSON(rawToChar(v$content))
  ID<-c()
  
  battletype<-c()
  for (i in 1:nrow(v1)) {ID[i]<-pp
  
  }
  
  for (i in 1:nrow(v1)) {
    battletype[i]<-v1$type[i]}
  
  for (i in 1:nrow(v1)) {
    Btime[i]<-v1$battleTime[i]
    
    if (v1$type[i] != "boatBattle"){
      Crw<-as.data.frame(v1$team[i])
      crowns[i]<-Crw$crowns
    }
    if (v1$type[i] == "boatBattle"){
      crowns[i]<-c(0)
    }
  }
  date<-c()
  for (i in 1:length(Btime)) {date<-append(date,as.Date(Btime[i],tz="UTC","%Y%m%dT%H%M%OSZ",i))}
  r<-data.frame(crowns,Btime,date,battletype,ID)
  print(r)
  write.csv(r,paste0("C:\\Users\\lucad\\OneDrive\\Bureau\\master\\data\\test\\",pp,"-",Sys.Date()))
  
  v1g<-as.data.frame(v1$gameMode)
  v1b<-as.data.frame(v1$arena)
  vx<-cbind(v1,v1b,v1g,crowns)
  vx$arena<- NULL
  vx$gameMode<- NULL
  vx$team<- NULL
  vx$opponent<- NULL
  write.csv(vx,paste0("C:\\Users\\lucad\\OneDrive\\Bureau\\master\\data\\",pp,"-",Sys.Date()))
  
  
  print(z)
  my_data[[z]]<-r
  
  
  
}
mydata<-c()
for (i in 1:length(playerlist)) {
  mydata<-rbind(mydata,my_data[[i]])
}
write.csv(mydata,paste0("C:\\Users\\lucad\\OneDrive\\Bureau\\master\\data\\","clashdata","-",Sys.Date()))
clashdata<-read.csv("C:\\Users\\lucad\\OneDrive\\Bureau\\master\\data\\clashdata",row.names = 1)


clashdata1<-rbind(clashdata,mydata)
clashdata<-distinct(clashdata1)


write.csv(clashdata,paste0("C:\\Users\\lucad\\OneDrive\\Bureau\\master\\data\\","clashdata"))
calshdata


my_data[[1]]
mydata
