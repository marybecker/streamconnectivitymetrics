setwd("") #Set working directory

#####LOAD Packages################
##########################################################################

library(ggplot2)
library(lubridate)
library(grid)
library(plyr)
library(reshape2)
library(stringr)

#####PREP DATA######################
##########################################################################

fobs<-read.csv("data/fobs.csv",header=TRUE) #Average Flow Observation Data from Trail Cameras
fobs$Date<-as.Date(fobs$Date) #Convert date to class date
fobs$Month<-month(fobs$Date) #Add month for parsing
fobs$JDay<-yday(fobs$Date) #Add julian day
fobs$DurObs<-ifelse(fobs$Obs>3,1,0) #Identify disconnected observations

#Limit data to Rearing and Growth Period Only (July - October Months)
fobs<- fobs[which(fobs$Month==7 |fobs$Month==8| fobs$Month==9 | fobs$Month==10),]

sites<- unique(fobs$STA_SEQ) ##list of stations
flowmetric<- matrix(ncol=31,nrow=length(sites)) #Empty matrix for flow metrics

#####RUN FLOW METRICS For All Sites################
##########################################################################

for (i in 1:length(sites)){

site <- fobs[which(fobs$STA_SEQ==sites[i]),]
site <- site[order(site$JDay),]

##Magnitude####
MA<- mean(site$Obs)
M50<- quantile(site$Obs,0.5)
M25<- quantile(site$Obs,0.25)
M75<- quantile(site$Obs,0.75)
Sept<-site[which(site$Month==9),]
MASept<- mean(Sept$Obs)
M50Sept<- quantile(Sept$Obs,0.5)

###Duration####
dur<-rle(site$Obs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==1] ## the run with a particular value 1
D1<- mean(durvalue)  ##the average duration of days

dur<-rle(site$Obs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==2] ## the run with a particular value 2
D2<- mean(durvalue)  ##the average duration of days

dur<-rle(site$Obs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==3] ## the run with a particular value 3
D3<- mean(durvalue)  ##the average duration of days

dur<-rle(site$Obs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==4] ## the run with a particular value 4
D4<- mean(durvalue)  ##the average duration of days

dur<-rle(site$DurObs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==0] ## the run with a particular value 0
DL<- mean(durvalue)  ##the average duration of days

dur<-rle(site$DurObs)  ##calculate the total runs in a sequence
durvalue<- dur$lengths[dur$values==1] ## the run with a particular value 1
DN<- mean(durvalue)  ##the average duration of days

###Frequency####
F1<- nrow(site[which(site$Obs==1),])
F2<- nrow(site[which(site$Obs==2),])
F3<- nrow(site[which(site$Obs==3),])
F4<- nrow(site[which(site$Obs==4),])
FL<- nrow(site[which(site$Obs<4),])
FN<- nrow(site[which(site$Obs>3),])
FPL<-(nrow(site[which(site$Obs<4),]))/(dim(site)[1])

###Frequency compared to reference gages##

G4F1<- nrow(site[which(site$index>3&site$Obs==1),])
G4F2<- nrow(site[which(site$index>3&site$Obs==2),])
G4F3<- nrow(site[which(site$index>3&site$Obs==3),])
G4FL<- nrow(site[which(site$index>3&site$Obs<4),])
G4FPL<- (nrow(site[which(site$index>3&site$Obs<4),]))/(dim(site)[1])

GNF1<- nrow(site[which(site$ngindex>3&site$Obs==1),])
GNF2<- nrow(site[which(site$ngindex>3&site$Obs==2),])
GNF3<- nrow(site[which(site$ngindex>3&site$Obs==3),])
GNFL<- nrow(site[which(site$ngindex>3&site$Obs<4),])
GNFPL<- (nrow(site[which(site$ngindex>3&site$Obs<4),]))/(dim(site)[1])

###Timing###

T1<- site[match(0,site$DurObs),]
T1<- as.numeric(T1['JDay'])

T2<- site[match(1,site$Obs),]
T2<- as.numeric(T2['JDay'])

#######Combine together
flowmetric[i,]<- rbind(MA,M50,M25,M75,MASept,M50Sept,
                       D1,D2,D3,D4,DL,DN,
                       F1,F2,F3,F4,FL,FN,FPL,
                       G4F1,G4F2,G4F3,G4FL,G4FPL,
                       GNF1,GNF2,GNF3,GNFL,GNFPL,
                       T1,T2)
}

###########Make dataframe#################################
flowmetric<- as.data.frame(flowmetric,row.names=sites)
colnames(flowmetric)<-c("MA","M50","M25","M75","MASept","M50Sept",
                        "D1","D2","D3","D4","DL","DN",
                        "F1","F2","F3","F4","FL","FN","FPL",
                        "G4F1","G4F2","G4F3","G4FL","G4FPL",
                        "GNF1","GNF2","GNF3","GNFL","GNFPL",
                        "T1","T2")
flowmetric$site<- row.names(flowmetric)
write.csv(flowmetric,"results/flowmetrics.csv")

####CREATE PLOTS in Paper########
##########################################################################

#Site data for plots
site.name<- as.data.frame(sort(factor(unique(fobs$Station_Name))))
colnames(site.name)[1]<-"Station_Name"
site.name$N<-as.numeric(row.names(site.name))
site.name$altname<-paste0("(",site.name$N,") ",word(site.name$Station_Name,1,2,sep=" "))

#####Flow Observation Plot###########################
fobs<-merge(fobs,site.name,by="Station_Name")
fobs$DurObs<-ifelse(fobs$Obs==1,"Dry",ifelse(fobs$DurObs==0,"Disconnected","Connected"))
fobs$DurObs<-factor(fobs$DurObs)
cols<-c("Disconnected"="#b2abd2","Connected"="#0571b0","Dry"="#ca0020")

fconnectdur<-
ggplot()+
  geom_line(data=fobs[fobs$N==1,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==2,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==3,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==4,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==5,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==6,],aes(Date,N,colour=DurObs,group=1),size=10)+
  geom_line(data=fobs[fobs$N==7,],aes(Date,N,colour=DurObs,group=1),size=10)+
  scale_y_reverse(breaks=1:7,labels=site.name[,3])+
  labs(colour=NULL,x=NULL,y=NULL)+
  scale_colour_manual(values=cols)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill = "white", colour = "grey50"))

fconnectdur

ggsave("figures/fconnectdur.jpg",fconnectdur,dpi=600)


####USGS FLow Compare LINE plot##########################################################
fobs$NF<-ifelse(fobs$index>3 & fobs$Obs <3,"Reference Gages At or Above 25th Percentile Flows 
                and Flow Disconnected","Reference Gages Below 25th Percentile Flows")
fobs$NF<-factor(fobs$NF)
cols<-c("Reference Gages At or Above 25th Percentile Flows 
                and Flow Disconnected"="cadetblue",
        "Reference Gages Below 25th Percentile Flows"="gray25")
xstart <- min(fobs$Date)-30  ##Specified for plot to ensure rects coverage
xend <- max(fobs$Date)+30
 
flowobswithgageinfo<-
ggplot(data=fobs)+
  geom_rect(aes(xmin=xstart,xmax=xend,
                           ymin = 0, ymax = 3),alpha = 0.3,fill="gray")+
  geom_line(aes(Date,Obs,colour=NF,group=N),size=1.5)+
  facet_grid(rows = vars(altname))+
  coord_cartesian(xlim=with(fobs,range(Date)))+
  labs(colour=NULL,x=NULL,y="Category")+
  scale_colour_manual(values=cols)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill="white",colour = "grey75"),
        strip.text.y=element_text(angle =0),strip.background = element_rect(fill="white"))

flowobswithgageinfo

ggsave("figures/flowobswithgageinfo.jpg",flowobswithgageinfo)

