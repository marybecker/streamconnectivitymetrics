setwd("P:/Projects/GitHub_Prj/streamconnectivitymetrics")

library(ggplot2)
library(lubridate)
library(grid)
library(plyr)
library(reshape2)
library(stringr)

fobs<-read.csv("data/TrailCam_FlowObs2017.csv",header=TRUE)
fobs$Date<-make_date(year=fobs$Year,month=fobs$Month,day=fobs$Day)
fobs$JDay<-yday(fobs$Date)
fobs$DurObs<-ifelse(fobs$Obs>3,1,0)
#fobs<- fobs[complete.cases(fobs),]
#fobs$ObsGraph<- fobs$Obs-1
#fobs$Obs_CAT<-factor(fobs$Obs_CAT,levels=c("Dry","No Flow","Low",
#"Normal","Above Normal","Flood"))

#RG Period Only
fobs<- fobs[which(fobs$Month==7 |fobs$Month==8| fobs$Month==9 | fobs$Month==10),]
##Excluding Poland, Rocky Gutter, Beacon Hill Brook & Cobble Trib for paper
fobs<- fobs[!(fobs$STA_SEQ==19708|fobs$STA_SEQ==19709|fobs$STA_SEQ==19600
              |fobs$STA_SEQ==19460),]

sites<- unique(fobs$STA_SEQ) ##list of stations
site.name<-fobs[,1:2]
site.name<-unique(site.name[c("STA_SEQ","Station_Name")])
colnames(site.name)<-c("site","SName")
flowmetric<- matrix(ncol=31,nrow=length(sites)) #Empty matrix for flow metrics

#####RUN FLOW METRICS For All Sites################
####################################

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
flowmetric<-merge(flowmetric,site.name,by="site")
flowmetric$site<-factor(flowmetric$site,levels=c("19657","15244",
                                                 "18513","16046","19141","15192",
                                                 "15193"))
flowmetric$SName<-factor(flowmetric$SName,levels=c("Bunnell Brook",
                                                   "Cobble Brook",
                                                 "Womenshenuck Brook",
                                                 "Chidsey Brook",
                                                 "Mill River",
                                                 "Honeypot Brook US",
                                                 "Honeypot Brook DS"))
write.csv(flowmetric,"flowmetrics.csv")





####DURATION LINE PLOT########
# site.name<-read.csv("sitename.csv",header=TRUE)
# site.name<-site.name[order(site.name$N),]

site.name<- as.data.frame(sort(factor(unique(fobs$Station_Name))))
colnames(site.name)[1]<-"Station_Name"
site.name$N<-as.numeric(row.names(site.name))
site.name$altname<-paste0("(",site.name$N,") ",word(site.name$Station_Name,1,2,sep=" "))
site.name

fobs<-merge(fobs,site.name,by="Station_Name")
fobs$DurObs<-ifelse(fobs$Obs==1,"Dry",ifelse(fobs$DurObs==0,"Disconnected","Connected"))
fobs$DurObs<-factor(fobs$DurObs)


#f1a340
#f7f7f7
#998ec3

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

ggsave("fconnectdur.jpg",fconnectdur,dpi=600)


####USGS FLow Compare LINE plot###########
fobs$NF<-ifelse(fobs$index>3 & fobs$Obs <3,"Reference Gages At or Above 25th Percentile Flows 
                and Flow Disconnected","Reference Gages Below 25th Percentile Flows")
fobs$NF<-factor(fobs$NF)
cols<-c("Reference Gages At or Above 25th Percentile Flows 
                and Flow Disconnected"="cadetblue",
        "Reference Gages Below 25th Percentile Flows"="gray25")

# fobs$NF<-ifelse(fobs$index>3,"Reference Gages At or Above 25th Percentile Flows",
#                 "Reference Gages Below 25th Percentile Flows")
# cols<-c("Reference Gages At or Above 25th Percentile Flows"="cadetblue",
#         "Reference Gages Below 25th Percentile Flows"="gray25")

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

ggsave("flowobswithgageinfo.jpg",flowobswithgageinfo)

###Alt Flow Over POR with lines#############################

cols<-c("Disconnected"="#b2abd2","Connected"="#0571b0","Dry"="#ca0020")

flowobs<-
  ggplot(fobs,aes(Date,Obs,colour=DurObs,group=1))+
  geom_line(size=1.5)+
  facet_grid(rows = vars(N))+
  labs(colour=NULL,x=NULL,y="Category")+
  scale_colour_manual(values=cols)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill="white",colour = "grey75"))

flowobs

ggsave("flowobs_cont.jpg",flowobs)


####################################################################################
####################################################################################

##Magnitude plots##

p1<- ggplot(flowmetric,aes(x=SName,y=MA))+
  geom_bar(stat="identity")+
  labs(title="Average Flow Category - R&G",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p2<- ggplot(flowmetric,aes(x=SName,y=M50))+
  geom_bar(stat="identity")+
  labs(title="Median Flow Category - R&G",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p3<- ggplot(flowmetric,aes(x=SName,y=M25))+
  geom_bar(stat="identity")+
  labs(title="25th Percentile Flow Category - R&G",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p4<- ggplot(flowmetric,aes(x=SName,y=M75),axis.text=element_text(size=rel(0.75)))+
  geom_bar(stat="identity")+
  labs(title="75th Percentile Flow Category - R&G",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p5<- ggplot(flowmetric,aes(x=SName,y=MASept),axis.text=element_text(size=rel(0.75)))+
  geom_bar(stat="identity")+
  labs(title="Average September Flow Category",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p6<- ggplot(flowmetric,aes(x=SName,y=M50Sept),axis.text=element_text(size=rel(0.75)))+
  geom_bar(stat="identity")+
  labs(title="Median September Flow Category",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))


##Duration plots##

p7<- ggplot(flowmetric,aes(x=SName,y=D1))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive Dry Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p8<- ggplot(flowmetric,aes(x=SName,y=D2))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive No Flow Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p9<- ggplot(flowmetric,aes(x=SName,y=D3))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive Disconnected Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p10<- ggplot(flowmetric,aes(x=SName,y=D4))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive Connected Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p11<- ggplot(flowmetric,aes(x=SName,y=DL))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive Disconnected, No Flow or Dry Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p12<- ggplot(flowmetric,aes(x=SName,y=DN))+
  geom_bar(stat="identity")+
  labs(title="Mean Duration of Consecutive Connected, Full or Flood Days",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

##Frequency Plots######
p13<- ggplot(flowmetric,aes(x=SName,y=F1))+
  geom_bar(stat="identity")+
  labs(title="Count of Dry Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p14<- ggplot(flowmetric,aes(x=SName,y=F2))+
  geom_bar(stat="identity")+
  labs(title="Count of No Flow Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p15<- ggplot(flowmetric,aes(x=SName,y=F3))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p16<- ggplot(flowmetric,aes(x=SName,y=F4))+
  geom_bar(stat="identity")+
  labs(title="Count of Connected Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p17<- ggplot(flowmetric,aes(x=SName,y=FL))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected, No Flow or Dry Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p18<- ggplot(flowmetric,aes(x=SName,y=FN))+
  geom_bar(stat="identity")+
  labs(title="Count of Connected, Full or Flood Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p19<- ggplot(flowmetric,aes(x=SName,y=FPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - R&G",
       y="Percent of Days - R&G")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

##Frequency compared to reference gages Plots##
p20<- ggplot(flowmetric,aes(x=SName,y=G4F1))+
  geom_bar(stat="identity")+
  labs(title="Count of Dry Days - Reference Gages > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p21<- ggplot(flowmetric,aes(x=SName,y=G4F2))+
  geom_bar(stat="identity")+
  labs(title="Count of No Flow Days - Reference Gages > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p22<- ggplot(flowmetric,aes(x=SName,y=G4F3))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected Days - Reference Gages 
       > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p23<- ggplot(flowmetric,aes(x=SName,y=G4FL))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected, No Flow or Dry Days - 
       Reference Gages > 25th Percentile Flow",
       y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p24<- ggplot(flowmetric,aes(x=SName,y=G4FPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - Reference Gages 
       > 25th Percentile Flow",
       y="Percent of Days - R&G")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

##Frequency compared to closest reference gages Plots##
p25<- ggplot(flowmetric,aes(x=SName,y=GNF1))+
  geom_bar(stat="identity")+
  labs(title="Count of Dry Days - Nearby Reference Gages 
       > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p26<- ggplot(flowmetric,aes(x=SName,y=GNF2))+
  geom_bar(stat="identity")+
  labs(title="Count of No Flow Days - Nearby Reference Gages 
       > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p27<- ggplot(flowmetric,aes(x=SName,y=GNF3))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected Days - 
       Nearby Reference Gages > 25th Percentile Flow",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p28<- ggplot(flowmetric,aes(x=SName,y=GNFL))+
  geom_bar(stat="identity")+
  labs(title="Count of Disconnected, No Flow or Dry Days - 
       Nearby Reference Gages > 25th Percentile Flow",
       y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p29<- ggplot(flowmetric,aes(x=SName,y=GNFPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - 
       Nearby Reference Gages > 25th Percentile Flow",
       y="Percent of Days - R&G")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

##Timing##

p30<- ggplot(flowmetric,aes(x=SName,y=T1))+
  geom_bar(stat="identity")+
  labs(title="Julian Day of First Disconnected, No Flow or Dry Days Observation",
       y="Julian Day")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))

p31<- ggplot(flowmetric,aes(x=SName,y=T2))+
  geom_bar(stat="identity")+
  labs(title="Julian Day of First Dry Day Observation",
       y="Julian Day")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.75)),
        plot.title=element_text(size=rel(1)))



#####MULTI-Plot Function##############
#######################################

#call this with p1,p,2,... cols=4
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##Magnitude plots##

mp1<- ggplot(flowmetric,aes(x=SName,y=MA))+
  geom_bar(stat="identity")+
  labs(title="Average Flow Category - R&G",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),axis.text.x=element_blank(),
        axis.title.y=element_text(size=rel(0.5)))+
  annotate("text", x=0.75,y=4.5,label="A",size=3)

mp2<- ggplot(flowmetric,aes(x=SName,y=MASept),axis.text=element_text(size=rel(0.5)))+
  geom_bar(stat="identity")+
  labs(title="Average September Flow Category",y="Category")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),
        axis.title.y=element_text(size=rel(0.5)))+
  annotate("text", x=0.75,y=4.5,label="B",size=3)

dpi=600
tiff("MagnitudePlots.tiff",width=6*dpi,height=5*dpi,res=dpi)
multiplot(mp1,mp2,cols=1)
dev.off()


##Frequency Plots######
fp1<- ggplot(flowmetric,aes(x=SName,y=F1))+
  geom_bar(stat="identity")+
  labs(title="Count of Dry Days - R&G",y="Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),axis.title.y=element_text(size=rel(0.5)),
        axis.text.x=element_blank())+
  annotate("text", x=0.75,y=45,label="A",size=3)

fp2<- ggplot(flowmetric,aes(x=SName,y=FPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - R&G",
       y="Percent of Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),axis.title.y=element_text(size=rel(0.5)),
        axis.text.x=element_blank())+
  annotate("text", x=0.75,y=0.7,label="B",size=3)

fp3<- ggplot(flowmetric,aes(x=SName,y=G4FPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - Reference Gages > 25th Percentile Flow",
       y="Percent of Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),axis.title.y=element_text(size=rel(0.5)),
        axis.text.x=element_blank())+
  annotate("text", x=0.75,y=0.25,label="C",size=3)

fp4<- ggplot(flowmetric,aes(x=SName,y=GNFPL))+
  geom_bar(stat="identity")+
  labs(title="Percent of Disconnected, No Flow or Dry Days - Nearby Reference Gages > 25th Percentile Flow",
       y="Percent of Days")+
  theme(axis.title.x=element_blank(),axis.text=element_text(size=rel(0.5)),
        plot.title=element_text(size=rel(0.5)),axis.title.y=element_text(size=rel(0.5)))+
  annotate("text", x=0.75,y=0.035,label="D",size=3)

dpi=600
tiff("FrequencyPlots.tiff",width=6*dpi,height=5*dpi,res=dpi)
multiplot(fp1,fp2,fp3,fp4,cols=1)
dev.off()









