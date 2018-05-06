pine_files=paste0(Sys.Date()-c(5:1),"_LI-7200_EP-Summary.txt" )
grass_files=paste0(Sys.Date()-c(5:1),"_AIU-1657_EP-Summary.txt")
clear_files=paste0(Sys.Date()-c(5:1),"_smart3-00140_EP-Summary(2).txt")

grass_ep=data.frame()
pine_ep=data.frame()
clear_ep=data.frame()

for(i in 5:1){
  if(file.exists(pine_files[i]))
  {
    temp_dataset <-read.table(pine_files[i], header=TRUE, sep="\t",fill=T,nrows = 3)
    mynames=names(temp_dataset)
    temp_dataset <- read.table(pine_files[i], skip=2, sep="\t",fill=T)
    names(temp_dataset)=mynames
    pine_ep<-rbind.fill(pine_ep, temp_dataset)
    rm(temp_dataset)
  }else{
    tmp_dataset=data.frame(date=rep(Sys.Date()-i,48))
    tmp_dataset=cbind(tmp_dataset,DOY=(as.POSIXlt(Sys.Date()-i,format="%d-%m-%Y")$yday+1)+
                        seq(0,.979,length.out = 48),time=
                        c('00:00:00', '00:30:00' ,'01:00:00' ,'01:30:00' ,'02:00:00' ,'02:30:00' ,'03:00:00' ,'03:30:00' ,'04:00:00' ,'04:30:00'
                          ,'05:00:00', '05:30:00' ,'06:00:00' ,'06:30:00' ,'07:00:00' ,'07:30:00' ,'08:00:00' ,'08:30:00' ,'09:00:00' ,'09:30:00'
                          ,'10:00:00', '10:30:00' ,'11:00:00' ,'11:30:00' ,'12:00:00' ,'12:30:00' ,'13:00:00' ,'13:30:00' ,'14:00:00' ,'14:30:00'
                          ,'15:00:00', '15:30:00' ,'16:00:00' ,'16:30:00' ,'17:00:00' ,'17:30:00' ,'18:00:00' ,'18:30:00' ,'19:00:00' ,'19:30:00'
                          ,'20:00:00', '20:30:00' ,'21:00:00' ,'21:30:00' ,'22:00:00' ,'22:30:00' ,'23:00:00' ,'23:30:00')
                      ,summary_exists='NO')
    pine_ep=rbind.fill(pine_ep, tmp_dataset)
    rm(tmp_dataset)
  }
  
  if(file.exists(grass_files[i]))
  {
    # if the merged dataset does exist, append to it
    
    temp_dataset <-read.table(grass_files[i], header=TRUE, sep="\t",fill=T,nrows = 3)
    mynames=names(temp_dataset)
    temp_dataset <- read.table(grass_files[i], skip=2, sep="\t",fill=T)
    names(temp_dataset)=mynames
    grass_ep<-rbind.fill(grass_ep, temp_dataset)
    rm(temp_dataset)
  }else{
    tmp_dataset=data.frame(date=rep(Sys.Date()-i,48))
    tmp_dataset=cbind(tmp_dataset,DOY=(as.POSIXlt(Sys.Date()-i,format="%d-%m-%Y")$yday+1)+
                        seq(0,.979,length.out = 48),time=
                        c('00:00:00', '00:30:00' ,'01:00:00' ,'01:30:00' ,'02:00:00' ,'02:30:00' ,'03:00:00' ,'03:30:00' ,'04:00:00' ,'04:30:00'
                          ,'05:00:00', '05:30:00' ,'06:00:00' ,'06:30:00' ,'07:00:00' ,'07:30:00' ,'08:00:00' ,'08:30:00' ,'09:00:00' ,'09:30:00'
                          ,'10:00:00', '10:30:00' ,'11:00:00' ,'11:30:00' ,'12:00:00' ,'12:30:00' ,'13:00:00' ,'13:30:00' ,'14:00:00' ,'14:30:00'
                          ,'15:00:00', '15:30:00' ,'16:00:00' ,'16:30:00' ,'17:00:00' ,'17:30:00' ,'18:00:00' ,'18:30:00' ,'19:00:00' ,'19:30:00'
                          ,'20:00:00', '20:30:00' ,'21:00:00' ,'21:30:00' ,'22:00:00' ,'22:30:00' ,'23:00:00' ,'23:30:00')
                      ,summary_exists='NO')                     
    
    grass_ep=rbind.fill(grass_ep, tmp_dataset)
    rm(tmp_dataset)
  }
  
  if(file.exists(clear_files[i]))
  {
    # if the merged dataset does exist, append to it
    
    temp_dataset <-read.table(clear_files[i], header=TRUE, sep="\t",fill=T,nrows = 3)
    mynames=names(temp_dataset)
    temp_dataset <- read.table(clear_files[i], skip=2, sep="\t",fill=T)
    names(temp_dataset)=mynames
    clear_ep<-rbind.fill(clear_ep, temp_dataset)
    rm(temp_dataset)
  }else{
    tmp_dataset=data.frame(date=rep(Sys.Date()-i,48))
    tmp_dataset=cbind(tmp_dataset,DOY=(as.POSIXlt(Sys.Date()-i,format="%d-%m-%Y")$yday+1)+
                        seq(0,.979,length.out = 48),time=
                        c('00:00:00', '00:30:00' ,'01:00:00' ,'01:30:00' ,'02:00:00' ,'02:30:00' ,'03:00:00' ,'03:30:00' ,'04:00:00' ,'04:30:00'
                          ,'05:00:00', '05:30:00' ,'06:00:00' ,'06:30:00' ,'07:00:00' ,'07:30:00' ,'08:00:00' ,'08:30:00' ,'09:00:00' ,'09:30:00'
                          ,'10:00:00', '10:30:00' ,'11:00:00' ,'11:30:00' ,'12:00:00' ,'12:30:00' ,'13:00:00' ,'13:30:00' ,'14:00:00' ,'14:30:00'
                          ,'15:00:00', '15:30:00' ,'16:00:00' ,'16:30:00' ,'17:00:00' ,'17:30:00' ,'18:00:00' ,'18:30:00' ,'19:00:00' ,'19:30:00'
                          ,'20:00:00', '20:30:00' ,'21:00:00' ,'21:30:00' ,'22:00:00' ,'22:30:00' ,'23:00:00' ,'23:30:00')
                      ,summary_exists='NO')                     
    
    clear_ep=rbind.fill(clear_ep, tmp_dataset)
    rm(tmp_dataset)
  }
}

pine_files=paste0(Sys.Date()-c(5:1),"_LI-7200_Summary.txt" )
pine_files2=paste0(Sys.Date()-c(5:1),"_LI-7200_Summary(2).txt" )
grass_files=paste0(Sys.Date()-c(5:1),"_AIU-1657_Summary.txt")


grass=data.frame()
pine=data.frame()

for(i in 5:1){
  if(file.exists(pine_files[i]))
  {
    temp_dataset <-read.table(pine_files[i], header=TRUE, sep="\t",fill=T)
    temp_dataset=temp_dataset[c('Date','Time','Flow.Drive....')]
    pine<-rbind.fill(pine, temp_dataset)
    rm(temp_dataset)
    if(file.exists(pine_files2[i]))
    {
      temp_dataset <-read.table(pine_files2[i], header=TRUE, sep="\t",fill=T)
      temp_dataset=temp_dataset[c('Date','Time','Flow.Drive....')]
      pine<-rbind.fill(pine, temp_dataset)
      rm(temp_dataset)
    }
  }else{
    tmp_dataset=data.frame(Date=rep(Sys.Date()-i,48))
    tmp_dataset=cbind(tmp_dataset,Time=
                        c('00:00:00', '00:30:00' ,'01:00:00' ,'01:30:00' ,'02:00:00' ,'02:30:00' ,'03:00:00' ,'03:30:00' ,'04:00:00' ,'04:30:00'
                          ,'05:00:00', '05:30:00' ,'06:00:00' ,'06:30:00' ,'07:00:00' ,'07:30:00' ,'08:00:00' ,'08:30:00' ,'09:00:00' ,'09:30:00'
                          ,'10:00:00', '10:30:00' ,'11:00:00' ,'11:30:00' ,'12:00:00' ,'12:30:00' ,'13:00:00' ,'13:30:00' ,'14:00:00' ,'14:30:00'
                          ,'15:00:00', '15:30:00' ,'16:00:00' ,'16:30:00' ,'17:00:00' ,'17:30:00' ,'18:00:00' ,'18:30:00' ,'19:00:00' ,'19:30:00'
                          ,'20:00:00', '20:30:00' ,'21:00:00' ,'21:30:00' ,'22:00:00' ,'22:30:00' ,'23:00:00' ,'23:30:00')
                      ,summary_exists='NO')
    
    pine=rbind.fill(pine, tmp_dataset)
    rm(tmp_dataset)
  }
  
  if(file.exists(grass_files[i]))
  {
    # if the merged dataset does exist, append to it
    
    temp_dataset <-read.table(grass_files[i], header=TRUE, sep="\t",fill=T)
    temp_dataset=temp_dataset[c('Date','Time','Flow.Drive....')]
    grass<-rbind.fill(grass, temp_dataset)
    rm(temp_dataset)
  }else{
    tmp_dataset=data.frame(Date=rep(Sys.Date()-i,48))
    tmp_dataset=cbind(tmp_dataset,Time=
                        c('00:00:00', '00:30:00' ,'01:00:00' ,'01:30:00' ,'02:00:00' ,'02:30:00' ,'03:00:00' ,'03:30:00' ,'04:00:00' ,'04:30:00'
                          ,'05:00:00', '05:30:00' ,'06:00:00' ,'06:30:00' ,'07:00:00' ,'07:30:00' ,'08:00:00' ,'08:30:00' ,'09:00:00' ,'09:30:00'
                          ,'10:00:00', '10:30:00' ,'11:00:00' ,'11:30:00' ,'12:00:00' ,'12:30:00' ,'13:00:00' ,'13:30:00' ,'14:00:00' ,'14:30:00'
                          ,'15:00:00', '15:30:00' ,'16:00:00' ,'16:30:00' ,'17:00:00' ,'17:30:00' ,'18:00:00' ,'18:30:00' ,'19:00:00' ,'19:30:00'
                          ,'20:00:00', '20:30:00' ,'21:00:00' ,'21:30:00' ,'22:00:00' ,'22:30:00' ,'23:00:00' ,'23:30:00')
                      ,summary_exists='NO')
    grass=rbind.fill(grass, tmp_dataset)
    rm(tmp_dataset)
  }
  
}

####Merging 2x Grass and Pine####
pine_ep$date=factor(pine_ep$date)
pine$Date=factor(pine$Date)
grass_ep$date=factor(grass_ep$date)
grass$Date=factor(grass$Date)

grass_ep=merge(grass_ep,grass,by.y=c('Date','Time'),by.x = c('date','time'),all=T)
pine_ep=merge(pine_ep,pine,by.y=c('Date','Time'),by.x = c('date','time'),all=T)

##############
grass_ep$PFT='GRASS'
pine_ep$PFT='PINE'
clear_ep$PFT='CLEAR'

pine_ep$C_RAIN= pine_ep$P_RAIN_1_1_1 #as.vector(unlist(by(pine_ep$P_RAIN_1_1_1,pine_ep$date,cumsum,simplify = T)))
grass_ep$C_RAIN= grass_ep$P_RAIN_1_1_1 #as.vector(unlist(by(grass_ep$P_RAIN_1_1_1,grass_ep$date,cumsum,simplify = T)))
clear_ep$C_RAIN= clear_ep$P_RAIN_1_1_1 #as.vector(unlist(by(clear_ep$P_RAIN_1_1_1,clear_ep$date,cumsum,simplify = T)))



pine_ep$co2_signal_strength=pine_ep$co2_signal_strength_7200_mean
grass_ep$co2_signal_strength=grass_ep$co2_signal_strength_7200_mean
clear_ep$co2_signal_strength=clear_ep$co2_signal_strength_7500_mean

grass_ep$NDVI=grass_ep$NDVI1_0_0_1
grass_ep$PRI=grass_ep$PRI1_0_0_1

clear_ep$NDVI=clear_ep$NDVI_1_1_1
clear_ep$PRI=clear_ep$PRI_1_1_1

###MERGING#####
summary_ep=merge(pine_ep,grass_ep,all = T)
summary_ep=merge(summary_ep,clear_ep,all = T)
summary_ep$ALBEDO=summary_ep$SWOUT_1_1_1/summary_ep$SWIN_1_1_1
summary_ep$SWDIF_2=summary_ep$SWIN_1_1_1 - summary_ep$SWOUT_1_1_1

#####Filtering######

summary_ep$ALBEDO[summary_ep$ALBEDO>1|summary_ep$ALBEDO<0]=NA
#tmp_albedo = rep(NA,length(summary_ep$ALBEDO))
#tmp_albedo[summary_ep$time=='11:00:00' | summary_ep$time=='12:00:00' | summary_ep$time=='13:00:00' | summary_ep$time=='14:00:00'] = 
#    summary_ep$ALBEDO[summary_ep$time=='11:00:00' | summary_ep$time=='12:00:00' | summary_ep$time=='13:00:00' | summary_ep$time=='14:00:00']
#summary_ep$ALBEDO = tmp_albedo
summary_ep[grep('SWC',names(summary_ep))][summary_ep[grep('SWC',names(summary_ep))]<0|
                                            summary_ep[grep('SWC',names(summary_ep))]>0.7]=NA
summary_ep[grep('SHF',names(summary_ep))][summary_ep[grep('SHF',names(summary_ep))]<(-300)|
                                            summary_ep[grep('SHF',names(summary_ep))]>300]=NA
summary_ep$bowen_ratio[summary_ep$daytime==0|summary_ep$bowen_ratio>1|summary_ep$bowen_ratio<(-0.1)]=NA
summary_ep$x_90.[summary_ep$x_90.>200|summary_ep$x_90.<0]=NA

summary_ep = summary_ep[,colSums(is.na(summary_ep))<nrow(summary_ep)]
summary_ep$DOY=round(summary_ep$DOY,2)

summary_ep$co2_flux[summary_ep$co2_flux>50|summary_ep$co2_flux< (-50)]=NA
summary_ep$LWOUT_1_1_1[summary_ep$LWOUT_1_1_1>1000|summary_ep$LWOUT_1_1_1< (-10)]=NA
summary_ep$RN_1_1_1[summary_ep$RN_1_1_1>1000|summary_ep$RN_1_1_1< (-10)]=NA
summary_ep$LE[summary_ep$LE< (-100)]=NA

summary_ep$NDVI[summary_ep$NDVI<0|summary_ep$NDVI>2]=NA
summary_ep$PRI[summary_ep$PRI<0|summary_ep$PRI>2]=NA
###############
main_vars=c('H','LE','co2_flux','ALBEDO'
            ,'SWIN_1_1_1','SWOUT_1_1_1','LWIN_1_1_1','LWOUT_1_1_1','NDVI','PRI'
            ,'air_temperature','RH','C_RAIN'
            ,'wind_speed','flowrate_mean','Flow.Drive....','co2_signal_strength'
            ,'u.',"RN_1_1_1",'SWDIF_1_1_1')
dat=gather_(summary_ep,'COMP','VALUE',main_vars)
dat=dat[order(dat$DOY),]


myat=unique(summary_ep$DOY[grep(pattern=glob2rx('*.5'),summary_ep$DOY)])
mylabels=substr(as.character(unique(summary_ep$date))
                ,6,10)
################
#show.settings()
mycolors=brewer.pal(8,"Dark2")
my.settings=list(
  superpose.symbol=list(col=mycolors[c(3,2,1)],alpha=0.8)
  ,superpose.line=list(col=mycolors[c(3,2,1)],alpha=0.8,lwd=2)
  ,strip.background=list(col=brewer.pal(3,'Greys'))
)
#show.settings(my.settings)
#############
plot1=xyplot(VALUE~DOY|factor(COMP, levels=unique(COMP))
             ,data=dat,groups=PFT,layout=c(4,5)
             ,scales=list(y=list(relation='free')
                          ,x=list(at=myat
                                  ,labels=mylabels
                                  ,alternating=3
                          )
             )
             ,type='l',xlab=NULL,ylab=NULL
             ,as.table=T
             ,par.settings=my.settings
             ,auto.key=list(points=F,lines=T,columns=3)
             
             ,panel=function(x,y,...){
               panel.grid(h=-1,v=0)
               panel.abline(h=0)
               panel.xyplot(x,y,...)
             }
)
########
soil_vars=names(summary_ep)[
  c(grep('TS_',names(summary_ep))
    ,grep('SWC_',names(summary_ep))
    ,grep('SHF_',names(summary_ep))
  )]
dat2=gather_(summary_ep,'COMP','VALUE',soil_vars)
dat2=dat2[order(dat2$DOY),]
dat2$METCOMP=sapply(strsplit(dat2$COMP,'_'),'[[',1)
dat2$SENSNUM=sapply(strsplit(dat2$COMP,'_'),'[[',2)
##########
mycolors2=brewer.pal(8,"Dark2")
my.settings2=list(
  superpose.symbol=list(col=mycolors2,alpha=0.8)
  ,superpose.line=list(col=mycolors2,alpha=0.8,lwd=2)
  ,strip.background=list(col=brewer.pal(3,'Greys'))
)
#show.settings(my.settings2)
#############
plot2=xyplot(VALUE~DOY|PFT+METCOMP
             ,data=dat2,groups=SENSNUM,layout=c(3,3)
             ,scales=list(y=list(relation='free')
                          ,x=list(at=myat
                                  ,labels=mylabels
                                  ,alternating=3
                          )
             )
             ,type='l',xlab=NULL,ylab=NULL
             ,as.table=T
             ,par.settings=my.settings2
             ,auto.key=list(points=F,lines=T,columns=3)
             
             ,panel=function(x,y,...){
               panel.grid(h=-1,v=0)
               panel.abline(h=0)
               panel.xyplot(x,y,...)
             }
)
#######
# mynames=names(summary_ep)
# exp_rem=c('PFT','DATAH.x','filename','Date','Time','DOY','file_records','used_records')
# rem=c(exp_rem,soil_vars,main_vars)
# othervars=mynames[! mynames %in% rem]
# 
# dat3=gather_(summary_ep,'COMP','VALUE',othervars)
# dat3=dat3[order(dat3$DOY),]
# ####
# plot3=xyplot(VALUE~DOY|factor(COMP, levels=unique(COMP))
#              ,data=dat3,groups=PFT,layout=c(4,5)
#              ,scales=list(y=list(relation='free')
#                           ,x=list(at=myat
#                                   ,labels=mylabels
#                                   ,alternating=3
#                           )
#              )
#              ,type='l',xlab=NULL,ylab=NULL
#              ,as.table=T
#              ,par.settings=my.settings
#              ,auto.key=list(points=F,lines=T,columns=2)
#              
#              ,panel=function(x,y,...){
#                panel.grid(h=-1,v=0)
#                panel.abline(h=0)
#                panel.xyplot(x,y,...)
#              }
# )
# #plot3
#######
