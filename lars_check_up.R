remove(list=ls())
require(tidyr)
require(plyr)
require(dplyr)
require(lattice)
require(RColorBrewer)

setwd('/Users/Ben/Dropbox/Checkup/')
#https://www.dropbox.com/sh/46i9ipvay7vk4fq/AAD-1_AXVTdvp3cbwxHdjhxWa?dl=0

source('/Users/Ben/Documents/r_scripts/LARS_CHECKUP_GUTS.R')

setwd('/Users/Ben/Dropbox/Plots/')
pdf(paste0(Sys.Date()-5,'_to_',Sys.Date()-1,'_diagnostic.pdf')
    ,width=14,height=12)
plot1
plot2
#plot3
dev.off()

q(save='no',status=0)

# ################
# 
# voi=c('H','LE','co2_flux')
# 
# 
# dat4=gather_(summary_ep,'COMP','VALUE',voi)
# dat4=dat3[order(dat3$DOY),]
# ####
# my.settings=list(
#   superpose.symbol=list(col=mycolors[c(7,3)],alpha=0.8)
#   ,superpose.line=list(col=mycolors[c(7,3)],alpha=0.8,lwd=4)
#   ,strip.background=list(col=brewer.pal(3,'Greys'))
# )
# ##############
# plot4=xyplot(VALUE~DOY|factor(COMP, levels=unique(COMP))
#              ,data=dat3,groups=PFT,layout=c(1,3)
#              ,scales=list(y=list(relation='free')
#                           ,x=list(at=myat
#                                   ,labels=mylabels
#                                   ,alternating=1
#                                   ,relation='free'
#                           )
#              )
#              # , strip = strip.custom(factor.levels = c(expression("Sensible Heat Flux " (W/m^2))
#              #                                         ,expression("Latent Heat Flux " (W/m^2))
#              #                                         ,expression("Net Ecosystem Production " (gC/m^2))
#              #                                         ))
#              ,type='l',xlab=NULL,ylab=NULL
#              ,as.table=T
#              ,par.settings=my.settings
#              ,auto.key=list(points=F,lines=T,columns=1,corner=c(0,.95),text=c('Switchgrass','Loblolly'))
#              
#              ,panel=function(x,y,...){
#                panel.grid(h=-1,v=0)
#                panel.abline(h=0)
#                panel.xyplot(x,y,...)
#              }
# )
# plot4
