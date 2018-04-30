remove(list=ls())
require(tidyr)
require(plyr)
require(dplyr)
require(lattice)
require(RColorBrewer)

#Where are the summary files?
setwd('/Users/Ben/Dropbox/Checkup/')
#https://www.dropbox.com/sh/46i9ipvay7vk4fq/AAD-1_AXVTdvp3cbwxHdjhxWa?dl=0

#Where is the GUTS script?
source('/Users/Ben/Documents/r_scripts/LARS_CHECKUP_GUTS.R')

#Where should the plots go?
setwd('/Users/Ben/Dropbox/Plots/')


#Plotting Data
pdf(paste0(Sys.Date()-5,'_to_',Sys.Date()-1,'_diagnostic.pdf')
    ,width=14,height=12)
plot1
plot2
#plot3
dev.off()

q(save='no',status=0)

