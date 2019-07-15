remove(list=ls())
require(tidyr)
require(plyr)
require(dplyr)
require(lattice)
require(RColorBrewer)

stop = FALSE

execute_hr <- 6

while(stop == FALSE){
  
  setwd("/Users/quinn/Dropbox/SBC_LARS_Diagnostics/")
  #setwd('/Users/Ben/Dropbox/Checkup/')
  
   setwd("/Users/quinn/Dropbox/SBC_LARS_Diagnostics/Summaries/")
  #setwd("/Users/Ben/Dropbox/Checkup/")
  
  system("curl -L -o grasssummaries.zip https://www.dropbox.com/sh/isxln5tq3vduhxq/AAD32JXBkcc9_pRvWQ8tmfzPa?dl=0")
  
  system("curl -L -o pinesummaries.zip https://www.dropbox.com/sh/y42p9if9rq3kfz3/AABc6q0m89mt_Vcw3GRwBpWna?dl=0")
  
  system("curl -L -o clearsummaries.zip https://www.dropbox.com/sh/2t98bokmxdk3qpu/AADdg1e1sX20fIpmKhPL8EiEa?dl=0")
  
  system("unzip -a -u grasssummaries.zip")
  system("unzip -a -u pinesummaries.zip")
  system("unzip -a -u clearsummaries.zip")
  
  system("rm *.zip")
  
  #Where are the summary files?
  setwd(‘/Users/quinn/Dropbox/SBC_LARS_Diagnostics/Summaries/‘)
  #setwd("/Users/Ben/Dropbox/Checkup/")
  
  #Where is the GUTS script?
  source("/Users/quinn/Dropbox/SBC_LARS_Diagnostics/LARS_CHECKUP_GUTS_TEMP.R")
  #source("/Users/Ben/Documents/GitHub/LARS-Diagnostic/LARS_CHECKUP_GUTS_TEMP.R")

  #Where should the plots go?
  setwd("/Users/quinn/Dropbox/SBC_LARS_Diagnostics/Plots/")
  #setwd("/Users/Ben/Dropbox/Plots/")
  
  #Plotting Data
  pdf(paste0(Sys.Date()-5,'_to_',Sys.Date()-1,'_diagnostic.pdf')
      ,width=14,height=12)
  plot1
  plot2
  plot3
  dev.off()
  
  Sys.sleep(60*60*24)
}

#q(save=‘no’,status=0)