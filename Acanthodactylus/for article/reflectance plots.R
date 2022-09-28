if (1 == 1){
  library(pavo)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(plotrix)
  library(grid)
  library(gridExtra)
  library(cowplot)
  
  resetPar <- function() { 
    ### for reseting par
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
  }
  
  #function that generates plots with same legend:
  grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),legend,ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),legend,ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
  }
}
# LOADING DATA---------------
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/for article")

if(1==1){
spec_all_avg_data=read.table("spec_all_avg_data.csv",header=TRUE,sep=",")
spec_all_avg_data=as.rspec(spec_all_avg_data)

schr_bkg2=read.table("schr_bkg2.csv",header=TRUE,sep=",")
schr_bkg2=as.rspec(schr_bkg2)

bosk_bkg2=read.table("bosk_bkg2.csv",header=TRUE,sep=",")
bosk_bkg2=as.rspec(bosk_bkg2)

bs_bkg2=read.table("bs_bkg2.csv",header=TRUE,sep=",")
bs_bkg2=as.rspec(bs_bkg2)
}




explorespec(spec_all_avg_data, by=5, lwd=2, legpos="topleft")

## schr- reflectance spectrum plots --
if (1==1){
  a_schr_plot=spec_all_avg_data%>%
    select(c(   wl,schr29_A_tail_dorsal_brown,  
                schr30_A_tail_dorsal_brown,schr31_A_tail_dorsal_brown,schr32_A_tail_dorsal_brown,
                schr33_A_tail_dorsal_brown,
                schr34_A_tail_dorsal_brown,schr35_A_tail_dorsal_brown,schr36_A_tail_dorsal_brown,
                schr37_A_tail_dorsal_brown,schr38_A_tail_dorsal_brown,
                schr4_J_tail_dorsal_red,schr11_J_tail_dorsal_red,schr12_J_tail_dorsal_red,schr13_J_tail_dorsal_red,
                schr14_J_tail_dorsal_red,schr15_J_tail_dorsal_red,schr19_J_tail_dorsal_red,
                schr20_J_tail_dorsal_red))
  a_schr_plot=as.rspec(a_schr_plot)
  b_schr_plot=schr_bkg2
  schr_plot=merge(a_schr_plot,b_schr_plot)
  names(schr_plot)
  names(schr_plot)[2:11]="schr_A_brown"
  names(schr_plot)[12:19]="schr_J_red"
  names(schr_plot)[20:69]="bkg_schr"
  
  par(resetPar()) 
  col.list= c( "saddlebrown","#F08080","#DEB887")
  aggplot(schr_plot,by=names(schr_plot),shadecol=col.list, lcol=col.list,legend = TRUE ,alpha=0.25,ylim = c(0,70))
  # explorespec(a_schr_plot, by=9, lwd=2, legpos="topleft")
}

## bosk- reflectance spectrum plots --
if (1==1){
  a_bosk_plot=spec_all_avg_data%>%
    select(c(   wl,bosk11_A_tail_dorsal_brown,bosk12_A_tail_dorsal_brown,bosk13_A_tail_dorsal_brown,bosk16_A_M_tail_dorsal_brown,
                bosk17_A_M_tail_dorsal_brown,bosk20_A_tail_dorsal_brown,bosk21_A_tail_dorsal_brown,
                bosk22_A_tail_dorsal_brown,bosk23_A_tail_dorsal_brown,bosk24_A_tail_dorsal_brown,bosk26_A_tail_dorsal_brown,
                bosk14_J_tail_dorsal_blue,bosk15_J_tail_dorsal_blue,
                bosk27_J_tail_dorsal_blue,bosk28_J_tail_dorsal_blue,bosk29_J_tail_dorsal_blue,bosk30_J_tail_dorsal_blue,
                bosk31_J_tail_dorsal_blue,bosk32_J_tail_dorsal_blue,bosk33_J_tail_dorsal_blue,bosk34_J_tail_dorsal_blue ))
  a_bosk_plot=as.rspec(a_bosk_plot)
  b_bosk_plot=bosk_bkg2
  bosk_plot=merge(a_bosk_plot,b_bosk_plot)
  
  names(bosk_plot)[2:12]="bosk_A_brown"
  names(bosk_plot)[13:22]="bosk_J_blue"
  names(bosk_plot)[23:72]="bkg_bosk"
  
  col.list= c("saddlebrown", "#87CEEB","#DEB887")
  aggplot(bosk_plot,by=names(bosk_plot),shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,70))
  # explorespec(a_bosk_plot, by=10, lwd=2, legpos="topleft")
}
## bs- reflectance spectrum plots --
if (1==1){
  a_bs_plot=spec_all_avg_data%>%
    select(c(   wl,bs13_J_tail_dorsal_blue,bs14_J_tail_dorsal_blue,bs15_J_tail_dorsal_blue,bs16_J_tail_dorsal_blue,
                bs17_J_tail_dorsal_blue,bs18_J_tail_dorsal_blue,bs19_J_tail_dorsal_blue,bs20_J_tail_dorsal_blue, 
                bs21_J_tail_dorsal_blue,bs22_J_tail_dorsal_blue,bs23_J_tail_dorsal_blue,
                bs34_A_tail_dorsal_brown,bs35_A_tail_dorsal_brown,bs36_A_tail_dorsal_brown,bs37_A_tail_dorsal_brown,
                bs38_A_tail_dorsal_brown,bs39_A_tail_dorsal_brown,bs40_A_tail_dorsal_brown,bs41_A_tail_dorsal_brown,
                bs42_A_tail_dorsal_brown,bs43_A_tail_dorsal_brown ))
  a_bs_plot=as.rspec(a_bs_plot)
  b_bs_plot=bs_bkg2
  bs_plot=merge(a_bs_plot,b_bs_plot)
  
  names(bs_plot)[2:12]="bs_J_blue"
  names(bs_plot)[13:22]="bs_A_brown" 
  names(bs_plot)[23:72]="bkg_bs"
  
  col.list= c("#87CEEB","saddlebrown" ,"#DEB887")
  aggplot(bs_plot,by=names(bs_plot),shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,70))
  # explorespec(a_bs_plot, by=10, lwd=2, legpos="topleft")
}



##------ dorsal brown and yellow stripes   ----------
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/for article")
dorsum=as.rspec(read.table("dorsum.csv",header=TRUE,sep=","))
bosk_bkg2=as.rspec(read.table("bosk_bkg2.csv",header=TRUE,sep=","))


if(1==1){
  a_bosk_plot=dorsum%>%
    select(c(   wl,bosk28_J_back_dorsal_brown,bosk29_J_back_dorsal_brown,bosk30_J_back_dorsal_brown,bosk31_J_back_dorsal_brown,bosk32_J_back_dorsal_brown,bosk33_J_back_dorsal_brown,bosk34_J_back_dorsal_brown,
                bosk28_J_back_dorsal_yellow,bosk29_J_back_dorsal_yellow,bosk30_J_back_dorsal_yellow,bosk31_J_back_dorsal_yellow,bosk32_J_back_dorsal_yellow,bosk33_J_back_dorsal_yellow,bosk34_J_back_dorsal_yellow
    ))
  a_bosk_plot=as.rspec(a_bosk_plot)
  b_bosk_plot=bosk_bkg2
  bosk_plot=merge(a_bosk_plot,b_bosk_plot)
  
  names(bosk_plot)[2:8]="bosk_J_brown"
  names(bosk_plot)[9:15]="bosk_J_yellow"
  names(bosk_plot)[16:65]="bkg_bosk"
  
  names_bosk=c(  "bosk_J_brown" ,"bosk_J_brown" ,"bosk_J_brown" ,"bosk_J_brown" ,"bosk_J_brown" ,"bosk_J_brown" ,
                 "bosk_J_brown" ,
                 "bosk_J_yellow",  "bosk_J_yellow" ,"bosk_J_yellow",  "bosk_J_yellow" ,"bosk_J_yellow",  "bosk_J_yellow" ,
                 "bosk_J_yellow",
                 "bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk",
                 "bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk",
                 "bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk",
                 "bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk",
                 "bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk","bkg_bosk")
  
  col.list= c("saddlebrown", "tan1","#DEB887")
  aggplot(bosk_plot,by=names_bosk,shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,70))
  # explorespec(a_bosk_plot, by=10, lwd=2, legpos="topleft")
}


