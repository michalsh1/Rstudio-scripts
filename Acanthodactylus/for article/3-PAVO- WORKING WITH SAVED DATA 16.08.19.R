### loding packeges and functions--------------
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


#### loading data-----------------
if (1==1){
  chr="character"
  num="numeric"
  s=c(chr,chr,chr,chr,chr,chr,chr,chr,chr,num,num,num,num)
  
  setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/NOISE MODEL RESULTS")
  noise_model_results=read.csv(file="noise_model_results.csv",sep=",", colClasses = "character")
  noise_model_mean_se=read.csv(file="noise_model_mean_se.csv",sep=",", colClasses=s)
  noise_model_for_statistics=read.csv(file="noise_model_for_statistics.csv",sep=",", colClasses = "character")
  
  # LOADING spectral DATA
  setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/SPECTRA DATA")
  spec_all_avg_data=read.table("spec_all_avg_data.csv",header=TRUE,sep=",")
  spec_all_avg_data=procspec(spec_all_avg_data,fixneg="zero")
  
  schr_bkg2=read.table("schr_bkg2.csv",header=TRUE,sep=",")
  schr_bkg2=procspec(schr_bkg2,fixneg="zero")
  
  bosk_bkg2=read.table("bosk_bkg2.csv",header=TRUE,sep=",")
  bosk_bkg2=procspec(bosk_bkg2,fixneg="zero")
  
  bs_bkg2=read.table("bs_bkg2.csv",header=TRUE,sep=",")
  bs_bkg2=procspec(bs_bkg2,fixneg="zero")
}


### ----- PLOTS ---------------
setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/PLOTS")
        
##colors for the plots:
if (1==1){
  bkg_color="#DEB887"
  adult_color="saddlebrown"
  schr_J_color="#F08080"
  bosk_J_color="cornflowerblue"
  bs_J_color="#87CEEB"
}


## A) reflectance plots------
#explorespec(spec_all_avg_data, by=5, lwd=2, legpos="topleft")

if (1==1){
  ## schr- reflectance spectrum plots --
  
  if (2==2){
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
    col.list= c( adult_color,schr_J_color,bkg_color)
    
    
    aggplot(schr_plot,by=names(schr_plot),shadecol=col.list, lcol=col.list,legend = TRUE ,alpha=0.25,ylim = c(0,65))
    dev.copy(png,'schr_spec.png', width = 1000, height = 600)
    dev.off()
    # explorespec(a_schr_plot, by=9, lwd=2, legpos="topleft")
  }
  
  ## bosk- reflectance spectrum plots --
  if (3==3){
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
    
    col.list= c(adult_color, bosk_J_color, bkg_color)
    aggplot(bosk_plot,by=names(bosk_plot),shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,65))
    dev.copy(png,'bosk_spec.png', width = 1000, height = 600)
    dev.off()
    
    
    # explorespec(a_bosk_plot, by=10, lwd=2, legpos="topleft")
  }
  ## bs- reflectance spectrum plots --
  if (4==4){
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
    
    col.list= c(bs_J_color, adult_color, bkg_color)
    aggplot(bs_plot,by=names(bs_plot),shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,65))
    dev.copy(png,'bs_spec.png', width = 1000, height = 600)
    dev.off()
    
    # explorespec(a_bs_plot, by=10, lwd=2, legpos="topleft")
  }
  
  
  ## all colorfultails- reflectance spectrum plots --
  if (5==5){
    colored_tails_plot=spec_all_avg_data%>%
      select(c(   wl,
                  schr4_J_tail_dorsal_red,schr11_J_tail_dorsal_red,schr12_J_tail_dorsal_red,schr13_J_tail_dorsal_red,
                  schr14_J_tail_dorsal_red,schr15_J_tail_dorsal_red,schr19_J_tail_dorsal_red,
                  schr20_J_tail_dorsal_red,
                  bosk14_J_tail_dorsal_blue,bosk15_J_tail_dorsal_blue,
                  bosk27_J_tail_dorsal_blue,bosk28_J_tail_dorsal_blue,bosk29_J_tail_dorsal_blue,bosk30_J_tail_dorsal_blue,
                  bosk31_J_tail_dorsal_blue,bosk32_J_tail_dorsal_blue,bosk33_J_tail_dorsal_blue,bosk34_J_tail_dorsal_blue,
                  bs13_J_tail_dorsal_blue,bs14_J_tail_dorsal_blue,bs15_J_tail_dorsal_blue,bs16_J_tail_dorsal_blue,
                  bs17_J_tail_dorsal_blue,bs18_J_tail_dorsal_blue,bs19_J_tail_dorsal_blue,bs20_J_tail_dorsal_blue, 
                  bs21_J_tail_dorsal_blue,bs22_J_tail_dorsal_blue,bs23_J_tail_dorsal_blue
      ))
    colored_tails_plot=as.rspec(colored_tails_plot)
    names(colored_tails_plot)
    names(colored_tails_plot)[2:9]="schr_J_red"
    names(colored_tails_plot)[10:19]="bosk_J_blue"
    names(colored_tails_plot)[20:30]="bs_J_blue"
    
    par(resetPar()) 
    col.list= c( schr_J_color, bosk_J_color, bs_J_color)
    aggplot(colored_tails_plot,by=names(colored_tails_plot),shadecol=col.list, lcol=col.list,legend = TRUE ,alpha=0.25,ylim = c(0,66))
    dev.copy(png,'all_spec.png', width = 1000, height = 600)
    dev.off()
    
    # explorespec(a_schr_plot, by=9, lwd=2, legpos="topleft")
  }  
}


#### B) noise model plots -----

## creating data frame with real habitat of each lizard in d65 irradiance:
if (1==1){
  noise_model_real_habitat=noise_model_mean_se%>%
    filter((patch2=="A. SCHREIBERI SUBSTRATE" & str_detect(patch1,"schr"))|
             (patch2=="A. BOSKIANUS SUBSTRATE" & str_detect(patch1,"bosk"))|
             (patch2=="A. BEERSHEBENSIS SUBSTRATE" & str_detect(patch1,"beershebensis")))
  
  noise_model_d65_real_habitat=noise_model_real_habitat%>%
    filter(irradiance=="Full sunlight")
  
  noise_model_woodland_real_habitat=noise_model_real_habitat%>%
    filter(irradiance=="Partial shade")
}

## 1) dS+dL- tail colors of the same species ------------------
if (1==1){
  vis=c("AVES_UV_d65", "AVES_V_d65", "LIZARD_d65","SNAKE_d65",
        "FOX_d65","HUMAN_d65","AVES_UV_woodland", "AVES_V_woodland",
        "LIZARD_woodland","SNAKE_woodland","FOX_woodland","HUMAN_woodland")
  
  vis_plots_schr_ds=c(NA)
  vis_plots_bosk_ds=c(NA)
  vis_plots_bs_ds=c(NA)
  vis_plots_schr_dl=c(NA)
  vis_plots_bosk_dl=c(NA)
  vis_plots_bs_dl=c(NA)
  
  c1=1
  c2=1
  c3=1
  
  for (i in vis){
    if (str_detect(i , "woodland")){
      a=noise_model_woodland_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"schr"))
      
      b=noise_model_woodland_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"bosk"))
      
      c=noise_model_woodland_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"beershebensis"))
    }
    else{
      a=noise_model_d65_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"schr"))
      
      b=noise_model_d65_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"bosk"))
      
      c=noise_model_d65_real_habitat%>%
        filter(visual_model==i)%>%
        filter(str_detect(patch1,"beershebensis"))
    }
    
    
    x= paste("schr_plot_dS",i,sep="_")
    plot= ggplot(a, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,schr_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    # print(plot)
    assign(x,plot)
    vis_plots_schr_ds[c1]=x
    
    
    x2= paste("schr_plot_dL",i,sep="_")
    plot2= ggplot(a, aes(x= patch1,y=mean_dL,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dL+se_dL,ymin=mean_dL-se_dL ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c("cornsilk4","cornsilk3")) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    # print(plot)
    assign(x2,plot2)
    vis_plots_schr_dl[c1]=x2
    
    c1=c1+1
    
    x= paste("bosk_plot_dS",i,sep="_")
    plot= ggplot(b, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,bosk_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x,plot)
    vis_plots_bosk_ds[c2]=x
    
    x2= paste("bosk_plot_dL",i,sep="_")
    plot2= ggplot(b, aes(x= patch1,y=mean_dL,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dL+se_dL,ymin=mean_dL-se_dL ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c("azure4","azure3")) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x2,plot2)
    vis_plots_bosk_dl[c2]=x2
    c2=c2+1
    
    
    
    x= paste("bs_plot_dS",i,sep="_")
    plot= ggplot(c, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,bs_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x,plot)
    vis_plots_bs_ds[c3]=x
    
    
    x2= paste("bs_plot_dL",i,sep="_")
    plot2= ggplot(c, aes(x= patch1,y=mean_dL,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dL+se_dL,ymin=mean_dL-se_dL ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c("azure4","azure3")) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x2,plot2)
    vis_plots_bs_dl[c3]=x2
    c3=c3+1
    
    
    
  }
  
  
  
  
  # plotting:
  
  ######DS:
  
  # printing schr d65:
  grid_arrange_shared_legend(schr_plot_dS_AVES_UV_d65,schr_plot_dS_AVES_V_d65,
                             schr_plot_dS_LIZARD_d65,schr_plot_dS_SNAKE_d65,
                             schr_plot_dS_FOX_d65, nrow = 1) ## can also control ncol
  
  dev.copy(png,'schr_ds_d65.png', width = 1000, height = 600)
  dev.off()
  
  
  # printing bosk d65:
  grid_arrange_shared_legend(bosk_plot_dS_AVES_UV_d65,bosk_plot_dS_AVES_V_d65,
                             bosk_plot_dS_LIZARD_d65,bosk_plot_dS_SNAKE_d65,
                             bosk_plot_dS_FOX_d65,  nrow = 1) ## can also control ncol
  dev.copy(png,'bosk_ds_d65.png', width = 1000, height = 600)
  dev.off()
  
  # printing bs d65:
  grid_arrange_shared_legend(bs_plot_dS_AVES_UV_d65,bs_plot_dS_AVES_V_d65,
                             bs_plot_dS_LIZARD_d65,bs_plot_dS_SNAKE_d65,
                             bs_plot_dS_FOX_d65, nrow = 1) ## can also control ncol
  dev.copy(png,'bs_ds_d65.png',width = 1000, height = 600)
  dev.off()
  
  
  # printing schr woodland:
  grid_arrange_shared_legend(schr_plot_dS_AVES_UV_woodland,schr_plot_dS_AVES_V_woodland,
                             schr_plot_dS_LIZARD_woodland,schr_plot_dS_SNAKE_woodland,
                             schr_plot_dS_FOX_woodland, nrow = 1) ## can also control ncol
  dev.copy(png,'schr_ds_woodland.png', width = 1000, height = 600)
  dev.off()
  
  
  ######DL:
  
  # printing schr d65:
  grid_arrange_shared_legend(schr_plot_dL_AVES_UV_d65,schr_plot_dL_AVES_V_d65,
                             schr_plot_dL_LIZARD_d65,schr_plot_dL_SNAKE_d65,
                             schr_plot_dL_FOX_d65, nrow = 1) ## can also control ncol
  dev.copy(png,'schr_dl_d65.png', width = 1000, height = 600)
  dev.off()
  
  # printing bosk d65:
  grid_arrange_shared_legend(bosk_plot_dL_AVES_UV_d65,bosk_plot_dL_AVES_V_d65,
                             bosk_plot_dL_LIZARD_d65,bosk_plot_dL_SNAKE_d65,
                             bosk_plot_dL_FOX_d65,  nrow = 1) ## can also control ncol
  dev.copy(png,'bosk_dl_d65.png',width = 1000, height = 600)
  dev.off()
  
  # printing bs d65:
  grid_arrange_shared_legend(bs_plot_dL_AVES_UV_d65,bs_plot_dL_AVES_V_d65,
                             bs_plot_dL_LIZARD_d65,bs_plot_dL_SNAKE_d65,
                             bs_plot_dL_FOX_d65, nrow = 1) ## can also control ncol
  dev.copy(png,'bs_dl_d65.png', width = 1000, height = 600)
  dev.off()
  
  
  # printing schr woodland:
  grid_arrange_shared_legend(schr_plot_dL_AVES_UV_woodland,schr_plot_dL_AVES_V_woodland,
                             schr_plot_dL_LIZARD_woodland,schr_plot_dL_SNAKE_woodland,
                             schr_plot_dL_FOX_woodland, nrow = 1) ## can also control ncol
  dev.copy(png,'schr_dl_woodland.png', width = 1000, height = 600)
  dev.off()
  
  
  
  # # printing bosk woodland:
  # a=grid_arrange_shared_legend(bosk_plot_dS_AVES_UV_woodland,bosk_plot_dS_AVES_V_woodland,
  #                              bosk_plot_dS_LIZARD_woodland,bosk_plot_dS_SNAKE_woodland,
  #                              bosk_plot_dS_FOX_woodland, nrow = 1) ## can also control ncol
  # # printing bs woodland:
  # a=grid_arrange_shared_legend(bs_plot_dS_AVES_UV_woodland,bs_plot_dS_AVES_V_woodland,
  #                              bs_plot_dS_LIZARD_woodland,bs_plot_dS_SNAKE_woodland,
  #                              bs_plot_dS_FOX_woodland, nrow = 1) ## can also control ncol
  
}



## 2) for each habitat- which color more conspicuous -----
### substrate opt 1:
if (1==1){
  noise_model_d65_SCHR=noise_model_mean_se%>%
    filter(irradiance=="Full sunlight")%>%
    filter(patch2=="A. SCHREIBERI SUBSTRATE")
  
  noise_model_d65_BOSK=noise_model_mean_se%>%
    filter(irradiance=="Full sunlight")%>%
    filter(patch2=="A. BOSKIANUS SUBSTRATE")
  
  noise_model_d65_BS=noise_model_mean_se%>%
    filter(irradiance=="Full sunlight")%>%
    filter(patch2=="A. BEERSHEBENSIS SUBSTRATE")
  
  
  vis=c("AVES_UV_d65", "AVES_V_d65", "LIZARD_d65","SNAKE_d65",
        "FOX_d65","HUMAN_d65","AVES_UV_woodland", "AVES_V_woodland",
        "LIZARD_woodland","SNAKE_woodland","FOX_woodland","HUMAN_woodland")
  
  count=1
  for (i in vis){
    # SCHR :
    a=noise_model_d65_SCHR%>%
      filter(str_detect( visual_model, i))%>%
      filter(age=="J")
    
    #BOSK :
    b=noise_model_d65_BOSK%>%
      filter(str_detect( visual_model, i))%>%
      filter(age=="J")
    
    #BS :
    d=noise_model_d65_BS%>%
      filter(str_detect( visual_model, i))%>%
      filter(age=="J")
    
    c=rbind(a,b,d)
    
    bplot<- ggplot(c, aes(x= patch2,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(bs_J_color,bosk_J_color, schr_J_color)) +
      ggtitle(b$visual_model[1])+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    name_a=paste("BOSK_SCHR_A_J_bkg_plot_d65",count,sep="")
    assign(name_a,bplot)
    count=count+1
  }
  grid_arrange_shared_legend(BOSK_SCHR_A_J_bkg_plot_d651 ,
                             BOSK_SCHR_A_J_bkg_plot_d652 ,
                             BOSK_SCHR_A_J_bkg_plot_d653 ,
                             BOSK_SCHR_A_J_bkg_plot_d654,
                             BOSK_SCHR_A_J_bkg_plot_d655,
                             nrow = 2,ncol=3) ## can also control ncol
  dev.copy(png,'substrate_opt_1.png', width = 1400, height = 1000)
  dev.off()
  
  count=NA
  
  
}



### substrate- opt. 2
if (1==1){
  count=1
  bkgs=c("SCHR","BOSK","BEERSHEBENSIS")
  
  for (i in bkgs){
    schr_patches=noise_model_mean_se%>%
      filter(species=="schr")%>%
      filter(irradiance=="Full sunlight")%>%
      filter(str_detect( patch2, i))%>%
      filter(age=="J")
    
    bosk_patches=noise_model_mean_se%>%
      filter(species=="bosk")%>%
      filter(irradiance=="Full sunlight")%>%
      filter(str_detect( patch2, i))%>%
      filter(age=="J")
    
    bs_patches=noise_model_mean_se%>%
      filter(species=="beershebensis")%>%
      filter(irradiance=="Full sunlight")%>%
      filter(str_detect( patch2, i))%>%
      filter(age=="J")
    
    
    c=rbind(schr_patches,bosk_patches,bs_patches)
    
    bplot<- ggplot(c, aes(x=visual_model ,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(bs_J_color,bosk_J_color, schr_J_color)) +
      ggtitle(c$patch2[1])+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    name_a=paste("patch_plot",count,sep="")
    assign(name_a,bplot)
    count=count+1
    
  }
  grid_arrange_shared_legend(patch_plot1,
                             patch_plot2,
                             patch_plot3,
                             nrow = 2,ncol=2) ## can also control ncol
  dev.copy(png,'substrate_opt_2.png', width = 1400, height = 1000)
  dev.off()
  
  
  count=NA
}



### substrate opt 3:
if (1==1){
  noise_model_d65_J_allspecies=noise_model_mean_se%>%
    filter(irradiance=="Full sunlight")%>%
    filter(age=='J')
  
  species=c("schr","bosk","beershebensis")
  
  for (i in species){
    a=noise_model_d65_J_allspecies%>%
      filter(str_detect(species, i))
    
    species_plot<- ggplot(a, aes(x=visual_model ,y=mean_dS,fill=patch2))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
      # scale_fill_manual(values=plot_colors) +
      ggtitle(i)+
      ylim(0,7.5)
  
  
  plotname=paste(i,"plot",sep="_")
  assign(plotname,species_plot)
    
    
  }
  grid_arrange_shared_legend(schr_plot,bosk_plot,beershebensis_plot,
                             nrow =1,ncol=3) ## can also control ncol
  dev.copy(png,'substrate_opt_3.png', width = 1400, height = 1000)
  dev.off()
  
  
}

# 
# # 3) different irradiance- two tail colors- colorful and brown ------------
# vision_name=c("vismod_uv","vismod_v","lizard","snake","red_fox" )
# count=1
# for( i in vision_name){
#   ##schr:
#   data=noise_model_mean_se%>%
#     filter((patch2=="A. SCHREIBERI SUBSTRATE" & str_detect(patch1,"schr")))%>%
#     filter(str_detect(visual_model,i))
#   
#   s_irradplot<- ggplot(data, aes(x= irradiance,y=mean_dS,fill=patch1))+
#     geom_bar(stat = "identity", position = position_dodge())+
#     geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
#     scale_fill_manual(values=c("#DEB887","#F08080")) +
#     ggtitle(data$visual_model[1])+
#     ylim(0,10)
#   # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
#   
#   name_a=paste("schr_irrad_plot",count,sep="")
#   assign(name_a,s_irradplot)
#   
#   ## bosk:
#   data=noise_model_mean_se%>%
#     filter((patch2=="A. BOSKIANUS SUBSTRATE" & str_detect(patch1,"bosk")))%>%
#     filter(str_detect(visual_model,i))
#   
#   b_irradplot<- ggplot(data, aes(x= irradiance,y=mean_dS,fill=patch1))+
#     geom_bar(stat = "identity", position = position_dodge())+
#     geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
#     scale_fill_manual(values=c("#DEB887","#87CEEB")) +
#     ggtitle(data$visual_model[1])+
#     ylim(0,10)
#   # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
#   
#   name_a=paste("bosk_irrad_plot",count,sep="")
#   assign(name_a,b_irradplot)
#   
#   
#   ##bs:
#   data=noise_model_mean_se%>%
#     filter((patch2=="A. BEERSHEBENSIS SUBSTRATE" & str_detect(patch1,"beershebensis")))%>%
#     filter(str_detect(visual_model,i))
#   
#   bs_irradplot<- ggplot(data, aes(x= irradiance,y=mean_dS,fill=patch1))+
#     geom_bar(stat = "identity", position = position_dodge())+
#     geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
#     scale_fill_manual(values=c("#DEB887","#87CEEB")) +
#     ggtitle(data$visual_model[1])+
#     ylim(0,10)
#   # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
#   
#   name_a=paste("bs_irrad_plot",count,sep="")
#   assign(name_a,bs_irradplot)
#   
#   count=count+1
#   
#   
#   
# }
# 
# 
# grid_arrange_shared_legend(bosk_irrad_plot1 ,bosk_irrad_plot2 ,bosk_irrad_plot3 ,bosk_irrad_plot4, bosk_irrad_plot5,nrow = 1) ## can also control ncol
# grid_arrange_shared_legend(schr_irrad_plot1 ,schr_irrad_plot2 ,schr_irrad_plot3 ,schr_irrad_plot4, schr_irrad_plot5, nrow = 1) ## can also control ncol
# grid_arrange_shared_legend(bs_irrad_plot1 ,bs_irrad_plot2 ,bs_irrad_plot3 ,bs_irrad_plot4, bs_irrad_plot5, nrow = 1) ## can also control ncol
# 
# 
# 
# ### 4) colorful tails in different light conditions
# 
# count=1
# for(i in vision_name){
#   ##schr:
#   data=noise_model_mean_se%>%
#     filter(str_detect(visual_model,i))%>%
#   filter((patch2=="A. SCHREIBERI SUBSTRATE" & str_detect(patch1,"schr"))|(patch2=="A. BOSKIANUS SUBSTRATE" & str_detect(patch1,"bosk"))|(patch2=="A. BEERSHEBENSIS SUBSTRATE" & str_detect(patch1,"beershebensis")))%>%
#   filter(age=="J")
#     
#   irradplot<- ggplot(data, aes(x= irradiance,y=mean_dS,fill=patch1))+
#     geom_bar(stat = "identity", position = position_dodge())+
#     geom_errorbar(aes(ymax=(mean_dS+se_dS),ymin=(mean_dS-se_dS) ,width=0.2),position=position_dodge(0.9))+
#     scale_fill_manual(values=c("blue","#87CEEB","#F08080","blue","#87CEEB","#F08080")) +
#     ggtitle(data$visual_model[1])+
#     ylim(0,10)
#   # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
#   
#   name_a=paste("irrad_plot",count,sep="")
#   assign(name_a,irradplot)
# 
#   count=count+1
# }
# 
# 
# grid_arrange_shared_legend(irrad_plot1 ,irrad_plot2 ,irrad_plot3,irrad_plot4,irrad_plot5, nrow = 1) ## can also control ncol
# 
# 
