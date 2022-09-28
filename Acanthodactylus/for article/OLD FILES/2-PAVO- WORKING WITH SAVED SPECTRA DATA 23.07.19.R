### running all packeges ----
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
setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article")
spec_all_avg_data=read.table("spec_all_avg_data.csv",header=TRUE,sep=",")
spec_all_avg_data=as.rspec(spec_all_avg_data)

schr_bkg2=read.table("schr_bkg2.csv",header=TRUE,sep=",")
schr_bkg2=as.rspec(schr_bkg2)

bosk_bkg2=read.table("bosk_bkg2.csv",header=TRUE,sep=",")
bosk_bkg2=as.rspec(bosk_bkg2)

bs_bkg2=read.table("bs_bkg2.csv",header=TRUE,sep=",")
bs_bkg2=as.rspec(bs_bkg2)

# VISUAL MODELLING --------------------------------------------------------

# a=vissyst  == the visual infrmation in PAVO
### A) inserting vision info for all models: ----
if (1 == 1){
  lizard = sensmodel(peaksens=c(365, 455, 494, 564), lambdacut=c(0, 378, 467, 502),oiltype = c('C','C','Y','R') )
  snake =sensmodel(peaksens=c(358.4, 481.2, 554))
  red_fox=sensmodel(peaksens=c(438,555))
  human = sensmodel(peaksens=c(420, 533.8, 562.8))
  human[1:100,]=0
  human[1:100,1]=c(300:399)
  # data for human & fox vision :
  spec_all_avg_data_400_700=spec_all_avg_data%>%
    filter(wl %in% seq(400,700))
  spec_all_avg_data_400_700=as.rspec(spec_all_avg_data_400_700)
  
  ### calculating visual model for each predator:
  # d65 daylight :
  # **no background inserted:
  LIZARD_d65 <- vismodel(spec_all_avg_data, visual = lizard, illum='D65', achromatic = "l", qcatch = "Qi", relative=FALSE )
  SNAKE_d65 <- vismodel(spec_all_avg_data, visual = snake, illum='D65', achromatic = "l", qcatch = "Qi", relative=FALSE )
  AVES_UV_d65<- vismodel(spec_all_avg_data, visual = "avg.uv", illum='D65',achromatic="l", qcatch = "Qi", relative=FALSE )
  AVES_V_d65<- vismodel(spec_all_avg_data, visual = "avg.v", illum='D65',achromatic="l", qcatch = "Qi", relative=FALSE )
  HUMAN_d65<- vismodel(spec_all_avg_data, visual = human, illum='D65', achromatic = "l", qcatch = "Qi", relative=FALSE )
  FOX_d65 <- vismodel(spec_all_avg_data, visual = red_fox, illum='D65', achromatic = "l", qcatch = "Qi", relative=FALSE )
  
  # WOODLAND SHADE: 
  # **no background inserted:
  LIZARD_woodland <- vismodel(spec_all_avg_data, visual = lizard, illum='bluesky', achromatic = "l", qcatch = "Qi", relative=FALSE )
  SNAKE_woodland  <- vismodel(spec_all_avg_data, visual = snake, illum='bluesky', achromatic = "l", qcatch = "Qi", relative=FALSE )
  AVES_UV_woodland <- vismodel(spec_all_avg_data, visual = "avg.uv", illum='bluesky',achromatic="l", qcatch = "Qi", relative=FALSE )
  AVES_V_woodland <- vismodel(spec_all_avg_data, visual = "avg.v", illum='bluesky',achromatic="l", qcatch = "Qi", relative=FALSE )
  HUMAN_woodland <- vismodel(spec_all_avg_data, visual = human, illum='bluesky', achromatic = "l", qcatch = "Qi", relative=FALSE )
  FOX_woodland  <- vismodel(spec_all_avg_data, visual = red_fox, illum='bluesky', achromatic = "l", qcatch = "Qi", relative=FALSE )
}
### B) running the vorobyev model for the visual models of each predator-----

noise_model_results=data.frame(visual_model=c(NA),irradiance=c(NA),patch1=c(NA),patch2=c(NA),dS=c(NA),dL=c(NA)) 
count=1

for (i in c("LIZARD_d65","SNAKE_d65","FOX_d65","AVES_UV_d65","AVES_V_d65","HUMAN_d65",
            "LIZARD_woodland","SNAKE_woodland","FOX_woodland","AVES_UV_woodland","AVES_V_woodland",
            "HUMAN_woodland")
) { 
  if (i== "LIZARD_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 1, 1.6, 16.4), achromatic = TRUE )
    ### default is : weber=0.1, by 4 (LWS)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="LIZARD_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  else if (i== "SNAKE_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 1.6, 7.3), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="SNAKE_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  
  else if (i== "FOX_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 1), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="FOX_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  
  else if (i== "AVES_UV_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 1.92, 2.68, 2.7), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="AVES_UV_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  } 
  else if (i== "AVES_V_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 1.9, 2.2, 2.1), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="AVES_V_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  
  else if (i== "HUMAN_d65"){
    A=coldist(get(i), noise='neural', n=c(1, 5.49, 10.99), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="HUMAN_d65"
      noise_model_results[count,2]="Full sunlight"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  
  else if (i== "LIZARD_woodland"){
    A=coldist(get(i), noise='neural', n=c(1, 1, 1.6, 16.4), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="LIZARD_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  else if (i== "SNAKE_woodland"){
    A=coldist(get(i), noise='neural', n=c(1, 1.6, 7.3), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="SNAKE_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  else if (i== "FOX_woodland"){
    A=coldist(get(i), noise='neural', n=c(1,1), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="FOX_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  
  else if (i== "AVES_UV_woodland"){
    A=coldist(get(i), noise='neural',n=c(1, 1.92, 2.68, 2.7), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="AVES_UV_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  } 
  else if (i== "AVES_V_woodland"){
    A=coldist(get(i), noise='neural',  n=c(1, 1.9, 2.2, 2.1), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="AVES_V_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
  else if (i== "HUMAN_woodland"){
    A=coldist(get(i), noise='neural',  n=c(1, 5.49, 10.99), achromatic = TRUE)
    for ( x in 1:length(A[,1])){
      noise_model_results[count,1]="HUMAN_woodland"
      noise_model_results[count,2]="Partial shade"
      noise_model_results[count,3]= as.character(A[x,1])
      noise_model_results[count,4]=as.character(A[x,2])
      noise_model_results[count,5]=as.character(A[x,3])
      noise_model_results[count,6]=as.character(A[x,4])
      count=count+1
    }
  }
}

### C) creating model results table-----
noise_model_mean_se=data.frame(species=c(NA),age=c(NA),color=c(NA),n=c(NA),visual_model=c(NA),
                               irradiance=c(NA),patch1=c(NA),patch2=c(NA),mean_dS=c(NA),se_dS=c(NA),mean_dL=c(NA),se_dL=c(NA))

count=1    
for (i in c("LIZARD_d65","SNAKE_d65","FOX_d65","AVES_UV_d65","AVES_V_d65","HUMAN_d65",
            "LIZARD_woodland","SNAKE_woodland","FOX_woodland","AVES_UV_woodland","AVES_V_woodland",
            "HUMAN_woodland")
){ 
  for (j in c("SCHR_bkg","BOSK_bkg","BS_bkg")){
    vis.i_bkg.j=noise_model_results%>%
      filter(visual_model==i)%>%
      filter(patch2==j)
    schr_red=vis.i_bkg.j%>%
      filter( str_detect(patch1,"red"))%>%
      filter( str_detect(patch1,"schr"))
    
    mean=mean(as.numeric(schr_red$dS))
    se= std.error(as.numeric(schr_red$dS))
    mean_dL=mean(as.numeric(schr_red$dL))
    se_dL= std.error(as.numeric(schr_red$dL))
    n=length(schr_red$visual_model)
    
    noise_model_mean_se[count,1]="schr"
    noise_model_mean_se[count,2]="J"
    noise_model_mean_se[count,3]="red"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. schreiberi hatchling red tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se$mean_dL[count]=mean_dL
    noise_model_mean_se$se_dL[count]=se_dL
    
    if (count==1){
      noise_model_for_statistics=schr_red
    }
    else{
      noise_model_for_statistics=rbind(noise_model_for_statistics,schr_red)
    }
    
    count=count+1
    
    
    # ***********************************************
    schr_brown_tail=vis.i_bkg.j%>%
      filter( str_detect(patch1,"schr"))%>%
      filter( str_detect(patch1,"brown"))%>%
      filter( str_detect(patch1,"tail"))
    mean=mean(as.numeric(schr_brown_tail$dS))
    se=std.error(as.numeric(schr_brown_tail$dS))
    mean_dL=mean(as.numeric(schr_brown_tail$dL))
    se_dL= std.error(as.numeric(schr_brown_tail$dL))
    n=length(schr_brown_tail$visual_model)
    
    noise_model_mean_se[count,1]="schr"
    noise_model_mean_se[count,2]="A"
    noise_model_mean_se[count,3]="brown"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. schreiberi adult brown tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se[count,11]=mean_dL
    noise_model_mean_se[count,12]=se_dL
    
    noise_model_for_statistics=rbind(noise_model_for_statistics,schr_brown_tail)
    count=count+1
    
    
    # ***********************************************
    bosk_blue=vis.i_bkg.j%>%
      filter(str_detect(patch1,"blue"))%>%
      filter( str_detect(patch1,"bosk"))
    mean=mean(as.numeric(bosk_blue$dS))
    se=std.error(as.numeric(bosk_blue$dS))
    mean_dL=mean(as.numeric(bosk_blue$dL))
    se_dL= std.error(as.numeric(bosk_blue$dL))
    n=length(bosk_blue$visual_model)
    
    noise_model_mean_se[count,1]="bosk"
    noise_model_mean_se[count,2]="J"
    noise_model_mean_se[count,3]="blue"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. boskianus hatchling blue tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se[count,11]=mean_dL
    noise_model_mean_se[count,12]=se_dL
    
    noise_model_for_statistics=rbind(noise_model_for_statistics,bosk_blue)
    count=count+1
    
    # ***********************************************
    bosk_brown_tail=vis.i_bkg.j%>%
      filter( str_detect(patch1,"brown"))%>%
      filter( str_detect(patch1,"bosk"))%>%
      filter( str_detect(patch1,"tail"))
    mean=mean(as.numeric(bosk_brown_tail$dS))
    se=std.error(as.numeric(bosk_brown_tail$dS))
    mean_dL=mean(as.numeric(bosk_brown_tail$dL))
    se_dL= std.error(as.numeric(bosk_brown_tail$dL))
    n=length(bosk_brown_tail$visual_model)
    
    noise_model_mean_se[count,1]="bosk"
    noise_model_mean_se[count,2]="A"
    noise_model_mean_se[count,3]="brown"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. boskianus adult brown tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se[count,11]=mean_dL
    noise_model_mean_se[count,12]=se_dL
    
    noise_model_for_statistics=rbind(noise_model_for_statistics,bosk_brown_tail)
    count=count+1
    
    # ***********************************************
    bs_blue=vis.i_bkg.j%>%
      filter(str_detect(patch1,"blue"))%>%
      filter( str_detect(patch1,"bs"))
    mean=mean(as.numeric(bs_blue$dS))
    se=std.error(as.numeric(bs_blue$dS))
    mean_dL=mean(as.numeric(bs_blue$dL))
    se_dL= std.error(as.numeric(bs_blue$dL))
    n=length(bs_blue$visual_model)
    
    noise_model_mean_se[count,1]="beershebensis"
    noise_model_mean_se[count,2]="J"
    noise_model_mean_se[count,3]="blue"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. beershebensis hatchling blue tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se[count,11]=mean_dL
    noise_model_mean_se[count,12]=se_dL
    
    noise_model_for_statistics=rbind(noise_model_for_statistics,bs_blue)
    count=count+1
    
    # ***********************************************
    bs_brown_tail=vis.i_bkg.j%>%
      filter( str_detect(patch1,"brown"))%>%
      filter( str_detect(patch1,"bs"))%>%
      filter( str_detect(patch1,"tail"))
    mean=mean(as.numeric(bs_brown_tail$dS))
    se=std.error(as.numeric(bs_brown_tail$dS))
    mean_dL=mean(as.numeric(bs_brown_tail$dL))
    se_dL= std.error(as.numeric(bs_brown_tail$dL))
    n=length(bs_brown_tail$visual_model)
    
    noise_model_mean_se[count,1]="beershebensis"
    noise_model_mean_se[count,2]="A"
    noise_model_mean_se[count,3]="brown"
    noise_model_mean_se[count,4]=n
    noise_model_mean_se[count,5]=i
    noise_model_mean_se[count,6]=vis.i_bkg.j$irradiance[1]
    noise_model_mean_se[count,7]="A. beershebensis adult brown tail"
    
    if (vis.i_bkg.j$patch2[1]=="BS_bkg"){
      noise_model_mean_se[count,8]="A. BEERSHEBENSIS SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="SCHR_bkg"){
      noise_model_mean_se[count,8]="A. SCHREIBERI SUBSTRATE"
    }
    else if(vis.i_bkg.j$patch2[1]=="BOSK_bkg"){
      noise_model_mean_se[count,8]="A. BOSKIANUS SUBSTRATE"
    }
    
    noise_model_mean_se[count,9]=mean
    noise_model_mean_se[count,10]=se
    noise_model_mean_se[count,11]=mean_dL
    noise_model_mean_se[count,12]=se_dL
    
    noise_model_for_statistics=rbind(noise_model_for_statistics,bs_brown_tail)
    count=count+1
  }
}



### SAVING RELEVANT DATA 
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/for article")
write.table(noise_model_results,file=paste("noise_model_results",Sys.Date(),".csv",sep=""),sep=",")
write.table(noise_model_mean_se,file=paste("noise_model_mean_se",Sys.Date(),".csv",sep=""),sep=",")
write.table(noise_model_for_statistics,file=paste("noise_model_for_statistics",Sys.Date(),".csv",sep=""),sep=",")



### ----- PLOTS ---------------
## A) reflectance ------

bkg_color="#DEB887"
adult_color="saddlebrown"
schr_J_color="#F08080"
bosk_J_color="cornflowerblue"
bs_J_color="#87CEEB"
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
  col.list= c( adult_color,schr_J_color,bkg_color)
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
  
  col.list= c(adult_color, bosk_J_color, bkg_color)
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
  
  col.list= c(bs_J_color, adult_color, bkg_color)
  aggplot(bs_plot,by=names(bs_plot),shadecol=col.list, lcol=col.list,legend = TRUE,alpha=0.25,ylim = c(0,70))
  # explorespec(a_bs_plot, by=10, lwd=2, legpos="topleft")
}


## all colorfultails- reflectance spectrum plots --
if (1==1){
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
  aggplot(colored_tails_plot,by=names(colored_tails_plot),shadecol=col.list, lcol=col.list,legend = TRUE ,alpha=0.25,ylim = c(0,70))
  # explorespec(a_schr_plot, by=9, lwd=2, legpos="topleft")
}  

#### B) noise model plots -----

## creating data frame with real habitat of each lizard in d65 irradiance:
noise_model_real_habitat=noise_model_mean_se%>%
  filter((patch2=="A. SCHREIBERI SUBSTRATE" & str_detect(patch1,"schr"))|
           (patch2=="A. BOSKIANUS SUBSTRATE" & str_detect(patch1,"bosk"))|
           (patch2=="A. BEERSHEBENSIS SUBSTRATE" & str_detect(patch1,"beershebensis")))

noise_model_d65_real_habitat=noise_model_real_habitat%>%
  filter(irradiance=="Full sunlight")

noise_model_woodland_real_habitat=noise_model_real_habitat%>%
  filter(irradiance=="Partial shade")

## 1)
## 1A) DS- tail colors of the same species ------------------
if (1==1){
  vis=c("AVES_UV_d65", "AVES_V_d65", "LIZARD_d65","SNAKE_d65",
        "FOX_d65","HUMAN_d65","AVES_UV_woodland", "AVES_V_woodland",
        "LIZARD_woodland","SNAKE_woodland","FOX_woodland","HUMAN_woodland")
  
  vis_plots_schr=c(NA)
  vis_plots_bosk=c(NA)
  vis_plots_bs=c(NA)
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
    x= paste("schr_plot",i,sep="_")
    plot= ggplot(a, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,schr_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    # print(plot)
    assign(x,plot)
    vis_plots_schr[c1]=x
    c1=c1+1
    
    x= paste("bosk_plot",i,sep="_")
    plot= ggplot(b, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,bosk_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x,plot)
    vis_plots_bosk[c2]=x
    c2=c2+1
    
    x= paste("bs_plot",i,sep="_")
    plot= ggplot(c, aes(x= patch1,y=mean_dS,fill=patch1))+
      geom_bar(stat = "identity", position = position_dodge())+
      geom_errorbar(aes(ymax=mean_dS+se_dS,ymin=mean_dS-se_dS ,width=0.2),position=position_dodge(0.9))+
      scale_fill_manual(values=c(adult_color,bs_J_color)) +
      ggtitle(i)+
      ylim(0,10)
    # geom_hline(yintercept =1,col="azure4",lwd=1 ,lty=2)
    
    assign(x,plot)
    vis_plots_bs[c3]=x
    c3=c3+1
  }
  
  
  
  
  # pltting my data:
  # printing schr d65:
  grid_arrange_shared_legend(schr_plot_AVES_UV_d65,schr_plot_AVES_V_d65,
                             schr_plot_LIZARD_d65,schr_plot_SNAKE_d65,
                             schr_plot_FOX_d65, nrow = 1) ## can also control ncol
  # printing bosk d65:
  grid_arrange_shared_legend(bosk_plot_AVES_UV_d65,bosk_plot_AVES_V_d65,
                             bosk_plot_LIZARD_d65,bosk_plot_SNAKE_d65,
                             bosk_plot_FOX_d65,  nrow = 1) ## can also control ncol
  # printing bs d65:
  grid_arrange_shared_legend(bs_plot_AVES_UV_d65,bs_plot_AVES_V_d65,
                             bs_plot_LIZARD_d65,bs_plot_SNAKE_d65,
                             bs_plot_FOX_d65, nrow = 1) ## can also control ncol
  
  
  
  # printing schr woodland:
  grid_arrange_shared_legend(schr_plot_AVES_UV_woodland,schr_plot_AVES_V_woodland,
                             schr_plot_LIZARD_woodland,schr_plot_SNAKE_woodland,
                             schr_plot_FOX_woodland, nrow = 1) ## can also control ncol
  
  # # printing bosk woodland:
  # a=grid_arrange_shared_legend(bosk_plot_AVES_UV_woodland,bosk_plot_AVES_V_woodland,
  #                              bosk_plot_LIZARD_woodland,bosk_plot_SNAKE_woodland,
  #                              bosk_plot_FOX_woodland, nrow = 1) ## can also control ncol
  # # printing bs woodland:
  # a=grid_arrange_shared_legend(bs_plot_AVES_UV_woodland,bs_plot_AVES_V_woodland,
  #                              bs_plot_LIZARD_woodland,bs_plot_SNAKE_woodland,
  #                              bs_plot_FOX_woodland, nrow = 1) ## can also control ncol
  
}


## 1B) DL- tail colors of the same species ------------------
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
  
  
  
  
  # pltting my data:
  
  ######DS:
  
  # printing schr d65:
  grid_arrange_shared_legend(schr_plot_dS_AVES_UV_d65,schr_plot_dS_AVES_V_d65,
                             schr_plot_dS_LIZARD_d65,schr_plot_dS_SNAKE_d65,
                             schr_plot_dS_FOX_d65, nrow = 1) ## can also control ncol
  # printing bosk d65:
  grid_arrange_shared_legend(bosk_plot_dS_AVES_UV_d65,bosk_plot_dS_AVES_V_d65,
                             bosk_plot_dS_LIZARD_d65,bosk_plot_dS_SNAKE_d65,
                             bosk_plot_dS_FOX_d65,  nrow = 1) ## can also control ncol
  # printing bs d65:
  grid_arrange_shared_legend(bs_plot_dS_AVES_UV_d65,bs_plot_dS_AVES_V_d65,
                             bs_plot_dS_LIZARD_d65,bs_plot_dS_SNAKE_d65,
                             bs_plot_dS_FOX_d65, nrow = 1) ## can also control ncol
  
  # printing schr woodland:
  grid_arrange_shared_legend(schr_plot_dS_AVES_UV_woodland,schr_plot_dS_AVES_V_woodland,
                             schr_plot_dS_LIZARD_woodland,schr_plot_dS_SNAKE_woodland,
                             schr_plot_dS_FOX_woodland, nrow = 1) ## can also control ncol
  
  
  ######DL:
  
  # printing schr d65:
  grid_arrange_shared_legend(schr_plot_dL_AVES_UV_d65,schr_plot_dL_AVES_V_d65,
                             schr_plot_dL_LIZARD_d65,schr_plot_dL_SNAKE_d65,
                             schr_plot_dL_FOX_d65, nrow = 1) ## can also control ncol
  # printing bosk d65:
  grid_arrange_shared_legend(bosk_plot_dL_AVES_UV_d65,bosk_plot_dL_AVES_V_d65,
                             bosk_plot_dL_LIZARD_d65,bosk_plot_dL_SNAKE_d65,
                             bosk_plot_dL_FOX_d65,  nrow = 1) ## can also control ncol
  # printing bs d65:
  grid_arrange_shared_legend(bs_plot_dL_AVES_UV_d65,bs_plot_dL_AVES_V_d65,
                             bs_plot_dL_LIZARD_d65,bs_plot_dL_SNAKE_d65,
                             bs_plot_dL_FOX_d65, nrow = 1) ## can also control ncol
  
  # printing schr woodland:
  grid_arrange_shared_legend(schr_plot_dL_AVES_UV_woodland,schr_plot_dL_AVES_V_woodland,
                             schr_plot_dL_LIZARD_woodland,schr_plot_dL_SNAKE_woodland,
                             schr_plot_dL_FOX_woodland, nrow = 1) ## can also control ncol
  
  
  
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
                             nrow = 2,ncol=3) ## can also control ncol
  
  
  
  count=NA
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
