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
setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/SPECTRA DATA")
spec_all_avg_data=read.table("spec_all_avg_data.csv",header=TRUE,sep=",")
spec_all_avg_data=procspec(spec_all_avg_data,fixneg="zero")

schr_bkg2=read.table("schr_bkg2.csv",header=TRUE,sep=",")
schr_bkg2=procspec(schr_bkg2,fixneg="zero")

bosk_bkg2=read.table("bosk_bkg2.csv",header=TRUE,sep=",")
bosk_bkg2=procspec(bosk_bkg2,fixneg="zero")

bs_bkg2=read.table("bs_bkg2.csv",header=TRUE,sep=",")
bs_bkg2=procspec(bs_bkg2,fixneg="zero")

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



### SAVING RELEVANT DATA ----------------
##save working data
setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/NOISE MODEL RESULTS")
write.table(noise_model_results,file="noise_model_results.csv",sep=",")
write.table(noise_model_mean_se,file="noise_model_mean_se.csv",sep=",")
write.table(noise_model_for_statistics,file="noise_model_for_statistics.csv",sep=",")


## backup with date
setwd("C:/Users/michalsh/Documents/R/Acanthodactylus/for article/NOISE MODEL RESULTS/backups")
write.table(noise_model_results,file=paste("noise_model_results",Sys.Date(),".csv",sep=""),sep=",")
write.table(noise_model_mean_se,file=paste("noise_model_mean_se",Sys.Date(),".csv",sep=""),sep=",")
write.table(noise_model_for_statistics,file=paste("noise_model_for_statistics",Sys.Date(),".csv",sep=""),sep=",")



