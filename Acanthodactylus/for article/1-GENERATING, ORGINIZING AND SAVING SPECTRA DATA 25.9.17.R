
 ### GENERATING, ORGINIZING AND SAVING SPECTRA DATA
#### running pakeges: -----
library(pavo)
library(dplyr)

# A) inserting lizards' data -----------------------------------------------------------
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/scans/all lizards files/final files for analysis/lizards")
all=list.files()

##1297-1328
a=read.table(file = all[1])
spec_data_original=data.frame(wl=c(NA),a1=c(NA))
counter=1
for (j in 1:1297){
  spec_data_original[counter,"wl"]= a[j,1]
  counter=counter+1
}
for (k in 1336:2032){
  spec_data_original[counter,"wl"]= a[k,1]
  counter=counter+1
}
counter=1
for (j in 1:1297){
  spec_data_original[counter,2]= a[j,2]
  counter=counter+1
}
for (k in 1336:2032){
  spec_data_original[counter,2]= a[k,2]
  counter=counter+1
}

#attaching to that table more info of the same individual
count=2
for (i in 1:length(all)){
  file=read.table(file = all[i])
  counter=1
  for (j in 1:1297){
    spec_data_original[counter,count]=file[j,2]
    counter=counter+1
  }
  for (k in 1336:2032){
    spec_data_original[counter,count]=file[k,2]
    counter=counter+1
  }
  a=all[i]
  names(spec_data_original)[i+1]=gsub("\\.txt","",a)
  count=count+1
}


original_lizards_spec_data=as.rspec(spec_data_original,lim=c(300,700))

 # explorespec(original_lizards_spec_data, by=5, lwd=2, legpos="topleft")
### generating data per individuals: ======
specs_by_ind= data.frame()
count=1
for (i in c('bosk11','bosk12','bosk13','bosk16','bosk17','bosk20','bosk21','bosk22','bosk23','bosk24','bosk26',###adults
            'bosk14','bosk15','bosk27','bosk28','bosk29','bosk30','bosk31','bosk32','bosk33','bosk34', ###juveniles
            'schr29','schr30','schr31','schr32','schr33',"schr34","schr35","schr36","schr37","schr38", ###adults
            'schr4','schr11','schr12','schr13','schr14','schr15','schr19','schr20', ###juveniles
            'bs13','bs14','bs15','bs16','bs17','bs18','bs19','bs20','bs21','bs22','bs23',
            'bs34','bs35','bs36','bs37','bs38','bs39','bs40','bs41','bs42','bs43')) {

  all_of_ind <- subset(original_lizards_spec_data, i)
  all_titles_1 = gsub('\\([0-9])', '', names(all_of_ind))[-1] ##obj titles= only the spots' names (without (1),(2)...)
  all_no_01 <- aggspec(all_of_ind, by=all_titles_1)   ##obj2= averagings of the spots
  b= paste(i,"_specs", sep="")
  all_titles_2 = gsub('_scan[0-9][0-9]', '', names(all_no_01))[-1]
  all_no_numbers <- aggspec(all_no_01, by= all_titles_2 )
  specs_i=all_no_numbers
  # specs_i <- procspec(all_no_numbers, opt='smooth', span = 0.1)  ### this may generate"new" negetive num. - so we need to fix it:
  # specs_i <- procspec(specs_i, fixneg='addmin') ### remove negetive numbers
  
  if(count==1){
  specs_by_ind  =rbind(specs_by_ind,specs_i)
  count=2
    }
  else{
   specs_by_ind  =merge(specs_by_ind,specs_i)
  }
}


### Manually fixing negetive values:
if(1==1){
  specs_by_ind$bosk11_A_tail_dorsal_brown[1:30]=specs_by_ind$bosk11_A_tail_dorsal_brown[31]
  specs_by_ind$bosk12_A_tail_dorsal_brown[1:53]=specs_by_ind$bosk12_A_tail_dorsal_brown[54]
  specs_by_ind$bosk13_A_tail_dorsal_brown[1:33]=specs_by_ind$bosk13_A_tail_dorsal_brown[34]
  specs_by_ind$bosk16_A_M_tail_dorsal_brown[17:28]=mean(specs_by_ind$bosk16_A_M_tail_dorsal_brown[16],specs_by_ind$bosk16_A_M_tail_dorsal_brown[29])
  specs_by_ind$bosk17_A_M_tail_dorsal_brown[10:39]=mean(specs_by_ind$bosk17_A_M_tail_dorsal_brown[9],specs_by_ind$bosk17_A_M_tail_dorsal_brown[40])
  
  specs_by_ind$bosk21_A_tail_dorsal_brown[1:88]=specs_by_ind$bosk21_A_tail_dorsal_brown[89]
  specs_by_ind$bosk21_A_tail_dorsal_brown[400:401]=specs_by_ind$bosk21_A_tail_dorsal_brown[399]
  

  specs_by_ind$schr4_J_tail_dorsal_red[21]=mean(specs_by_ind$schr4_J_tail_dorsal_red[20],specs_by_ind$schr4_J_tail_dorsal_red[22])
  specs_by_ind$schr4_J_tail_dorsal_red[38:73]=mean(specs_by_ind$schr4_J_tail_dorsal_red[37],specs_by_ind$schr4_J_tail_dorsal_red[74])
  
  specs_by_ind$schr11_J_tail_dorsal_red[17:79]=mean(specs_by_ind$schr11_J_tail_dorsal_red[16],specs_by_ind$schr11_J_tail_dorsal_red[80])
  specs_by_ind$schr12_J_tail_dorsal_red[18:80]=mean(specs_by_ind$schr12_J_tail_dorsal_red[17],specs_by_ind$schr12_J_tail_dorsal_red[81])
  specs_by_ind$schr13_J_tail_dorsal_red[15:82]=mean(specs_by_ind$schr13_J_tail_dorsal_red[14],specs_by_ind$schr13_J_tail_dorsal_red[83])
  specs_by_ind$schr14_J_tail_dorsal_red[15:79]=mean(specs_by_ind$schr14_J_tail_dorsal_red[14],specs_by_ind$schr14_J_tail_dorsal_red[80])
  specs_by_ind$schr15_J_tail_dorsal_red[19:82]=mean(specs_by_ind$schr15_J_tail_dorsal_red[18],specs_by_ind$schr15_J_tail_dorsal_red[83])
  specs_by_ind$schr19_J_tail_dorsal_red[41:73]=mean(specs_by_ind$schr19_J_tail_dorsal_red[40],specs_by_ind$schr19_J_tail_dorsal_red[74])
  
  specs_by_ind$bs36_A_tail_dorsal_brown[49:52]=mean(specs_by_ind$bs36_A_tail_dorsal_brown[48],specs_by_ind$bs36_A_tail_dorsal_brown[52])
  specs_by_ind$bs36_A_tail_dorsal_brown[61:70]=mean(specs_by_ind$bs36_A_tail_dorsal_brown[60],specs_by_ind$bs36_A_tail_dorsal_brown[71])
  
  specs_by_ind$bs37_A_tail_dorsal_brown[46:70]=mean(specs_by_ind$bs37_A_tail_dorsal_brown[45],specs_by_ind$bs37_A_tail_dorsal_brown[71])
}
## function to check that the data has no negetive numbers (if there is- it'll print where it is)
# if (1==1){
#   c=0
#   for (i in 1:length(specs_by_ind)){
#     for (j in 1:401){
#       if (specs_by_ind[j,i]<0){
#         print(names(specs_by_ind[i]))
#       }
#       else if(specs_by_ind[j,i]>0){
#         c=c+1
#       }
#     }
#   }
#   c
# }

## smoothening:
specs_by_ind <- procspec(specs_by_ind, opt='smooth', span = 0.2)

### first look - plotting: --------------
#   a_schr_plot=specs_by_ind%>%
#   select(c(   wl,schr5_A_F_tail_dorsal_brown,schr6_tail_dorsal_brown,schr8_A_M_tail_dorsal_brown,
#               schr9_A_M_tail_dorsal_brown,schr10_A_F_tail_dorsal_brown,schr29_A_tail_dorsal_brown,  
#               schr30_A_tail_dorsal_brown,schr31_A_tail_dorsal_brown,schr32_A_tail_dorsal_brown,
#               schr33_A_tail_dorsal_brown,schr4_J_tail_dorsal_red,schr11_J_tail_dorsal_red,schr12_J_tail_dorsal_red,schr13_J_tail_dorsal_red,
#               schr14_J_tail_dorsal_red,schr15_J_tail_dorsal_red,schr19_J_tail_dorsal_red,
#               schr20_J_tail_dorsal_red))
# a_schr_plot=as.rspec(a_schr_plot)
# explorespec(a_schr_plot, by=5, lwd=2, legpos="topleft")
# 
# a_bosk_plot=specs_by_ind%>%
#   select(c(   wl,bosk11_A_tail_dorsal_brown,bosk12_A_tail_dorsal_brown,bosk13_A_tail_dorsal_brown,bosk16_A_M_tail_dorsal_brown,
#               bosk17_A_M_tail_dorsal_brown,bosk20_A_tail_dorsal_brown,bosk21_A_tail_dorsal_brown,
#               bosk22_A_tail_dorsal_brown,bosk23_A_tail_dorsal_brown,bosk24_A_tail_dorsal_brown,bosk26_A_tail_dorsal_brown,
#               bosk14_J_tail_dorsal_blue,bosk15_J_tail_dorsal_blue,bosk27_J_tail_dorsal_blue,bosk28_J_tail_dorsal_blue,bosk29_J_tail_dorsal_blue,bosk30_J_tail_dorsal_blue,
#               bosk31_J_tail_dorsal_blue,bosk32_J_tail_dorsal_blue,bosk33_J_tail_dorsal_blue,bosk34_J_tail_dorsal_blue ))
# a_bosk_plot=as.rspec(a_bosk_plot)
# explorespec(a_bosk_plot, by=7, lwd=2, legpos="topleft")
# 
#   a_bs_plot=specs_by_ind%>%
#   select(c(   wl,bs13_J_tail_dorsal_blue,bs14_J_tail_dorsal_blue,bs15_J_tail_dorsal_blue,bs16_J_tail_dorsal_blue,
#               bs17_J_tail_dorsal_blue,bs18_J_tail_dorsal_blue,bs19_J_tail_dorsal_blue,bs20_J_tail_dorsal_blue, 
#               bs21_J_tail_dorsal_blue,bs22_J_tail_dorsal_blue,bs23_J_tail_dorsal_blue,
#               bs34_A_tail_dorsal_brown,bs35_A_tail_dorsal_brown,bs36_A_tail_dorsal_brown,bs37_A_tail_dorsal_brown,
#               bs38_A_tail_dorsal_brown,bs39_A_tail_dorsal_brown,bs40_A_tail_dorsal_brown,bs41_A_tail_dorsal_brown,
#               bs42_A_tail_dorsal_brown,bs43_A_tail_dorsal_brown ))
# a_bs_plot=as.rspec(a_bs_plot)
# explorespec(a_bs_plot, by=5, lwd=2, legpos="topleft")

# B) inserting background data ---------------------------------------------------------
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/scans/all lizards files/final files for analysis/backgrounds")
all=list.files()

#creating basic table:
##1297-1328
a=read.table(file = all[1])
bkg_data_original=data.frame(wl=c(NA),a1=c(NA))
counter=1
for (j in 1:1297){
  bkg_data_original[counter,"wl"]= a[j,1]
  counter=counter+1
}
for (k in 1336:2032){
  bkg_data_original[counter,"wl"]= a[k,1]
  counter=counter+1
}
counter=1
for (j in 1:1297){
  bkg_data_original[counter,2]= a[j,2]
  counter=counter+1
}
for (k in 1336:2032){
  bkg_data_original[counter,2]= a[k,2]
  counter=counter+1
}
count=2
for (i in 1:length(all)){
  file=read.table(file = all[i])

  counter=1
  for (j in 1:1297){
    bkg_data_original[counter,count]=file[j,2]
    counter=counter+1
  }
  for (k in 1336:2032){
    bkg_data_original[counter,count]=file[k,2]
    counter=counter+1
  }
  a=all[i]

  names(bkg_data_original)[i+1]=gsub("\\.txt","",a)
  count=count+1
}


original_bkg_spec_data=as.rspec(bkg_data_original,lim=c(300,700))

### generating one mean bkg for all samples by species (bosk_bkg2, bs_bkg2, schr_bkg2) -------
for (i in c('bosk', 'schr',"bs")) {
  obj <- subset(original_bkg_spec_data, i)

  titles = gsub('\\([0-9])', '', names(obj))[-1]
  obj<- aggspec(obj, by=titles)

  titles = gsub('_scan[0-9][0-9]', '', names(obj))[-1]
  obj <- aggspec(obj, by=titles)

  titles = gsub('Reflection', '', names(obj))[-1]
  obj <- aggspec(obj, by=titles)
  # obj <- procspec(obj, opt='smooth', span = 0.2)  ### this may generate"new" negetive num. - so we need to fix it:
  # obj <- procspec(obj, fixneg='addmin') ### remove negetive numbers
  assign(paste(i,'_bkg2',sep=''), obj)

  
  titles = gsub('reflection', '', names(obj))[-1]
  obj <- aggspec(obj, by=titles)
  # obj <- procspec(obj, opt='smooth', span = 0.2)  ### this creates "new" minuses where it used to be 0
  # obj <- procspec(obj, fixneg='addmin') ### remove negetive numbers
  assign(paste(i,'_bkg2',sep=''), obj)
} 
  




### Manually fixing negetive values:  
if (1==1){
  bs_bkg2$ground02_park.hales_bs[29:75]=mean(bs_bkg2$ground02_park.hales_bs[28],bs_bkg2$ground02_park.hales_bs[76])
  bs_bkg2$ground03_park.hales_bs[29:75]=mean(bs_bkg2$ground03_park.hales_bs[28],bs_bkg2$ground03_park.hales_bs[76])
  bs_bkg2$ground04_park.hales_bs[27:83]=mean(bs_bkg2$ground04_park.hales_bs[26],bs_bkg2$ground04_park.hales_bs[84])
  bs_bkg2$ground05_park.hales_bs[41:71]=mean(bs_bkg2$ground05_park.hales_bs[40],bs_bkg2$ground05_park.hales_bs[72])
  bs_bkg2$ground06_park.hales_bs[48:64]=mean(bs_bkg2$ground06_park.hales_bs[47],bs_bkg2$ground06_park.hales_bs[65])
  bs_bkg2$ground07_park.hales_bs[31:76]=mean(bs_bkg2$ground07_park.hales_bs[30],bs_bkg2$ground07_park.hales_bs[77])
  bs_bkg2$ground08_park.hales_bs[41:72]=mean(bs_bkg2$ground08_park.hales_bs[40],bs_bkg2$ground08_park.hales_bs[73])
  bs_bkg2$ground09_park.hales_bs[46:65]=mean(bs_bkg2$ground09_park.hales_bs[45],bs_bkg2$ground09_park.hales_bs[66])
  bs_bkg2$ground10_park.hales_bs[46:64]=mean(bs_bkg2$ground10_park.hales_bs[45],bs_bkg2$ground10_park.hales_bs[65])
  bs_bkg2$ground11_park.hales_bs[30:77]=mean(bs_bkg2$ground11_park.hales_bs[29],bs_bkg2$ground11_park.hales_bs[78])
  schr_bkg2$ground04_HaderaSands1_schreiberi[40:70]=mean(schr_bkg2$ground04_HaderaSands1_schreiberi[39],schr_bkg2$ground04_HaderaSands1_schreiberi[71])
}
## function to check that the data has no negetive numbers (if there is- it'll print where it is)
# if (1==1){
#   c=0
#   for (i in 1:length(schr_bkg2)){
#     for (j in 1:401){
#     if (schr_bkg2[j,i]<0){
#       print(names(schr_bkg2[i]))
#     }
#       else if(schr_bkg2[j,i]>0){
#         c=c+1
#       }
#     }
#   }
#   c
# }

## smoothening:
bosk_bkg2 <- procspec(bosk_bkg2, opt='smooth', span = 0.2)
bs_bkg2 <- procspec(bs_bkg2, opt='smooth', span = 0.2)
schr_bkg2 <- procspec(schr_bkg2, opt='smooth', span = 0.2)




  #### generating table with mean for each species' habitat soil ------
count=1
for (i in c("bosk","schr","bs")){
    if (i=="bosk"){
    obj=bosk_bkg2
    titles = gsub('ground[0-9][0-9]_*', '', names(obj))[-1]
    obj <- aggspec(obj, by=titles)
        titles = gsub('ianus', '', names(obj))[-1]
        obj <- aggspec(obj, by=titles)
      titles = gsub('BorHemet', '', names(obj))[-1]
        obj <- aggspec(obj, by=titles)
      titles = gsub('nahalziporim', '', names(obj))[-1]
        obj <- aggspec(obj, by=titles)
      titles = gsub('Zin', '', names(obj))[-1]
        obj <- aggspec(obj, by=titles)
      titles = gsub('.[0-9]', '', names(obj))[-1]
        obj <- aggspec(obj, by=titles)
    
        if (count==1){
          bkg_spec_data_avg=obj
        }
        else{
          bkg_spec_data_avg=merge(bkg_spec_data_avg,obj)
        }
        count=count+1
      }
   
  else if (i =="schr"){
    obj=schr_bkg2
    titles = gsub('ground[0-9][0-9]_*', '', names(obj))[-1]
     obj <- aggspec(obj, by=titles)
    titles = gsub('HaderaSands', '', names(obj))[-1]
      obj <- aggspec(obj, by=titles)

    titles = gsub('X[0-9]', '', names(obj))[-1]
     obj <- aggspec(obj, by=titles)
    
     bkg_spec_data_avg=merge(bkg_spec_data_avg,obj)
  }
  
  else if (i=="bs"){
    obj=bs_bkg2
    titles = gsub('ground[0-9][0-9]_*', '', names(obj))[-1]
      obj <- aggspec(obj, by=titles)
    
      bkg_spec_data_avg=merge(bkg_spec_data_avg,obj)
  }
}

  names(bkg_spec_data_avg)[2]<-paste("BOSK_bkg")
  names(bkg_spec_data_avg)[3]<-paste("SCHR_bkg")
  names(bkg_spec_data_avg)[4]<-paste("BS_bkg")


## C) merging all data --------
spec_all_avg_data= merge(specs_by_ind,bkg_spec_data_avg)

# D) saving data --------
setwd("C:/Users/michalsh/Google Drive/Lab/spectrometer/for article")
  
  ##original files I used:
  write.table(original_bkg_spec_data,file=paste("original_bkg_spec_data.csv",sep=""),sep=",")
  write.table(original_lizards_spec_data,file=paste("original_lizards_spec_data.csv",sep=""),sep=",")
  
  ### file with all average data for analysisL
  write.table(spec_all_avg_data,file=paste("spec_all_avg_data.csv",sep=""),sep=",")
  # write.table(specs_by_ind,file=paste("specs_by_ind.csv",sep=""),sep=",") ## no need- all data in spec_all_avg_data!

  # bkg files- with all spots for each zpecies' habitat soil
  write.table(bosk_bkg2,file=paste("bosk_bkg2.csv",sep=""),sep=",")
  write.table(schr_bkg2,file=paste("schr_bkg2.csv",sep=""),sep=",")
  write.table(bs_bkg2,file=paste("bs_bkg2.csv",sep=""),sep=",")

