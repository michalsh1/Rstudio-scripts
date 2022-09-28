library(readxl)
library(writexl)
library(taxize)
library(tibble)

##fixing names:
# dataset$scientific[row]
# row



setwd("C:/Users/michalsh/Documents/Programs and programming/R/Rstudio-scripts/taxons_get_names")


create_empty_dataset=function(taxons_path){
  dataset <- read_excel(taxons_path)
  dataset['3']=NA
  dataset['4']=NA
  dataset['5']=NA
  dataset['6']=NA
  dataset['7']=NA
  dataset['8']=NA
  dataset['9']=NA
  dataset['10']=NA
  dataset['11']=NA
  dataset['12']=NA
  dataset['13']=NA
  dataset['14']=NA
  dataset['15']=NA
  dataset['16']=NA
  dataset['17']=NA
  dataset['18']=NA
  dataset['19']=NA
  dataset['20']=NA
  dataset['21']=NA
  dataset['22']=NA
  dataset['23']=NA
  dataset['24']=NA
  dataset['25']=NA
  dataset['26']=NA
  dataset['27']=NA
  dataset['28']=NA
  dataset['29']=NA
  dataset['30']=NA
  dataset['31']=NA
  dataset['32']=NA
  dataset['33']=NA
  dataset['34']=NA
  dataset['35']=NA
  dataset['36']=NA
  dataset['37']=NA
  
  return(dataset)
}


--------------------------------------------------------------------------------------------
###find common names, incl. heb :
find_eol_names= function(dataset){
  for (row in seq(1,length(rownames(dataset)))){
    print(row)
    print(dataset$id[row])
    
    sci=dataset$scientific[row]
    pageid <-eol_search(sci)$pageid[1]
    x <-eol_pages(taxonconceptID = pageid, common_names = TRUE)
    
    if (length(x$vernacular$language)>0){
      en=c()
      he=c()
      for (j in 1:length(x$vernacular$language)){
        lan= x$vernacular$language[j]
        if (lan=='he'){
          he_name= x$vernacular$vernacularname[j]
          he= append(he, he_name)
        }
        else if(lan=='en' && x$vernacular$eol_preferred[j]==TRUE){
          en_name= x$vernacular$vernacularname[j]
          # print(en_name)
          # print(x$vernacular)
          en= append(en,en_name)
        }
      }
      # print(he)
      
      if (length(he)>0){
        for (h in 1:length(he)){
          column_he = h+2
          dataset[row,column_he]=he[h]
        }
      }
      if (length(en)>0){
        for (s in 1:length(en)){
          column = s+5
          dataset[row,column]=en[s]
          
        }
        # print(en)
      }
    }
  }
  return(dataset)
}

taxon_path="C:/Users/michalsh/Documents/Programs and programming/R/Rstudio-scripts/taxons_get_names/TAXONS.xlsx"
empty_dataset=create_empty_dataset(taxon_path)
dataset=find_eol_names(dataset=empty_dataset)
new_file_name="TAXONS_names"
write_xlsx(dataset,path = paste(new_file_name,'.xlsx',sep=''))

----------------------------------------------------------------------------------------


###find Hebrew names only:
find_heb_only_eol_names= function(dataset){
  for (row in seq(1,length(rownames(dataset)))){
    # print(row)
    # print(dataset$id[row])
    
    sci=dataset$scientific[row]
    pageid <-eol_search(sci)$pageid[1]
    x <-eol_pages(taxonconceptID = pageid, common_names = TRUE)
    he=c()
    if (length(x$vernacular$language)>0){
      he=c()
      for (j in 1:length(x$vernacular$language)){
        lan= x$vernacular$language[j]
        if (lan=='he'){
          he_name= x$vernacular$vernacularname[j]
          he= append(he, he_name)
        }
      }
      print(c(sci,he))
      
      
      if (length(he)>0){
        for (h in 1:length(he)){
          column_he = h+2
          dataset[row,column_he]=he[h]
        }
      }
    }
  }
  return(dataset)
}

taxon_path="C:/Users/michalsh/Documents/Programs and programming/R/Rstudio-scripts/taxons_get_names/FISHES_FOR_HEB_NAMES.xlsx"
empty_dataset=create_empty_dataset(taxon_path)
dataset=find_heb_only_eol_names(dataset=empty_dataset)
new_file_name="FISHES_FOR_HEB_NAMES_names.xlsx"
write_xlsx(dataset,path = paste(new_file_name,'.xlsx',sep=''))


-------------------------------------------------------------------------------

###find only valid name-- only species or genus:
get_valid_name = function(sci){
  taxon_data=get_ids_(sci, db="gbif",rows=1)
  taxon_data[[1]][1]
  taxon_data_available <- unlist(taxon_data)
  key=paste("gbif.",sci,".species",sep='') 
  valid_name=taxon_data_available[key]
  #valid_name =  taxon_data_available[14][1]
  print(valid_name)
  return(valid_name)
}
find_valid_names= function(dataset){
  for (row in seq(1,length(rownames(dataset)))){
    print(row)
    print(dataset$id[row])
    sci = dataset$scientific[row]
    valid_name = get_valid_name(sci)
    if (!is.null(valid_name)){
      print(valid_name)
      column = 3
      dataset[row,column]=valid_name
    }      
  }
  return(dataset)
}

taxon_path="C:/Users/michalsh/Documents/Programs and programming/R/Rstudio-scripts/taxons_get_names/FISHES_FOR_VALID_NAMES.xlsx"
empty_dataset=create_empty_dataset(taxon_path)
dataset=find_valid_names(dataset=empty_dataset)
new_file_name="FISHES_FOR_VALID_NAMES_valid_names"
write_xlsx(dataset,path = paste(new_file_name,'.xlsx',sep=''))



####PLAYGROUND:
#get_ids(sci, db="gbif")[1]
#gbif_name_usage(2383314)$species

#xx=get_ids_(sci, db="gbif",rows=1)
#xx[1]$gbif$`Chelinius undulatus`$species
#paste("xx[1]$gbif$",sci,"$species",sep='')
#scientific_name=paste(sci)
#xx[1]$gbif$sci$species

sci="Chelinus undulatus"
