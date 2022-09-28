library(readxl)
library(writexl)

setwd("C:/Users/michalsh/Documents/Programs and programming/R/costum_coord_round")
coord_round_2_km= function(coord){
  print(coord)
  a= as.numeric(substr(coord, 0, 2))*10000
  b= as.numeric(substr(coord, 3, 6))
  print(a)
  print(b)
  
  if(nchar(trunc(b))<4){ ### 0000-0999 = 1000
    print('b<1000')
    n=1000
  }
  else if (b %in% seq(1000,1999)){ ### 1000-1999 = 1000
    print('1000<b<1999 ')
    n=1000
  }
  else if(b %in% seq(2000,3999)){### 1000-1999 = 3000
    print('2000<b<3999 ')
    n=3000
  }
  else if(b %in% seq(4000,5999)){### 4000-5999 = 5000
    print('4000<b<5999 ')
    n=5000
  }
  else if(b %in% seq(6000,7999)){### 6000-7999 = 7000
    print('6000<b<7999 ')
    n=7000
  }
  else if(b %in% seq(8000,9999)){### 8000-9999 = 9000
    print('8000<b<9999 ')
    n=9000
  }
  print(n)
  new_coord=a+n
  print(new_coord)
  return(new_coord)
}

dataset <- read_excel(
  "C:/Users/michalsh/Documents/Programs and programming/R/costum_coord_round/data.xlsx")

dataset['round_ITM-LAT']=NA
dataset['round_ITM-LONG']=NA
colnames(dataset)=c('ID', 'ITM-LONG', 'ITM-LAT', 'round_ITM-LONG', 'round_ITM-LAT')

for (row_num in seq(1,length(rownames(dataset)))){
  print(row_num)
  print(dataset$ID[row_num])
  
  itm_long=dataset$`ITM-LONG`[row_num]
  round_itm_long= coord_round_2_km(itm_long)
  dataset$`round_ITM-LONG`[row_num]=round_itm_long
  
  itm_lat=dataset$`ITM-LAT`[row_num]
  round_itm_lat= coord_round_2_km(itm_lat)
  dataset$`round_ITM-LAT`[row_num]=round_itm_lat
} 



write_xlsx(dataset,path = 'rounded_coords_2km.xlsx')
