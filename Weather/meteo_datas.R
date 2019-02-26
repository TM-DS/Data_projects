#Meteo Datas
#Fichier CSV recuperables sur OpenDataSoft
#https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/table/?sort=date&refine.nom=LYON-ST+EXUPERY

Halfday_encoding <-function(begining,ending){
  toto <- difftime(as.POSIXct(ending, tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S"),as.POSIXct(begining, tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S"))
  nday <- abs(round(as.numeric(toto)))
  test_dataframe <<- RAW_DATA_S[0,]
  Formated_time <<- c()
  for (i in 1:(2*nday)){
    temp_table <- RAW_DATA_S[as.numeric(RAW_DATA_S$Date)> ((i-1)*3600*12)+as.numeric(as.POSIXct(begining, tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S")) & as.numeric(RAW_DATA_S$Date)<= ((i)*3600*12)+as.numeric(as.POSIXct(begining, tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S")),]
    for (j in 1:ncol(temp_table)){
      
      if (is.numeric(temp_table[,j]) | is.integer(temp_table[,j])){
        
        test_dataframe[i,j] <<- round(mean(temp_table[,j],na.rm = T),digits = 2) 
      }
      else{test_dataframe[i,j] <<- temp_table[1,j]}
      
      
      
      
      
    }
    if (i%%2 == 0){dimi_jour <<- 2}
    if (i%%2 == 1){dimi_jour <<- 1}
    gne <<- print.Date(temp_table[2,2])
    
    Formated_time <<- c(Formated_time,as.character(paste(str_sub(gne, 1, str_length(gne)-9),dimi_jour,sep = " ")))
    print(as.character(paste(str_sub(gne, 1, str_length(gne)-9),dimi_jour,sep = " "))) 
    
  }
  test_dataframe <<- cbind(test_dataframe,Formated_time)  
}

############# LIBRARIES #########
# install.packages(c("rvest","tidyverse","rebus","lubridate"))   #Decommenter pour installer le package
library(stringr)
#################################

#### REPERTOIRE DE TRAVAIL #####################
setwd("C:/Users/...")    #Emplacement du dossier de travail
################################################


#### PRETRAITEMENT DES DONNEES ###################
RAW_DATA <- read.csv("Meteo_data.csv",sep = ";", encoding = "UTF-8") #Ouverture du fichier de donnees meteo
RAW_DATA$Date <- gsub("T", " ",RAW_DATA$Date)
RAW_DATA$Date <- gsub("02:00", "",RAW_DATA$Date)
RAW_DATA$Date <- gsub("\\+", "",RAW_DATA$Date)

RAW_DATA$Date <- as.POSIXct(RAW_DATA$Date, tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S") #Conversion de la date

Bornes <- c("2018-10-01 00:00:00","2018-11-01 00:00:00")
RAW_DATA_sorted <- RAW_DATA[order(RAW_DATA$Date),]
RAW_DATA_S <- RAW_DATA_sorted[,c(1,2,3,8,10,15,17,33,36,37,40,41,42,43,60:64)]    #Selection des colonnes de donnees d'interet
RAW_DATA_S$Température <- RAW_DATA_S$Température - 273.15                         #Conversion en celcius
RAW_DATA_S$Rafales.sur.une.période <- RAW_DATA_S$Rafales.sur.une.période * 3.6000 #Conversion de m/s a km/h de la vitesse du vent
RAW_DATA_S <- RAW_DATA_S[-which(is.na(RAW_DATA_S$Date)),]
################################################


#### Encryption demi-journee ###################
Bornes <- c("2018-01-01 00:00:00","2019-01-01 00:00:00")    #Definition des bornes d'interet
Halfday_encoding(Bornes[1],Bornes[2])                                #Fonction creant les donnees par demi-journee
################################################


#### REMOVE USELESS VARIABLES ##################
rm("RAW_DATA")
rm("RAW_DATA_sorted")
rm("RAW_DATA_S")
rm(list = c("Bornes","dimi_jour","Formated_time"))
rm("gne")
################################################

