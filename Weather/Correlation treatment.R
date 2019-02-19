# Etude exploratoire donnees capteurs
# install.packages("smooth")
setwd("C:/Users/GnOu/Desktop/Birdz")
require(smooth)
require(dtw)
#raw_datas <- read.csv("Weather_datas_ts_sub.csv")
ma <- function(arr, n=20){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n):i])
  }
  res
}#Moving average function
invert_scale <- function(vect){
  vect <- -vect + max(vect)
  return(vect)
} # in order to corelate reverse NER 






raw_datas <- read.csv("EXTERNAL_TS.csv")
raw_datas$NER[which(is.na(raw_datas$NER))] <- 0




raw_test <- raw_datas
#raw_test <- raw_datas[with(raw_datas,grepl("^A532214",SARG_ISAADDRESS)),]
nrow(raw_test)/730
raw_test$date <- gsub("UTC","",raw_test$date)
raw_test$date <- substr(raw_test$date,1,nchar(raw_test$date)-9)


#####SELECT 200 CAPTEURS ###########

raw_test <- raw_test[which(raw_test$SARG_ISAADDRESS %in% unique(raw_test$SARG_ISAADDRESS)[as.vector(sample(1:length(unique(raw_test$SARG_ISAADDRESS)),200))]),]







rm("raw_datas")

for (i in 1:nrow(raw_test)){
  if (raw_test$demijournee[i]== "matin"){
    raw_test$date[i] <- paste(raw_test$date[i]," 01:00:00",sep = "")
    
  }
  if (raw_test$demijournee[i]== "aprem"){
    raw_test$date[i] <- paste(raw_test$date[i]," 14:00:00",sep="")
    
  }
  
 print(i) 
}

raw_test$date <- as.POSIXct(raw_test$date,tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S")     

test_ordering <- raw_test[order(raw_test$date),]
test_ordering$date <- as.character(test_ordering$date)
toto_sample <- test_ordering


df <- data.frame(matrix(ncol = length(unique(toto_sample$SARG_ISAADDRESS)), nrow = length(unique(toto_sample$date))))
row.names(df)<- unique(toto_sample$date)
colnames(df)<- unique(toto_sample$SARG_ISAADDRESS)

require(data.table)
row_list <- as.list(transpose(toto_sample))

length(row_list)

mapping_function <- function(x){
  df[which(row.names(df)== x[[2]][1]),which(colnames(df)== x[[1]][1])] <<- x[[6]][1]
  
  
  
}

lapply(row_list,mapping_function)

df$date <- unique(toto_sample$date)
df$date <- as.POSIXct(df$date,tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S")
for (i in 1:200){
  df[,i]<- as.numeric(df[,i])
  
}


df$Nebulosite <- test_dataframe$Nebulosité.totale
df$Pluviometrie <- test_dataframe$Précipitations.dans.les.24.dernières.heures



# install.packages("imputeTS")
df$to_predict <- 

require(imputeTS)
df$Pluviometrie_fix <-na.kalman(df$Pluviometrie)
df$Hauteur_Nuages_fix <- na.kalman(test_dataframe$Hauteur.de.la.base.des.nuages.de.l.étage.inférieur)
df$Temperature_fix <- na.kalman(test_dataframe$Température)
df$Humidite_fix <- na.kalman(test_dataframe$Humidité)
df$Nebulosite_fix <- na.kalman(df$Nebulosite)



sapply(df, function(y) sum(length(which(is.na(y)))))



-plot(df$date,ma(df$Pluviometrie_fix,n=4), type = "l", ylim = c(0,0.8*max(df[,45])))
lines(df$date,ma(invert_scale(df[,75]),n=4),col="red")




df$count <- 1:730

capteur_list <- colnames(df)

choosen_capteur <- capteur_list[sample(1:length(capteur_list),1)]
capt_collumn <- which(colnames(df)== choosen_capteur)
sample_smooth <- loess(invert_scale(get(choosen_capteur))~as.numeric(date),data=df,span = 0.025)
df$to_predict<- predict(sample_smooth)
weather_smooth <- loess(df$Pluviometrie_fix~as.numeric(df$date),span = 0.025)
weather_smooth <- predict(weather_smooth)





plot(weather_smooth~df$date, type= "l", ylim=c(0,1.2*max(df$to_predict)),xlab="Date",ylab = "Signal degradation indicator")
legend("topleft", legend=c("Precipitations", "Performance"),col=c("black", "darkred"), lty=1:1, cex=0.8)
lines(df$to_predict~df$date,col="darkred")


i <- 1
test_regression <- lm(df$to_predict[1:650] ~ poly(ma(df$Humidite_fix[1:650],n=5),i)+poly( ma(df$Temperature_fix[1:650],n=5),i)+poly( ma(df$Pluviometrie_fix[1:650],n=5),i)+poly( ma(df$Nebulosite_fix[1:650],n=5),i))
df$regression <- predict(test_regression,data=df$to_predict)
summary(test_regression)

par(mfrow=c(1,1))


plot(df$to_predict,type="l")
lines(predict(test_regression,data=df$to_predict),col="chartreuse4")


ggplot(data = df) + geom_line(aes(x=date,y=to_predict),color='black') + 
  geom_line(aes(x=date,y=regression),color='chartreuse4') + 
  ylab('Values')+xlab('date')+ggtitle("Polynomial regression order 1")+theme_bw()

time_serie <- ts(df$to_predict,start = c(2018,1),frequency = 730)
time_serie
acf(time_serie)
library(forecast)

regressors <- cbind(df$Nebulosite_fix,df$Humidite_fix,df$Hauteur_Nuages_fix,df$Pluviometrie_fix,df$Temperature_fix)

fit <- auto.arima(time_serie,seasonal = T)

tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
fcast <- forecast(fit, h=30)
lot(fcast)



fit_regressors <- arima(time_serie[1:690],order = c(0,0,0),xreg = regressors[1:690,])
fit_regressors <- auto.arima(time_serie[1:690],xreg = regressors[1:690,],seasonal = F)
?auto.arima



fcast_regressors <- predict(fit_regressors,newxreg=regressors[691:730,])
fcast_regressors <- forecast(fit_regressors,xreg=regressors[691:730,])
plot(as.vector(time_serie[691:730]),col="red",type = "l")
lines(as.vector(fcast_regressors$pred))
plot(as.vector(predict(fit_regressors,newxreg=regressors[1:690,])[[1]]),type = "l",ylim = c(0.9*min(as.vector(time_serie)),1.1*max(as.vector(time_serie)) ) )
lines(as.vector(time_serie[1:690]),col="red")

predict(model_arima)


?arima
#plot results
par(mfrow=c(3,1))
plot(df[,capt_collumn]~df$date,type = "l",ylab = "NER",main = "RAW NER",xlab="")
plot(invert_scale(df[,capt_collumn])~ df$date,type = "l",ylab = "Inverted NER",main = "INVERTED NER",xlab="")
plot(invert_scale(df[,capt_collumn])~ df$date,type = "l",lty=2,col="gray",ylim = c(0,1.2*max(df[,capt_collumn])),ylab = "Inverted NER",main = "INVERTED NER + LOESS SMOOTHING",xlab="")
lines(df$to_predict~df$date,col="red")



smoothed_ner <- data.frame(matrix(ncol= 200,nrow=730))
for (i in 1:200){
  smoothed <- loess(invert_scale( df[,i])~as.numeric(df$date),span = 0.025)
  predicted <- predict(smoothed)
  smoothed_ner[,i]<- predicted
  
  
  
}


smoothed_ner <- transpose(smoothed_ner)

Time_series_clustering <- function(datas,choice_dist,hclust_method,n_max_pam,cutree_threhold,nom_figures,Manual_PAM_cluster = 0){
  require(TSclust)
  require(plsgenomics)
  require(factoextra)
  
  
  
  
  par(mfrow=c(1,1))
  working_matrix <- as.matrix(datas)
  D1 <- diss(working_matrix,choice_dist)
  #fviz_dist(D1)
  matrix.heatmap(D1)
  hc.rows <- hclust(D1, method = hclust_method)
  plot(hc.rows)
  hc.row.cut <- cutree(hc.rows, k =cutree_threhold)
  
  rect.hclust(hc.rows,k=cutree_threhold,border = 2:6)
  abline(h=1.3,col='red')
  
  #avg_dend_obj <- as.dendrogram(hc.rows)
  #avg_col_dend <- color_branches(avg_dend_obj, k=cutree_threhold)
  #plot(avg_col_dend,main=paste("Dendogramme",cutree_threhold,"clusters de",nom_figures,sep=" "))
  
  colo <- c("antiquewhite4","chartreuse4","cyan4","gold4","gray")
  
  assign(paste(colnames(datas)[6],"Cut","with","HCLUST","dist",choice_dist,sep="_"),hc.row.cut,envir = .GlobalEnv)
  par(mfrow= c(cutree_threhold,1))
  for(j in 1:cutree_threhold){
    ploting <-datas[which(hc.row.cut == j),]  
    
    
    for (i in 1:nrow(ploting)){
      if(i == 1){
        plot(t(as.matrix(ploting[1,])), col=colo[j],xlim=c(0,730),ylim=c(0,1.2*max(datas)),ylab= paste("Cluster",j,sep=" "))#,type='l'
      }else{
        lines(as.vector(as.matrix(ploting[i,])),col=colo[j])
      }}
  }
  PAM_optim(D1,n_max_pam,ajout_tempo = 1,data_initiale = datas,nclust_PAM = Manual_PAM_cluster,distance = choice_dist)
  
  
}
PAM_optim <- function(matrice,n_max, nclust_PAM = 0, ajout_tempo =0,data_initiale,distance){
  library(cluster)
  temp_datas <- data.frame(n_kluster = c(1),avg_width= c(0))
  for(i in 2:n_max){
    PAM_D1 <- pam(matrice, k = i) #,metric = "euclidian"
    temp_datas[i,1] <- i
    temp_datas[i,2]<- PAM_D1$silinfo$avg.width
  }
  
  plot(temp_datas$avg_width ~ temp_datas$n_kluster , type= "b", col= "darkgray" ,lwd = 1.5,xlab="Nombre de Cluster",ylab="Hauteur moyenne (figure silhouette)")
  title(main = "Figure d'optimisation de PAM")
  abline(v = temp_datas$n_kluster[which.max(temp_datas$avg_width)], col= "red", lwd=1)
  if(nclust_PAM !=0){
    abline(v=nclust_PAM, col= "green", lwd=1 )
  }
  if (nclust_PAM == 0){ 
    Choosen_PAM <<- pam(matrice, k =which.max(temp_datas$avg_width) )#,metric = "euclidian"
    plot(Choosen_PAM)
  }else{
    Choosen_PAM <<- pam(matrice, k =nclust_PAM )#,metric = "euclidian"
    plot(Choosen_PAM)  
    
  }
  if (ajout_tempo == 1){
    
    
    
    if(nclust_PAM != 0){Nclust_max <- nclust_PAM}else{Nclust_max <- which.max(temp_datas$avg_width)}
    par(mfrow=c(1,Nclust_max))
    
    colo <- c("antiquewhite4","chartreuse4","cyan4","gold4","gray")
    assign(paste(colnames(data_initiale)[6],"Cut","with","PAM","dist",distance,sep="_"),Choosen_PAM$clustering,envir = .GlobalEnv)
    
    
    
    for(j in 1:Nclust_max){
      ploting <-data_initiale[which(Choosen_PAM$clustering == j),]  
      
      
      for (i in 1:nrow(ploting)){
        if(i == 1){
          plot(t(as.matrix(ploting[1,])), col=colo[j],xlim=c(0,730),ylim=c(0,1.2*max(data_initiale)),ylab= paste("Cluster",j,sep=" "))#,type='l'
        }else{
          lines(as.vector(as.matrix(ploting[i,])),col=colo[j])
        }}
    }
    
    
    
    
  }
  
}


Time_series_clustering(smoothed_ner,choice_dist = "DTWARP",hclust_method = "ward.D2",n_max_pam = 10,cutree_threhold = 2,nom_figures = "LOESS distances")







x <- transpose(df)

Comparison_df <- test_dataframe[,c(2,4,5,6,8,14,7)]
Comparison_df <- as.matrix(Comparison_df)
for (i in 1:ncol(x)){
  Comparison_df <-  cbind(Comparison_df,x[,i])
  Comparison_df[,ncol(Comparison_df)]<- as.numeric(Comparison_df[,ncol(Comparison_df)])
  
}
Comparison_df <- as.data.frame(Comparison_df)

for (i in 8:54){
  Comparison_df[,i]<- as.numeric(Comparison_df[,i])
  
}
Comparison_df$Date <- as.POSIXct(Comparison_df$Date,tz = "Europe/Paris", format ="%Y-%m-%d %H:%M:%S")
Comparison_df <- Comparison_df[-which(is.na(Comparison_df$Date)),]
Comparison_df <- Comparison_df[-which(is.na(Comparison_df$Précipitations.dans.les.24.dernières.heures)),]
Comparison_df$Précipitations.dans.les.24.dernières.heures <- as.numeric(Comparison_df$Précipitations.dans.les.24.dernières.heures)
par(mfrow=c(1,1))



plot(Comparison_df$Date,ma(Comparison_df$Nebulosité.totale,n=6), type= "l")
lines(Comparison_df$Date,10*ma(Comparison_df$V28,n=6),type="l",col="red")



alignment<-dtw(Comparison_df$V10,Comparison_df$Précipitations.dans.les.24.dernières.heures,keep=TRUE);
plot(alignment,type="threeway")




plot(Comparison_df$Précipitations.dans.les.24.dernières.heures)

dist_matr <- t(cbind(Comparison_df$Précipitations.dans.les.24.dernières.heures,Comparison_df$V10))
library(TSclust)

diss(dist_matr,METHOD = "COR")




df$Pluviometrie_fix



######DTW#############
install.packages("dtw")
require(dtw)
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;

## A cosine is for template; sin and cos are offset by 25 samples
template<-cos(idx)

## Find the best match with the canonical recursion formula
library(dtw);
alignment<-dtw(df$Pluviometrie_fix,sample_predict,keep=TRUE);

## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")

## Align and plot with the Rabiner-Juang type VI-c unsmoothed recursion
plot(
  dtw(df$Pluviometrie_fix,sample_predict,keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2);

## See the recursion relation, as formula and diagram
rabinerJuangStepPattern(6,"c")
plot(rabinerJuangStepPattern(6,"c"))



range01 <- function(x){(x-min(x))/(max(x)-min(x))}

pluvio_sma <- ma(df$Pluviometrie_fix,n=4)
results <-data.frame(matrix(nrow = 200,ncol = 1))
for (i in 1:200){
  x <- smoothed_ner[i,]
dist_meteo <- diss(rbind(x,pluvio_sma),METHOD = "CORT")
results[i,] <-dist_meteo[1]
print(i)
}

# results_CORT_unscaled <- results

which(results < 0.25)
plot_smooth <- transpose(smoothed_ner)

as.ts()
ts.intersect(as.ts(pluvio_sma),as.ts(x))

########## CROSS CORRELATION ###########
correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[[graph1Id]]
    print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[[graph2Id]]
      if(graph1Id == graph2Id){
        break;
      } else {
        correlation = ccf(graph1, graph2, lag.max = 0)
        cross[graph1Id, graph2Id] = correlation$acf[1]
      }
    }
  }
  cross
}

corr = correlationTable(plot_smooth)

findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}

highCorr = which(corr > 0.4, arr.ind = TRUE)
match = findCorrelated(4, highCorr)
match # print 6 12 23 42 44 45  3



bound = function(graphs, orign, match) {
  graphOrign = graphs[[orign]]
  graphMatch = graphs[match]
  allValue = c(graphOrign)
  for(m in graphMatch){
    allValue = c(allValue, m)
  }
  c(min(allValue), max(allValue))
}

plotSimilar = function(graphs, orign, match){
  lim = bound(graphs, orign, match)
  
  graphOrign = graphs[[orign]]
  plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+25), lwd=3)
  title(paste("Similar to", orign, "(black bold)"))
  
  cols = c()
  names = c()
  for(i in 1:length(match)) {
    m = match[[i]]
    matchGraph = graphs[[m]]
    lines(x = 1:length(matchGraph), y=matchGraph, col=i)
    
    cols = c(cols, i)
    names = c(names, paste0(m))
  }
  legend("topright", names, col = cols, lty=c(1,1))
}

plotSimilar(plot_smooth, 4, match)

#######################################################
install.packages("marima")
library(marima)
?marima

plot(range01(plot_smooth[,2]),type = "l")


plot(plot_smooth[,which(results ==min(results))]~df$date,col="red",type="l")
lines(ma(df$Pluviometrie_fix,n=4)~df$date,type="l")



plot(test_dataframe$Précipitations.dans.les.24.dernières.heures,type="l",ylab="Precipitations (en mm)")
plot(df$Pluviometrie_fix,type= "l",ylab="Precipitations (en mm)")
plot(ma(df$Pluviometrie_fix,n=4),type= "l",ylab="Precipitations (en mm)",main = "SMA, h=3, Precipitations")
