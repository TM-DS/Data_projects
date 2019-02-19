
datas_dirr <- "    " # Mettre le chemin au dossier datas dcontenu dans partie3.2-3.3
#Pointer vers le dossier datas
setwd(datas_dirr)

library(caret)
library(ranger)
library(dplyr)
library(plyr)
library(randomForest)
library(randomForestExplainer)
library(xgboost)
library(e1071)
library(dendextend)
library(TSclust)
library(TSdist)
library(dtwclust)
library(plotly)
library(plsgenomics)
library(FactoMineR)
library(factoextra)
library(clue)
library(gridExtra)
library(onehot)
library(readxl)
library(data.table)
library(corrplot)
library(corrr)
library(ggplot2)
library(grid)
#corelation
#chi 2 
#anova + test studien 2 a 2 comparaison var quantitative a qualitative
#studient 
#Matrice de confusion
#Courbe ROC

#Fonctions
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
          plot(t(as.matrix(ploting[1,])), col=colo[j],xlim=c(0,8),ylim=c(0,1.2*max(data_initiale)),ylab= paste("Cluster",j,sep=" "))#,type='l'
        }else{
          lines(as.vector(as.matrix(ploting[i,])),col=colo[j])
        }}
    }
    
    
    
    
  }
  
}
Classes_prediction <- function(datas_sel){
  require(rpart.plot)
  for(j in 1:6 ){
    datas <- datas_sel[,c(1:24,(25+j))]
    trctrl <- trainControl(method = "repeatedcv", number = 30, repeats = 5)
    
    dtree_fit <- train(datas[,1:24],datas[,25], method = "rpart",
                       
                       trControl=trctrl,
                       tuneLength = 20)
    
    prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2,main=paste("Decision_Tree_of_",colnames(datas_sel)[25+j],"_Prediction=",as.character(dtree_fit$bestTune),sep = ""),cex.main = 0.7)
    rf <- randomForest(datas[,1:24],datas[,25],data=datas)
    
    plot(rf, main= paste(colnames(datas_sel)[25+j],"  _ Prediction = ",as.character(1-mean(rf$err.rate[nrow(rf$err.rate),]))),cex.main=0.7)
    print(rf)
    print(j)
    
  }
}
Rename_factors <- function(Nom_dataframe){
  RPUTB_raw <- get(Nom_dataframe)
  for (i in 1:nrow(RPUTB_raw)){
    if(RPUTB_raw$Indication[i]== "1"){
      
      RPUTB_raw$Indication[i] <- "Retension_vesicale"
      
    }
    if(RPUTB_raw$Indication[i]== "2"){
      
      RPUTB_raw$Indication[i] <- "Echec_du_traitement"
      
    }
    if(RPUTB_raw$Indication[i]== "3"){
      
      RPUTB_raw$Indication[i] <- "Retension_vesicale_chronique"
      
    }
    if(RPUTB_raw$Indication[i]== "4"){
      
      RPUTB_raw$Indication[i] <- "Insuffisance_renale"
      
    }
    if(RPUTB_raw$Indication[i]== "5"){
      
      RPUTB_raw$Indication[i] <- "Lithiase_Vesicale"
      
    }
    if(RPUTB_raw$Indication[i]== "6"){
      
      RPUTB_raw$Indication[i] <- "Diverticule"
      
    }
    if(RPUTB_raw$Indication[i]== "7"){
      
      RPUTB_raw$Indication[i] <- "Hematurie"
      
    }
    if(RPUTB_raw$Indication[i]== "8"){
      
      RPUTB_raw$Indication[i] <- "Infection"
      
    }
    print(i)
    if(RPUTB_raw$Anesthesie[i] == 1){
      RPUTB_raw$Anesthesie[i] <- "Anestesie-locoregionale"
    }
    if(RPUTB_raw$Anesthesie[i] == 2){
      RPUTB_raw$Anesthesie[i] <- "Anestesie-generale"
    }
    if(RPUTB_raw$Technique[i] == 1){
      RPUTB_raw$Technique[i] <- "RPUTB"
    }
    if(RPUTB_raw$Technique[i] == 2){
      RPUTB_raw$Technique[i] <- "VBBP"
    }
    if(RPUTB_raw$Technique[i] == 3){
      RPUTB_raw$Technique[i] <- "VAPOR"
    }
    if(RPUTB_raw$Evenement_HD[i] == 0){
      RPUTB_raw$Evenement_HD[i] <- "Event_Aucun"
    }
    if(RPUTB_raw$Evenement_HD[i] == 1){
      RPUTB_raw$Evenement_HD[i] <- "Hypotention"
    }
    if(RPUTB_raw$Evenement_HD[i] == 2){
      RPUTB_raw$Evenement_HD[i] <- "Bracardie"
    }
    if(RPUTB_raw$Evenement_HD[i] == 3){
      RPUTB_raw$Evenement_HD[i] <- "Malaise_Vagual"
    }
    print(i+1)
  }
  
  
  assign(paste(Nom_dataframe,"_to_oneHot",sep=""), RPUTB_raw,envir = .GlobalEnv)
}
KMeans_clustering <- function(X,n,title_graph){
  means <-0
  cnter <- 2
  datas <- data.frame()
  
  for (i in 2:n){
    means <-kmeans(get(X),i,nstart = 100)
    vari <- means$tot.withinss/means$totss
    datas[cnter-1,1] <- cnter
    datas[cnter-1,2] <- vari
    cnter <- cnter +1
  } 
  colnames(datas)<- c("Nbre_clust","Varintra/Vartot")
  assign(paste("My_data"),datas,envir = .GlobalEnv)
  par(mfrow=c(1,1))
  plot(My_data, type = "l")
  title(main = paste("Kmean Clustering with 100 starts"))
  
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
Time_series_clustering <- function(datas,choice_dist,hclust_method,n_max_pam,cutree_threhold,nom_figures,Manual_PAM_cluster = 0){
  
  
  par(mfrow=c(1,1))
  working_matrix <- as.matrix(datas)
  D1 <- diss(working_matrix,choice_dist)
  fviz_dist(D1)
  matrix.heatmap(D1)
  hc.rows <- hclust(D1, method = hclust_method)
  plot(hc.rows)
  hc.row.cut <- cutree(hc.rows, k =cutree_threhold)
  
  rect.hclust(hc.rows,k=cutree_threhold,border = 2:6)
  abline(h=1.3,col='red')
  
  avg_dend_obj <- as.dendrogram(hc.rows)
  avg_col_dend <- color_branches(avg_dend_obj, k=cutree_threhold)
  plot(avg_col_dend,main=paste("Dendogramme",cutree_threhold,"clusters de",nom_figures,sep=" "))
  
  colo <- c("antiquewhite4","chartreuse4","cyan4","gold4","gray")
  
  assign(paste(colnames(datas)[6],"Cut","with","HCLUST","dist",choice_dist,sep="_"),hc.row.cut,envir = .GlobalEnv)
  par(mfrow= c(1,cutree_threhold))
  for(j in 1:cutree_threhold){
    ploting <-datas[which(hc.row.cut == j),]  
    
    
    for (i in 1:nrow(ploting)){
      if(i == 1){
        plot(t(as.matrix(ploting[1,])), col=colo[j],xlim=c(0,8),ylim=c(0,1.2*max(datas)),ylab= paste("Cluster",j,sep=" "))#,type='l'
      }else{
        lines(as.vector(as.matrix(ploting[i,])),col=colo[j])
      }}
  }
  
  PAM_optim(D1,n_max_pam,ajout_tempo = 1,data_initiale = datas,nclust_PAM = Manual_PAM_cluster,distance = choice_dist)
  Plot_variables(Var_to_test =colnames(datas)[6],typeofdist = choice_dist )
  
  
  
}
Plot_variables <- function(Var_to_test,method,typeofdist){
  Aggregated_datas$Hclust <- get(paste(Var_to_test,"Cut_with_PAM_dist",typeofdist,sep = "_"))
  Aggregated_datas$PAM <- get(paste(Var_to_test,"Cut_with_HCLUST_dist",typeofdist,sep = "_"))
  Nom_variables_a_tester <- c("Age","Volume_prostatique","Duree_Traitement","Temps_OP") #Selection des variables a tester
  for (i in 1:length(Nom_variables_a_tester)){
    
    q1 <- ggplot(Aggregated_datas,aes(Hclust,get(Nom_variables_a_tester[i])))+geom_boxplot(aes(group=Hclust),outlier.alpha = 0.3)  + geom_jitter(width = 0.1, aes(color=Technique)) +ggtitle(paste(length(unique(Aggregated_datas$Hclust)),"clusters en HCLUST",sep = " "))+ xlab("Clusters")+ylab(Nom_variables_a_tester[i])+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    q2 <- ggplot(Aggregated_datas,aes(PAM,get(Nom_variables_a_tester[i])))+geom_boxplot(aes(group=PAM),outlier.alpha = 0.3)  + geom_jitter(width = 0.1,aes(color=Technique)) +ggtitle(paste(length(unique(Aggregated_datas$PAM)),"clusters en PAM",sep = " "))+ xlab("Clusters")+ylab(Nom_variables_a_tester[i])+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    grid.arrange(q1,q2,ncol=2)
  }
}
Number_of_predictor_optim <- function(datas,predict_max=ncol(datas)-2,n_tree =1500,split_ratio = 0.70, mtry_choosen = 10){
  oob.err=double(predict_max)
  test.err=double(predict_max)
  inTraining <- createDataPartition(datas[,1], p = .75, list = FALSE)
  
  for(mtry in 1:predict_max) {
    rf=randomForest(as.formula(paste(colnames(datas)[ncol(datas)],"~.")) , data = datas , subset = inTraining,mtry=mtry,ntree=n_tree) 
    oob.err[mtry] = rf$mse[n_tree] #Error of all Trees fitted
    
    pred<-predict(rf,datas[-inTraining,]) #Predictions on Test Set for each Tree
    test.err[mtry]= sum((pred-datas[-inTraining,ncol(datas)])**2)/length(pred) #Mean Squared Test Error
    
    cat(mtry," ") #printing the output to the console
    
  }
  #Trace les graphiques d'analyse des random forest
  matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
  legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
  
  results <<-randomForest(as.formula(paste(colnames(datas)[ncol(datas)],"~.")) , data = datas , subset = inTraining,mtry=mtry_choosen,ntree=n_tree,localImp = TRUE)
  
  
  par(mfrow=c(1,1))
  
}
Benchmark_modeles <- function(dataset){
  require(skimr)
  inTraining <- createDataPartition(dataset[,ncol(dataset)], p = .75, list = FALSE) #Training rows
  training_set <- dataset[inTraining,]     #Splited set
  test_set <- dataset[-inTraining,]       
  x= training_set[,1:(ncol(training_set)-1)]
  y= training_set[,ncol(training_set)]
  skimmed <- skim_to_wide(training_set)
  control <- trainControl(method="repeatedcv", number=10, repeats=2,p=0.7,verboseIter= T,savePredictions="final")
  
  model1 <- train(as.formula(paste(colnames(training_set)[ncol(training_set)],"~.")),data=training_set, method="ranger" ,trControl=control) #trControl=control
  prediction1 <- predict(model1,test_set)
  cbind(prediction1,test_set[,ncol(test_set)])
  Rsq_Random_Forest <-cor(test_set[,ncol(test_set)],prediction1)**2
  RMSE_Random_Forest <-RMSE(test_set[,ncol(test_set)],prediction1)
  ######### SVM MODEL #############################
  model3 <- train(as.formula(paste(colnames(training_set)[ncol(training_set)],"~.")),data=training_set, method="svmRadial", trControl=control, preProc = c("center", "scale"))
  prediction3 <- predict(model3,test_set)
  cbind(prediction3,test_set[,ncol(test_set)])
  RMSE_SVM <-RMSE(test_set[,ncol(test_set)],prediction3)
  Rsq_SVM <-cor(prediction3,test_set[,ncol(test_set)])**2
  
  #########PLots and results ######################
  results <- resamples(list(Random_Forest=model1, SVM=model3))
  PLotting_Rsquared <<- cbind(Rsq_SVM,Rsq_Random_Forest)
  PLotting_RMSE <<- cbind(RMSE_SVM,RMSE_Random_Forest)
  
  
  par(mfrow=c(1,1))
  Number_of_predictor_optim(dataset,mtry_choosen = 10)
  
  par(mfrow=c(1,2))
  
  namesto <- c("SVM","Random_Forest")
  barplot(PLotting_Rsquared,ylim = c(0,1),main = paste("Rsq on",colnames(training_set)[ncol(training_set)]),cex.axis=0.6, cex.names=1,names.arg =namesto,col = "steelblue" )
  barplot(PLotting_RMSE,ylim = c(0,1.1*max(PLotting_RMSE)),main = paste("RMSE on",colnames(training_set)[ncol(training_set)]),cex.axis=0.6, cex.names=1,names.arg =namesto,col = "steelblue" )
  
  
  
  
  
  
  
  
}


##### Donnes initiales
RPUTB_raw <-read_xlsx("ADENOME-PROST.xlsx", sheet = 2,skip = 1)
RPUTB_raw <- RPUTB_raw[,-1]
RPUTB_raw <- RPUTB_raw[1:36,]
VAPOR_raw <-read_xlsx("ADENOME-PROST.xlsx", sheet = 1,skip = 1)
VAPOR_raw <-VAPOR_raw[,-1]
VBBPS_raw <-read_xlsx("ADENOME-PROST.xlsx", sheet = 3,skip = 1)
VBBPS_raw <- VBBPS_raw[,-1]
noms_colonnes <- c("Age","Comorbidite","Duree_Traitement","Porteur_de_sonde","IPSS_PO","QoL_PO","QMax_PO","PSA","Volume_prostatique","Residu_post_mictionnel","Indication","Anesthesie","Evenement_HD","Technique","Transfusion_pero","Temps_OP","Volume_resseque","Delais_Ablation_jour","Caillotage","Reprise_au_bloc","IPSS_1","QoL_1","Qmax_1","IPSS_3","QoL_3","Qmax_3","IPSS_6","QoL_6","Qmax_6","IPSS_9","QoL_9","Qmax_9","IPSS_12","QoL_12","Qmax_12","IPSS_15","QoL_15","Qmax_15","IPSS_18","QoL_18","Qmax_18")
colnames(VAPOR_raw) <-noms_colonnes
colnames(RPUTB_raw)<-noms_colonnes
colnames(VBBPS_raw)<-noms_colonnes
liste_var <- c("RPUTB_raw","VAPOR_raw","VBBPS_raw")
VAPOR_raw$Technique <- as.character(VAPOR_raw$Technique)
RPUTB_raw$Technique <- as.character(RPUTB_raw$Technique)
VBBPS_raw$Technique <- as.character(VBBPS_raw$Technique)
VAPOR_raw$Indication <- as.character(VAPOR_raw$Indication)
RPUTB_raw$Indication <- as.character(RPUTB_raw$Indication)
VBBPS_raw$Indication <- as.character(VBBPS_raw$Indication)

Rename_factors(liste_var[1])
Rename_factors(liste_var[2])
Rename_factors(liste_var[3])


Aggregated_datas <- rbind(RPUTB_raw_to_oneHot,VAPOR_raw_to_oneHot,VBBPS_raw_to_oneHot,deparse.level = 1) 
Aggregated_datas$Indication <- as.factor(Aggregated_datas$Indication)
Aggregated_datas$Anesthesie <- as.factor(Aggregated_datas$Anesthesie)
Aggregated_datas$Technique <- as.factor(Aggregated_datas$Technique)
Aggregated_datas$Evenement_HD <- as.factor(Aggregated_datas$Evenement_HD)


Aggregated_datas$QMax_PO <- as.numeric(Aggregated_datas$QMax_PO)
Aggregated_ENCODED <- onehot(Aggregated_datas)
Aggregated_ENCODED <- predict(Aggregated_ENCODED,Aggregated_datas,stringsAsFactors=TRUE)
Aggregated_ENCODED <- as.data.frame(Aggregated_ENCODED)








#Selection buisness
first_select <- Aggregated_datas[ ,c(5,6,7,21:41)]
first_select$Method <-as.character(  Aggregated_datas$Technique)
IPSS_time <- first_select[,c(1,4,7,10,13,16,19,22,25)]
QoL_time <- first_select[,c(2,5,8,11,14,17,20,23,25)]
Qmax_time <- first_select[c(3,6,9,12,15,18,21,24,25)]


IPSS_Vapor <- subset(IPSS_time, IPSS_time$Method == "VAPOR")[,-ncol(IPSS_time)]
IPSS_Other <- subset(IPSS_time, IPSS_time$Method != "VAPOR")[,-ncol(IPSS_time)]
QoL_Vapor <- subset(QoL_time, QoL_time$Method == "VAPOR")[,-ncol(QoL_time)]
QoL_Other <- subset(QoL_time, QoL_time$Method != "VAPOR")[,-ncol(QoL_time)]
Qmax_Vapor <- subset(Qmax_time, Qmax_time$Method == "VAPOR")[,-ncol(Qmax_time)]
Qmax_Other <- subset(Qmax_time, Qmax_time$Method != "VAPOR")[,-ncol(Qmax_time)]



Time_series_clustering(QoL_time[,-9],choice_dist = "EUCL",hclust_method = "ward.D2",n_max_pam = 8,cutree_threhold = 3,nom_figures = "QoL")
Time_series_clustering(QoL_time[,-9],choice_dist = "COR",hclust_method = "complete",n_max_pam = 8,cutree_threhold = 4,nom_figures = "QoL")
Qmax_time$QMax_PO <- as.numeric(Qmax_time$QMax_PO)
Time_series_clustering(Qmax_time[,-9],choice_dist = "EUCL",hclust_method = "ward.D2",n_max_pam = 5,cutree_threhold = 2,nom_figures = "Qmax",Manual_PAM_cluster = 2)
Qmax_time$QMax_PO <- as.numeric(Qmax_time$QMax_PO)
Time_series_clustering(Qmax_time[,-9],choice_dist = "COR",hclust_method = "complete",n_max_pam = 5,cutree_threhold = 3,nom_figures = "Qmax")
Time_series_clustering(IPSS_time[,-9],choice_dist = "EUCL",hclust_method = "ward.D2",n_max_pam = 5,cutree_threhold = 4,nom_figures = "IPSS")
Time_series_clustering(IPSS_time[,-9],choice_dist = "COR",hclust_method = "complete",n_max_pam = 10,cutree_threhold = 4,nom_figures = "IPSS",Manual_PAM_cluster = 6)











rm(list = ls()[-c(1,2)])
Reg_datas <- Aggregated_ENCODED[,c(1:29,42:44)]
Reg_datas <- Reg_datas[,c(-10,-21,-22,-23)]
colnames(Reg_datas)<- gsub("=","_",colnames(Reg_datas))
colnames(Reg_datas)<- gsub("-","_",colnames(Reg_datas))

Reg_main_IPSS <- Reg_datas[,-c(22,ncol(Reg_datas)-1,ncol(Reg_datas))]
Reg_main_QoL <- Reg_datas[,-c(22,ncol(Reg_datas)-2,ncol(Reg_datas))]
Reg_main_Qmax <- Reg_datas[,-c(22,ncol(Reg_datas)-2,ncol(Reg_datas)-1)]





Benchmark_modeles(Reg_main_Qmax)
min_depth_frame <- min_depth_distribution(results)
plot_min_depth_distribution(min_depth_frame)
importance_frame <- measure_importance(results)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")  




#Predictions sur Qol

Reg_main_QoL <- Reg_main_QoL[,-12]
Benchmark_modeles(Reg_main_QoL)
min_depth_frame <- min_depth_distribution(results)
plot_min_depth_distribution(min_depth_frame)
importance_frame <- measure_importance(results)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")  



#Predictions sur IPSS

Reg_main_IPSS <- Reg_main_IPSS[,-12]
Benchmark_modeles(Reg_main_IPSS)
min_depth_frame <- min_depth_distribution(results)
plot_min_depth_distribution(min_depth_frame)
importance_frame <- measure_importance(results)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes") 











##Variables pour la partie 3.3
var_clust <- select_vars(ls(),contains("12_Cut_With_HCLUST"))#Select variables containing clusters
datas_for_clust <- Aggregated_ENCODED[,1:29]
datas_for_clust <- datas_for_clust[,-c(10,21,22,23,26)]
datas_for_clust <- as.data.frame(cbind(datas_for_clust,Aggregated_datas$Technique))
colnames(datas_for_clust)[ncol(datas_for_clust)]<- "Technique"
colnames(datas_for_clust)<- gsub("=","_",colnames(datas_for_clust))
colnames(datas_for_clust)<- gsub("-","_",colnames(datas_for_clust))
for (i in 1:length(var_clust)){
  datas_for_clust <-as.data.frame(cbind(datas_for_clust,get(var_clust[i])))
  colnames(datas_for_clust)[ncol(datas_for_clust)]<- var_clust[i]
  
}
Rows_Vapor <- which(datas_for_clust$Technique == "VAPOR") #Lignes de la technique Vapor
VAPOR_main_cluster <- datas_for_clust[Rows_Vapor,]
VAPOR_main_cluster[,26]<- as.factor(VAPOR_main_cluster[,26])
VAPOR_main_cluster[,27]<- as.factor(VAPOR_main_cluster[,27])
VAPOR_main_cluster[,28]<- as.factor(VAPOR_main_cluster[,28])
VAPOR_main_cluster[,29]<- as.factor(VAPOR_main_cluster[,29])
VAPOR_main_cluster[,30]<- as.factor(VAPOR_main_cluster[,30])
VAPOR_main_cluster[,31]<- as.factor(VAPOR_main_cluster[,31])

RPUTB_VBBPS_main_cluster <- datas_for_clust[-Rows_Vapor,]
RPUTB_VBBPS_main_cluster[,26]<- as.factor(RPUTB_VBBPS_main_cluster[,26])
RPUTB_VBBPS_main_cluster[,27]<- as.factor(RPUTB_VBBPS_main_cluster[,27])
RPUTB_VBBPS_main_cluster[,28]<- as.factor(RPUTB_VBBPS_main_cluster[,28])
RPUTB_VBBPS_main_cluster[,29]<- as.factor(RPUTB_VBBPS_main_cluster[,29])
RPUTB_VBBPS_main_cluster[,30]<- as.factor(RPUTB_VBBPS_main_cluster[,30])
RPUTB_VBBPS_main_cluster[,31]<- as.factor(RPUTB_VBBPS_main_cluster[,31])



#Prediction des classes
Classes_prediction(VAPOR_main_cluster)

Classes_prediction(RPUTB_VBBPS_main_cluster)


