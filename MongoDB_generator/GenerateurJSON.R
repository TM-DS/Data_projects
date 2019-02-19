
#INstallation du package jsonlite
#install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')


#Choix du repertoir de travail
where_save <- choose.dir()
setwd(where_save)


#Chargement de la librairie jsonlite
library(jsonlite)





#Fixe l'aleatoire afin de pouvoir generer des fichiers identiques
set.seed(123456)


#Fonction permettant de generer une liste d'enfants
generateur_enfant <- function( age){
  enfant_list <- list()
  if (age > 18){
    nbreenfants <- sample(0:3,1)
    for (i in 1:nbreenfants){
      MenorWOmen <-sample(0:1,1)
      if (MenorWOmen == 0){
        Prenom <-prenom_femme[sample(1:length(prenom_femme),1)]
        Sexe <- "F"
      }else{
        Prenom <-prenom_homme[sample(1:length(prenom_homme),1)]
        Sexe <-"M"}
      if (age<40){minage <- 1}else{minage <- age -40}
      enf_age <- sample(minage:(age-17),1)
      buff_list <- list(prenom = Prenom,sexe = Sexe, age =enf_age)
      enfant_list <- append(enfant_list, list(buff_list))    
    }
    assign("list_backup",enfant_list,envir = .GlobalEnv)
    return(enfant_list)
  }
}



#Fonction permettant de generer n individus 
Generateur_global <- function(nombre_personne){
  prenom_homme <<- c("Adam","Anthony","Charles","Alex","Alexis","Dylan","Jacob","Louis","Lucas","Logan","Olivier","Philippe","Nathan","Guillaume","Nicolas","Emilien","Thibault","Victor","Noah","Tristan","Victor","Simon","Hugo")
  prenom_femme <<- c("Alexia","Alice","Clara","Lara","Justine","Maude","Jade","Aude","Sarah","Olivia","Rosalie","Maya","Sofia","Arianne","Charlotte","Eve","Emma","Audrey","Rose","Laurie","Annabelle","Elizabeth","Gabrielle","Laurence","Marilou")
  ville <<- c("Paris","Marseille","Lyon","Toulouse","Nantes","Montpelier","Strasbourg","Bordeaux","Lille","Rennes","Reims","Le Havre","Saint-Etienne","Toulon","Grenoble","Dijon","Angers","Nimes","Saint-Denis","Le Mans","Brest")
  sports_pratiques <<- c("Football","Tennis","Equitation","Basketball","Gymnastique","Golf","Rugby","Tir","Judo","Petanque","Voile","Natation","Athletisme","Karate","Cyclisme")
  hobbies <<- c("Cuisine","Radio Amateur","Restauration d'ouvrages","Cosplay","Crochet","Peinture","Theatre","Fabrication de couteaux","Magie","Modelisme","Origami","Yoga","Chant","Jeux Video","Puzzles","Potterie","Taxidermie","Sculpture")
  film_favori <<- c("12 Hommes en colere","Harakiri","Barberousse","Le bon, la brute et le truand","Les sept Samourais","Il etait une fois l'Amerique","Le Parrain","Il etait une fois dans l'ouest","Le trou","Point limite","L'aurore","Le voyage de chihiro","Sherlock junior","Princesse Mononoke","Vol au dessus d'un nid de coucou","Pulp fiction")
  
  note_senscritique <<- c(8.2, 8.1, 7.6, 8.4,6.4,5.2,7.4,4.3,5.8,4.7,8.7,6.9,6.6,5.5,4.4, 9.2)
  appreciation_globale <<- c("TB","TB","B","TB","B","B","TB","AB","B","AB","TB","B","B","AB","AB","TB")
  classement_top111 <<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  
  
  habitant_ville <<-c(2000000,100000,800000,400000,250000,300000,15000,600000,300000,100000,180000,200000,170000,160000,80000,60000,50000,140000,110000,140000,140000)
  zip_code <<- c(75000,13000,69000,31000,44000,34000,67000,30072,59000,35000,51100,76600,42000,83000,38000,21000,49000,30000,93200,72000,29200)
  
  
  nb_adherent<<-c(2200000,1018721,687207,668312,300000,410000,457000,199000,295000,301762,316905,300451,2537000,119280,20000)
  pourcentage_femme<<-c(20,45,65,48,38,35,18,29,10,39,47,36,23,22,38)
  
  
  globallist <- list()
  globallist2 <- list()
  globallist3 <- list()
  globallist4 <- list()
  my_finaltext <- character()
  for (j in 1:nombre_personne) {
    MenorWOmen <-sample(0:1,1)
    if (MenorWOmen == 0){
      Prenom <-prenom_femme[sample(1:length(prenom_femme),1)]
      Sex <- "F"
    }else{
      Prenom <-prenom_homme[sample(1:length(prenom_homme),1)]
      Sex <- "M"}
    Age <- sample(18:77,1)
    nbsports <- sports_pratiques[sample(1:length(sports_pratiques),sample(1:3,1))]
    
    film_fav <- film_favori[sample(1:length(film_favori),1)]
    
    hobby <- hobbies[sample(1:length(hobbies),sample(1:2,1))]
    
    listedesenfants <- generateur_enfant(Age)
    Base_id <- "000000"
    ID <- paste(substr(Base_id,1,nchar(Base_id)-nchar(as.character(j))),as.character(j),sep = "")
    
    ville_naissance <-ville[sample(1:length(ville),1)]
    ville_actuelle <-ville[sample(1:length(ville),1)]
    
    
    mylist <- list(name=Prenom, caracteristiques = list(age=Age, sexe=Sex,lieu_de_naissance = ville_naissance,lieu_de_residence = ville_actuelle, sport=nbsports,film=film_fav,hobbie = hobby),children=listedesenfants)
    
    
    
   
    ##globallist <- append(globallist,mylist)
    globallist[[j]] <- mylist
    if (j <= length(film_favori)){
      mylist2 <- list( film=film_favori[j],caracteristiques = list(note = note_senscritique[j],appreciation = appreciation_globale[j],classement = classement_top111[j]))
      globallist2[[j]]<- mylist2
    }
    
    if (j <= length(ville)){
      mylist3 <- list(ville_fr=ville[j],nbr_habitant = habitant_ville[j], code_postal=zip_code[j])
      globallist3[[j]]<- mylist3
    }
    
    
    if (j <= length(sports_pratiques)){
      mylist4 <- list(sport=sports_pratiques[j],adherents = nb_adherent[j], pourentageF = pourcentage_femme[j])
      globallist4[[j]]<- mylist4
    }
    
    #my_text <-  toJSON(mylist, pretty=TRUE, auto_unbox=TRUE)
    #my_finaltext <- paste(my_finaltext,my_text,sep = "")
    print(paste("Avancee: ",as.character(j/nombre_personne),sep = ""))
    
  }
  assign("Collection_personne",globallist,envir = .GlobalEnv)
  assign("Collection_films",globallist2,envir = .GlobalEnv)
  assign("Collection_ville",globallist3,envir = .GlobalEnv)
  assign("Collection_sports",globallist4,envir = .GlobalEnv)
  
  #assign("Final_CHAR",my_finaltext,envir = .GlobalEnv)
}


##Genere 100 individus
Generateur_global(20000)


#Formate la variable (liste de liste) en format json
toto <- jsonlite::toJSON(Collection_personne, pretty=TRUE, auto_unbox=TRUE)
toto2 <- jsonlite::toJSON(Collection_films, pretty=TRUE, auto_unbox=TRUE)
toto3 <- jsonlite::toJSON(Collection_ville, pretty=TRUE, auto_unbox=TRUE)
toto4 <- jsonlite::toJSON(Collection_sports, pretty=TRUE, auto_unbox=TRUE)


#Affiche le json generer
toto
toto2
toto3
toto4


#Sauvegarde le json dans le dossier de travail sous le nom indique
write(toto,file = "personnes.json")
write(toto2,file = "films.json")
write(toto3,file = "villes.json")
write(toto4,file = "sport.json")





