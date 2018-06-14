library(dplyr) 
library(ROCR) 
library(lubridate) 
library(stringr) 
library(randomForest)  
library(kernlab) 

# Lecture du fichier contenant l'ensemble des données liées aux incendies (cf. annexe D2 sur la préparation des données) : météo, données liées aux incendies (date, délai et durée d'intervention, surface brûlée, type de forêt, démographie,...) 
feux=read.csv(file= "feux_2002_2014_meteo_demographie.csv", na="") 

# On ne garde que les lignes où le type végétation est renseigné 
vec_na = which(is.na(feux$TYPE_FORET)) 
feux <- feux[-vec_na,] 

# On crée  6 catégories de population  
feux$rural =feux$Population 
feux$rural[feux$Population <= 100] = 1 
feux$rural[feux$Population>100 & feux$Population <= 500] = 2 
feux$rural[feux$Population>500 & feux$Population<=1500] = 3 
feux$rural[feux$Population>1500& feux$Population<=3000]=4 
feux$rural[feux$Population>3000& feux$Population<=6000]=5 
feux$rural[feux$Population > 6000 ] = 6 
# On réalise l'analyse de variance par rapport à la démographie et à la végétation 
res.aov = aov(feux$SURFACE~feux$rural) 
summary(res.aov) 
res.aov = aov(feux$SURFACE~feux$TYPE_FORET) 
summary(res.aov) 


# Affichage des box-plot  
boxplot(feux$SURFACE/10000.0~feux$TYPE_FORET,col="red",ylim=c(0, 30) , names = c("Maquis","Taillis","Fut. Feuillus", "Fut. résineux", "Fut. mixtes","Reboisement")) 
boxplot(feux$SURFACE/10000.0~feux$rural,col="red",ylim=c(0,20), names=c("Catégorie 1", "Catégorie 2", "Catégorie 3", "Catégorie 4", "Catégorie 5", "Catégorie 6")) 
# Variable à prédire : Y = 1 si surface brûlée > 5 hectares et Y = -1 sinon 
Y = feux$SURFACE 
Y[feux$SURFACE > 50000] = 1 
Y[feux$SURFACE <= 50000] = -1 
feux$Y = Y 

# On initialise le générateur de nombres aléatoires (indispensable pour les random forest) 
set.seed(123) 

# Tracé de l'histogramme indiquant la répartition du nombre d'incendies en fonction du délai d'intervention 
hist(as.integer(feux$DELAI), breaks = c(0:120), xlab = "Délai d'intervention (en minutes)", ylab = "Fréquence") 

# On crée des variables de type FACTEUR pour les variables qualitatives telles que le département, le type de forêt, le nombre d'habitants. nécessaire pour les random forest 
feux$Yb = as.factor(feux$Y) 
feux$dep1 = as.factor(feux$dep) 
feux$TYPE_FORET1 = as.factor(feux$TYPE_FORET) 
feux$rural1 = as.factor(feux$rural) 

# Sélection des variables intervenant dans le modèle : T°C, Vent, Humidité, précipitations, département, démographie, superficie de la commune, végétation 
feux2 <- select(feux, Tmoy, Vmoy, Prec, Yb, rural1, Superficie_Commune, dep1, TYPE_FORET1) 

# Application de l'algorithme à la dataframe résultante 
fit <- randomForest(formula = Yb ~ .,  # On prédit la variable Yb à partir de l'ensemble des prédicteurs retenus ci-dessus 
                    data = feux2,  # data frame contenant les prédicteurs 
                    na.action = na.roughfix,  # On remplace les valeurs non déterminées d'un prédicteur par sa valeur moyenne  
                    ntree= 5000, # Nombre d'arbres aléatoires 
                    mtry = 3) # Nombre de prédicteurs retenus pour chaque arbre (par défaut sqrt(n), n étant le nombre total de prédicteurs) 

# Visualisation de la matrice de confusion 
print(fit) 
# Visualisation de l'importance des différentes variables 
varImpPlot(fit, labels=c("T moyenne", "Vitesse vent moyenne", "Précipitations", "Population", "Superficie commune",  "Département", "Type de forêt")) 
# Tracé du taux d'erreur en fonction du nombre d'arbres 
plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB") 

# Passage aux SVM avec les mêmes données d'entrée 
# On coupe les données en données d'entraînement et en données de validation 
feux_train<-filter(feux,ANNEE%in%c(2002,2003,2004,2005,2006,2007,2008,2009,2010)) 
feux_test <- filter(feux, ANNEE %in% c(2013,2014,2011,2012)) 
# On transforme pour chacun de ces 2 échantillons les variables qualitatives en variables binaires : indispensable pour les SVM avec variables qualitatives 
rural_train = acm.disjonctif(data.frame(feux_train$rural)) 
foret_train = acm.disjonctif(data.frame(feux_train$TYPE_FORET)) 
rural_test = acm.disjonctif(data.frame(feux_test$rural)) 
foret_test = acm.disjonctif(data.frame(feux_test$TYPE_FORET)) 
dep_train = acm.disjonctif(data.frame(feux_train$dep)) 
dep_test = acm.disjonctif(data.frame(feux_test$dep)) 
# On regroupe pour les 2 échantillons (entraînement et validation) ces variables binaires avec les données quantitatives intervenant dans le modèle, notamment la météo 
feux_train2=data.frame(dep_train,rural_train,foret_train,dep_train,feux_train$Tmoy,feux_train$Hmoy,feux_train$Vmoy,feux_train$SUPERFICIE_COMMUNE) 
feux_test2=data.frame(dep_test,rural_test,foret_test,dep_test,feux_test$Tmoy,feux_test$Hmoy,feux_test$Vmoy,feux_test$SUPERFICIE_COMMUNE) 
Y_train <- select(feux_train, Y) 
Y_test <- select(feux_test, Y) 
# On fait varier C et sigma entre 0 et 50, comme précédemment 
C = seq(0.01, 50, length = 100) 
sigma = seq(0.01, 50, length = 100) 
RESULTS = matrix(0, length(C), length(sigma)) 
for (i in 1 : length(C)){ 
  for (j in 1 : length(sigma)){ 
    res.svm = ksvm(as.matrix(feux_train2), Y_train,  
                   type = "C-svc", 
                   kernel = "rbfdot",
                   kpar = list(sigma = sigma[j]), 
                   cross = 7, 
                   C=C[i]) 
    RESULTS[i, j] = res.svm@cross 
  } 
} 
# Visualisation des taux d'erreurs 
pheatmap(RESULTS,  cluster_rows = FALSE,   cluster_cols = FALSE) 
# On récupère et affiche les indices du tableau donnant le couple (C??? ,??) optimal 
re = which(RESULTS == min(RESULTS), arr.ind=TRUE) 
re 
# On retient le modèle associé au couple (C??? ,??) optimal 
resfinal = ksvm(as.matrix(feux_train2), Y_train, kernel = "rbfdot", 
                type = "C-svc", 
                kpar = list(sigma = sigma[37]), 
                C = C[71], 
                cross=7) 
# Matrice de confusion obtenues à partir des prévisions du modèle appliqué aux données de validation 
Yhat = predict(resfinal, feux_test2, type = "response") 
CONF = table(as.matrix(Y_test), Yhat) 
CONF