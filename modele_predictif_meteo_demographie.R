library(dplyr) 
library(ROCR) 
library(lubridate) 
library(stringr) 
library(randomForest)  
library(kernlab) 

# Lecture du fichier contenant l'ensemble des donn�es li�es aux incendies (cf. annexe D2 sur la pr�paration des donn�es) : m�t�o, donn�es li�es aux incendies (date, d�lai et dur�e d'intervention, surface br�l�e, type de for�t, d�mographie,...) 
feux=read.csv(file= "feux_2002_2014_meteo_demographie.csv", na="") 

# On ne garde que les lignes o� le type v�g�tation est renseign� 
vec_na = which(is.na(feux$TYPE_FORET)) 
feux <- feux[-vec_na,] 

# On cr�e  6 cat�gories de population  
feux$rural =feux$Population 
feux$rural[feux$Population <= 100] = 1 
feux$rural[feux$Population>100 & feux$Population <= 500] = 2 
feux$rural[feux$Population>500 & feux$Population<=1500] = 3 
feux$rural[feux$Population>1500& feux$Population<=3000]=4 
feux$rural[feux$Population>3000& feux$Population<=6000]=5 
feux$rural[feux$Population > 6000 ] = 6 
# On r�alise l'analyse de variance par rapport � la d�mographie et � la v�g�tation 
res.aov = aov(feux$SURFACE~feux$rural) 
summary(res.aov) 
res.aov = aov(feux$SURFACE~feux$TYPE_FORET) 
summary(res.aov) 


# Affichage des box-plot  
boxplot(feux$SURFACE/10000.0~feux$TYPE_FORET,col="red",ylim=c(0, 30) , names = c("Maquis","Taillis","Fut. Feuillus", "Fut. r�sineux", "Fut. mixtes","Reboisement")) 
boxplot(feux$SURFACE/10000.0~feux$rural,col="red",ylim=c(0,20), names=c("Cat�gorie 1", "Cat�gorie 2", "Cat�gorie 3", "Cat�gorie 4", "Cat�gorie 5", "Cat�gorie 6")) 
# Variable � pr�dire : Y = 1 si surface br�l�e > 5 hectares et Y = -1 sinon 
Y = feux$SURFACE 
Y[feux$SURFACE > 50000] = 1 
Y[feux$SURFACE <= 50000] = -1 
feux$Y = Y 

# On initialise le g�n�rateur de nombres al�atoires (indispensable pour les random forest) 
set.seed(123) 

# Trac� de l'histogramme indiquant la r�partition du nombre d'incendies en fonction du d�lai d'intervention 
hist(as.integer(feux$DELAI), breaks = c(0:120), xlab = "D�lai d'intervention (en minutes)", ylab = "Fr�quence") 

# On cr�e des variables de type FACTEUR pour les variables qualitatives telles que le d�partement, le type de for�t, le nombre d'habitants. n�cessaire pour les random forest 
feux$Yb = as.factor(feux$Y) 
feux$dep1 = as.factor(feux$dep) 
feux$TYPE_FORET1 = as.factor(feux$TYPE_FORET) 
feux$rural1 = as.factor(feux$rural) 

# S�lection des variables intervenant dans le mod�le : T�C, Vent, Humidit�, pr�cipitations, d�partement, d�mographie, superficie de la commune, v�g�tation 
feux2 <- select(feux, Tmoy, Vmoy, Prec, Yb, rural1, Superficie_Commune, dep1, TYPE_FORET1) 

# Application de l'algorithme � la dataframe r�sultante 
fit <- randomForest(formula = Yb ~ .,  # On pr�dit la variable Yb � partir de l'ensemble des pr�dicteurs retenus ci-dessus 
                    data = feux2,  # data frame contenant les pr�dicteurs 
                    na.action = na.roughfix,  # On remplace les valeurs non d�termin�es d'un pr�dicteur par sa valeur moyenne  
                    ntree= 5000, # Nombre d'arbres al�atoires 
                    mtry = 3) # Nombre de pr�dicteurs retenus pour chaque arbre (par d�faut sqrt(n), n �tant le nombre total de pr�dicteurs) 

# Visualisation de la matrice de confusion 
print(fit) 
# Visualisation de l'importance des diff�rentes variables 
varImpPlot(fit, labels=c("T moyenne", "Vitesse vent moyenne", "Pr�cipitations", "Population", "Superficie commune",  "D�partement", "Type de for�t")) 
# Trac� du taux d'erreur en fonction du nombre d'arbres 
plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB") 

# Passage aux SVM avec les m�mes donn�es d'entr�e 
# On coupe les donn�es en donn�es d'entra�nement et en donn�es de validation 
feux_train<-filter(feux,ANNEE%in%c(2002,2003,2004,2005,2006,2007,2008,2009,2010)) 
feux_test <- filter(feux, ANNEE %in% c(2013,2014,2011,2012)) 
# On transforme pour chacun de ces 2 �chantillons les variables qualitatives en variables binaires : indispensable pour les SVM avec variables qualitatives 
rural_train = acm.disjonctif(data.frame(feux_train$rural)) 
foret_train = acm.disjonctif(data.frame(feux_train$TYPE_FORET)) 
rural_test = acm.disjonctif(data.frame(feux_test$rural)) 
foret_test = acm.disjonctif(data.frame(feux_test$TYPE_FORET)) 
dep_train = acm.disjonctif(data.frame(feux_train$dep)) 
dep_test = acm.disjonctif(data.frame(feux_test$dep)) 
# On regroupe pour les 2 �chantillons (entra�nement et validation) ces variables binaires avec les donn�es quantitatives intervenant dans le mod�le, notamment la m�t�o 
feux_train2=data.frame(dep_train,rural_train,foret_train,dep_train,feux_train$Tmoy,feux_train$Hmoy,feux_train$Vmoy,feux_train$SUPERFICIE_COMMUNE) 
feux_test2=data.frame(dep_test,rural_test,foret_test,dep_test,feux_test$Tmoy,feux_test$Hmoy,feux_test$Vmoy,feux_test$SUPERFICIE_COMMUNE) 
Y_train <- select(feux_train, Y) 
Y_test <- select(feux_test, Y) 
# On fait varier C et sigma entre 0 et 50, comme pr�c�demment 
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
# On r�cup�re et affiche les indices du tableau donnant le couple (C??? ,??) optimal 
re = which(RESULTS == min(RESULTS), arr.ind=TRUE) 
re 
# On retient le mod�le associ� au couple (C??? ,??) optimal 
resfinal = ksvm(as.matrix(feux_train2), Y_train, kernel = "rbfdot", 
                type = "C-svc", 
                kpar = list(sigma = sigma[37]), 
                C = C[71], 
                cross=7) 
# Matrice de confusion obtenues � partir des pr�visions du mod�le appliqu� aux donn�es de validation 
Yhat = predict(resfinal, feux_test2, type = "response") 
CONF = table(as.matrix(Y_test), Yhat) 
CONF