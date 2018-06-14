library(dplyr)
library(lubridate)
library(stringr)
library(reshape2)
library(factoextra)

feux = read.csv("./data/feux_2010_2014.csv", header=TRUE, sep=";")

# Parsing de la date au format DATE de R
feux$DATE_HEURE = parse_date_time(feux$DATE,'%d/%m/%y %h:%m')
DATE_JOUR <- NULL
# On enlève l'heure de la date
for (i in 1:length(feux$ANNEE))
{
  DATE_JOUR[i] = as.character(str_split(feux$DATE[i], " ")[[1]][1],orders="dmy")
}
feux$DATE_JOUR = DATE_JOUR
# Extraction du mois à partir de la date
month = month(feux$DATE)
feux$month = month

# Extraction du code postal et du département associé aux feux
feux$CP = str_c("0",feux$CP)
feux$CP = str_sub(feux$CP,-5)
feux$dep = NULL
code_dep = c("83","2A","26","30","84","04","06","11","2B","13","07","48","05","66","34")
no_dep = c(1:15)
for (i in 1:length(feux$CP))
{
  feux$dep[i] = no_dep[which(code_dep == str_sub(feux$CP[i],1,2))]
}

feux_sav = feux

# On agrège le nombre d'incendies sur les départements et les 12 mois de l'année 
feuxMois = aggregate(feux_sav$SURFACE/10000.0, by=feux_sav[c("month","dep")], FUN="length")

# On convertit le data frame obtenu en tableau avec en ligne les départements et en colonnes les 12 mois de l'année
tabMoisDep = acast(feuxMois,dep~month)
# Cas où aucun incendie n'a eu lieu pour un département donné durant un mois donné
tabMoisDep[is.na(tabMoisDep)] = 0
# On somme le nombre d'incendie pour chaque département
somme = apply(tabMoisDep,1,sum)
# On normalise chaque ligne par le nombre total d'incendie sur cette ligne pour obtenir des pourcentages par mois
for (i in 1:15){
  for (j in 1:12){
    tabMoisDep[i,j] = tabMoisDep[i,j]*100.0 / somme[i]
  }
}

# Nommage des lignes et colonnes
colnames(tabMoisDep) = c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre")
rownames(tabMoisDep) = code_dep

# Calcul de l'analyse en composantes principales
res.pca = prcomp(tabMoisDep, scale. = TRUE)
summary(res.pca)
# Affichage de la variance expliquée par chacun des 12 axes factoriels obtenus
# Les 2 premiers axes factoriels expliquent 64% de la variance totale
plot(res.pca, main = "explained variance")

# Clustering ascendant hiérarchique sur le tableau de données
# Distance : norme euclidienne standard 
# Ultramétrique : distance de Ward
res.hclust = hclust(dist(tabMoisDep, method = "euclidean")^2, method = "ward.D")
# Affichage du dendrogramme
plot(res.hclust)
rect.hclust(res.hclust,2)

# Affichage du bi-plot associé aux deux axes factoriels de l'ACP
# Les départements sont affichés avec 2 couleurs différentes qui correspondent aux 2 groupes
# résultant de la coupure en 2 du dendrogramme 
fviz_pca_biplot(res.pca, col.ind = cutree(res.hclust,2), repel = TRUE)

# On constitue finalement 3 grands groupes de département résultant de l'ACP et du clustering hiérarchique : Corse, départements littoraux et départements de l'intérieur
vec_med = which(feux$dep %in% c(1,4,5,8,10,14,15))
feux_littoraux <- feux_sav[-vec_med,]
vec_corse = which(feux$dep %in% c(2,9))
feux_corse <- feux_sav[-vec_corse,]
vec_montagne = which(feux$dep %in% c(3,6,7,11,12,13))
feux_montagne <- feux_sav[-vec_montagne,]

# On compte, pour chaque département, le nombre total d'incendies pour chaque mois
feuxMoisLitt = aggregate(feux_littoraux$aeroport, by=feux_littoraux[c("month")], FUN="length")
feuxMoisMontagne = aggregate(feux_montagne$aeroport, by=feux_montagne[c("month")], FUN="length")
feuxMoisCorse = aggregate(feux_corse$aeroport, by=feux_corse[c("month")], FUN="length")

# Affichage sur le même graphe du nombre d'incendies par mois pour chaque département littoral de la zone Prométhée
feuxVar = feuxMois[which(feuxMois$dep == 1),]
plot(feuxVar$month, feuxVar$x, pch = 21, col = "blue", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral", ylim=c(0,230))
par(new=TRUE)
feuxGard = feuxMois[which(feuxMois$dep == 4),]
plot(feuxGard$month, feuxGard$x, pch = 21, col = "green", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
feuxVaucluse = feuxMois[which(feuxMois$dep == 5),]
par(new=TRUE)
plot(feuxVaucluse$month, feuxVaucluse$x, pch = 21, col = "cyan", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
feuxAude = feuxMois[which(feuxMois$dep == 8),]
par(new=TRUE)
plot(feuxAude$month, feuxAude$x, pch = 21, col = "red", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
feuxBouches = feuxMois[which(feuxMois$dep == 10),]
par(new=TRUE)
plot(feuxBouches$month, feuxBouches$x, pch = 21, col = "purple", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
feuxPyr = feuxMois[which(feuxMois$dep == 14),]
par(new=TRUE)
plot(feuxPyr$month, feuxPyr$x, pch = 21, col = "yellow", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
feuxHerault = feuxMois[which(feuxMois$dep == 15),]
par(new=TRUE)
plot(feuxHerault$month, feuxHerault$x, pch = 21, col = "black", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Littoral",ylim=c(0,230))
legend(x=9.3, y=240, legend=c("Bouches du Rhône", "Hérault", "Aude", "Var", "Gard", "Pyrénées Orientales", "Vaucluse"), col=c("purple", "black", "red", "blue", "green", "yellow", "cyan"), lty = 1,cex=0.8)

# Affichage sur le même graphe du nombre d'incendies par mois pour chaque département de l'intérieur de la zone Prométhée
feuxDrome = feuxMois[which(feuxMois$dep == 3),]
plot(feuxDrome$month, feuxDrome$x, pch = 21, col = "cyan", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne", ylim=c(0,100))
par(new=TRUE)
feuxHP = feuxMois[which(feuxMois$dep == 6),]
plot(feuxHP$month, feuxHP$x, pch = 21, col = "green", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne",ylim=c(0,100))
feuxAM = feuxMois[which(feuxMois$dep == 7),]
par(new=TRUE)
plot(feuxAM$month, feuxAM$x, pch = 21, col = "blue", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne",ylim=c(0,100))
feuxArdeche = feuxMois[which(feuxMois$dep == 11),]
par(new=TRUE)
plot(feuxArdeche$month, feuxArdeche$x, pch = 21, col = "red", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne",ylim=c(0,100))
feuxLozere = feuxMois[which(feuxMois$dep == 12),]
par(new=TRUE)
plot(feuxLozere$month, feuxLozere$x, pch = 12, col = "purple", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne",ylim=c(0,100))
feuxHA = feuxMois[which(feuxMois$dep == 13),]
par(new=TRUE)
plot(feuxHA$month, feuxHA$x, pch = 13, col = "yellow", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Montagne",ylim=c(0,100))
legend(x=9, y=100, legend=c("Lozère", "Ardèche", "Alpes Maritimes", "Alpes Haute Provence", "Hautes Alpes", "Drôme"), col=c("purple", "red", "blue", "green", "yellow", "cyan"), lty = 1,cex=0.8)

# Affichage sur le même graphe du nombre d'incendies survenu en Corse pour chaque mois
feuxCorseA = feuxMois[which(feuxMois$dep == 2),]
feuxCorseB = feuxMois[which(feuxMois$dep == 9),]
feuxCorseA$x = feuxCorseA$x + feuxCorseB$x
plot(feuxCorseA$month, feuxCorseA$x, pch = 21, col = "blue", xlab = "Mois", ylab = "Nombre d'incendies", xlim=c(1,12), type="l", main="Corse",ylim=c(0,450))

