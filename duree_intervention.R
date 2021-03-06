library(stringr)
library(dplyr)
library(lubridate)

# Lecture des donn�es concernant la dur�e d'intervention fournies par le r�seau Prom�th�e
feux = read.csv("./data/Donnees_SDIS_1996-2015_Duree_intervention.csv", header=TRUE, sep=";")

names(feux)[4]="DEBUT"
names(feux)[5]="FIN"

# Conversion des dates de d�but et de fin d'incendie au format correspondant aux donn�es fournies
feux$date_debut = parse_date_time(feux$DEBUT,'%d/%m/%y %h:%M')
feux$date_fin = parse_date_time(feux$FIN,'%d/%m/%y %h:%M')

# Calcul de la dur�e en jours de chaque incendie
duree <- NULL
for (i in 1:length(feux$DEBUT))
{
  duree[i] = difftime(feux$date_fin[i] , feux$date_debut[i], units="days")
}
# On arrondit la dur�e en jour � la valeur enti�re inf�rieure
feux$duree = round(duree)
head(feux$duree)

# On calcule puis on affiche la surface moyenne de for�t br�l�e pour les incendies durant entre 0 et 1 jour, entre 1 et 2 jours, entre 2 et 3 jours, etc.
vec1 = which(feux$duree == 0.0)
vec2 = which(feux$duree == 1.0)
vec3 = which(feux$duree == 2.0)
vec4 = which(feux$duree == 3.0)
vec5 = which(feux$duree == 4.0)
vec6 = which(feux$duree == 5.0)
vec7 = which(feux$duree == 6.0)

mean1 <- mean(feux[vec1,]$Surface.parcourue..m2.)/10000
mean2 <- mean(feux[vec2,]$Surface.parcourue..m2.)/10000
mean3 <- mean(feux[vec3,]$Surface.parcourue..m2.)/10000
mean4 <- mean(feux[vec4,]$Surface.parcourue..m2.)/10000
mean5 <- mean(feux[vec5,]$Surface.parcourue..m2.)/10000
mean6 <- mean(feux[vec6,]$Surface.parcourue..m2.)/10000
mean7 <- mean(feux[vec7,]$Surface.parcourue..m2.)/10000
mean1
mean2
mean3
mean4
mean5
mean6
