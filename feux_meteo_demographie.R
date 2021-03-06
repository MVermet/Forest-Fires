library(stringr) # Manipulation des cha�nes de caract�res. 
library(dplyr)   # Manipulation simple et rapide des donn�es dans les dataframes notamment 
library(rvest)  # librairie contenant des outils de webscrapping 
library(ggmap) # librairie contenant des fonctions de visualisation de donn�es spatiales et de          calculs de positions et d'itin�raires 
library(NISTunits) # librairie fournissant les valeurs des constantes physiques fondamentales et permettant aussi de convertir des unit�s entre elles. 
Library(lubridate) # librairie permettant la manipulation des dates 

# Fonction qui calcule la distance entre l'a�roport et l'incendie selon la norme WGS84. La formule est donn�e dans le chapitre 5.1 
# LatAero et longAero d�signent la latitude et la longitude de l'a�roport ; latInc et longInc d�signent la latitude et la longitude de l'incendie 
distance <- function(latAero, longAero, latInc, longInc) 
{ 
  # Conversion degr�s->radian des coordonn�es g�ographiques 
  latAero = NISTdegTOradian(latAero) 
  longAero = NISTdegTOradian(longAero) 
  latInc = NISTdegTOradian(latInc) 
  longInc = NISTdegTOradian(longInc) 
  
  Na <- 6378.0^2 / sqrt(6378.0^2*cos(latAero)^2 + 6356.0^2*sin(latAero)) 
  Ni <- 6378.0^2 / sqrt(6378.0^2*cos(latInc)^2 + 6356.0^2*sin(latInc)) 
  Xa = Na * cos(latAero) * cos(longAero) 
  Ya = Na * cos(latAero) * sin(longAero) 
  Za = (6356.0^2/6378.0^2) * Na * sin(latAero) 
  
  Xi = Ni * cos(latInc) * cos(longInc) 
  Yi = Ni * cos(latInc) * sin(longInc) 
  Zi = (6356.0^2/6378.0^2) * Na * sin(latInc) 
  
  d = sqrt((Xi - Xa)^2 + (Yi - Ya)^2 + (Zi - Za)^2) 
  return (d) 
} 


# La function getValue retourne, pour un nom de variable (temp�rature, vitesse du vent,.) dont la balise HTML est pr�sente dans la page, la valeur num�rique qui lui est associ�e. 
# La variable html d�signe le code HTML de la page et td le nom de la variable (� temperature �, � precipitation �,.). 
getValue <- function(html,td){ 
  indice = -1 
  # On recherche la position de la balise contenant le nom de la variable dans le code HTML. 
  indice = which(html==td) 
  # La balise "Precipitation" appara�t deux fois dans les pages. 
  if ((length(indice) >= 0) && (td=="Precipitation")){ 
    indice = indice[2] 
  } 
  chaine = "" 
  if ((length(indice) > 0) && (indice >= 0)){ 
    # Lorsque la balise a �t� trouv�e dans la page, on r�cup�re la cha�ne de caract�res situ�e dans la balise suivante qui correspond alors � la valeur num�rique associ�e � la variable.  
    # Rappelons que les donn�es sont r�pertori�es dans un tableau, deux balises cons�cutives correspondent donc � deux cases cons�cutives d'une m�me ligne. 
    chaine = html[indice+1] 
    # on �limine les espaces contenus dans la cha�ne. 
    chaine = str_trim(chaine) 
    ind = str_length(chaine) 
    # On �limine les caract�res associ�s � l'unit� de la variable 
    if (grepl("Temperature",td) !=0)     { 
      ind = str_locate(chaine,"�C")     } 
    if (grepl("Precipitation",td) !=0)     { 
      ind = str_locate(chaine,"mm")     } 
    if (grepl("Wind",td) !=0)     { 
      ind = str_locate(chaine,"km/h")     }  
    if (grepl("Humidity",td) == 0) { 
      chaine = str_sub(chaine,1,ind[1]-1)     } 
    chaine = str_trim(chaine) 
  } 
  # Enfin, on convertit la cha�ne de caract�re r�sultante en variable num�rique 
  value = as.numeric(chaine) 
  return (value) 
} 

# URL o� se trouve la liste compl�te des a�roports dont on veut extraire les donn�es m�t�orologiques (cf. annexe A) 
list.url<-"http://www.wunderground.com/history/index.html?error=AMBIGUOUS&query=France&day=17&month=8&year=2015&MR=1" 
# Liste des balises contenant les liens vers chacun des a�roports 
node <- "#history-station-search-row li a" 
# Capture des liens vers les pages des a�roports 
airports.list  <- read_html(list.url) %>% html_nodes(node) %>% html_attr("href") 
airports.names <- read_html(list.url) %>% html_nodes(node) %>% html_text 
# On retient parmi la liste des noms d'a�roports obtenus ceux qui sont pr�sent dans la zone couverte par le r�seau Prom�th�e et dont l'historique m�t�o est disponible sur le site. 
airports_list<-airports.names[c(4,5,23,29,50,58,60,64,90,93,96,109,111,117,120,129,130,134,135, 138, 147,159,164,168,171)] 
# On ajoute l'a�roport de Levaldigi (cf. 2.2) 
airports_list[length(airports_list)+1] = "Levaldigi, Italy" 
# On calcule pour chaque a�roport ses coordonn�es g�ographiques (latitude, longitude) � partir du nom de la ville sur laquelle ils sont situ�s 
latlong <- geocode(airports_list) 
aeroports = as.data.frame(airports_list) 
aeroports$long = latlong$lon 
aeroports$lat = latlong$lat 
# On associe � chaque a�roport de la liste la cha�ne de caract�re qui le caract�rise dans l'URL associ� � son historique (cf. 5.1) 
airports.code<-c("LFKJ","LFCI","LFLW","LFKB","LFKC","LFKF","LFMK","LFBC","LFLS","LFTH","LFMI", "LFMC","LFHP","LFLY","LFML","LFLQ","LFMT","LFMN","LFME","LFMO","LFMP","LFCR","LFMH","LF MY","LFKS","LIMZ","LFBO") 
aeroports$code = airports.code 


# On r�cup�re la liste des incendies de 2002 � 2014. 
feux_2002_2014=read.csv("./data/feux_2002_2014.csv", header=FALSE, sep=";") 
# On conserve les colonnes suivantes : ann�e, code postal, heure et date de l'alerte incendie, surface br�l�e au d�but et � la fin de l'intervention, carreau DFCI, type de for�t, distance des habitations, heure et date de d�but d'intervention, heure et date de fin d'intervention 
feux = feux_2002_2014 
# On convertit les dates au format date de R 
feux$DATE_HEURE = parse_date_time(feux$DATE,'%d/%m/%y %h:%M') 
feux$DEB_INT = parse_date_time(feux$DEB_INT,'%d/%m/%y %h:%M') 
feux$FIN_INT = parse_date_time(feux$FIN_INT,'%d/%m/%y %h:%M') 
# On calcule le d�lai entre l'alerte et le d�but de l'intervention (en minutes) 
feux$DELAI = round(difftime(feux$DEB_INT , feux$DATE_HEURE, units="mins")) 
# On calcule la dur�e de l'intervention (en minutes) 
feux$DUREE = round(difftime(feux$FIN_INT , feux$DATE_HEURE, units="mins")) 

# On rajoute un '0' devant les codes INSEE � 4 chiffres ; ceci concerne les d�partements dont le num�ro INSEE est inf�rieur � 10 (Alpes Maritimes (06), Alpes de Haute Provence (04),.). 
# Cette modification est indispensable pour g�ocoder l'incendie. 
feux$CP = str_c("0",feux$CP) 
feux$CP = str_sub(feux$CP,-5) 

# Cr�ation de la variable d�partement 
feux$dep = NULL 
code_dep = c("83","2A","26","30","84","04","06","11","2B","13","07","48","05","66","34") 
no_dep = c(1:15) 
for (i in 1:length(feux$CP)) 
{ 
  feux$dep[i] = no_dep[which(code_dep == str_sub(feux$CP[i],1,2))] 
} 

# Ouverture du fichier contenant l'ensemble des informations d�mographiques et g�ographiques des communes de France : population, superficie et altitude (obtenu sur le site officiel de l'INSEE) 
donnees_Communes=read.csv("./data/communes.csv", header=TRUE, sep=";") 
donnees_Communes$Population = donnees_Communes$Population * 1000.0 
donnees_Communes$Superficie = donnees_Communes$Superficie * 0.01 
donnees_Communes$densite = donnees_Communes$Population / donnees_Communes$Superficie 
donnees_Communes$Code = str_c("0",donnees_Communes$Code) 
donnees_Communes$Code = str_sub(donnees_Communes$Code,-5) 
# On rattache � chaque feu les donn�es de la commune sur laquelle il est apparu 
Densite = NULL 
Population = NULL 
Superficie = NULL 
for (i in 1:length(feux$DATE_HEURE)) 
{ 

    Densite[i] = floor(donnees_Communes$densite[which(donnees_Communes$Code == feux$CP[i])]) 
    Population[i] = donnees_Communes$Population[which(donnees_Communes$Code == feux$CP[i])] 
    Superficie[i] = donnees_Communes$Superficie[which(donnees_Communes$Code == feux$CP[i])] 
} 
feux$densitePopulation = Densite 
feux$Population = Population 
feux$Superficie_Commune = Superficie 

# Pour chaque incendie, nous recherchons maintenant l'a�roport le plus proche. On utilise pour cela la fonction distance d�finie plus haut ; on calcule la distance entre l'incendie et chaque a�roport de la liste et on retient l'a�roport dont la distance � l'incendie est la plus faible. 
aeroportFeux <- NULL 
code <- NULL 
for (i in 1:length(feux$LAT)) 
{ 
  d = 1000000; 
  for (j in 1:length(aeroports$lat)) 
  { 
    dist = distance(aeroports$lat[j], aeroports$long[j], feux$latitude[i], feux$longitude[i]) 
    if (dist < d) 
    { 
      d = dist 
      aeroportFeux[i] = as.character(aeroports$airports_list[j]) 
      code[i] = as.character(aeroports$code[j]) 
    } 
  } 
} 

# On ajoute � la data frame FEUX le code de l'a�roport identifiant l'URL de son historique ainsi que le nom de l'a�roport. 
feux$code = code 
feux$aeroport = aeroportFeux 
# Initialisation des tableaux contenant les donn�es m�t�o de chaque incendie (Temp�rature moyenne du jour de l'alerte incendie, temp�rature max, taux de pr�cipitation, taux d'humidit� moyen, taux d'humidit� max, vitesse du vent moyenne et vitesse du vent max) 
Tmoy <- NULL 
Tmax <- NULL 
Prec <- NULL 
Hmoy <- NULL 
Hmax <- NULL 
Vmoy <- NULL 
Vmax <- NULL 
url_base = "https://www.wunderground.com/history/airport" 
url_end = "DailyHistory.html" 
# Boucle sur l'ensemble des incendies dans laquelle nous irons r�cup�rer pour chacun d'entre eux les donn�es m�t�o disponibles sur le site wunderground.com 
for (i in 1:length(feux$ANNEE)) { 
  url_complete = "" 
  # La date des incendies est �crite sous la forme JJ/MM/AAAA HH:MM:SS ; on �te donc de la cha�ne de caract�re la partie droite associ�e � l'heure de l'alerte incendie. On enl�ve ensuite les '/' de la partie gauche dans la mesure o� ils ne figurent pas dans l'URL des historiques du site wunderground 
  date = str_split(feux $DATE[i], " "); 
  date2 = str_split(date[[1]][1],"/"); 
  # On peut alors �crire l'URL compl�te fournissant les donn�es m�t�o de l'a�roport associ� � l'incendie au jour de l'alerte en concat�nant les �l�ments n�cessaires dans l'ordre suivant : code de l'a�roport, puis jour, mois et ann�e de l'incendie 
  url_complete=paste(url_base,feux$code[i],date2[[1]][3],date2[[1]][2],date2[[1]][1],url_end,sep="/"); 
  # On lit le code HTML de la page associ�e 
  page <- read_html(url_complete) 
  # On r�cup�re toutes les balises associ�es � une case de tableau (balise de type � td � en langage HTML) 
  temp <- html_nodes(page, xpath="//table//td") %>% html_text() 
  # On r�cup�re les donn�es m�t�o parmi ces balises � l'aide de la function getValue d�finie plus haut 
  Tmoy[i] = getValue(temp,"Mean Temperature") 
  Tmax[i] = getValue(temp,"Max Temperature") 
  Prec[i] = getValue(temp,"Precipitation") 
  Hmoy[i] = getValue(temp,"Average Humidity") 
  Hmax[i] = getValue(temp,"Maximum Humidity") 
  Vmoy[i] = getValue(temp,"Wind Speed") 
  Vmax[i] = getValue(temp,"Max Wind Speed") 
}

# On ajoute les colonnes ainsi obtenues � la data frame feux 
feux$Tmoy = Tmoy 
feux$Tmax = Tmax 
feux$Prec = Prec 
feux$Hmoy = Hmoy 
feux$Hmax = Hmax 
feux$Vmoy = Vmoy 
feux$Vmax = Vmax 

# Tri des donn�es 
# On �te les lignes dont la valeur du taux de pr�cipitation est ind�termin�e. 
vec_na = which(is.na(feux$Prec)) 
feux <- feux[-vec_na,] 
# On �te les lignes dont la valeur de la temp�rature moyenne est ind�termin�e. 
vec_na = which(is.na(feux$Tmoy)) 
feux <- feux[-vec_na,] 
# On �te les feux de for�t dont la surface br�l�e est inf�rieure � un hectare. 
vec_feu = which(feux$SURFACE <= 10000) 
feux <- feux[-vec_feu,] 
# On �te les feux apparaissant en dehors de la p�riode estivale. 
month = month(feux$DATE) 
feux$month = month 
vec_ete = which(feux$month %in% c(1:6,10:12)) 
feux <- feux[-vec_ete,] 
# Sauvegarde de la data frame feux avec les coordonn�es g�ographiques des incendies et des donn�es m�t�o du jour des alertes incendies 
write.csv(feux,file="C:/feux_2002_2014_meteo_demographie.csv",row.names = FALSE, na="") 