library(rvest) # librairie contenant des outils de webscrapping
library(ggmap) # librairie contenant des fonctions de visualisation de données spatiales et de calculs de positions et d'itinéraires
library(RgoogleMaps) # affichage de fonds de carte issus de googlemap 

# URL où se trouve la liste complète des aéroports dont on veut extraire les données météorologiques
list.url <- "http://www.wunderground.com/history/index.html?error=AMBIGUOUS&query=France&day=17&month=8&year=2015&MR=1"
# Liste des balises contenant les liens vers chacun des aéroports
node     <- "#history-station-search-row li a"

# Capture des liens vers les pages des aéroports
airports.list  <- read_html(list.url) %>% html_nodes(node) %>% html_attr("href")
# Capture du texte associé au lien (nom de la ville où se situe l'aéroport)
airports.names <- read_html(list.url) %>% html_nodes(node) %>% html_text

# On ne retient dans cette liste que les liens vers les aéroports de la zone Prométhée ou proches de la zone Prométhée et possédant un historique météorologique. Chaque aéroport est en fait défini par la ville à laquelle il est rattaché
aeroports_hist <- airports.names[c(4,5,14,33,35,45,49,70,73,75,78,80,95,106,109,121,128,129,132,142,154,161,167,168,170,176)]
# On remplace l'aéroport d'Embrun (inexistant après vérification) par celui de Levaldigi
aeroports_hist[8] = "Levaldigi, Italy"
aeroports_hist

latlong <- geocode(aeroports_hist)
latlong_Levaldigi <- geocode(aeroports_hist[8])

# Affichage des aéroports du quart sud-est métropolitain
France <- GetMap(center=c(44, 4.5), zoom=7,maptype="terrain")
# Affichage sur ce fond de carte de chaque aéroport sous forme de points rouge (ou bleu pour Levaldigi)
PlotOnStaticMap(France, lat = as.numeric(latlong$latitude), lon = as.numeric(latlong$longitude),cex = 2, pch = 19, col = "red", add = FALSE)
PlotOnStaticMap(France, lat = as.numeric(latlong_Levaldigi$latitude), lon = as.numeric(latlong_Levaldigi$longitude),cex = 2, pch = 19, col = "blue", add = TRUE)
# On importe le fichier shape définissant les frontières des départements français
shp<-importShapefile("./data/departements-20140306-100m.shp",readDBF=FALSE)

# Affichage sur le fond de carte des frontières sous forme de lignes noires
PlotPolysOnStaticMap(France,shp,lwd=1., col=rgb(0,0,0,0), add = T)

# Affichage des aéroports présents sur le territoire Corse
France <- GetMap(center=c(42.5, 6.9), zoom=7,maptype="terrain")
PlotOnStaticMap (France, lat = as.numeric(latlong$latitude), lon = as.numeric(latlong$longitude),cex = 2, pch = 19, col = "red", add = FALSE)
