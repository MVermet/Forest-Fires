library(dplyr)
library(stringr)
library(ggplot2)

feux = read.csv("./data/feux_2010_2014.csv", header=TRUE, sep=";")

vec_na = which(is.na(feux$Prec))
feux <- feux[-vec_na,]
vec_na = which(is.na(feux$Tmoy))
feux <- feux[-vec_na,]

code_dep <- c("05","04","06","83","84","13","2A","2B","30","26","07","48","34","11","66")
Dep <- c("Hautes Alpes","Haute Provence","Alpes Maritimes","Var","Vaucluse","Bouches du Rhônes",
                   "Corse Sud","Haute Corse",
                   "Gard","Drôme","Ardèche", "Lozère",
                   "Hérault","Aude","Pyrénées Orientales")

print(Dep)
feux$dep <- NULL
feux$CP = str_c("0",feux$CP)
feux$CP = str_sub(feux$CP,-5)
for (i in 1:length(feux$CP))
{
  feux$dep[i] = Dep[which(code_dep == str_sub(feux$CP[i],1,2))]
}
head(feux$dep,100)
statDep = aggregate(feux$SURFACE/10000.0, by=list(feux$dep), FUN="mean")
statDep =statDep[order(statDep[,2]), ]
statDep
ggplot(data=statDep, aes(x=Group.1, y=x))+geom_bar(stat="identity",color="black",fill="blue")+coord_flip()+ylab("Surface brûlée moyenne (en hectares)")

