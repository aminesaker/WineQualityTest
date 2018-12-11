#chargement des librairies necessaires

library(plyr)
library(scales)
library(randomForest)
library(rpart)
library(rpart.plot)

##chargements des fontions
source("./fonctionsVin.R")

##chargement des données CSV
vinRouge = read.csv("./winequality-red.csv", sep = ';', header = TRUE)

###################################
##Analyse descriptive des données##
###################################


##Nombre d'individus:
nrow(vinRouge) 
dim(vinRouge)

##le jeux de données vinRouge comporte 1599 entrées et 12 variables
 
##Nombre de variables 
head(vinRouge)
colnames(vinRouge)
##Il y a 11 variables explicatives
#la variable cible est "quality", 12éme colonne

##la valeur maximale, la valeur minimale et la moyenne de la colonne quality
summary(vinRouge$quality)


#######################################################
##Graphe de chaque variable explicative sur "quality"##
#######################################################


plot(vinRouge$fixed.acidity, vinRouge$quality)
plot(vinRouge$volatile.acidity, vinRouge$quality)
plot(vinRouge$citric.acid, vinRouge$quality)
plot(vinRouge$residual.sugar, vinRouge$quality)
plot(vinRouge$chlorides, vinRouge$quality)
plot(vinRouge$free.sulfur.dioxide, vinRouge$quality)
plot(vinRouge$total.sulfur.dioxide, vinRouge$quality)
plot(vinRouge$density, vinRouge$quality)
plot(vinRouge$pH, vinRouge$quality)
plot(vinRouge$sulphates, vinRouge$quality)
plot(vinRouge$alcohol, vinRouge$quality)



#######################
##Regression linéaire##
#######################

##Séparation des données en apprentissage et test

n = nrow(vinRouge)
n_app = round(0.75*n)
n_test = n - n_app
s =sample(n)
apprentissage = vinRouge[s[1:n_app],]
test = vinRouge[s[(n_app+1):n],]
dim(apprentissage)
dim(test)

######Modéles de prédiction à la variable prédictrice####


## D'aprés les différentes représentations graphiques, on peut voir que les variables
## qui semblent plus liées à la variable cible quality sont:
##alcohol, volatile.acidity, sulphates

##On peut appuyer ce résultat en calculant le coefficient de correlation 
##entre les differentes variables explicatives et la variable cible
cor(vinRouge[,1:12])

#########################################
##                     quality
##fixed.acidity         0.12405165
##volatile.acidity     -0.39055778
##citric.acid           0.22637251
##residual.sugar        0.01373164
##chlorides            -0.12890656
##free.sulfur.dioxide  -0.05065606
##total.sulfur.dioxide -0.18510029
##density              -0.17491923
##pH                   -0.05773139
##sulphates             0.25139708
##alcohol               0.47616632
##quality               1.00000000
#########################################

##Création d'un modéle de régression linéaire mod1 permettant de prédire la variable quality à partir de la variable alcohol (11éme colonne).
##Test de fisher
##Prédiction à partir du modéle mod1
##Méthode de validation croisé en utilisant la variable alcohol
##Méthode de séparation en utilisant la variable alcohol

##model1##
mod1 = lm(quality~alcohol, data =apprentissage)

##données Fisher de mod1
summary(mod1)$fstatistic

##quantile de fisher
qf(0.95, 1, 1197)

##On remarque summary(mod1)$fstatistic > qf(0.95, 1, 1597) soit 
##350.6616   > 3.849239, donc ce modèle est significativement utile pour prédire quality

p1 = predict(mod1, test)
p1
erreur_gen(test$quality, p1)
sqrt(erreur_gen(test$quality, p1))
hist(p1-test$quality)

vc1 = validation_croisee(vinRouge, 11, 12, 10)
vc1 
sp1 = separation(vinRouge, 11, 12, 10)
sp1 

##Création d'un modéle de régression linéaire mod2 permettant de prédire la variable quality à partir de la variable volatile.acidity(2éme colonne).
##Prédiction à partir du modéle mod2
##Méthode de validation croisé en utilisant la variable volatile.acidity
##Méthode de séparation en utilisant la variable volatile.acidity


##model2##
mod2 = lm(quality~volatile.acidity, data =apprentissage)



##données Fisher de mod2
summary(mod2)$fstatistic

##quantile de fisher
qf(0.95, 1, 1197)

##On remarque summary(mod2)$fstatistic > qf(0.95, 1, 1597) soit 
##211.9763  > 3.849239, donc ce modèle est significativement utile pour prédire quality

p2 = predict(mod2, test)
p2
erreur_gen(test$quality, p2)
hist(p2-test$quality)


vc2 = validation_croisee(vinRouge, 2, 12, 10)
vc2 
sp2 = separation(vinRouge, 2, 12, 10)
sp2

##Création d'un modéle de régression linéaire mod3 permettant de prédire la variable quality à partir de la variable sulphates(10éme colonne).
##Prédiction à partir du modéle mod3
##Méthode de validation croisé en utilisant la variable sulphates
##Méthode de séparation en utilisant la variable sulphates


##model3##
mod3 = lm(quality~sulphates, data =apprentissage)


##données Fisher de mod3
summary(mod3)$fstatistic

##quantile de fisher
qf(0.95, 1, 1197)

##On remarque summary(mod3)$fstatistic > qf(0.95, 1, 1597) soit 
##72.47549 > 3.849239, donc ce modèle est significativement utile pour prédire quality

p3 = predict(mod3, test)
p3
erreur_gen(test$quality, p3)


vc3 = validation_croisee(vinRouge, 10, 12, 10)
vc3
sp3 = separation(vinRouge, 10, 12, 1)
sp3


##Essayons avec la variable residual.sugar qui faiblement corrélée avec la variable quality
mod4 = lm(quality~residual.sugar, data =apprentissage)

##données Fisher de mod4
summary(mod4)$fstatistic

##quantile de fisher
qf(0.95, 1, 1197)

##On remarque summary(mod4)$fstatistic < qf(0.95, 1, 1597) soit 
## 0.5683555  < 3.849239, donc ce modèle n'est pas utile pour prédire quality


############################################################################
##Selection de variable avec les méthodes séparation et validation croisée##
############################################################################


##Selection ascendante avec la méthode de séparation


sel_asc_sep = selec_asc_sep(vinRouge,c(1:11),12,10)
sel_asc_sep 

####Selection ascendante avec la méthode de validation croisée


sel_asc_val = selec_asc_val(vinRouge,c(1:11),12,10)
sel_asc_val


##on voit que les résultats sont plus stables avec la méthodes de séparations avec validation croisée avec K=10
##les variables explicatives retournées sont plutot celle qui sont le plus corrélées avec la variable cible: quality

