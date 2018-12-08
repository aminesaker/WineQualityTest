#chargement des librairies necessaires

library(plyr)
library(scales)
library(randomForest)
library(rpart)
library(rpart.plot)

##chargements des fontions
source("./fonctionsVin.R")

##chargement des donn�es CSV
vinBlanc = read.csv("./winequality-white.csv", sep = ';', header = TRUE)

###################################
##Analyse descriptive des donn�es##
###################################


##Nombre d'individus:
nrow(vinBlanc ) 
dim(vinBlanc )

##le jeux de donn�es vinRouge comporte 4898  entr�es et 12 variables
 
##Nombre de variables 
head(vinBlanc )
colnames(vinBlanc )
##Il y a 11 variables explicatives
#la variable cible est "quality", 12�me colonne

##la valeur maximale, la valeur minimale et la moyenne de la colonne quality
summary(vinBlanc$quality)


#######################################################
##Graphe de chaque variable explicative sur "quality"##
#######################################################


plot(vinBlanc$fixed.acidity, vinBlanc$quality)
plot(vinBlanc$volatile.acidity, vinBlanc$quality)
plot(vinBlanc$citric.acid, vinBlanc$quality)
plot(vinBlanc$residual.sugar, vinBlanc$quality)
plot(vinBlanc$chlorides, vinBlanc$quality)
plot(vinBlanc$free.sulfur.dioxide, vinBlanc$quality)
plot(vinBlanc$total.sulfur.dioxide, vinBlanc$quality)
plot(vinBlanc$density, vinBlanc$quality)
plot(vinBlanc$pH, vinBlanc$quality)
plot(vinBlanc$sulphates, vinBlanc$quality)
plot(vinBlanc$alcohol, vinBlanc$quality)




#######################
##Regression lin�aire##
#######################

##S�paration des donn�es en apprentissage et test

n = nrow(vinBlanc)
n_app = round(0.75*n)
n_test = n - n_app
s =sample(n)
apprentissage = vinBlanc[s[1:n_app],]
test = vinBlanc[s[(n_app+1):n],]
dim(apprentissage)
dim(test)

######Mod�les de pr�diction � la variable pr�dictrice####


## D'apr�s les diff�rentes repr�sentations graphiques, on peut voir que les variables
## qui semblent plus li�es � la variable cible quality sont:
##alcohol, volatile.acidity, sulphates

##On peut appuyer ce r�sultat en calculant le coefficient de correlation 
##entre les differentes variables explicatives et la variable cible
cor(vinBlanc[,1:12])

#########################################
                          quality
##fixed.acidity        -0.113662831
##volatile.acidity     -0.194722969
##citric.acid          -0.009209091
##residual.sugar       -0.097576829
##chlorides            -0.209934411
##free.sulfur.dioxide   0.008158067
##total.sulfur.dioxide -0.174737218
##density              -0.307123313
##pH                    0.099427246
##sulphates             0.053677877
##alcohol               0.435574715
##quality               1.000000000

#########################################

##Cr�ation d'un mod�le de r�gression lin�aire mod1 permettant de pr�dire la variable quality � partir de la variable alcohol (11�me colonne).
##Test de fisher
##Pr�diction � partir du mod�le mod1
##M�thode de validation crois� en utilisant la variable alcohol
##M�thode de s�paration en utilisant la variable alcohol

##model1##
mod1 = lm(quality~alcohol, data =apprentissage)



##donn�es Fisher de mod1
summary(mod1)$fstatistic

##quantile de fisher
qf(0.95, 1, 3672)

##On remarque summary(mod1)$fstatistic > qf(0.95, 1, 1597) soit 
##841.8831   > 3.843993, donc ce mod�le est significativement utile pour pr�dire quality

p1 = predict(mod1, test)
p1
erreur_gen(test$quality, p1)
hist(p1-test$quality)

vc1 = validation_croisee(vinBlanc, 11, 12, 10)
vc1 
sp1 = separation(vinBlanc, 11, 12, 10)
sp1 

##Cr�ation d'un mod�le de r�gression lin�aire mod2 permettant de pr�dire la variable quality � partir de la variable volatile.acidity(8�me colonne).
##Pr�diction � partir du mod�le mod2
##M�thode de validation crois� en utilisant la variable density
##M�thode de s�paration en utilisant la variable density


##model2##
mod2 = lm(quality~density, data =apprentissage)

##donn�es Fisher de mod2
summary(mod2)$fstatistic

##quantile de fisher
qf(0.95, 1, 3672)

##On remarque summary(mod2)$fstatistic > qf(0.95, 1, 1597) soit 
##401.3164  > 3.843993, donc ce mod�le est significativement utile pour pr�dire quality

p2 = predict(mod2, test)
p2
erreur_gen(test$quality, p2)
hist(p2-test$quality)

vc2 = validation_croisee(vinBlanc, 2, 12, 10)
vc2 
sp2 = separation(vinBlanc, 1, 12, 1)
sp2



##Essayons avec la variable citric.acid qui faiblement corr�l�e avec la variable quality
mod3 = lm(quality~citric.acid , data =apprentissage)


##donn�es Fisher de mod4
summary(mod3)$fstatistic

##quantile de fisher
qf(0.95, 1, 3672)

##On remarque summary(mod3)$fstatistic < qf(0.95, 1, 1597) soit 
##  0.1962403 < 3.843993, donc ce mod�le n'est pas utile pour pr�dire quality


############################################################################
##Selection de variable avec les m�thodes s�paration et validation crois�e##
############################################################################


##Selection ascendante avec la m�thode de s�paration


sel_asc_sep = selec_asc_sep(vinBlanc,c(1:11),12,10)
sel_asc_sep 

####Selection ascendante avec la m�thode de s�paration


sel_asc_val = selec_asc_val(vinBlanc,c(1:11),12,10)
sel_asc_val


##on voit que les r�sultats sont plus stables avec la m�thodes de s�parations avec validation crois�e avec K=10
##les variables explicatives retourn�es sont plutot celle qui sont le plus corr�l�es avec la variable cible: quality
