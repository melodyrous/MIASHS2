
# Import des données
data = read.csv(file="./data/Data_cancer_sein.csv",header=T)
head(data)
tail(data)
names(data)

# Analyse exploratoire des données
table(data$surv_cancer)
table(data$type)

min(data$age)

str(data)
summary(data)

# Factor
names(data[,c(2,3,5,6,7)])
data_col = c(2,3,5,6,7)
for (i in 1:5){
  data[,data_col[i]] = factor(data[,data_col[i]])
}
str(data)
summary(data)

# Ajout de labels
data[,2] = factor(data[,2],levels=c(levels(data[,2])),c("Controle","Cancer"))
data[,3] = factor(data[,3],levels=c(levels(data[,3])),c("Controle","Infiltrant","CIS"))
data[,5] = factor(data[,5],levels=c(levels(data[,5])),c("Negatif","Positif"))
data[,6] = factor(data[,6],levels=c(levels(data[,6])),c("Negatif","Positif"))
data[,7] = factor(data[,7],levels=c(levels(data[,7])),c("Negatif","Positif"))
str(data)
summary(data)

# Visualisations
boxplot(data[,8],main = "",xlab = "HSP60",ylab = "Densité optique")
hist(data[,4],col = "lightblue",main = "Age",xlab = "",ylab = "")


#Discrétisation de la variable âge:
age_cl = rep(0,length(data$age))
age_cl[data$age<=55] = 1 
age_cl[data$age>55 & data$age<=65] = 2
age_cl[data$age>65] = 3
age_cl = as.factor(age_cl)
age_cl= factor(age_cl,levels=c(levels(age_cl)),c("<=55","]55;65]",">65"))
table(age_cl)


# Modélisation
# link="logit" pour signifier un modele logistique, famille pour spécifier une variable binaire
modele=glm(surv_cancer~age_cl+hsp60+muc1+prdx2+ppia+fkbp52,family=binomial(link="logit"), data = data)
summary(modele)
attributes(modele)
# Estimate = Beta. Transformer les variables qualitative en variable indicatrices
# Odd Ration = exponentielle du coefficient
exp(1.0290) # x2,8 probabilité d'avoir le cancer par rapport à la classe de référence exp(0)=1 
# Test de VALD pour conclure si le coefficient est différent de 0. Avec la p value on regarde l'impact de la variable sur le modèle.
# On supprime les variables non significatives qui n'ont pas été rejetées par le test et qui sont donc = à 0.05.
# On accepte l'hypothèse nulle, la variable est significative

# Calcul de la Statistique de test de WALD sur ppia et p-value
# 6.2640  /  2.1286   =  2.943  0.00325 

# Modèle 2 sans variable prdx2
modele2=glm(surv_cancer~age_cl+hsp60+muc1+ppia+fkbp52,family=binomial(link="logit"), data = data)
summary(modele2)
attributes(modele2)

modele2$fitted.values
premiere_patiente =exp( -3.0483 + 1.7205 +2.8793*0.1406276 + -3.5021*0.201 + 6.3151*0.158 + 2.5567*0.207) / (1 + exp(-3.0483 + 1.7205 +2.8793*0.1406276 + -3.5021*0.201 + 6.3151*0.158 + 2.5567*0.207))
modele2$coefficients

#Intervalles de confiance pour le OR ou le Beta
# Le 1 ne doit pas appartenir au OR + ou - l'intervalle de confiance
## TODO


# Validation du modèle
# Statistique de vraissemblance ou déviance
# BIC pénalise le nombre de paramètres de modèle
# AIC, BIC nécéssite une valeur proche de 0
# 1 on teste les variable avec Wlad
# 2 On vérifie que le scores diminuent
AIC(modele2)
BIC(modele2)
# La déviance nulle (sous l'hypothèse nulle)
modele2$deviance

# Calibration du modèle: test de Hosmer Lemeshow
library(generalhoslem)
logitgof(modele2$y, modele2$fitted.values)
# Ici on a besoin dune p-value superieur à 0.05 pour ne pas rejetter l'hypothese nulle et dire que le modèle est calibré
library(ModelGood)
calPlot2(modele2)

# Test du rapport de vraissemblance
# Pour regarder l'apport d"une variable quantitative
library(lmtest)
lrtest(modele, modele2)
# ou fonction ANOVA
anova(modele2, modele, )
# Le test de vraisemblance = test du Chi2
#Function pchisq récupère la valeurde la p value

# Les courbes ROC = 1- spécificité pour chaque seuil de probabilité prédite. Si la courbe se rapproche
# de la droite, plus le modele est inutile et se rapproche du hasard. Plus (AUC) l'aire sous la courbe est
# grand, plus la probabilité prédite est discriminante
# Sensibilité Vrai Positifs
# Spécficité Faux positifs
library(ROCR)
? ROCR
pred <- prediction(modele2$fitted.values, modele2$y) 
class(pred)

roc.perf = performance(pred, measure = "sens", x.measure = "fpr" ) #fpr = 1- sensibilité
plot(roc.perf)
abline(a=0, b=1)

auc.perf = performance(pred, measure = "auc")
plot(roc.perf)
abline(a=0, b=1)


# CONSTRUCTION DES MODELES
str_constant = "~ 1"
str_all = "~age_cl+hsp60+muc1+prdx2+ppia+fkbp52"
library(MASS)
modele_constant = glm(surv_cancer~1,family = binomial(link="logit"), data = data)
# Forward
modele_forward = stepAIC(modele_constant,scope = list(lower = str_constant, upper = str_all),trace = TRUE,direction = "forward", data =data)
summary(modele_forward)

# Pour la modélisation commencer par le modèle univarié et calculer le OR pour chaque variable. (Pré-selection)
# L'OR peut varier légèrement, s'il augmente fortement il peut y avoir des interactions ou autre à prévoir.

modele_constant = glm(surv_cancer ~ 1, family = binomial(link="logit"), dat = data)
modele_both= stepAIC(modele_constant,scope = list(lower = str_constant, upper = str_all),trace = TRUE,direction = "both")
summary(modele_both)

# Exercice 2 
modele_backward = glm(surv_cancer~age_cl+hsp60+muc1+prdx2+ppia+fkbp52,family = binomial(link="logit"), data = data)

stepAIC(modele_backward,trace = TRUE,direction = "backward", data =data)
summary(modele_forward)


