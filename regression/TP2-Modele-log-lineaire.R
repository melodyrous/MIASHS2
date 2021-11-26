##############################
#### REGRESSION DE POISSON ###
##############################


# PRESENTATION DES DONNEES
library(MASS)
head(epil)
tail(epil)
names(epil)

# Analyse exploratoire
str(epil)
summary(epil)
data = epil
table(data[,2])

# TRANSFORMATION DES DONNEES
# On transforme en facteur les données dichotomiques
data_col = c(5,6,7)
for (i in 1:3){
  data[,data_col[i]] = factor(data[,data_col[i]])}

# Discrétisation de Variable base à partir de la médiane
median(data2[,3])
min(data2[,3])
max(data2[,3])
borne = c(0,22,max(epil[,3]))
base_cl = cut(epil[,3], breaks = borne, include.lowest = TRUE)
table(base_cl)

# Discrtisation de la variable age
borne = c(0,28,max(epil[,4]))
age_cl = cut(epil[,4], breaks = borne, include.lowest = TRUE)
table(age_cl)

# ANALYSE EXPLORATOIRE
y=epil[,1]
trt=epil[,2]
base=epil[,3]
age=epil[,4]
V4=epil[,5]
subject=epil[,6]
periode=epil[,7]
plot(periode,y,col = "black", xlab = "",ylab = "")

y1=epil[trt=="progabide",1]
periode1=epil[trt=="progabide",7]
y2=epil[trt=="placebo",1]
periode2=epil[trt=="placebo",7]
plot(periode1,y1,col = "red", xlab = "",ylab = "")
points(periode2,y2,col = "blue", xlab = "",ylab = "")

# MODELE LOG-LINEAIRE
modele1 = glm(y~periode+age_cl+trt+base, family = poisson, data = data)
summary(modele1)
AIC(modele1)
BIC(modele1)

modele2 = glm(y~periode+age_cl+trt+base+base:trt, family = poisson, data = data)
summary(modele2)
AIC(modele2)
BIC(modele2)

library(lmtest)
lrtest(modele1, modele2)

# Exercice 1 
## RR Rapport de taux
# Effet multiplicatif sur le taux de la loi de Poisson
exp(0.3559324)
exp(-0.2719296)
# Pour que ce soit significatif, il ne faut pas que le 1 soit présent dans l'intervalle 
# de confiance.
exp(cbind(coef(modele1), confint(modele1)))
# Fitted values prédit le nombre de crises d'épilepsie
# Si tu as plus de 28, tu as une chance d'avoir 1.4 plus de crise d'épilepsi prédit. 