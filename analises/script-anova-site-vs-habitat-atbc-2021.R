#import excel spreadsheet with data and classifying variables
#
library("MASS")
library(lawstat)
atbc2021 = audiomoth_indices_for_r_nosd_atbc_2021
summary(atbc2021)
depvar = atbc2021$meanbio
#
anovaresul<-aov(depvar ~ habitat+site+habitat:site, atbc2021)
summary(anovaresul)
print(anovaresul)
?interaction.plot
interaction.plot(atbc2021$site, atbc2021$habitat, atbc2021$meanbio)
#
#Test de normalidade para compreender os residuos
shapiro.test(resid(anovaresul))
#
#Homocedasticidade: Mesma variancia lawstat package levene test
levene.test(depvar, atbc2021$site)
#p-valor = 0.9443
levene.test(depvar, atbc2021$habitat)
#
#Test de Tukey para ver quais grupos diferem entre si
#
#EXISTE VARIACAO ENTRE OS GRUPOS, MAS QUAIS QUE VARIAM?
#PARA SABER ISSO, VERIFICAMOS QUAIS GRUPOS DIFEREM ENTRE SI.
#OU SEJA, EM QUAIS PARES DE AMOSTRAS ACONTECEM AS DIFERENCIAS
#TODOS OS VALORES DE LWR (LIMITE INFERIOR) POSITIVO SAO SIGNIFICATIVOS.
# DIFF SIGNIFICA DIFERENCIA
plot(anovaresul)
#boxplots for each variable per site
boxplot(meanaci~site, data=atbc2021)
boxplot(meanadi~site, data=atbc2021)
boxplot(meanaeve~site, data=atbc2021)
boxplot(meanbio~site, data=atbc2021)
boxplot(meanh~site, data=atbc2021)
boxplot(meanm~site, data=atbc2021)
boxplot(meanndsi~site, data=atbc2021)
#
TukeyHSD(anovaresul)
plot(TukeyHSD(anovaresul))
