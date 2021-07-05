#import excel spreadsheet with data and classifying variables
#
library("MASS")
library(lawstat)
atbc2021 = audiomoth_indices_for_r_nosd_atbc_2021
summary(atbc2021)
#
anovaresul<-aov(meanbio ~ site+habitat+site:habitat, atbc2021)
summary(anovaresul)
?interaction.plot
interaction.plot(atbc2021$site, atbc2021$habitat, atbc2021$meanbio)
#
#Test de normalidade para compreender os residuos
shapiro.test(resid(anovaresul))
#
#Homocedasticidade: Mesma variancia lawstat package levene test
levene.test(atbc2021$meanbio, atbc2021$site)
#p-valor = 0.9443
levene.test(atbc2021$meanbio, atbc2021$habitat)
#
#Test de Tukey para ver quais grupos diferem entre si
#
#EXISTE VARIACAO ENTRE OS GRUPOS, MAS QUAIS QUE VARIAM?
#PARA SABER ISSO, VERIFICAMOS QUAIS GRUPOS DIFEREM ENTRE SI.
#OU SEJA, EM QUAIS PARES DE AMOSTRAS ACONTECEM AS DIFERENCIAS
#TODOS OS VALORES DE LWR (LIMITE INFERIOR) POSITIVO SAO SIGNIFICATIVOS.
# DIFF SIGNIFICA DIFERENCIA
plot(anovaresul)
boxplot(meanbio~site, data=atbc2021)
TukeyHSD(anovaresult)
plot(TukeyHSD(anovaresult))