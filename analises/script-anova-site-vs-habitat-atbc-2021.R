#import excel spreadsheet with data and classifying variables
#
library("MASS")
library(lawstat)
summary(datasetname)
#
anovaresul<-aov(depvarname ~ site+habitat+site:habitat, datasetname)
summary(anovaresul)
?interaction.plot
interaction.plot(datasetname$site, datasetname$habitat, datasetname$depvarname)
#
#Test de normalidade para compreender os residuos
shapiro.test(resid(anovaresult))
#
#Homocedasticidade: Mesma variancia lawstat package levene test
levene.test(datasetname$depvarname, datasetname$site)
#p-valor = 0.9443
levene.test(datasetname$depvarname, datasetname$habitat)
#
#Test de Tukey para ver quais grupos diferem entre si
#
#EXISTE VARIACAO ENTRE OS GRUPOS, MAS QUAIS QUE VARIAM?
#PARA SABER ISSO, VERIFICAMOS QUAIS GRUPOS DIFEREM ENTRE SI.
#OU SEJA, EM QUAIS PARES DE AMOSTRAS ACONTECEM AS DIFERENCIAS
#TODOS OS VALORES DE LWR (LIMITE INFERIOR) POSITIVO SAO SIGNIFICATIVOS.
# DIFF SIGNIFICA DIFERENCIA
plot(anovaresul)
boxplot(depvarname~site, data=datasetname)
TukeyHSD(anovaresult)
plot(TukeyHSD(anovaresult))