#install.packages("factoextra")
library(factoextra)
library(psych)
#import excel spreadsheet with data and classifying variables
#
atbc2021 <- audiomoth_indices_for_r_nosd_atbc_2021
#plot the data and correlations between variables
#scatter plot and correlations between variables one command below
#if this does not work just use the plot command instead of pairs.panels
pairs.panels(atbc2021[,5:11],
gap = 0,
pch=21)
#
# select only values variables for the pca matrix dataframe to run
datatorun <- atbc2021[,5:11]
#
#make a dataframe of site names for labeling
sitenames <- atbc2021[,2]
#
#make dataframe of site hours for labeling
sitehours <-atbc2021[,4]
sitehours$period= "noite"
for (row in 1:nrow(sitehours)) {
  if (sitehours$horautc[row] > 5) {(sitehours$period[row] = "manha")}
  if (sitehours$horautc[row] > 13) {(sitehours$period[row] = "tarde")}
 
 
    }
 
#run pca
pcaresult <- prcomp(datatorun,
center = TRUE,
scale = TRUE)
#print the results
print(pcaresult)
summary(pcaresult)
#
#plot the results using the factoextra package, the pcaresult object, and the sitenames object
# thiago r-bloggers example
#
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#
#PCA graph labeled with site name, this works
#
g <- ggbiplot(pcaresult, obs.scale = 1, var.scale = 1,
groups = sitenames$site, ellipse = TRUE,
circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
legend.position = 'top')
print(g)
#
#same graph now with time of day labeling
#
g <- ggbiplot(pcaresult, obs.scale = 1, var.scale = 1,
              groups = sitehours$period, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
#
# another plot from the breast cancer example pca analysis  peter nistrup towards data science
# Not working yet
fviz_pca_ind(pcaresult)
#
fviz_pca_ind(pcaresult, geom.ind = "point", pointshape = 21,
pointsize = 2,
fill.ind = sitenames,
col.ind = "black",
palette = "jco",
addEllipses = TRUE,
label = "var",
col.var = "black",
repel = TRUE,
legend.title = "Diagnosis") +
ggtitle("2D PCA-plot from 30 feature dataset") +
theme(plot.title = element_text(hjust = 0.5))
#