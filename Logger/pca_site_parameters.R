library(ggplot2)
library(FactoInvestigate)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(missMDA)
library(corrplot)
library(RColorBrewer)

#remember: eigenvalue -> amount of variance retained by each principal component
metadata$Temp_median=metadata_numeric$Temp_median
metadata_pca=metadata[,-c(1,2,4,5,7,8,17:26,28:32)]
#drop water logger and water surface logger
metadata_pca=metadata_pca[metadata_pca$Loggertyp!="WL",]
metadata_pca=metadata_pca[metadata_pca$Loggertyp!="WOL",]

#set % values from NA to 0
metadata_pca2=metadata_pca
metadata_pca2[is.na(metadata_pca2)]=0
metadata_numeric[,c(2:4, 6:8)]=metadata_numeric2[,c(2:4, 6:8)] #CONTINUE HERE (adapt col nr)
rm(metadata_pca2)
#pca with ungrouped variables 
res=PCA(metadata, quali.sup=1:4, quanti.sup=41, graph=T, scale.unit = T)


get_eigenvalue(res) #extract eigenvalues
fviz_eig(res, addLabels=T) #visualize eigenvalues with scree plot

get_pca_ind(res) #extract results for indivduals
fviz_pca_ind(res) #visualize results for individuals

var=get_pca_var(res) #extract results for variables
fviz_pca_var(res, col.var="black", repel=T) #visualze results for variables
var$coord #coordinates of variables
var$cor #correlation variables and dimensions
var$cos2 #quality of representation for variables
#high cos2 -> good representation of variable on PC
corrplot(var$cos2, is.corr=F) #correlation plot
fviz_cos2(res, choice="var", axes = 1) #barplot for dimensin 1
fviz_cos2(res, choice="var", axes = 2) #barplot for dimension 2
fviz_pca_var(res, col.var="cos2", repel=T) #plot with color according to co2 value

var$contrib #contributions of the variables in %
fviz_pca_var(res, col.var="contrib", repel=T) #plot with color according to contribution to PC
corrplot(var$contrib, is.corr=F)
fviz_contrib(res, choice="var") #contribution to dim 1
fviz_contrib(res, choice="var", axes=2) #contribution to dim 2

fviz_pca_biplot(res) #visualize both (add axes= e.g. 1 to see contribution to single dimension or 1:2 for multiple)

ind=get_pca_ind(res)
fviz_pca_ind(res, col.ind="cos2", repel=T)

res.dim=dimdesc(res, axes=c(1,2,3), proba = 0.05) #identify most significantly associated variables with given PC
res.dim$Dim.1
res.dim$Dim.2
res.dim$Dim.3

fviz_pca_ind(res, col.ind="cos2", repel=T) #plot individuals colored in relation to cos2 value
fviz_pca_ind(res, pointsize="cos2", repel=T) #plot individuals colored in relation to cos2 value

fviz_cos2(res, choice="ind", axes=1:2) #bar plot for dim 1 and 2 with contribution of individuals

#color by groups
#for Treatment
fviz_pca_ind(res, geom.ind="point", 
             col.ind=data$Treatment2, addEllipses = T, 
             legend.title="Treatment")
#for Variety
fviz_pca_ind(res, geom.ind="point", 
             col.ind=data$Variety, addEllipses = T,
             legend.title="Variety")
#for type
fviz_pca_ind(res, geom.ind="point", 
             col.ind=data$Type, addEllipses = T,
             legend.title="Type")

#biplot with dimensions (grouped)
#by treatment
fviz_pca_biplot(res, col.ind=data$Treatment1, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Treatment")
fviz_pca_biplot(res, col.ind=data$Treatment2, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Treatment")

#by variety
fviz_pca_biplot(res, col.ind=data$Variety, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Variety")
#by treatment
fviz_pca_biplot(res, col.ind=dat$Type, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Type")

res$quali.sup #predicited results for the supplementary qualitative variables
fviz_pca_ind(res, habillage = 3, #graph according to supplementary variables
             addEllipses =TRUE, ellipse.type = "confidence",
             repel = TRUE) 
#habillage= index of supplementary variable to show
#1 = Variety
#2 = Type
#3 = Treatment

#plot selected variables and individuals
fviz_pca_var(res, select.var=list(cos2=0.6)) #visualize only variables with cos2>0.6

fviz_pca_biplot(res, select.ind=list(contrib=20)) #display the 20 most contributing individuals

#export the results to csv
write.infile(res, "pca2.csv", sep=";")