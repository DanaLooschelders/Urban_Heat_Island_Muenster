library(ggplot2)
library(FactoInvestigate)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(missMDA)
library(corrplot)
library(RColorBrewer)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data/pca")

#remember: eigenvalue -> amount of variance retained by each principal component
metadata$Temp_median=metadata_numeric$Temp_median
metadata_pca=metadata[,-c(1,2,4,5,8,12,17:26,28:32)]
#drop water logger and water surface logger
metadata_pca=metadata_pca[metadata_pca$Loggertyp!="WL",]

#set % values from NA to 0
metadata_pca2=metadata_pca
metadata_pca2[is.na(metadata_pca2)]=0
metadata_pca[,c(3:5, 7:9)]=metadata_pca2[,c(3:5, 7:9)] #CONTINUE HERE (adapt col nr)
rm(metadata_pca2)
metadata_pca2=metadata_pca[,-1]

#pca with ungrouped variables 
res=PCA(metadata_pca, graph=T, scale.unit = T, quali.sup = c(1,3))


get_eigenvalue(res) #extract eigenvalues
fviz_eig(res, addLabels=T) #visualize eigenvalues with scree plot
ggsave(filename="fviz_eig.pdf",device = "pdf")


get_pca_ind(res) #extract results for indivduals
fviz_pca_ind(res) #visualize results for individuals
ggsave(filename="fviz_pca_ind.pdf",device="pdf")


var=get_pca_var(res) #extract results for variables
fviz_pca_var(res, col.var="black", repel=T) #visualze results for variables
ggsave(filename="fviz_pca_var.pdf", device="pdf")
var$coord #coordinates of variables
var$cor #correlation variables and dimensions
var$cos2 #quality of representation for variables
#high cos2 -> good representation of variable on PC

corrplot(var$cos2, is.corr=F) #correlation plot
ggsave(filename="corrplot_cos2.pdf", device="pdf")


fviz_cos2(res, choice="var", axes = 1) #barplot for dimensin 1
ggsave(filename="fviz_cos2_dim1.pdf", device="pdf")


fviz_cos2(res, choice="var", axes = 2) #barplot for dimension 2
ggsave(filename="fviz_cos2_dim2.pdf", device="pdf")


fviz_pca_var(res, col.var="cos2", repel=T) #plot with color according to co2 value
ggsave(filename="fviz_pca_var_cos2.pdf", device="pdf")

var$contrib #contributions of the variables in %

fviz_pca_var(res, col.var="contrib", repel=T) #plot with color according to contribution to PC
ggsave(filename="fviz_pca_var_contrib.pdf", device="pdf")

pdf(file="corrplot_contrib.pdf", paper = "a4")
corrplot(var$contrib, is.corr=F)
dev.off()

fviz_contrib(res, choice="var") #contribution to dim 1
ggsave(filename="fviz_contrib_dim1.pdf", device="pdf")


fviz_contrib(res, choice="var", axes=2) #contribution to dim 2
ggsave(filename="fviz_contrib_dim2.pdf", device="pdf")

fviz_pca_biplot(res) #visualize both (add axes= e.g. 1 to see contribution to single dimension or 1:2 for multiple)
ggsave(filename="fviz_pca_biplot2.pdf", device="pdf")

ind=get_pca_ind(res)

#CHECK THIS AND EXPORT
res.dim=dimdesc(res, axes=c(1,2,3), proba = 0.05) #identify most significantly associated variables with given PC
res.dim$Dim.1
res.dim$Dim.2
res.dim$Dim.3

fviz_pca_ind_cos2=fviz_pca_ind(res, col.ind="cos2", repel=T) #plot individuals colored in relation to cos2 value
ggsave(filename="fviz_pca_ind_cos2.pdf", device="pdf")

fviz_pca_ind(res, pointsize="cos2", repel=T) #plot individuals colored in relation to cos2 value
ggsave(filename="fviz_pca_ind_point_cos2.pdf", device="pdf")

fviz_cos2(res, choice="ind", axes=1:2) #bar plot for dim 1 and 2 with contribution of individuals
ggsave(filename="fviz_cos2_ind_dim1_2.pdf", device="pdf")

#color by groups
fviz_pca_ind(res, geom.ind="point", 
             col.ind=metadata_pca$Loggertyp, addEllipses = T, 
             legend.title="Loggertyp")
ggsave(filename="fviz_pca_Loggertype.pdf", device="pdf")


fviz_pca_ind(res, geom.ind="point", 
             col.ind=metadata_pca$Standort, addEllipses = T,
             legend.title="Standort")
ggsave(filename="fviz_pca_Standort.pdf", device="pdf")

#biplot with dimensions (grouped)
fviz_pca_biplot(res, col.ind=metadata_pca$Loggertyp, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Loggertyp")
ggsave(filename="fviz_biplot_Loggertype.pdf", device="pdf")

fviz_pca_biplot(res, col.ind=metadata_pca$Standort, addEllipses = T,
                label="var", col.var="black", repel=T, legend.title="Standort")
ggsave(filename="fviz_biplot_Standort.pdf", device="pdf")

res$quali.sup #predicited results for the supplementary qualitative variables
#for Loggertype

fviz_pca_ind(res, habillage = 1, #graph according to supplementary variables
             addEllipses =TRUE, ellipse.type = "confidence",
             repel = TRUE)
ggsave(filename="fviz_pca_supp_Loggertype.pdf", device="pdf")

#for site
fviz_pca_ind(res, habillage = 3, #graph according to supplementary variables
             addEllipses =TRUE, ellipse.type = "confidence",
             repel = TRUE) 
ggsave(filename="fviz_pca_supp_Standort.pdf", device="pdf")
#habillage= index of supplementary variable to show

#plot selected variables and individuals

fviz_pca_var(res, select.var=list(cos2=0.6)) #visualize only variables with cos2>0.6
ggsave(filename="fviz_pca_var_ind_cos2_greater_0.6.pdf", device="pdf")


fviz_pca_biplot(res, select.ind=list(contrib=5)) #display the 20 most contributing individuals
ggsave(filename="fviz_biplot_contrib_ind.pdf", device="pdf")
#export the results to csv
write.infile(res, "pca2.csv", sep=";")
