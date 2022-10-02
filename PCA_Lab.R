library(tidyverse) #data manipilation
library(GGally) # nice scatterplot matrix
library(FactoMineR) # PCA computation
library(factoextra) # nice plotting for PCA objects
library(missMDA) # to determine number of PC's through crossvalidation
library(gridExtra) # to build grid of plots

#check you have the correct data set
cereals <- read.table("pca.txt", header=TRUE, as.is=TRUE, na.strings="-1")
cereals1=na.omit(cereals)
rownames(cereals1)=abbreviate(cereals1$Name)
cereal.pc= dplyr::select(cereals1,calories,protein,fat,sodium,fiber,carbo,sugars,potass,shelf)
cereal.pc=na.omit(cereal.pc)
head(cereal.pc)
cereal.pc$shelf=as.factor(cereal.pc$shelf)

#summary of the variables, shelf is a factor
summary(cereal.pc)
str(cereal.pc)

#covariance matrix, very different variances
cov(cereal.pc[,-9])

# Before starting with PCA, a nice scatterplot matrix
ggpairs(cereal.pc, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=shelf)))

#load FactoMineR and perform a PCA analysis on matrix R, not S. Store the results in object cereal_pca_r. We are using shelf as
# a supplementary qualitative variable. By default 5 components are calculated, use ncp= to change it.

cereal_pca_r=PCA(cereal.pc,quali.sup=9,ncp=8,scale.unit=TRUE, graph=FALSE)

# by default dimensions 1 and 2 are plotted. We are using that option. To change them use axes=c(1,3).
# type cereal_pca_r to see the extensive list of results provided in the output of PCA()
cereal_pca_r

#summary of the numerical output
summary(cereal_pca_r)

#Working on the map of points, representation of individuals
plot(cereal_pca_r, cex=0.7)

# coloring points by variable shelf
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9)
# change individual color by group
fviz_pca_ind(cereal_pca_r,  habillage="shelf")
fviz_pca_ind(cereal_pca_r,  label="none", habillage="shelf")

#labels for those points with cosin squared greater than 0.7, for example.
# It selects observations with cos2[,1]+cos2[,2]>0.7
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7")
# plotting only previously selected observations
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect=1)
#selecting a color for unselected points
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect="grey70")
# To select the five observations that contribute the most to the two first components, the more extreme individuals in both components
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 5", unselect=1)

#selecting particular individuals by their names
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=c("Trix", "H_N_", "Whts"))
#selecting particular individuals by their row in the dataset
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=1:10)
# Controlling automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
fviz_pca_ind(cereal_pca_r, col.ind="cos2", repel=TRUE) +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)
fviz_pca_ind(cereal_pca_r, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# What if I want the previous plot but showing also the shelf each cereal is on? 
# Solution
newdf=data.frame(cos2=cereal_pca_r$ind$cos2[,1]+cereal_pca_r$ind$cos2[,2],shelf=cereal.pc$shelf, 
                 PC1=cereal_pca_r$ind$coord[,1],PC2=cereal_pca_r$ind$coord[,2])

ggplot(data=newdf, aes(x=PC1, y=PC2, colour=cos2, shape=shelf))+
  geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter(size=2)

## Working on variables, circle of correlations
plot(cereal_pca_r, choix="var")
## or with package factoextra
fviz_pca_var(cereal_pca_r)

# Ploting the 3 variables that contribute the most to the representation 
plot(cereal_pca_r, shadow=TRUE,choix="var", select="contrib 3" )
# selecting variables by their contributions, quality of representation greater than 0.7
plot(cereal_pca_r, shadow=TRUE,choix="var", select="cos2 0.7" )
# control variable colors using their contribution
fviz_pca_var(cereal_pca_r, col.var="contrib")

# Change the gradient color
fviz_pca_var(cereal_pca_r, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55)+theme_bw()

# Quality of each variable representation on these two dimensions
cereal_pca_r$var$cos2[,1:2][,1]+ cereal_pca_r$var$cos2[,1:2][,2]

# First a table, then a barplot of variables contribution to each dimension with ggplot
rbind(cereal_pca_r$var$contrib, TOTAL=colSums(cereal_pca_r$var$contrib))
var.contrib=cereal_pca_r$var$contrib
my.grid=expand.grid(x=rownames(var.contrib), y=colnames(var.contrib))
my.grid$values= as.vector(var.contrib)
ggplot(my.grid, aes(x=y, y=values))+geom_bar(stat="identity", aes(fill=x), position=position_dodge())+
  scale_fill_brewer(palette="Dark2")
ggplot(my.grid, aes(x=x, y=values))+geom_bar(stat="identity", aes(fill=y), position=position_dodge())

### Objects (observations) contribution
objcontrib=data.frame(C1=cereal_pca_r$ind$contrib[,1],C2=cereal_pca_r$ind$contrib[,2],n=rownames(cereal.pc))

# Barplots of object contributions to PC 1 with ggplot and package gridExtra
G1=ggplot(objcontrib[1:36,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
G2=ggplot(objcontrib[31:74,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
grid.arrange(G1,G2,nrow=2)
# compare them with the position of each point in the observations plot

#If you want to order observations by the first component value (score), make a data frame with the two first scores, for instance 
obsor=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2])
head(obsor[order(obsor[,1], decreasing=TRUE),])

# Checking some equalities
sum(cereal_pca_r$var$cos2[1,1:8])
sum(cereal_pca_r$ind$cos2[1,])
###
sum(cereal_pca_r$var$cor[1:8,1]^2)
mean(cereal_pca_r$ind$coord[,1]^2)
###
sum(cereal_pca_r$ind$contrib[,1])
sum(cereal_pca_r$var$contrib[1,])


#scree plot
par(mfrow=c(1,1))
plot(cereal_pca_r$eig[,1], type="l")
points(cereal_pca_r$eig[,1])

# scree plot (barplot type)
barplot(cereal_pca_r$eig[,1], names.arg=rownames(cereal_pca_r$eig))

#biplot
fviz_pca_biplot(cereal_pca_r, repel=TRUE)

# Eigenvectors. They are collinear with the correlations between variables and
# components. 
cereal_pca_r$svd$V
cereal_pca_r$svd$V[,1] # first eigenvector, coefficients for the linear combination
# that defines the first principal component.
# correlations are the corresponding entry in the corresponding eigenvector times the corresponding
# singular value
cereal_pca_r$var$cor[1,1] # correlation first var first component
cereal_pca_r$svd$V[1,1]*cereal_pca_r$svd$vs[1]
cereal_pca_r$var$cor[2,1] # correlation second variable first component
cereal_pca_r$svd$V[2,1]*cereal_pca_r$svd$vs[1]

# correlations between variables and dimensions and significance tests
# For the first (second) component, are there any differences in the categorical variable?
# 
dimdesc(cereal_pca_r,axes=c(1,2)) 

concat1 = cbind.data.frame(shelf=cereal.pc[,9],cereal_pca_r$ind$coord[,1:2])
boxplot(concat1[,2]~concat1[,1])
summary(aov(concat1[,2]~concat1[,1]))
summary(lm(concat1[,2]~concat1[,1]))

# Determining the number of components with cross validation
estim_ncpPCA(cereal.pc, ncp.min = 0, ncp.max = 5, 
             scale = TRUE, method.cv = c("Kfold"), nbsim = 100,
             pNA = 0.05, ind.sup=NULL, quanti.sup=NULL, quali.sup=9,
             threshold=1e-4, verbose = TRUE)

##Ellipses
#This function draws confidence ellipses around the categories of a supplementary categorical variable. The objective is to see 
#whether the categories of a #categorical variable are significantly different from each other.
#It uses a data set with the categorical variable and the coordinates of the individuals on the principal components.
fviz_pca_ind(cereal_pca_r, label="none", habillage=cereal.pc$shelf,
             addEllipses=TRUE, ellipse.level=0.95)

## reconstruction of the original data matrix usig 2 PC's
reconstruction1 = reconst(cereal_pca_r,ncp=2)
coeffRV(reconstruction1, cereal.pc[,1:8])
## coeffRV: it measures the closeness of two sets of points. It's a multivariate generalization
## of the squared Pearson correlation coefficient (0-1). It uses the concept of permutation distribution.

# Small example. Gene expression in 9 tumors. 9 genes. 
tumor_1 <- c(1,1,0,-0.5,-1,-1,0,1,1) 
tumor_2 <- c(-1,-1,-1 ,-0.5,0,0,1,1,1) 
tumor_3 <- c(1,1,1 ,1,1,1,-1,-1,-1)

set.seed(755) 
tumor_a <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_b <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_c <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_d <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_e <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_f <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_g <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_h <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2) 
tumor_i <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2)
genes=data.frame(tumor_a, tumor_b, tumor_c,tumor_d,tumor_e,tumor_f,tumor_g,tumor_h, tumor_i)
rownames(genes)=c("gene1","gene2","gene3","gene4","gene5","gene6","gene7","gene8","gene9")
genes=t(genes)
genes_pca=PCA(genes,  scale.unit=TRUE, ncp=9)
fviz_pca_biplot(genes_pca)


# Correspondence analysis example. Writers dataset.
writers=data.frame(expand.grid(Author=c("ChD1","ChD2","ChD3", "RD1", "RD2", "RD3", "ThH1", "ThH2","ThH3", "MSh1","MSh2", "MSh3",
                                        "MT1","MT2","MT3", "TextX1","TextX2"), chars=c("B", "C", "D", "F", "G","H","I","L","M","N","P","R","S","U","W","Y")), 
                   freq=c(34,18,32,13,8,9,15,18,19,13,17,13,16,15,19,24,19,37,33,43,31,28,34,20,14,18,29,34,22,18,21,17,26,33,44,47,36,55,34,43,28,40,41,49,43,43,56,66,70,
                          80,35,27,24,12,29,24,25,18,25,26,31,29,16,13,21,12,17,22,19,14,21,15,17,18,19,21,19,16,14,11,27,19,28,32,40,39,38,51,62,68,68,65,60,58,61,62,70,67,50,53,
                          91,96,74,66,75,74,75,84,82,70,64,73,64,68,61,62,72,86,116,44,41,33,43,34,25,34,15,18,36,26,46,43,50,39,54,39,27,36,23,28,25,32,29,37,38,29,26,35,20,24,22,
                          32,40,61,72,60,73,70,76,89,80,78,69,71,57,63,68,71,91,129,12,15,24,8,16,14,11,15,15,13,26,30,14,14,11,19,17,65,62,68,59,56,69,47,65,65,63,78,71,43,40,40,
                          58,72,69,63,85,54,72,64,74,68,72,58,64,57,67,58,67,93,104,22,31,18,32,31,27,18,21,20,18,21,19,34,31,25,50,30,14,12,13,19,14,11,22,25,20,20,18,22,41,36,41,
                          58,25,21,18,14,20,11,18,17,9,11,25,12,20,23,26,17,30,24))
writers_table=xtabs(freq~Author+chars, data=writers)
ca_writers=CA(writers_table,row.sup = c(16,17))
summary(ca_writers)
plot(ca_writers)
plot(ca_writers,selectRow="cos2 4", selectCol="cos2 3")
# Row plot
plot(ca_writers,invisible="col")
# Column plot
plot(ca_writers,invisible="row")
# Hierarchical clustering on principal components
hcpc_col=HCPC(ca_writers,cluster.CA="columns")
hcpc_rows=HCPC(ca_writers,cluster.CA="rows")
fviz_dend(hcpc_rows, show_labels = FALSE)
fviz_cluster(hcpc_rows,  main = "Factor map")