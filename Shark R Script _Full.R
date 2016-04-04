#Read in the Shark data

```{r}
library(vegan)
library(dummies)

sharkdat <- read.delim("~/Shark-Teeth-Morphometrics/sharkdat.txt")
View(sharkdat)

```
#Subset species and jaw type.
#The three species below are in the genus
#Carcharhinus or requiem sharks. 

```{r} 

altimus<-sharkdat[sharkdat$species=="Ca",]
brevipinna<-sharkdat[sharkdat$species=="Cb",]
falciformis<-sharkdat[sharkdat$species=="Cf",]

#subset of upper and lower jaws (includes all species)

upper<-sharkdat[sharkdat$jaw.type=="U",]
lower<-sharkdat[sharkdat$jaw.type=="L",]

upper_coord<- upper[, 6:33]
lower_coord <- lower[, 6:33]

#coordinates of just altimus sp. 

altimus_coords <- altimus[ ,6:33]
upper_alt_coord <- upper_alt[ ,6:33 ]
lower_alt_coord <- lower_alt[ ,6:33 ]

#subset jaw and tooth from the entire dataset

jaw <- as.factor(sharkdat$jaw.type)
tooth <- as.factor(sharkdat$tooth.position)

#subset jaws: upper and lower for altimus

upper_alt <- altimus[altimus$jaw.type=="U",]
lower_alt <-  altimus[altimus$jaw.type=="L",]

#subset upper and lower jaws for brevipinna

upper_brevi <- brevipinna[brevipinna$jaw.type=="U",]
lower_brevi <-  brevipinna[brevipinna$jaw.type=="L",]

#subset upper and lower jaws for falciformis

upper_falci <- falciformis[falciformis$jaw.type=="U",]
lower_falci <- falciformis[falciformis$jaw.type=="L",]

```
#Loop to edit data frame into matrix

```{r}

#fucntion for transforming data frame into a matrix

get_xydat = function(ct_mat) {
  if (class(ct_mat) != 'matrix') {
    ct_mat = as.matrix(ct_mat)
    warning('Converting input to a matrix')
  }
  nmarkers =  ncol(ct_mat) /2 
  xy_mat = matrix(NA, nrow= nrow(ct_mat) * nmarkers, ncol=4)
  colnames(xy_mat) = c('indiv', 'landmark', 'x','y')
  startrow = 1
  for(i in 1:nrow(ct_mat)) { 
    x = ct_mat[i, seq(1, ncol(ct_mat) - 1, 2)]
    y = ct_mat[i, seq(2, ncol(ct_mat), 2)]
    endrow = i * nmarkers
    xy_mat[startrow:endrow , ] = cbind(rep(i, nmarkers), 1:nmarkers, x,y)
    startrow = endrow + 1
  }
  xy_mat = as.data.frame(xy_mat)
  return(xy_mat)
}

```

#All Species Coordinate Data

```{r}

#all coordinates for entire shark data set

coords = sharkdat[ , 6:33]
all_shark_coords <- get_xydat(coords)
all_shark_coords

head(all_shark_coords) 

tail(all_shark_coords)

all_x <- all_shark_coords$x
all_y <- all_shark_coords$y



```

#just coordinates of upper and lower jaws for each species; take out metadata

```{r}


#ALTIMUS

altimus_coords <- altimus[ ,6:33]
xydat_alti_all <- get_xydat(altimus_coords) #xy data for all altimus teeth

upper_alt_coord <- upper_alt[ ,6:33 ]
xydat_alti_upper <- get_xydat(upper_alt_coord)

lower_alt_coord <- lower_alt[ ,6:33 ]
xydat_alti_lower <- get_xydat(lower_alt_coord)


#BREVIPINNA

brevipinna_coords <- brevipinna[ , 6:33]
xydat_brevi_all <-  get_xydat(brevipinna_coords) #xy data for all brevi teeth

upper_brevi_coord <- upper_brevi[ ,6:33]
xydat_brevi_upper <-  get_xydat(upper_brevi_coord) #xy data upper brevi teeth

lower_brevi_coord <- lower_brevi[ ,6:33]
xydat_brevi_lower <- get_xydat(lower_brevi_coord) #xy data lower brevi teeth


#FALCIFORMIS

falciformis_coords <- falciformis[ ,6:33]
xydat_falci_all = get_xydat(falciformis_coords) #xy data for all falci

upper_falci_coord <- upper_falci[ ,6:33]
xydat_falci_upper <- get_xydat(upper_falci_coord) #xy data for just upper jaw of 
#falci

lower_falci_coord <- lower_falci[ ,6:33]
xydat_falci_lower <- get_xydat(lower_falci_coord) #xy data for lower jaw of falci

```
#Mean Upper and Lower Jaw Coordinates for Altimus. 

```{r}

xydat_alti_upper
head(xydat_alti_upper)
class(xydat_alti_upper)
tapply(xydat_alti_upper$x, xydat_alti_upper$landmark, mean)

#mean coordinates for altimus upper jaw

x_avg_alt_upper = tapply(xydat_alti_upper$x, xydat_alti_upper$landmark, mean)
y_avg_alt_upper = tapply(xydat_alti_upper$y, xydat_alti_upper$landmark, mean)

tapply(xydat_alti_upper$x, xydat_alti_upper$landmark, sd)

#mean coordinates for lower jaw of altimus

x_avg_alt_lower <- tapply(xydat_alti_lower$x, xydat_alti_lower$landmark, mean)
y_avg_alt_lower <- tapply(xydat_alti_lower$y, xydat_alti_lower$landmark, mean)

```

##average shape of Upper and lower jaw of Car. Altimus

```{r}
#Upper Jaw

plot(x_avg_alt_upper, y_avg_alt_upper, pch=19, col='red', xlim=range(xydat_alti_upper$x),
     ylim=range(xydat_alti_upper$y)) #plot of mean shape. mean of each landmark
range(xydat_alti_upper$x)

points(xydat_alti_upper$x, xydat_alti_upper$y) #all landmarks ploted
plot(xydat_alti_upper$x, xydat_alti_upper$y)
points(x_avg_alt_upper, y_avg_alt_upper, pch=19, col='red') #mean plotted in red


#Lower Jaw

plot(x_avg_alt_lower, y_avg_alt_lower)
plot(x_avg_alt_lower, y_avg_alt_lower, pch=19, col='red')

points(xydat_alti_lower$x, xydat_alti_lower$y)
plot(x_avg_alt_lower, y_avg_alt_lower, pch=19, col='red', xlim=range(xydat_alti_lower$x), ylim=range(xydat_alti_lower$y))
range(xydat_alti_lower$x)

points(xydat_alti_lower$x, xydat_alti_lower$y)
plot(xydat_alti_lower$x, xydat_alti_lower$y)
points(x_avg_alt_lower, y_avg_alt_lower, pch=19, col='red')

```

##PCA

```{r}

upper<-sharkdat[sharkdat$jaw.type=="U",]
lower<-sharkdat[sharkdat$jaw.type=="L",]

upper_coord<- upper[, 6:33]
lower_coord <- lower[, 6:33]


#Run PCA on all of the shark data; 3 species

coords = sharkdat[ , 6:33]
row.names(coords) = sharkdat$Id
pca = princomp(coords)  #pca of all coordainates
biplot(pca, cex=.5)
pca$loadings
summary(pca)

plot(pca)


#look at pca of upper and lower jaw 

upper_coord
row.names(upper_coord) <- upper$species
pca_upper <- princomp(upper_coord)
biplot(pca_upper, cex = .5)

lower_coord
row.names(lower_coord) <- lower$species
pca_lower <- princomp(lower_coord)
biplot(pca_lower, cex= .5)

```

##RDA

```{r}

all_rda <- rda(coords ~ sharkdat$species + as.factor(sharkdat$jaw.type) +
                 as.factor(sharkdat$tooth.position))

anova(all_rda, by="margin")

RsquareAdj(all_rda)

plot(all_rda, type='n', scaling=1)
orditorp(all_rda, display='sp', cex=0.5, scaling=1, col='blue')
text(all_rda, display='bp', col='red')

#VarPart for Variance Partitioning

sp_dum = dummy(sharkdat$species)
jaw_dum = dummy(sharkdat$jaw.type)
pos_dum = dummy(sharkdat$tooth.position)

varpart(coords, sp_dum, jaw_dum, pos_dum)
showvarparts(3)

```

##Cva using Package Morpho

```{r}

install.packages("Morpho")
library(Morpho)

coords <- sharkdat[ , 6:33]
factor <- sharkdat[ ,4]

#get the probabilities and resulting classifications

cva_1 <- CVA(coords, groups=factor, weighting = TRUE, tolinv = 1e-10, plot = TRUE,
             rounds = 0, cv = FALSE, p.adjust.method = "none")

typprobs <- typprobClass(cva_1$CVscores,groups=factor)
print(typprobs)


```

##LDA of entire (all) matrix by species

```{r}
library(MASS)


upper_coord<- upper[, 6:33]
lower_coord <- lower[, 6:33]

jaw_type <-as.factor(upper[ ,4])


#make a matrix of all coords, then separate matrix to upper and lower.

matshark <- as.matrix(coords)

matshark_upper <- as.matrix(upper_coord)
matshark_lower <- as.matrix(lower_coord)


#overall lda

shark_lda <- lda(matshark, as.factor(sharkdat[ ,2]))
plot(shark_lda, col = 'blue')


```

##Mahalanobis Distance Between Shark Species

```{r}
meangroup <- shark_lda$mean
dist(predict(shark_lda, meangroup)$x)

#find optimal number of clusters for dataset

p<-as.matrix(coords)

totv <- sum(diag(var(p[, 1:28])))*388
SSratio <- numeric(10)
for (i in 2:10)
{mod <- kmeans(p[, 1:28],i)
SSratio[i]<-(totv-sum(mod$withinss))/totv}


plot(1:10, SSratio, xlab="number of clusters", ylab="% of explained variance")
points(1:10, c(0, SSratio[2:10]), pch=3)

```
##Mahalanobis Distance Between Shark Species

```{r}
meangroup <- shark_lda$mean
dist(predict(shark_lda, meangroup)$x)

```
#Clustering our Data Set; Cluster Analysis

```{r}
totv <- sum(diag(var(p[, 1:28])))*387
SSratio <- numeric(10)
for (i in 2:10)
{mod <- kmeans(p[, 1:28],i)
SSratio[i]<-(totv-sum(mod$withinss))/totv}

#Plot using k means method to estimate ideal number of clusters

plot(1:10, SSratio, xlab="number of clusters", ylab="% of explained variance")
points(1:10, c(0, SSratio[2:10]), pch=3)


library("clusterGeneration", lib.loc="~/R/win-library/3.1")
pam(p[,1:28], 6)$clustering

plot(pam(x=p[,1:28], k=6), main= "", col.p="black")

pam(x=p[,1:28], 2)$clustering
plot(pam(x=p[,1:28], k=2), main= "", col.p="black")

```
#LDA of Species, Upper and Lower Jaws Separated. 

```{r}
upper_coord<- upper[, 6:33]
lower_coord <- lower[, 6:33]

jaw_type <-as.factor(upper[ ,4])

matshark <- as.matrix(coords)
matshark_upper <- as.matrix(upper_coord)
matshark_lower <- as.matrix(lower_coord)

#Upper

upper_lda <- lda(matshark_upper, as.factor(upper[ ,2]))
plot(lda(matshark_upper, as.factor(upper[ ,2])))

#Lower

lower_lda <- lda(matshark_lower, as.factor(lower[ ,2]))
plot(lda(matshark_lower, as.factor(lower[ ,2])))

```

#Mahalanobis Distance Between Upper jaws species and lower jaws species

```{r}
uppermean <- upper_lda$mean
dist(predict(upper_lda, uppermean)$x)


lowermean <- lower_lda$mean
dist(predict(lower_lda, lowermean )$x)

```
##Extract Tooth Position 5 for the Upper Jaw for 3 species

```{r}
upper<-sharkdat[sharkdat$jaw.type=="U",]
fifth <- upper[upper$tooth.position=="5",]

#extract just the 5th tooth for each species 
a5<-fifth[fifth$species=="Ca",]
b5<-fifth[fifth$species=="Cb",]
f5<-fifth[fifth$species=="Cf",]


#just coordinates
a5_coords <- a5[ ,6:33]
b5_coords <- b5[ ,6:33]
f5_coords <- f5[ ,6:33]

#mean of all 5th position
m5_coords <- fifth[ ,6:33]

#xy data matrix
xydat_a5 <- get_xydat(a5_coords)
xydat_b5 <- get_xydat(b5_coords)
xydat_f5 <- get_xydat(f5_coords)

xydat_m5 <- get_xydat(m5_coords)

#altimus
x_avg_a5 <- tapply(xydat_a5$x, xydat_a5$landmark, mean)
y_avg_a5 <-tapply(xydat_a5$y, xydat_a5$landmark, mean)

#brevipinna
x_avg_b5 <- tapply(xydat_b5$x, xydat_b5$landmark, mean)
y_avg_b5 <-tapply(xydat_b5$y, xydat_b5$landmark, mean)

#falciformis
x_avg_f5 <- tapply(xydat_f5$x, xydat_f5$landmark, mean)
y_avg_f5 <-tapply(xydat_f5$y, xydat_f5$landmark, mean)


#mean
x_avg_m5 <- tapply(xydat_m5$x, xydat_m5$landmark, mean)
y_avg_m5 <- tapply(xydat_m5$y, xydat_m5$landmark, mean)
```

#Plot the 5th position for each species

```{r}
#altimus
plot(x_avg_a5, y_avg_a5, pch=19, col='red') 

#brevi
points(x_avg_b5, y_avg_b5, pch=19, col='blue')
#falci
points(x_avg_f5, y_avg_f5, pch=19, col='green')
#mean shape of all 
points(x_avg_m5, y_avg_m5, pch=19, col='grey')
```

```{r}

#matrix of each species, upper jaw

alti <- cbind(x_avg_a5, y_avg_a5) #red
brevi <- cbind(x_avg_b5, y_avg_b5) #blue
falci <- cbind(x_avg_f5, y_avg_f5) #grenen
mean_5 <- cbind(x_avg_m5, y_avg_m5) #grey

#calculate the interlandmark distance between each landmark, for each species
#and for the mean
#(i.e: distance between landmark 1>2>3>4>5>6...14)

dist_alti <- vegdist(alti, method="euclidean")
dist_alti <- vegdist(brevi, method="euclidean")
dist_alti <- vegdist(falci, method="euclidean")

dist_mean <- vegdist(mean_5, method="euclidean")

```
compare to see how these distances differ from the mean (which landmarks attribute
                                                         to the greatest change per/ species.) 

##Euclidean distance of species from mean (upper jaw, 5th pos.)

```{r}

#difference of altimus from mean
apply(mean_5 - alti,1,function(x){sqrt(x[1]^2+x[2]^2)})

#difference of brevipinna from mean
apply(mean_5 - brevi,1,function(x){sqrt(x[1]^2+x[2]^2)})

#difference of falciformis from mean
apply(mean_5 - falci,1,function(x){sqrt(x[1]^2+x[2]^2)})


adist = apply(mean_5 - alti,1,function(x){sqrt(x[1]^2+x[2]^2)})
bdist = apply(mean_5 - alti,1,function(x){sqrt(x[1]^2+x[2]^2)})
bdist = apply(mean_5 - brevi,1,function(x){sqrt(x[1]^2+x[2]^2)})
fdist = apply(mean_5 - falci,1,function(x){sqrt(x[1]^2+x[2]^2)})
cbind(adist,bdist,fdist)

apply(cbind(adist,bdist,fdist),1,sd)

```
