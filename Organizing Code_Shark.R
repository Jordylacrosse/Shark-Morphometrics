Script just for organizing the code


#Read in the Shark data

```{r}

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