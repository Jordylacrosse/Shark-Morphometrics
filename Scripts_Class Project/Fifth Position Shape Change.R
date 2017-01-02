##Extract Tooth Position 5 for the Upper Jaw for all species
#what landmarks attribute to the most variation in shape among species (euclidean distance)

upper<-ExtantData[ExtantData$jaw.type=="U",]
fifth <- upper[upper$tooth.position=="05",]

#extract just the 5th tooth for each species 
Ca5<-fifth[fifth$species=="Ca",]
ca5<-fifth[fifth$species=="ca",]
Cb5<-fifth[fifth$species=="Cb",]
Cf5<-fifth[fifth$species=="Cf",]
Pg5<-fifth[fifth$species=="Pg",]
Ci5<-fifth[fifth$species=="Ci",]
Cu5<-fifth[fifth$species=="Cu",]
Sl5<-fifth[fifth$species=="Sl",]
Cl5<-fifth[fifth$species=="Cl",]
Cg5<-fifth[fifth$species=="Cg",]
Co5<-fifth[fifth$species=="Co",]
Io5<-fifth[fifth$species=="Io",]
Ip5<-fifth[fifth$species=="Ip",]
Cp5<-fifth[fifth$species=="Cp",]
Rt5<-fifth[fifth$species=="Rt",]
St5<-fifth[fifth$species=="St",]



#just coordinates for the 5th position for each species
Ca5_coords <- Ca5[ ,7:34]
ca5_coords <- ca5[ ,7:34]
Cb5_coords <- Cb5[ ,7:34]
Cf5_coords <- Cf5[ ,7:34]
Pg5_coords <- Pg5[ ,7:34]
Ci5_coords <- Ci5[ ,7:34]
Cu5_coords <- Cu5[ ,7:34]
Sl5_coords <- Sl5[ ,7:34]
Cl5_coords <- Cl5[ ,7:34]
Cg5_coords <- Cg5[ ,7:34]
Co5_coords <- Co5[ ,7:34]
Io5_coords <- Io5[ ,7:34]
Ip5_coords <- Ip5[ ,7:34]
Cp5_coords <- Cp5[ ,7:34]
Rt5_coords <- Rt5[ ,7:34]
St5_coords <- St5[ ,7:34]


#mean of all 5th position
m5_coords <- fifth[ ,7:34]
xydat_m5 <- get_xydat(m5_coords)
x_avg_m5 <- tapply(xydat_m5$x, xydat_m5$landmark, mean)
y_avg_m5 <- tapply(xydat_m5$y, xydat_m5$landmark, mean)
#mean shape of all 
points(x_avg_m5, y_avg_m5, pch=19, col='grey')

#xy data matrix
xydat_Ca5 <- get_xydat(Ca5_coords)
xydat_ca5 <- get_xydat(ca5_coords)
xydat_Cb5 <- get_xydat(Cb5_coords)
xydat_Cf5 <- get_xydat(Cf5_coords)
xydat_Pg5 <- get_xydat(Pg5_coords)
xydat_Ci5 <- get_xydat(Ci5_coords)
xydat_Cu5 <- get_xydat(Cu5_coords)
xydat_Sl5 <- get_xydat(Sl5_coords)
xydat_Cl5 <- get_xydat(Cl5_coords)
xydat_Cg5 <- get_xydat(Cg5_coords)
xydat_Co5 <- get_xydat(Co5_coords)
xydat_Io5 <- get_xydat(Io5_coords)
xydat_Ip5 <- get_xydat(Ip5_coords)
xydat_Cp5 <- get_xydat(Cp5_coords)
xydat_Rt5 <- get_xydat(Rt5_coords)
xydat_St5 <- get_xydat(St5_coords)



#I'm plotting the tooth shape for each species, position 5 of the upper jaw,
#but the scaling of the axes are distorting some tooth shape.

#acronotus
x_avg_Ca5 <- tapply(xydat_Ca5$x, xydat_Ca5$landmark, mean)
y_avg_Ca5 <-tapply(xydat_Ca5$y, xydat_Ca5$landmark, mean)

plot(x_avg_Ca5, y_avg_Ca5, pch=19, col='red') #mean plotted in red

#altimus

x_avg_ca5 <- tapply(xydat_ca5$x, xydat_ca5$landmark, mean)
y_avg_ca5 <-tapply(xydat_ca5$y, xydat_ca5$landmark, mean)

points(x_avg_ca5, y_avg_ca5, pch=19, col='darkorange4')

#brevipinna
x_avg_Cb5 <- tapply(xydat_Cb5$x, xydat_Cb5$landmark, mean)
y_avg_Cb5 <-tapply(xydat_Cb5$y, xydat_Cb5$landmark, mean)

points(x_avg_Cb5, y_avg_Cb5, pch=19, col='blue')


#falciformis
x_avg_Cf5 <- tapply(xydat_Cf5$x, xydat_Cf5$landmark, mean)
y_avg_Cf5 <-tapply(xydat_Cf5$y, xydat_Cf5$landmark, mean)

points(x_avg_Cf5, y_avg_Cf5, pch=19, col='green')



#Isodon

x_avg_Ci5 <- tapply(xydat_Ci5$x, xydat_Ci5$landmark, mean)
y_avg_Ci5 <-tapply(xydat_Ci5$y, xydat_Ci5$landmark, mean)


points(x_avg_Ci5, y_avg_Ci5, pch=19, col='darkmagenta')

#Leucas

x_avg_Cu5 <- tapply(xydat_Cu5$x, xydat_Cu5$landmark, mean)
y_avg_Cu5 <-tapply(xydat_Cu5$y, xydat_Cu5$landmark, mean)


points(x_avg_Cu5, y_avg_Cu5, pch=19, col='blue')

#Lewini

x_avg_Sl5 <- tapply(xydat_Sl5$x, xydat_Sl5$landmark, mean)
y_avg_Sl5 <-tapply(xydat_Sl5$y, xydat_Sl5$landmark, mean)


points(x_avg_Sl5, y_avg_Sl5, pch=19, col='darkturquoise')

#Limbatus

x_avg_Cl5 <- tapply(xydat_Cl5$x, xydat_Cl5$landmark, mean)
y_avg_Cl5 <-tapply(xydat_Cl5$y, xydat_Cl5$landmark, mean)


points(x_avg_Cl5, y_avg_Cl5, pch=19, col='brown')


#Longimanus

x_avg_Cg5 <- tapply(xydat_Cg5$x, xydat_Cg5$landmark, mean)
y_avg_Cg5 <-tapply(xydat_Cg5$y, xydat_Cg5$landmark, mean)

points(x_avg_Cg5, y_avg_Cg5, pch=19, col='grey')


#obscurus

x_avg_Co5 <- tapply(xydat_Co5$x, xydat_Co5$landmark, mean)
y_avg_Co5 <-tapply(xydat_Co5$y, xydat_Co5$landmark, mean)

points(x_avg_Co5, y_avg_Co5, pch=19, col='darkslategray')



#plumbeus

x_avg_Cp5 <- tapply(xydat_Cp5$x, xydat_Cp5$landmark, mean)
y_avg_Cp5 <-tapply(xydat_Cp5$y, xydat_Cp5$landmark, mean)

points(x_avg_Cp5, y_avg_Cp5, pch=19, col='darkred')

#Rhiz
x_avg_Rt5 <- tapply(xydat_Rt5$x, xydat_Rt5$landmark, mean)
y_avg_Rt5 <-tapply(xydat_Rt5$y, xydat_Rt5$landmark, mean)

points(x_avg_Rt5, y_avg_Rt5, pch=19, col='darksalmon')


#Tiburo

x_avg_St5 <- tapply(xydat_St5$x, xydat_St5$landmark, mean)
y_avg_St5 <-tapply(xydat_St5$y, xydat_St5$landmark, mean)

points(x_avg_St5, y_avg_St5, pch=19, col='dodgerblue4')

-----------
#new plot with new scaling
  
#Paucus

x_avg_Ip5 <- tapply(xydat_Ip5$x, xydat_Ip5$landmark, mean)
y_avg_Ip5 <-tapply(xydat_Ip5$y, xydat_Ip5$landmark, mean)


plot(x_avg_Ip5, y_avg_Ip5, pch=19, col='deeppink4')

#Oxy

x_avg_Io5 <- tapply(xydat_Io5$x, xydat_Io5$landmark, mean)
y_avg_Io5 <-tapply(xydat_Io5$y, xydat_Io5$landmark, mean)

points(x_avg_Io5, y_avg_Io5, pch=19, col='darkslategray')



#Prionace

x_avg_Pg5 <- tapply(xydat_Pg5$x, xydat_Pg5$landmark, mean)
y_avg_Pg5 <-tapply(xydat_Pg5$y, xydat_Pg5$landmark, mean)

points(x_avg_Pg5, y_avg_Pg5, pch=19, col='yellow')



##Calculate the distance between those landmarks

#The distance between two landmarks is the square root of the sum of the 
#squared differences between each landmark. (Euclidean distance)
#This allows us to calculate a single value distance from one landmark to the 
#next in each species, so then we can use these to compare to the next species.
#We are going to calculate the Euclidean distance because it is a coordinate free approach for comparing biological shapes using landmark data.


#matrix of each species, upper jaw

acro <- cbind(x_avg_Ca5, y_avg_Ca5) 
alti <- cbind(x_avg_ca5, y_avg_ca5)
brevi <- cbind(x_avg_Cb5, y_avg_Cb5)
falci <- cbind(x_avg_Cf5, y_avg_Cf5) 
prio <- cbind(x_avg_Pg5, y_avg_Pg5)
iso<- cbind(x_avg_Ci5, y_avg_Ci5)
leuc<- cbind(x_avg_Cu5, y_avg_Cu5)
lew<- cbind(x_avg_Sl5, y_avg_Sl5)
limb<- cbind(x_avg_Cl5, y_avg_Cl5)
long<- cbind(x_avg_Cg5, y_avg_Cg5)
obscur<- cbind(x_avg_Co5, y_avg_Co5)
rinch<- cbind(x_avg_Io5, y_avg_Io5)
pauc<- cbind(x_avg_Ip5, y_avg_Ip5)
plum<- cbind(x_avg_Cp5, y_avg_Cp5)
rhz<- cbind(x_avg_Rt5, y_avg_Rt5)
tib<- cbind(x_avg_St5, y_avg_St5)


mean_5 <- cbind(x_avg_m5, y_avg_m5) #grey

#calculate the interlandmark distance between each landmark, for each species
#and for the mean
#(i.e: distance between landmark 1>2>3>4>5>6...14)

dist_acro <- vegdist(acro, method="euclidean")
dist_alti <- vegdist(alti, method="euclidean")
dist_brevi <- vegdist(brevi, method="euclidean")
dist_falci <- vegdist(falci, method="euclidean")
dist_prio <- vegdist(prio, method="euclidean")
dist_iso <- vegdist(iso, method="euclidean")
dist_leuc <- vegdist(leuc, method="euclidean")
dist_lew <- vegdist(lew, method="euclidean")
dist_limb <- vegdist(limb, method="euclidean")
dist_long <- vegdist(long, method="euclidean")
dist_obscur <- vegdist(obscur, method="euclidean")
dist_rinch <- vegdist(rinch, method="euclidean")
dist_pauc <- vegdist(pauc, method="euclidean") 
dist_plum <- vegdist(plum, method="euclidean")
dist_rhz <- vegdist(rhz, method="euclidean")
dist_tib <- vegdist(tib, method="euclidean")

dist_mean <- vegdist(mean_5, method="euclidean")


#compare to see how these distances differ from the mean (which landmarks attribute
#to the greatest change per/ species.) 


##Euclidean distance of species from mean (upper jaw, 5th pos.)

#I want to see what landmarks are attributing to the most change in tooth shape
#for each species ( from mean reference shape, to target shape:species)
#Shape change in the 5th position, upper jaw only
#difference of acronotus from mean

#Using the mean is not really helpful because we have such a wide range of shapes 
#across species

apply(mean_5 - acro,1,function(x){sqrt(x[1]^2+x[2]^2)})

#difference of brevipinna from mean
apply(mean_5 - brevi,1,function(x){sqrt(x[1]^2+x[2]^2)})

#difference of falciformis from mean
apply(mean_5 - falci,1,function(x){sqrt(x[1]^2+x[2]^2)})


adist = apply(mean_5 - acro,1,function(x){sqrt(x[1]^2+x[2]^2)})
bdist = apply(mean_5 - brevi,1,function(x){sqrt(x[1]^2+x[2]^2)})
fdist = apply(mean_5 - falci,1,function(x){sqrt(x[1]^2+x[2]^2)})
cbind(adist,bdist,fdist)

apply(cbind(adist,bdist,fdist),1,sd)

#so we can see that the 4th landmark (the tip of the blade) contributes to
#the most change in shape with regards to species in the upper jaw.
#we can see a considerable amount of variation due to landmark 3, 10, and 13 as well.

 
-----------------------------------------

#find the landmark distance between very similar species (tooth shape)
##Euclidean distance of species from closely related species deducted from
#mahalanobis dendrogram on previous analysis (upper jaw, 5th pos. only)

#first cluster of : Cu, Cg, Co (uppers only)
obscurdist <- apply(obscur - long,1,function(x){sqrt(x[1]^2+x[2]^2)})
longdist <- apply(long - leuc,1,function(x){sqrt(x[1]^2+x[2]^2)})
leucdist <- apply(leuc - obscur,1,function(x){sqrt(x[1]^2+x[2]^2)})

cbind(obscurdist,longdist,leucdist)


#Rt, Ca, Cb

brevidist <- apply(brevi - acro,1,function(x){sqrt(x[1]^2+x[2]^2)})
rhizodist  <- apply(rhz - brevi,1,function(x){sqrt(x[1]^2+x[2]^2)})
acronotusdist <- apply(acro - rhz,1,function(x){sqrt(x[1]^2+x[2]^2)})

cbind(brevidist, rhizodist, acronotusdist)



#difference between paucus and oxyrinchus

apply(pauc - rinch,1,function(x){sqrt(x[1]^2+x[2]^2)})


#isodon and limbatus (uppers)

apply(iso - limb,1,function(x){sqrt(x[1]^2+x[2]^2)})


