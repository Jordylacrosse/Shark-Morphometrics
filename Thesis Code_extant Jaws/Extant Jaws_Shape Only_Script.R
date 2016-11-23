
#For each species and jaw type, I want to look at just the average shape 
#for both jaw types. Basically visually shape change between landmarks 


#Species and Jaw: I want to find the mean x and y coordinates for each landmark position (14) for all upper jaw teeth, and lower jaw teeth for each species. 
#averaging individual specimen and tooth position for both jaw types 
#for each species.)

#just coordinates of upper and lower jaws for each species; take out metadata


acronotus<-   ExtantData[ExtantData$species=="Ca",]
altimus<-     ExtantData[ExtantData$species=="ca",]
brevipinna<-  ExtantData[ExtantData$species=="Cb",]
falciformis<- ExtantData[ExtantData$species=="Cf",]
longimanus<-  ExtantData[ExtantData$species=="Cg",]
leucas<-      ExtantData[ExtantData$species=="Cu",]
prionace<-    ExtantData[ExtantData$species=="Pg",]
lewini<-      ExtantData[ExtantData$species=="Sl",]
rhizo<-       ExtantData[ExtantData$species=="Rt",]
isodon<-      ExtantData[ExtantData$species=="Ci",]
limbatus<-    ExtantData[ExtantData$species=="Cl",]
obscurus<-    ExtantData[ExtantData$species=="Co",]
plumbeus<-    ExtantData[ExtantData$species=="Cp",]
taurus<-      ExtantData[ExtantData$species=="Ct",]
white<-       ExtantData[ExtantData$species=="Gw",]
oxy<-         ExtantData[ExtantData$species=="Io",]
tiburo<-      ExtantData[ExtantData$species=="St",]
paucus<-      ExtantData[ExtantData$species=="Ip",]


#acronotus

#subset jaws: upper and lower for altimus

upper_acronotus <- acronotus[acronotus$jaw.type=="U",]
lower_acronotus <- acronotus[acronotus$jaw.type=="L",]

upper_acronotus_coord <- upper_acronotus[ ,7:34 ]
lower_acronotus_coord <- lower_acronotus[ ,7:34 ]


acronotus_coords <- acronotus[ ,7:34]
xydat_acronotus_all <- get_xydat(acronotus_coords) #xy data for all acronotus teeth

upper_acronotus_coord <- upper_acronotus[ ,7:34 ]
xydat_acronotus_upper <- get_xydat(upper_acronotus_coord)

lower_acronotus_coord <- lower_acronotus[ ,7:34 ]
xydat_acronotus_lower <- get_xydat(lower_acronotus_coord)

----------------------------------------------------------------------

#altimus

upper_altimus <- altimus[altimus$jaw.type=="U",]
lower_altimus <- altimus[altimus$jaw.type=="L",]

upper_altimus_coord <- upper_altimus[ ,7:34 ]
lower_altimus_coord <- lower_altimus[ ,7:34 ]


altimus_coords <- altimus[ ,7:34]
xydat_altimus_all <- get_xydat(altimus_coords) #xy data for all altimus teeth

upper_altimus_coord <- upper_altimus[ ,7:34 ]
xydat_altimus_upper <- get_xydat(upper_altimus_coord)

lower_altimus_coord <- lower_altimus[ ,7:34 ]
xydat_altimus_lower <- get_xydat(lower_altimus_coord)

#Mean Upper and Lower Jaw Coordinates for Altimus. 


#find the mean of x and y for each landmark position
#mean coordinates for altimus upper jaw

x_avg_alt_upper = tapply(xydat_altimus_upper$x, xydat_altimus_upper$landmark, mean)
y_avg_alt_upper = tapply(xydat_altimus_upper$y, xydat_altimus_upper$landmark, mean)


#mean coordinates for lower jaw of altimus

x_avg_alt_lower <- tapply(xydat_altimus_lower$x, xydat_altimus_lower$landmark, mean)
y_avg_alt_lower <- tapply(xydat_altimus_lower$y, xydat_altimus_lower$landmark, mean)

#This gives you the mean coordinate for each landmark found on both the #upper and lower jaw teeth. Allows us to plot the overall shape (next step)
#mean shape, and analyze the distribution of landmarks between tooth 
#positions for upper and lower jaw teeth of altimus. This can be done
#for all jaws for all species, but this is just for visual sake. 


##average shape of Upper and lower jaw of Car. Altimus
#Upper Jaw

plot(x_avg_alt_upper, y_avg_alt_upper, pch=19, col='red', xlim=range(xydat_alti_upper$x), ylim=range(xydat_alti_upper$y)) #plot of mean shape. mean of each landmark
range(xydat_alti_upper$x)

points(xydat_alti_upper$x, xydat_alti_upper$y) #all landmarks ploted
plot(xydat_alti_upper$x, xydat_alti_upper$y)
points(x_avg_alt_upper, y_avg_alt_upper, pch=19, col='red') #mean plotted in red

#plot the mean shape of upper jaw teeth of altimus in Red, and the black 
#points represent the distribution of coordinates around each landmark.
#these are due to positional variation (position of tooth in the jaw. 
#indicated in data set by tooth.position in sharkdat)


#do the same for the Lower Jaw

plot(x_avg_alt_lower, y_avg_alt_lower, pch=19, col='red', xlim=range(xydat_alti_lower$x), ylim=range(xydat_alti_lower$y))
range(xydat_alti_lower$x)

points(xydat_alti_lower$x, xydat_alti_lower$y)
plot(xydat_alti_lower$x, xydat_alti_lower$y)
points(x_avg_alt_lower, y_avg_alt_lower, pch=19, col='red')

#when we do the same thing for the lower jaw of altimus, we can see that
#(visually) that the mean shape changes due to the jaw type *lower* and 
#the distribution of coordinates around each landmark changes. 

#we could do this same visual technique to analyze shape change for each species,
#but the focus is to look at shape change between the species. Not positional 
#change.

----------------------------------------------------------------------

#BREVIPINNA

#subset upper and lower jaws for brevipinna

upper_brevi <- brevipinna[brevipinna$jaw.type=="U",]
lower_brevi <-  brevipinna[brevipinna$jaw.type=="L",]

brevipinna_coords <- brevipinna[ , 7:34]
xydat_brevi_all <-  get_xydat(brevipinna_coords) #xy data for all brevi teeth

upper_brevi_coord <- upper_brevi[ ,7:34]
xydat_brevi_upper <-  get_xydat(upper_brevi_coord) #xy data upper brevi teeth

lower_brevi_coord <- lower_brevi[ ,7:34]
xydat_brevi_lower <- get_xydat(lower_brevi_coord) #xy data lower brevi teeth

----------------------------------------------------------------------
#FALCIFORMIS

#subset upper and lower jaws for falciformis

upper_falci <- falciformis[falciformis$jaw.type=="U",]
lower_falci <- falciformis[falciformis$jaw.type=="L",]

falciformis_coords <- falciformis[ ,7:34]
xydat_falci_all = get_xydat(falciformis_coords) #xy data for all falci

upper_falci_coord <- upper_falci[ ,7:34]
xydat_falci_upper <- get_xydat(upper_falci_coord) #xy data for just upper jaw of #falci

lower_falci_coord <- lower_falci[ ,7:34]
xydat_falci_lower <- get_xydat(lower_falci_coord) #xy data for lower jaw of falci


----------------------------------------------------------------------
  
#Longimanus

upper_longimanus <- longimanus[longimanus$jaw.type=="U",]
lower_longimanus <- longimanus[longimanus$jaw.type=="L",]

longimanus_coords <- longimanus[ ,7:34]
xydat_longimanus_all = get_xydat(longimanus_coords) #xy data for all longimanus

upper_longimanus_coord <- upper_longimanus[ ,7:34]
xydat_longimanus_upper <- get_xydat(upper_longimanus_coord) #xy data for just upper jaw of #longimanus

lower_longimanus_coord <- lower_longimanus[ ,7:34]
xydat_longimanus_lower <- get_xydat(lower_longimanus_coord) #xy data for lower jaw of longimanus

----------------------------------------------------------------------

#Leucas

upper_leucas <- leucas[leucas$jaw.type=="U",]
lower_leucas <- leucas[leucas$jaw.type=="L",]

upper_leucas_coord <- upper_leucas[ ,7:34 ]
lower_leucas_coord <- lower_leucas[ ,7:34 ]


leucas_coords <- leucas[ ,7:34]
xydat_leucas_all <- get_xydat(leucas_coords) #xy data for all acronotus teeth

upper_leucas_coord <- upper_leucas[ ,7:34 ]
xydat_leucas_upper <- get_xydat(upper_leucas_coord)

lower_leucas_coord <- lower_leucas[ ,7:34 ]
xydat_leucas_lower <- get_xydat(lower_leucas_coord)

----------------------------------------------------------------------

#prionace

  
upper_prionace <- prionace[prionace$jaw.type=="U",]
lower_prionace <- prionace[prionace$jaw.type=="L",]

upper_prionace_coord <- upper_prionace[ ,7:34 ]
lower_prionace_coord <- lower_prionace[ ,7:34 ]


prionace_coords <- prionace[ ,7:34]
xydat_prionace_all <- get_xydat(prionace_coords) #xy data for all acronotus teeth

upper_prionace_coord <- upper_prionace[ ,7:34 ]
xydat_prionace_upper <- get_xydat(upper_prionace_coord)

lower_prionace_coord <- lower_prionace[ ,7:34 ]
xydat_prionace_lower <- get_xydat(lower_prionace_coord)

----------------------------------------------------------------------

#lewini

  
upper_lewini <- lewini[lewini$jaw.type=="U",]
lower_lewini <- lewini[lewini$jaw.type=="L",]

upper_lewini_coord <- upper_lewini[ ,7:34 ]
lower_lewini_coord <- lower_lewini[ ,7:34 ]


lewini_coords <- lewini[ ,7:34]
xydat_lewini_all <- get_xydat(lewini_coords) #xy data for all lewini teeth

upper_lewini_coord <- upper_lewini[ ,7:34 ]
xydat_lewini_upper <- get_xydat(upper_lewini_coord)

lower_lewini_coord <- lower_lewini[ ,7:34 ]
xydat_lewini_lower <- get_xydat(lower_lewini_coord)


----------------------------------------------------------------------

#rhizo

  

upper_rhizo <- rhizo[rhizo$jaw.type=="U",]
lower_rhizo <- rhizo[rhizo$jaw.type=="L",]

upper_rhizo_coord <- upper_rhizo[ ,7:34 ]
lower_rhizo_coord <- lower_rhizo[ ,7:34 ]


rhizo_coords <- rhizo[ ,7:34]
xydat_rhizo_all <- get_xydat(rhizo_coords) #xy data for all rhizo teeth

upper_rhizo_coord <- upper_rhizo[ ,7:34 ]
xydat_rhizo_upper <- get_xydat(upper_rhizo_coord)

lower_rhizo_coord <- lower_rhizo[ ,7:34 ]
xydat_rhizo_lower <- get_xydat(lower_rhizo_coord)

----------------------------------------------------------------------

#isodon

  
  
upper_isodon <- isodon[isodon$jaw.type=="U",]
lower_isodon <- isodon[isodon$jaw.type=="L",]

upper_isodon_coord <- upper_isodon[ ,7:34 ]
lower_isodon_coord <- lower_isodon[ ,7:34 ]


isodon_coords <- isodon[ ,7:34]
xydat_isodon_all <- get_xydat(isodon_coords) #xy data for all isodon teeth

upper_isodon_coord <- upper_isodon[ ,7:34 ]
xydat_isodon_upper <- get_xydat(upper_isodon_coord)

lower_isodon_coord <- lower_isodon[ ,7:34 ]
xydat_isodon_lower <- get_xydat(lower_isodon_coord)

----------------------------------------------------------------------

#limbatus

upper_limbatus <- limbatus[limbatus$jaw.type=="U",]
lower_limbatus <- limbatus[limbatus$jaw.type=="L",]

upper_limbatus_coord <- upper_limbatus[ ,7:34 ]
lower_limbatus_coord <- lower_limbatus[ ,7:34 ]


limbatus_coords <- limbatus[ ,7:34]
xydat_limbatus_all <- get_xydat(limbatus_coords) #xy data for all isodon teeth

upper_limbatus_coord <- upper_limbatus[ ,7:34 ]
xydat_limbatus_upper <- get_xydat(upper_limbatus_coord)

lower_limbatus_coord <- lower_limbatus[ ,7:34 ]
xydat_limbatus_lower <- get_xydat(lower_limbatus_coord)

----------------------------------------------------------------------

#obscurus

upper_obscurus <- obscurus[obscurus$jaw.type=="U",]
lower_obscurus <- obscurus[obscurus$jaw.type=="L",]

upper_obscurus_coord <- upper_obscurus[ ,7:34 ]
lower_obscurus_coord <- lower_obscurus[ ,7:34 ]


obscurus_coords <- obscurus[ ,7:34]
xydat_obscurus_all <- get_xydat(obscurus_coords) #xy data for all isodon teeth

upper_obscurus_coord <- upper_obscurus[ ,7:34 ]
xydat_obscurus_upper <- get_xydat(upper_obscurus_coord)

lower_obscurus_coord <- lower_obscurus[ ,7:34 ]
xydat_obscurus_lower <- get_xydat(lower_obscurus_coord)

----------------------------------------------------------------------

#plumbeus


upper_plumbeus <- plumbeus[plumbeus$jaw.type=="U",]
lower_plumbeus<- plumbeus[plumbeus$jaw.type=="L",]

upper_plumbeus_coord <- upper_plumbeus[ ,7:34 ]
lower_plumbeus_coord <- lower_plumbeus[ ,7:34 ]


plumbeus_coords <- plumbeus[ ,7:34]
xydat_plumbeus_all <- get_xydat(plumbeus_coords) #xy data for all isodon teeth

upper_plumbeus_coord <- upper_plumbeus[ ,7:34 ]
xydat_plumbeus_upper <- get_xydat(upper_plumbeus_coord)

lower_plumbeus_coord <- lower_plumbeus[ ,7:34 ]
xydat_plumbeus_lower <- get_xydat(lower_plumbeus_coord)

----------------------------------------------------------------------

#tarus




  ----------------------------------------------------------------------

#white

upper_white <- white[white$jaw.type=="U",]
lower_white<- white[white$jaw.type=="L",]

upper_white_coord <- upper_white[ ,7:34 ]
lower_white_coord <- lower_white[ ,7:34 ]


white_coords <- white[ ,7:34]
xydat_white_all <- get_xydat(white_coords) #xy data for all isodon teeth

upper_white_coord <- upper_white[ ,7:34 ]
xydat_white_upper <- get_xydat(upper_white_coord)

lower_white_coord <- lower_white[ ,7:34 ]
xydat_white_lower <- get_xydat(lower_white_coord)

----------------------------------------------------------------------

#oxy

upper_oxy <- oxy[oxy$jaw.type=="U",]
lower_oxy<- oxy[oxy$jaw.type=="L",]

upper_oxy_coord <- upper_oxy[ ,7:34 ]
lower_oxy_coord <- lower_oxy[ ,7:34 ]


oxy_coords <- oxy[ ,7:34]
xydat_oxy_all <- get_xydat(oxy_coords) #xy data for all isodon teeth

upper_oxy_coord <- upper_oxy[ ,7:34 ]
xydat_oxy_upper <- get_xydat(upper_oxy_coord)

lower_oxy_coord <- lower_oxy[ ,7:34 ]
xydat_oxy_lower <- get_xydat(lower_oxy_coord)

----------------------------------------------------------------------
  
#tiburo

upper_tiburo <- tiburo[tiburo$jaw.type=="U",]
lower_tiburo<- tiburo[tiburo$jaw.type=="L",]

upper_tiburo_coord <- upper_tiburo[ ,7:34 ]
lower_tiburo_coord <- lower_tiburo[ ,7:34 ]


tiburo_coords <- tiburo[ ,7:34]
xydat_tiburo_all <- get_xydat(tiburo_coords) #xy data for all isodon teeth

upper_tiburo_coord <- upper_tiburo[ ,7:34 ]
xydat_tiburo_upper <- get_xydat(upper_tiburo_coord)

lower_tiburo_coord <- lower_tiburo[ ,7:34 ]
xydat_tiburo_lower <- get_xydat(lower_tiburo_coord)

----------------------------------------------------------------------

#paucus

upper_paucus <- paucus[paucus$jaw.type=="U",]
lower_paucus<- paucus[paucus$jaw.type=="L",]

upper_paucus_coord <- upper_paucus[ ,7:34 ]
lower_paucus_coord <- lower_paucus[ ,7:34 ]


paucus_coords <-paucusx[ ,7:34]
xydat_paucus_all <- get_xydat(paucus_coords) #xy data for all isodon teeth

upper_paucus_coord <- upper_paucus[ ,7:34 ]
xydat_paucus_upper <- get_xydat(upper_paucus_coord)

lower_paucus_coord <- lower_paucus[ ,7:34 ]
xydat_paucus_lower <- get_xydat(lower_paucus_coord)































```