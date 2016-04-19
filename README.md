Readme File containing Description of Project Repo

Identifying shape change in the Upper and Lower jaw teeth within 3 species of Carcharhinus

Objectives of this Project: (1) if teeth could be quantitatively distinguished from one another on the species level, (2) the effect of species, jaw type, individual, and tooth position on shape, (3) are some teeth better than others for distinguishing among species, and (4) what landmarks attribute to the most variation in shape among species.

1: the objective of the project
This project is a portion of my graduate thesis, which is involved with characterizing tooth shape to various species of sharks. The goal of this in class project is aimed to see if there is a difference in tooth shape between 3 species of Carcharhinus. Many extant sharks can be identified on the basis of tooth morphology, which has been typically qualitative in nature.
This sorting “technique” has led to taxonomic problems in the fossil record, because there are several patterns of heterodonty that influence tooth shape. So here, I aim to characterize tooth shape to 3 different species and see if there are differences in tooth shape, and what variables influence tooth shape most. The predicting variables used in this study are: 1- species, 2- tooth position, 3- jaw type (upper and lower) , 4- individual.




2.	the structure of the code-base

The data set includes 3 species of Carcharhinus: altimus, brevipinna, and falciformis. These are all requeiem sharks, and when looking at the jaws of these species, they share a visually similar shape. There are 5 individuals (jaws) that represent each species (so data collected from 15 jaws overall). Tooth position is a categorical 
variable and represents each of the 15 positions of teeth moving from the front of the mouth to the back of the mouth (position 1 being the very first tooth in the mouth, position 15 being the last.) The upper and lower jaw of sharks are functionally and morphologically different, so the jaw type has been distinguished. (analyze your own mouth for a reference, your upper and lower jaw teeth are different, as well as the
teeth in the front are different from ones in the back).

The response variable, is shape. Shape is defined as the distribution of 14 landmarks plotted on each tooth (see figure 1a. for reference in paper). These 14 landmarks are plotted on each tooth using a program called MorphoJ. The shape information is all the geometric information about a configuration of landmarks on an object except for overall size, position, and orientation . These landmarks are discrete anatomical locations that are homologous in all individuals being considered. Shape information is extracted by using Procrustes superimposition which removes the information that is not about shape. Traditionally, shape has been a collection of angles, measurements, and or distances; however, advances in programming have shifted the procedure to analyzing Cartesian coordinates of specific anatomical landmarks. 
Each tooth was photographed, digitized, and a procrustes superimposition was applied too all 389 teeth. The coordinates that represent each tooth are now fitted to only describe information about shape. The 14 x/y coordinates are reprented in columns 6:33 in the sharkdat file (located in Shark Data), the header being ProcCoord1, 2,3,...28. For example: ProcCoord1 and ProcCoord 2 represent Landmark 1, x and y. To see the positions of landmarks on a tooth, analyze diagram attached below.

To analyze a visual of overall shape, the upper and lower jaw for each species has been extracted. A pca and Cva (2 ordination methods) have been done on the overall data set, along with an RDA to analyze overall variation and partitioning to each predictor. From here, I focused on the upper and lower jaws as the main predictors of variation and chose to cluster my data set using a calculated mahalanobis distance between the upper and lower jaws, to show that these are the main predictors of variation and shows the most separation in terms of clustering the data. From here, looking at the just the upper jaw, analyzing which landmark is the most variable across the 3 species to see which might landmark position(s) might be the cause of shape change in these 3 shark species.



The Scripts (found in scripts folder) should be Run in the order as Follows: 

1)Subsetting Shark Data 
2)Shark_Ordination
3) Shark_RDA
4) Shark_LDA
5) Shark_Euclidean
6) To go through the entire project flow in one mark down,use Shark Full 
Code_MrkDwn

The figures have been isolated in a separate folder named "Figures"

Comments for various sections of code have been labeled with a '#'. 
