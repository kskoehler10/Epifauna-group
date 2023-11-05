#####
##### Karl's attempt at wrangling the data for subsequent analyses
#####
library(dplyr)


### Load most recent epifauna data
### See Carmen's code and readme on google drive for the detailed formation of this dataset. 

### Looking at carmens new data set posted 8/24/2023 on google drive
taxa1 <- read.csv("C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/20230824_allepifauna_wide_macrophyte.csv")
nrow(taxa1) #1017
ncol(taxa1) #345


## First need to deal with some duplicate row names. I found the appropriate row names from data sheet scans on Box and fixed them.

# There is a BB row (BB.A.L5.m16.2019) that appears to be mislabeled, thereby creating a duplicate.
# There are more elegant ways to fix this but this works.
taxa1[149,1]<-"BB.A.L4.m16.2019"

# Same thing happened with WA.A.U2.m16.2019.
taxa1[876,1]<-"WA.A.U1.m16.2019"

# and WA.D.U3.m4.2021
taxa1[983,1]<-"WA.E.U3.m4.2021"

## Now make samples our rowname
rownames(taxa1)<-taxa1$quadrat_unique_code
taxa1<-taxa1[-1]


nrow(taxa1)###1017 samples
ncol(taxa1)## 344


#####################
####ORGANIZE ROWS####
#####################

### Remove species we didn't target that we don't want in nay analyses (ostracods, cumaceans, fish, etc)
##also remove nonsense columns and empty taxa
## Used the following before but it stopped working for me#  taxa1<-taxa1%>% select(-c(Allorchestes.sp.,bivalve.D,Cumella.vulgaris,EMPTY,fish,Fish.larva,Neanthes.sp.,Nippoleucon.hinumensis,Pipefish))
#### taxa1<-taxa1[-c(Allorchestes.sp.,bivalve.D,Cumella.vulgaris,EMPTY,fish,Fish.larva,Neanthes.sp.,Nippoleucon.hinumensis,Pipefish)]
###I have removed 9 columns so far but there are probably others to get rid of
taxa1<-taxa1[ ,!names(taxa1) %in% c("Allorchestes.sp.","bivalve.D","Cumella.vulgaris","EMPTY","fish","Fish.larva","Neanthes.sp.","Nippoleucon.hinumensis","Pipefish")]

###Rename any taxa whose nomenclature is not current with WORMS####

## Carmen already fixed this in the 8/24/2023 version of data
###I did not do a thorough search, just happened to find these
#taxa1<-taxa1 %>%rename(Mitrella.gausapata = Astyris.gausapata,Angustassiminea.californica=Assiminea.californica,Megasyllis.nipponica=Typosyllis.nipponica)
### I will also rename some things that we've putatively improved the ID's of since data was collected.
## There were more of these to do in previous versions of dataset
#taxa1<-taxa1 %>%rename(Nassarius.tiarula=gastropod.N) 

## Combine some columns of unknowns with their updated and preexisting identities
taxa1$Zygonemertes.sp.<-taxa1$Zygonemertes.sp.+taxa1$green.worm.thing
taxa1<-taxa1[ ,!names(taxa1) %in% c("green.worm.thing")]
taxa1$Oxydromus.pugettensis<-taxa1$Oxydromus.pugettensis+taxa1$Hesionidae
taxa1<-taxa1[ ,!names(taxa1) %in% c("Hesionidae")]


##################################
####Get rid of suspect samples####
##################################

## Site SD.C in 2020 had no eelgrass so there are NA's in macrophyte columns and 
## I would consider the epifaunal samples inconsistent given that there was no eelgrass to collect.
## I personally took these samples and recommend that we do not include them
missing.macro <- row.names(taxa1 %>%
                             filter(is.na(macrophyte_wet_mass_g)))
missing.macro  ## there were more sites from OR.A 2021 missing macrophyte mass in previous versions but Carmen fixed them
taxa1<-taxa1[ !row.names(taxa1) %in% missing.macro,]


### Combine drop trap samples by transect
## Since we only have structural data at the transect level it will probably make sense to lump data at that level
taxa.transect1<-aggregate(taxa1[-c(4:7)],by=list(transect_unique_code=taxa1$transect_unique_code),sum)
rownames(taxa.transect1)<-taxa.transect1$transect_unique_code
taxa.transect1<-taxa.transect1[-1]

## I had to recreate the region, site, and year columns
taxa.transect1$region<-substr(rownames(taxa.transect1),1,2)
taxa.transect1$site<-substr(rownames(taxa.transect1),4,4)
taxa.transect1$year<-substr(rownames(taxa.transect1),9,12)
### And a site unique code for when we want to ignore region
taxa.transect1$site.unique.no.year<-substr(rownames(taxa.transect1),1,4)

## This line removes site SD.A.U3.2020 from epifauna data (and subsequently env data) because there is no 
# corresponding environmental data for this transect.
taxa.transect1<-taxa.transect1[ !row.names(taxa.transect1) %in% c("SD.A.U3.2020"),]


## get rid of broad taxonomic categories that include small counts
taxa.transect.large<-taxa.transect1[ ,!names(taxa.transect1) %in% c("mollusca_abundance_all","crustacea_abundance_all","amphipod_abundance_all",
                                                                    "gastropod_abundance_all","isopod_abundance_all","ampithoid_abundance_all","idoteid_abundance_all","lacuna_abundance_all")]
## Remove sums that include small counts
taxa.transect.large<-taxa.transect.large[ ,!names(taxa.transect.large) %in% c("richness_sample_all","richness_site_all","epifauna_abundance_total")]
### This data set could be useful for looking at some large only community level data but I will move on and ignore it for now.


####################################################################
### Next step is to create a data frame that includes only taxa ####
####################################################################

## get rid of all broad taxonomic categories
taxa.only.transect<-taxa.transect.large[ ,!names(taxa.transect.large) %in% c("mollusca_abundance_large","crustacea_abundance_large","amphipod_abundance_large",
                                                                             "gastropod_abundance_large","isopod_abundance_large","ampithoid_abundance_large","idoteid_abundance_large","lacuna_abundance_large")]

# And make a data frame of only taxa for diversity measures etc
taxa.only.transect<-taxa.only.transect[ ,!names(taxa.only.transect) %in% c("Seagrass.WetMass.g",	"Macroalgae.WetMass.g","macrophyte_wet_mass_g","site_unique_code","transect_unique_code",
                                                                           "richness_sample_large","richness_site_large","epifauna_abundance_large","region","site","year","site.unique.no.year")]

## Should we remove the one sample with no epifauna? I'll refrain for now
## Check for rows with no epifauna
#no.epi2<-names(which (rowSums(taxa.only.transect) == 0))
##and remove those samples from taxa onlyh data data
#taxa.only.transect<-taxa.only.transect[ !row.names(taxa.only.transect) %in% no.epi2,]
# and from basic taxa dataframe

dim(taxa.only.transect) ## 508,304

############################################
##### This data frame contains only taxa observed at the transect level per year and is suitable for calculating diversity indices #####
write.csv(taxa.only.transect,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/taxa.only.transectv2.csv")

## Create transformed version of taxa only
sqrt.taxa.only.transect<-sqrt(taxa.only.transect)

### If we want to standardize abundance per sample by eelgrasss biomass use this data
taxa.only.transect.biomass<-as.data.frame(as.matrix(taxa.only.transect)/as.vector(taxa.transect1$macrophyte_wet_mass_g))
write.csv(taxa.only.transect.biomass,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/taxa.only.transect.biomass.csv")


##### Now we can calculate diversity indices and insert into transect level data####
taxa.transect1$simpsons<-diversity(taxa.only.transect,index="simpson")
taxa.transect1$shannon<-diversity(taxa.only.transect,index="shannon")

## Using sqrt transformed abundances
taxa.transect1$simpsons.sqrt<-diversity(sqrt.taxa.only.transect,index="simpson")
taxa.transect1$shannon.sqrt<-diversity(sqrt.taxa.only.transect,index="shannon")

##Abundance
taxa.transect1$abundance<-rowSums(taxa.only.transect)

##Richness
taxa.transect1$richness<-apply(taxa.only.transect>0,1,sum)


########################################
##### Univariate taxanomic data set ####
########################################

#### This data can be used for univariate community level statistics
write.csv(taxa.transect1,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/taxa.transect1.csv")




######################################################
####### Environmental data ######
######################################

env<-read.csv("C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/EGWD_transect_metrics_20230925.csv",header=TRUE,row.names=56)

## Keep only rows that are represnted in epifauna data
rmlist3<-as.list(row.names(taxa.only.transect))
length(rmlist3)
dim(env)
env<-env[ (row.names(env) %in% rmlist3),]
dim(env)

## Make a data frame containing env data and some univariate metrics we might want to model
transect.uni1<-cbind(env[-c(78:82,84)],taxa.transect1[c(1:3,308:339)])
write.csv(transect.uni1,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/transect.uni1.csv")




#####READY FOR BASIC UNIVARIATE STATS#####
### At this point we should be able to calculate accurate richness and abundance measures
## This assumes that all taxa columns remaining are not redundant, 
## or if they are (e.x. Ericthonius sp. and Ericthonius brasiliensis) that the redundant taxa are
## never both encountered in the same sample ##

## We might want to use inverse Simpson's for diveristy to downweight the numerous rare taxa whose IDs
## are often uncertain. (Boye et al. 2017; Hill 1973)

################################
###############################


#######PREPARE DATA FOR MULTIVARIATE ANALYSES###########

#### First, we need to choose some sort of cutoff threshold to remove the very rare taxa from our data.
## This will reduce noise in our community composition data and also conveniently get rid of most of 
## the taxa which we do not have good IDs for.

#### Many studies do not employ a cutoff value. That being said, many of these studies have lower
## diversity or only ID to the family level. We have a lot of unknown signletons and very rare taxa 
## which I do not think we have enough information about to make intelligible conclusions so I
## think we need some cutoff.

### There are different thresholds used in the literature.

## Murphy et al. (2021) exclude taxa with less than 5 individuals across the study, unidentified taxa,
## and taxa not strictly considered epifauna.

## Whippo et al. 2018 got rid of all taxa they did not consider to be strictly "epifaunal"

## The PRIMER manual by Clarke et al. suggests using including only taxa that make up at least 
## (q > 1%-3%) of any single sample in the study. See Field et al. 1982 as well.

## Transforming the data will also affect the weight of rare species in subsequent analyses.
## Square root and log transformations downweight highly abundant taxa and elevate rare taxa.
## Hellinger transformation reduces the weight of rare taxa
## (Boye et al. 2017; Legendre and Gallagher 2001)

##### I will use a q value of 3% below
dim(taxa.only.transect)

abund.vec<-as.vector(taxa.transect1$abundance)
taxamat1<-as.matrix(taxa.only.transect)
length(abund.vec)
dim(taxamat1)

### produce a matrix of the percentage each species makes up of each sample it is found in.
taxamat2<-taxamat1/(abund.vec)
taxamat2[is.na(taxamat2)] <- 0
dim(taxamat2)
### Find the maximum proportion it makes up of any sample in the study
clmx1<-apply(taxamat2,MARGIN=2,max)
glimpse(clmx1)
### Define q value here.
clmx2<-clmx1[clmx1<.03]
glimpse(clmx2)
### create list of taxa to be included
rmlist1<-names(clmx2)
length(rmlist1)
dim(taxa.only.transect)
### create new data frame with only taxa above threshold
taxa.sub1<-taxa.only.transect[ , !(names(taxa.only.transect) %in% rmlist1)]

## How much does this cut out?
dim(taxa.sub1)  ## Leaves us with 144 taxa (removes 157 taxa)
taxa.removed<-taxa.only.transect[ , (names(taxa.only.transect) %in% rmlist1)]
dim(taxa.removed)
total.removed<-sum(rowSums(taxa.removed)) ##only 836 individuals removed from data
total.removed
total.removed/sum(taxa.transect1$abundance) ## Which amounts to less than 1 percent of total abundance


write.csv(taxa.sub1,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/taxa.sub1.csv")



#### To compare community composition between samples we need to make sure that there are
## no duplicate taxa represented by different names in different regions.

#### For example  Ericthonius sp. (as they are called in BB) and Ericthonius brasiliensis (as they are called elsewhere) 
## should probably be treated as synonymous and combined into a single column.
#### For situations where there is one species level ID in some sites that corresponds to a genus level
## ID in other sites, I will use the genus level ID for all cases.

#### For situations where there are columns for a genus and multiple species within that genus
## I will combine into one genus level ".spp" column

spe2<-taxa.sub1

spe2$Acteocina.sp.<-spe2$Acteocina.sp.+spe2$Acteocina.inculta
spe2<-spe2[ ,!names(spe2) %in% c("Acteocina.inculta")]

spe2$Ampithoe.spp<-spe2$Ampithoe.sp.+spe2$Ampithoe.valida+spe2$Ampithoe.lacertosa
rm.ampithoe<-c("Ampithoe.sp.","Ampithoe.valida","Ampithoe.lacertosa")
spe2<-spe2[ ,!names(spe2) %in% rm.ampithoe]

#Also conflict with Aoridae
spe2$Aoroides.spp<-spe2$Aoroides.sp.+spe2$Aoroides.columbiae
spe2<-spe2[ ,!names(spe2) %in% c("Aoroides.sp.","Aoroides.columbiae")]

spe2$Calliopius.spp<-spe2$Calliopius.sp.+spe2$Calliopius.carinatus
spe2<-spe2[ ,!names(spe2) %in% c("Calliopius.carinatus","Calliopius.sp.")]

spe2$Cancer.spp<-spe2$Cancer.sp.+spe2$Cancer.branneri
spe2<-spe2[ ,!names(spe2) %in% c("Cancer.branneri","Cancer.sp.")]

spe2$Caprella.spp<-spe2$Caprella.sp.+spe2$Caprella.californica+spe2$Caprella.laeviuscula
spe2<-spe2[ ,!names(spe2) %in% c("Caprella.californica","Caprella.laeviuscula","Caprella.sp.")]

#Went with E.brasiliensis because I know it is common throughout the range
spe2$Ericthonius.brasiliensis<-spe2$Ericthonius.brasiliensis+spe2$Ericthonius.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Ericthonius.sp.")]

spe2$Exogone.spp<-spe2$Exogone.lourei+spe2$Exogone.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Exogone.lourei","Exogone.sp.")]

spe2$Haminoea.japonica
spe2$Haminoea.spp<-spe2$Haminoea.sp.+spe2$Haminoea.vesicula
spe2<-spe2[ ,!names(spe2) %in% c("Haminoea.sp.","Haminoea.vesicula")]

spe2$Heptacarpus.sp.<-spe2$Heptacarpus.sp.+spe2$Heptacarpus.paludicola
spe2<-spe2[ ,!names(spe2) %in% c("Heptacarpus.paludicola")]

spe2$Hippolyte.sp.<-spe2$Hippolyte.sp.+spe2$Hippolyte.californiensis
spe2<-spe2[ ,!names(spe2) %in% c("Hippolyte.californiensis")]

spe2$Idotea.sp.<-spe2$Idotea.sp.+spe2$Idotea.ochotensis
spe2<-spe2[ ,!names(spe2) %in% c("Idotea.ochotensis")]

spe2$Lacuna.spp<-spe2$Lacuna.sp.+spe2$Lacuna.variegata+spe2$Lacuna.vincta+spe2$Lacuna.marmorata
spe2<-spe2[ ,!names(spe2) %in% c("Lacuna.sp.","Lacuna.variegata","Lacuna.vincta","Lacuna.marmorata")]

spe2$Monocorophium.spp<-spe2$Monocorophium.acherusicum+spe2$Monocorophium.insidiosum+spe2$Monocorophium.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Monocorophium.acherusicum","Monocorophium.insidiosum","Monocorophium.sp.")]


##What to do about "Nereid" and "Nereis sp." along with "Platynereis.bicanaliculata" etc?

spe2$Pentidotea.spp<-spe2$Pentidotea.resecata+spe2$Pentidotea.schmitti+spe2$Pentidotea.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Pentidotea.resecata","Pentidotea.schmitti","Pentidotea.sp.")]

spe2$Photis.spp<-spe2$Photis.brevipes+spe2$Photis.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Photis.brevipes","Photis.sp.")]

spe2$Podocerus.spp<-spe2$Podocerus.fulanus+spe2$Podocerus.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Podocerus.fulanus","Podocerus.sp.")]

spe2$Polynoid.spp<-spe2$Polynoid.A+spe2$Polynoid.C+spe2$Polynoidae+spe2$Polynoid..unknown.
spe2<-spe2[ ,!names(spe2) %in% c("Polynoid.A","Polynoid.C","Polynoidae","Polynoid..unknown.")]

spe2$Phyllodocidae.spp<-spe2$Phyllodocidae+spe2$Phyllodocidae.B+spe2$Phyllodocidae.C
spe2<-spe2[ ,!names(spe2) %in% c("Phyllodocidae","Phyllodocidae.B","Phyllodocidae.C")]

spe2$Pontogeneia.spp<-spe2$Pontogeneia.rostrata+spe2$Pontogeneia.sp.
spe2<-spe2[ ,!names(spe2) %in% c("Pontogeneia.rostrata","Pontogeneia.sp.")]


## I will also remove all singletons. There are 7 in total
singletons<-names(which (colSums(spe2) == 1))
taxa.multi<-spe2[ ,!names(spe2) %in% singletons]
dim(taxa.multi)
sum(taxa.multi)

##  Now we have a dataset which is ideal for multivariate comparisons of community composition
## Total of 135075 individuals representing 111 taxa 
write.csv(taxa.multi,"C:/Users/kskoe/OneDrive/Desktop/wasting disease 10-23/taxa.multi.csv")


