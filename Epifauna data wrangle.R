#####
##### Karl's attempt at wrangling the data for subsequent analyses
#####


### Load most recent epifauna data
### See Carmen's code and readme on google drive for the detailed formation of this dataset. 

### Looking at carmens new data set posted 8/24/2023 on google drive
taxa1 <- read.csv("C:/Users/Karl/Desktop/Grad School/Lab/eel grass/eg wasting epifauna/8262023/20230824_allepifauna_wide_macrophyte.csv")
nrow(taxa1) #1006
ncol(taxa1) #333


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
taxa1<-taxa1%>% select(-c(Allorchestes.sp.,bivalve.D,Cumella.vulgaris,EMPTY,fish,Fish.larva,Neanthes.sp.,Nippoleucon.hinumensis,Pipefish))
###I have removed 9 columns so far but there are probably others to get rid of


###Rename any taxa whose nomenclature is not current with WORMS####
## Carmen already fixed this in teh 8/24/2023 version of data
###I did not do a thorough search, just happened to find these
#taxa1<-taxa1 %>%rename(Mitrella.gausapata = Astyris.gausapata,Angustassiminea.californica=Assiminea.californica,Megasyllis.nipponica=Typosyllis.nipponica)

### I will also rename some things that we've putatively improved the ID's of since data was collected.
## There were more of these to do in previous versions of dataset
taxa1<-taxa1 %>%
  rename(Nassarius.tiarula=gastropod.N) 
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
write.csv(taxa.only.transect,"C:/Users/Karl/Desktop/Grad School/Lab/eel grass/eg wasting epifauna/8262023/taxa.only.transect.csv")

### If we want to standardize abundance per sample by eelgrasss biomass use this data
taxa.only.transect.biomass<-as.data.frame(as.matrix(taxa.only.transect)/as.vector(taxa.transect1$macrophyte_wet_mass_g))
write.csv(taxa.only.transect.biomass,"C:/Users/Karl/Desktop/Grad School/Lab/eel grass/eg wasting epifauna/8262023/taxa.only.transect.biomass.csv")


##### Now we can calculate diversity indices and insert into transect level data####
taxa.transect1$simpsons<-diversity(taxa.only.transect,index="simpson")
taxa.transect1$shannon<-diversity(taxa.only.transect,index="shannon")
taxa.transect1$invsimpsons<-diversity(taxa.only.transect,index="invsimpson")


########################################
##### Univariate taxanomic data set ####
########################################

#### This data can be used for univariate community level statistics
write.csv(taxa.transect1,"C:/Users/Karl/Desktop/Grad School/Lab/eel grass/eg wasting epifauna/8262023/taxa.transect1.csv")
