
# Aim : adding temporal arthropods dataset and AMF dataset to the diversity dataset.
# Goal : clean this, and source script from analysis_nonpublic.R as part of dataset assembly
# 
# This script is called from calc_betadiversitites.Rmd.
# 
# Data to add arthropod temporal dataset (not in BExis!)
# 21969_4_data.txt
# Temporal_arth_trophicGroups_withNAs.csv
# 26008_2_data.txt

# # # # # # # # # # # # # # # # # # # # 
#
# Add ARTHROPOD TEMPORAL data          ----
#
# # # # # # # # # # # # # # # # # # # # 

## Add temporal arthropods to synthesis dataset
#This script can be used to add the arthropod temporal dataset to the synthesis dataset
#Script by Caterina Penone
#minor changes by Noëlle Schenk (paths, dependencies given from package). Based on version as is 28.09.23
#this script is also here: https://github.com/biodiversity-exploratories-synthesis/Synthesis_useful_functions

# dependencies
# data is read in by script analysis_nonpublic.R

allspecies <- spdiv
fg.class <- spinfo

## change column name "value" to "Abundance"
names(allspecies)[names(allspecies)=="value"] <- "Abundance"

## edit : drop Plot_bexis column (not DataID and Dataversion)
allsp2 <- allspecies[,-c(1)]

## merge the datafiles
allsp2 <- merge(allsp2, fg.class, by ="Species")

# # # # # # # # # # # # # # # # # # # # 
# add temporal arthropods 
arthro$Species <- gsub(" ","_",arthro$Species)
##trophic group information has NAs if not at species level, unless no other species of the genus are there (those have NA in fun_group_fine)
arth_tr <- arth_tr[!is.na(Trophic_level)]

#remove data with no species information (order or genus)
arthro <- arthro[!is.na(Species)]

#merge with trait info, this also removes all occurrences not identified at the species level
arthro2 <- merge(arthro, arth_tr[,!names(arth_tr) %in% c("Order", "Suborder"), with=F], by="Species") 

#add missing zeros for all year X month X species combinations
length(unique(arthro2$Species))*length(unique(arthro2$PlotID))*length(unique(arthro2$CollectionYear))*length(unique(arthro2$CollectionMonth))
nrow(arthro2) #all zeros are missing
sum(is.na(arthro2$NumberAdults)) #no NAs
#remove non-target columns
arthro2 <- arthro2[,!c("TrapID", "Exploratory", "Traptype", "VIP_EP", "Order", "Suborder", "Family", "Trophic_level", "Fun_group_broad", "Fun_group_fine")]

arthro3 <- setDT(arthro2)[CJ(Species=Species, PlotID=PlotID, 
                             CollectionYear=CollectionYear,
                             CollectionMonth=CollectionMonth,
                             unique=T), on=.(Species, PlotID, CollectionYear, CollectionMonth)]

arthro3[is.na(NumberAdults), NumberAdults := 0 ]

#exclude plot and year combinations that need to be removed
arth_remove$Collection <- gsub("-01-01", "", arth_remove$Collection)
arth_remove[,temp:=paste(PlotIDBexis, Collection, sep="_")]
arthro3[,temp:=paste(PlotID, CollectionYear, sep="_")]
arthro3 <- arthro3[!temp %in% arth_remove$temp]
arthro3$temp <- NULL

# edit : based on Caterinas recommendation : 
# average per species and plot, all months, but separately per year
summary(arthro3$NumberAdults)
arthro3[,value:=mean(NumberAdults), by=c("Species", "PlotID", "CollectionYear")]
summary(arthro3$value)
arthro3 <- unique(arthro3[,.(Species, PlotID, value, CollectionYear)])
length(unique(arthro3$PlotID)) #150
arthro3[duplicated(arthro3[,.(Species, PlotID, CollectionYear)])]
# edit : based on C recommendation : use presence-absence, not abundance
arthro3[value > 0, value := 1]

#add trophic info
arthro3 <- merge(arthro3, arth_tr, by="Species")

#homogenise column names to merge with main dataset
arthro3 <- data.table(BEplotZeros(arthro3, "PlotID", plotnam = "Plot"))
arthro3$PlotID <- NULL
arthro3$Suborder <- NULL
setnames(arthro3, c("Order", "value", "CollectionYear"), c("Group_broad", "Abundance", "Year"))
# edit : add dataset and version info
arthro3[, DataID := 21969]
arthro3[, Dataversion := 4]

#homogenise trophic group names
sort(unique(allsp2$Trophic_level))
sort(unique(arthro3$Trophic_level))
arthro3[Trophic_level=="decomposer", Trophic_level:="decomposer.arthropod"]
arthro3[Trophic_level=="herbivore", Trophic_level:="herbivore.arthropod"]
arthro3[Trophic_level=="omnivore", Trophic_level:="omnivore.arthropod"]
arthro3[Trophic_level=="pollinator", Trophic_level:="pollinator.arthropod"]
arthro3[Trophic_level=="secondary.consumer", Trophic_level:="secondary.consumer.arthropod"]

#remove 2008 data from main dataset and add temporal dataset for "Hemiptera"  "Coleoptera" "Araneae"    "Orthoptera"
allsp2 <- allsp2[!Group_broad %in% c("Hemiptera", "Coleoptera", "Araneae", "Orthoptera")]
#check if duplicate species
intersect(unique(arthro3$Species), unique(allsp2$Species)) #none
#all fine, rbind
allsp2 <- rbindlist(list(allsp2, arthro3), use.names = T, fill=T)
rm(arthro, arthro2, arthro3, arth_tr, arth_remove)
# allsp2 is a dataset containing all previous data, as well as the arthropod data. Column names
#    do not correspond to spdiv.
# add data ID information
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # 
#
# Add AMF data                        ----
#
# # # # # # # # # # # # # # # # # # # # 

# This script collects all things to do during the next update of the dataset
# Script by: Caterina Penone
# minor edits by Noëlle Schenk
# Data to add AMF dataset is in folder AMF2023
#    script analysis_nonpublic.R reads the datasets
# Script here: https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_diversity_grassland/blob/master/Next_Update.R

setnames(allsp2, old = "Abundance", new = "value")


#############Replace symbionts with proper AMF datasets ##########################
# note : datasets are read in analysis_nonpublic.R

#stack all years
a11$Year <- 2011
a14$Year <- 2014
a17$Year <- 2017

a11$DataID <- 27687
a14$DataID <- 27689
a17$DataID <- 27691

amf <- rbindlist(list(a11, a14, a17), use.names = T)
amf$Dataversion <- 3
rm(a11, a14, a17)

#harmonise names and data structure with synthesis dataset
setnames(amf, c("Plotid", "AMF", "Abundance"), c("Plot_bexis", "Species", "value"))
amf <- data.table(BEplotZeros(amf, "Plot_bexis", plotnam="Plot")) #add plot name with zeros (e.g. AEG1 to AEG01)
amf$type <- "ASV_number"

#taxa table
amf$Group_broad <- "soilfungi"
amf$Group_fine <- "Glomeromycotina"
amf$Trophic_level <- "symbiont.soilfungi"
amf$Fun_group_broad <- "soilfungi.symbiont"
amf$Fun_group_fine <- "AMF"

sort(names(allsp2))
sort(names(amf))
amf[, c("Plot_bexis") := NULL] # edit : keep DataID and Dataversion

#remove amf measured in soils from synthesis dataset and add speciic AMF
unique(allsp2[Trophic_level=="symbiont.soilfungi", .(Group_broad, Group_fine, Fun_group_broad, Fun_group_fine)]) #check
allsp2 <- allsp2[!Fun_group_fine=="AMF"]
# combine with all species data
spdiv <- rbindlist(list(allsp2, amf), use.names = T)
rm(allsp2, fg.class, lt, amf, allspecies)
