# This is the nonpublic file for the analysis

# PACKAGE
# load functions from the package
devtools::load_all()

# LOCAL ABSOLUTE PATHS
# note : for IPS Computer : "/run/user/1001/...", but for laptop : "/run/user/1000/..."
pathtoexplosynthesis <- "/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Data"
pathtodata <- "/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/"
pathtoout <- "~/Documents/github/BetaDivMultifun/vignettes/out"

# GENERAL
# is the updated version of plotNAset, selection based on diversity AND functions
# first version constructed in calc_betadiversities.Rmd, second version in function_imputation.Rmd
plotNAset <- readRDS(paste(pathtodata, "/data_assembly/helper_data/plotNAset.rds", sep = ""))
info_data <- data.table::fread(paste(pathtodata, "/data_assembly/helper_data/info_data.csv", sep = ""), header=T)
usedforBetadivMultifun <- info_data[`used for BetadivMultiFun` == "yes", ColumnName]
usefulplotids <- readRDS(paste(pathtodata, "/data_assembly/helper_data/useful_plot_ids.rds", sep="")) # used in calc_covariates.Rmd
colorder_gdm_input <- readRDS(paste(pathtodata, "/analysis/helper_data/colorder_gdminput.rds", sep = "")) # used in prepare_and_run_GDM.Rmd

# a <- c(paste("AEG", stringr::str_pad(seq(1, 50), 2, pad = "0"), sep = ""),
#   paste("HEG", stringr::str_pad(seq(1, 50), 2, pad = "0"), sep = ""),
#   paste("SEG", stringr::str_pad(seq(1, 50), 2, pad = "0"), sep = ""))
# b <- c(paste("AEG", seq(1, 50), sep = ""),
#   paste("HEG", seq(1, 50), sep = ""),
#   paste("SEG", seq(1, 50), sep = ""))
# usefulplotids <- data.table(Plot = b, Plotn = a)


######################
# CONSTRUCTION OF ASSEMBLED DATA FROM RAW
######################
# # COVARIATES
# small_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/BE env covariates.txt", sep=""))
# main_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/Grassland EP soil descriptors.txt", sep=""))
# names_gl <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20826_plotNames.txt", sep=""))
# plt.sur <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/18148_landscape.txt", sep=""))
# geodist <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20907.txt", sep=""))


# FUNCTIONS
# store raw grassland functions for gapfilling
# raw_grlfuns <- data.table::fread(paste(pathtoexplosynthesis, "/Grassland_functions/27087_grassland_functions_bexis/27087_25_Dataset/bexis_to_wide_format_output.csv", sep = ""), header=T)
# # #TODO : in first functions script : run new reformatting script
# # selection of required functions for gapfilling
# selection <- info_data[`used for Gapfilling` == "yes", ColumnName]
# raw_grlfuns <- raw_grlfuns[, ..selection]
# rm(selection)



######################
# ASSEMBLED DATA INPUT FOR ANALYSIS
######################

# IMPUTED FUNCTIONS
# imputed_grlfuns <- readRDS(paste(pathtodata, "/data_assembly/output_data/imputed_function_values.rds", sep = ""))

# FUNCTIONAL DISSIMILARITY
EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster.rds", sep = ""))
# EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/singleEFdist.rds", sep = "")) # single functions, already scaled
# EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFbeta_abund.rds", sep = "")) # EFbeta abund
# EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFquantile_median.rds", sep = "")) # EFbeta abund
# EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster_all_thresholds.rds", sep = ""))


## PREPARED COVARIATES
predictors <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors.rds", sep = ""))
predictors_soil <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors_soil.rds", sep = ""))

# DIVERSITY
spdiv <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/201124_EP_species_diversity_GRL.txt", sep=""))
spinfo <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/210112_EP_species_info_GRL.txt", sep=""))
names_trlevel <- data.table::fread(paste(pathtodata, "/data_assembly/helper_data/trophic_level_renaming.csv", sep = ""))

# BETADIVERSITY
trlevels <- readRDS(paste(pathtodata, "/data_assembly/output_data/trlevels_vector.rds", sep = ""))
# masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta.rds", sep = ""))
# masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_incl_poll_and_tertcons.rds", sep = ""))
masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_no_bats_with_birds.rds", sep = "")) # THIS WAS ACTIVE LAST TIME
# masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_abundance.rds", sep = "")) # note: needs some special modifications in scripts

# LUI
# rawlui <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/25086_Input Data for LUI Calculation Tool  of all grassland plots since 2006 - revised 2019_1.1.0/25086.txt", sep = ""), header=T)
LUI <- readRDS(paste(pathtodata, "/data_assembly/output_data/LUI.rds", sep = ""))
distLUI <- readRDS(paste(pathtodata, "/data_assembly/output_data/distLUI.rds", sep = ""))



# ADDITIONAL
# masterdiversity <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_diversity_alpha_beta.rds", sep = ""))
masterdiversity <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_diversity_alpha_beta_gamma.rds", sep = ""))
# note that for the above masterdiv : 2 columns with plot names are given, so is alphadiv given in 2 columns.
# master_alpha <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_alpha.rds", sep = ""))
# master_gamma <- ... "master_gamma.rds"