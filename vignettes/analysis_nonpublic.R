# This is the nonpublic file for the analysis

# Usage : 
# - in header of each script, the variables needed to be loaded are specified (elements of vector sections_to_be_loaded)
#    save the analysis_nonpublic.R with the analysis and source it with this specified vector in order to load the
#    needed variables

# # please specify the vector of wanted datasets. The following vector will load all datasets.
# sections_to_be_loaded <- c("assemble_covariates",
#                            "assemble_functions",
#                            "imputed_functions",
#                            "functions_dissimilarity",
#                            "prepared_covariates",
#                            "raw_diversity",
#                            "betadiversity",
#                            "raw_lui",
#                            "LUI",
#                            "additional",
#                                "master_diversity_alpha_beta_gamma",
#                                "to_be_specified", #TODO check if needed
#                            "plotting")

# PACKAGE
# load functions from the package
devtools::load_all()

# LOCAL ABSOLUTE PATHS
# note : for IPS Computer : "/run/user/1001/...", but for laptop : "/run/user/1000/..."
pathtoexplosynthesis <- "/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Data"
pathtodata <- "/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun"
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
if("assemble_covariates" %in% sections_to_be_loaded){
  # COVARIATES
  small_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/BE env covariates.txt", sep=""))
  main_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/Grassland EP soil descriptors.txt", sep=""))
  names_gl <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20826_plotNames.txt", sep=""))
  plt.sur <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/18148_landscape.txt", sep=""))
  geodist <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20907.txt", sep=""))
}

if("assemble_functions" %in% sections_to_be_loaded){
  # FUNCTIONS
  # store raw grassland functions for gapfilling
  raw_grlfuns <- data.table::fread(paste(pathtoexplosynthesis, "/Grassland_functions/27087_grassland_functions_bexis/27087_25_Dataset/bexis_to_wide_format_output.csv", sep = ""), header=T)
  #TODO : in first functions script : run new reformatting script
  # selection of required functions for gapfilling
  selection <- info_data[`used for Gapfilling` == "yes", ColumnName]
  raw_grlfuns <- raw_grlfuns[, ..selection]
  rm(selection)
}


######################
# ASSEMBLED DATA INPUT FOR ANALYSIS
######################

if("imputed_functions" %in% sections_to_be_loaded){
  # IMPUTED FUNCTIONS
  imputed_grlfuns <- readRDS(paste(pathtodata, "/data_assembly/output_data/imputed_function_values.rds", sep = ""))
}

if("functions_dissimilarity" %in% sections_to_be_loaded){
  # FUNCTIONS DISSIMILARITY
  EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster.rds", sep = ""))
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/singleEFdist.rds", sep = "")) # single functions, already scaled
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFbeta_abund.rds", sep = "")) # EFbeta abund
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFquantile_median.rds", sep = "")) # EFbeta abund
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster_all_thresholds.rds", sep = ""))
}

if("prepared_covariates" %in% sections_to_be_loaded){
  ## PREPARED COVARIATES
  predictors <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors.rds", sep = ""))
  predictors_soil <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors_soil.rds", sep = ""))
}

if("raw_diversity" %in% sections_to_be_loaded){
  # DIVERSITY
  spdiv <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/201124_EP_species_diversity_GRL.txt", sep=""))
  spinfo <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/210112_EP_species_info_GRL.txt", sep=""))
  names_trlevel <- data.table::fread(paste(pathtodata, "/data_assembly/helper_data/trophic_level_renaming.csv", sep = ""))
}

if("betadiversity" %in% sections_to_be_loaded){
  # BETADIVERSITY
  trlevels <- readRDS(paste(pathtodata, "/data_assembly/output_data/trlevels_vector.rds", sep = ""))
  # masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta.rds", sep = ""))
  # masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_incl_poll_and_tertcons.rds", sep = ""))
  masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_no_bats_with_birds.rds", sep = "")) # THIS WAS ACTIVE LAST TIME
  # masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_abundance.rds", sep = "")) # note: needs some special modifications in scripts
}

if("raw_lui" %in% sections_to_be_loaded){
  # LUI
  rawlui <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/25086_Input Data for LUI Calculation Tool  of all grassland plots since 2006 - revised 2019_1.1.0/25086.txt", sep = ""), header=T)
}
if("LUI" %in% sections_to_be_loaded){
  # LUI
  LUI <- readRDS(paste(pathtodata, "/data_assembly/output_data/LUI.rds", sep = ""))
  distLUI <- readRDS(paste(pathtodata, "/data_assembly/output_data/distLUI.rds", sep = ""))
}

if("additional" %in% sections_to_be_loaded){
  
  if("master_diversity_alpha_beta_gamma" %in% sections_to_be_loaded){
    masterdiversity <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_diversity_alpha_beta_gamma.rds", sep = ""))
  }
  
  if("to_be_specified" %in% sections_to_be_loaded){
    # ADDITIONAL
    # masterdiversity <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_diversity_alpha_beta.rds", sep = ""))
    masterdiversity <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_diversity_alpha_beta_gamma.rds", sep = ""))
    # note that for the above masterdiv : 2 columns with plot names are given, so is alphadiv given in 2 columns.
    # master_alpha <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_alpha.rds", sep = ""))
    # master_gamma <- ... "master_gamma.rds"
  }
}


if("plotting" %in% sections_to_beloaded){
  # get nicenames
  #TODO add overview colors
  #   - check if gray is working well
  nicenames <- data.table::data.table(read.csv("vignettes/nicenames.csv"))
}

if("results" %in% sections_to_be_loaded){
  model_names <- read.csv("vignettes/helper_model_names.csv")
  # for(i in nrow(model_names)){
  #   source("vignettes/plot_GDM.Rmd") # by hand! no automation
  # }
  # i <- 30
  
  # #TODO : produce new deviance explained
  # # note : save from model_overview.ods
  model_results <- data.table::fread(paste(pathtodata, "/analysis/output_datasets/model_results.csv", sep = ""), header = T, skip = 0)
  # model_results <- model_results[1:(nrow(model_results)-1)] # skipt last line
  
  # # gdminput
  # funs <- "EFturnover_0.9"
  # compon_lui <- "LUI" #"components"
  # modelname <- paste("gdm", funs, compon_lui, sep = "_")
  # gdminput <- readRDS(paste(pathtodata, paste("/analysis/output_datasets/gdm", funs, compon_lui, "input.Rds", sep = "_"), sep = ""))
}
