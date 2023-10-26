# This is the nonpublic file for the data assembly and analysis

# Usage : 
# - in header of each script, the variables needed to be loaded are specified (elements of vector sections_to_be_loaded)
#    (is done automatically) : source analysis_nonpublic.R with this specified vector in order to load the needed variables
# - **Important note** about transferring RDS files from linux to windows systems : can not be done using copy-paste.
#    need to state that it is a binary file. Can be avoided using .csv files. For Rds, no solution was currently found.
#    https://stackoverflow.com/questions/30892505/trouble-unzipping-file-under-windows

#TODO user : connect to planteco and nsch to get input files

# Explanation of name:
# Was historically thought to not be public because contains file paths, but decided to keep it close to all code.

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
#                            "plotting",
#                            "results",
#                            "gdminput",
#                            "gdmoutput")
if(!exists("sections_to_be_loaded")){
  sections_to_be_loaded <- c()
}


# PACKAGE
# load functions and small helper datasets from the package in case not already loaded
if(!"BetaDivMultifun" %in% (.packages())){
  devtools::load_all()
}


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
data("colorder_gdm_input")
data("usefulplotids")
data("model_names")
data("nicenames")
# settings for plotting
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(cowplot))
theme_set(theme_half_open())

######################
# CONSTRUCTION OF ASSEMBLED DATA FROM RAW
######################
if("assemble_covariates" %in% sections_to_be_loaded){
  # COVARIATES
  small_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/BE env covariates.txt", sep=""))
  main_glsoil <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/Grassland EP soil descriptors.txt", sep=""))
  glsoil <- fread(paste0(pathtodata, "/data_assembly/output_data/intermediate_glsoil.csv"))
  names_gl <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20826_plotNames.txt", sep=""))
  plt.sur <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/18148_landscape.txt", sep=""))
  geodist <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/20907.txt", sep=""))
  landscape <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/31018_5_Dataset/31018_5_data.csv", sep=""))
  landscape2 <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/LandUse-31319_31369/LandUse/LandscapeMetrics_acrossYears_acrossScales_Long_UpdatedOct22.csv", sep=""))
}

if("assemble_functions" %in% sections_to_be_loaded){
  # FUNCTIONS
  # store raw grassland functions for gapfilling
  raw_grlfuns <- data.table::fread(paste(pathtoexplosynthesis, "/Grassland_functions/27087_grassland_functions_bexis/27087_25_Dataset/bexis_to_wide_format_output.csv", sep = ""), header=T)
  #TODO : in first functions script : run new reformatting script
  #TODO : run bexis_to_wide_format.R here already!
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
  if(!exists("model_names_selection") | model_names_selection$model_class == "multifun"){
    # if no specific class of EFmaster is selected, or the multifunctionality models are selected.
    EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster.rds", sep = ""))
  } else if(model_names_selection$model_class == "singlefun"){
    EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/singleEFdist.rds", sep = "")) # single functions, already scaled
  } else {
    stop("Error : unclear which Functions input should be read, please indicate in model_names_selection.
         In case EFbeta or EFbeta abundance was intented, please run by hand or implement, the automatic running
         of these response variables is deprecated.")
  }
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFbeta_abund.rds", sep = "")) # EFbeta abund
  # EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFquantile_median.rds", sep = "")) # EFbeta abund
}

if("prepared_covariates" %in% sections_to_be_loaded){
  ## PREPARED COVARIATES
  predictors <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors.rds", sep = ""))
  predictors_soil <- readRDS(paste(pathtodata, "/data_assembly/output_data/predictors_soil.rds", sep = ""))
}

if("raw_diversity" %in% sections_to_be_loaded){
  # DIVERSITY
  #Note 28.9.23 : used updated diversity dataset. (errors were corrected.) Will delete the outdated diversity dataset
  #TODO delete 2 rows below, AND delete old dataset in rawdata
  # spdiv <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/201124_EP_species_diversity_GRL.txt", sep=""))
  # spinfo <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/210112_EP_species_info_GRL.txt", sep=""))
  spdiv <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/221102_EP_species_diversity_GRL_Patch22.txt", sep=""))
  spinfo <- data.table::fread(paste(pathtodata, "/data_assembly/raw_data/221102_EP_species_info_GRL_patch22.txt", sep=""))
  # new arthropod data
  arthro <- fread(paste0(pathtodata, "/data_assembly/raw_data/21969_4_data.txt")) # temporal arthropod dataset
  arth_tr <- fread(paste0(pathtodata, "/data_assembly/raw_data/Temporal_arth_trophicGroups_withNAs.csv"))
  arth_remove <- fread(paste0(pathtodata, "/data_assembly/raw_data/26008_2_data.txt"))
  # new fungal symbionts data
  lt <- fread(paste0(pathtodata, "/data_assembly/raw_data/AMF2023/27686_3_data.txt")) #lookout table
  a11 <- fread(paste0(pathtodata, "/data_assembly/raw_data/AMF2023/27687_3_data.txt")) #2011
  a14 <- fread(paste0(pathtodata, "/data_assembly/raw_data/AMF2023/27689_3_data.txt")) #2014
  a17 <- fread(paste0(pathtodata, "/data_assembly/raw_data/AMF2023/27691_3_data.txt")) #2017
  # run script to add arthropod and AMF data to synthesis dataset
  source("vignettes/diversity_dataset_update.R")
  names_trlevel <- data.table::fread(paste(pathtodata, "/data_assembly/helper_data/trophic_level_renaming.csv", sep = ""))
  #TODO names_trlevel is currently not used any more --> probably delete
}

if("betadiversity" %in% sections_to_be_loaded){
  # BETADIVERSITY
  trlevels <- readRDS(paste(pathtodata, "/data_assembly/output_data/trlevels_vector.rds", sep = ""))
  masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta.rds", sep = ""))
  # masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_incl_poll_and_tertcons.rds", sep = ""))
  # masterbeta <- readRDS(paste(pathtodata, "/data_assembly/output_data/master_beta_no_bats_with_birds.rds", sep = ""))
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

if("gdminput" %in% sections_to_be_loaded){
  # gdminput
  # if not specified differently, will read in the results from the GDM model EFturnover 0.7 with LUI
  if(!exists("model_names_selection")){
    print("model selection is not given, default is EFturnover_0.7")
    model_names_selection <- model_names[which(model_names$modelname == "gdm_EFturnover_0.7_LUI"), ]
  }
  # modelname <- model_names_selection$modelname
  # model_name <- modelname #TODO delete soon (Nov 2022) just left to see if I will miss this
  gdminput <- readRDS(paste(pathtodata, paste("/analysis/output_datasets/gdm", 
                                              model_names_selection$funs, 
                                              model_names_selection$lui, "input.Rds", sep = "_"), sep = ""))
}


if("gdmoutput" %in% sections_to_be_loaded){
  if(!exists("model_names_selection")){
    print("model selection is not given, default is EFturnover_0.7")
    model_names_selection <- model_names[which(model_names$modelname == "gdm_EFturnover_0.7_LUI"), ]
  }
  # model_name <- paste("gdm", funs, lui, sep = "_") #TODO delete soon if realise that not needed (Nov 2022)
  gdmoutput <- readRDS(paste_gdm_input_path_together(pathtoout = pathtodata, 
                                                     name = paste("gdm_", model_names_selection$funs, 
                                                                  "_", model_names_selection$lui, 
                                                                  sep = "")))
  model_specs <- data.table::fread(paste(pathtodata, "/analysis/output_datasets/", 
                          model_names_selection$modelname, "_GDM_model_specs.csv", sep = ""))
  # overviewbar_data <- readRDS(paste(pathtodata, "/analysis/output_datasets/", 
                                  # model_names_selection$modelname, "_overviewbar_data.RDS", sep = ""))
}

if("thresholds" %in% sections_to_be_loaded){
  EFmaster <- readRDS(paste(pathtodata, "/data_assembly/output_data/EFmaster_all_thresholds.rds", sep = ""))
  # note : overwrites current EFmaster in case "functions_dissimilarity" is also loaded
  
  # deviance explained of all models
  devexpl <- readRDS(paste(pathtodata, "/analysis/output_datasets/devexpl_all_models.RDS", sep = ""))
  # note : previously called model_results.csv and read in by "results" section of analysis_nonpublic.R.
  
  # isplines of all models
  isplines_all_models <- readRDS(paste(pathtodata, "/analysis/output_datasets/isplines_all_threshold_models.RDS", sep = ""))
  
  # #
  # ispline uncertainty of all threshold models
  threshold_model_names <- c(paste("gdm_EFturnover_", seq(0.1, 0.9, 0.1), sep = ""), 
    paste("gdm_EFnestedness_", seq(0.1, 0.9, 0.1), sep = ""))
  # create vector with all paths of the expected models
  threshold_model_paths <- paste(pathtodata, "/analysis/output_datasets/uncertainty_calc/", threshold_model_names, "_LUI_uncertainty.Rds",sep = "")
  
  # edit start ---
  # ispline_uncertainty_all_thresholds <- lapply(X = threshold_model_paths,FUN = function(x) readRDS(x)) # read all threshold files in a list
  # in case the above line does not work (random readRDS error)
  EFturn01 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.1_LUI_uncertainty.Rds")
  EFturn02 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.2_LUI_uncertainty.Rds")
  EFturn03 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.3_LUI_uncertainty.Rds")
  EFturn04 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.4_LUI_uncertainty.Rds")
  EFturn05 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.5_LUI_uncertainty.Rds")
  EFturn06 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.6_LUI_uncertainty.Rds")
  EFturn07 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.7_LUI_uncertainty.Rds")
  EFturn08 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.8_LUI_uncertainty.Rds")
  EFturn09 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFturnover_0.9_LUI_uncertainty.Rds")
  EFnes01 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.1_LUI_uncertainty.Rds")
  EFnes02 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.2_LUI_uncertainty.Rds")
  EFnes03 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.3_LUI_uncertainty.Rds")
  EFnes04 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.4_LUI_uncertainty.Rds")
  EFnes05 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.5_LUI_uncertainty.Rds")
  EFnes06 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.6_LUI_uncertainty.Rds")
  EFnes07 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.7_LUI_uncertainty.Rds")
  EFnes08 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.8_LUI_uncertainty.Rds")
  EFnes09 <- readRDS("/run/user/1000/gvfs/smb-share:server=nas-ips,share=ips/groups/planteco/PROJECTS/Exploratories Synthesis/Research Projects/BetaDivMultifun/analysis/output_datasets/uncertainty_calc/gdm_EFnestedness_0.9_LUI_uncertainty.Rds")
  
  ispline_uncertainty_all_thresholds <- list(EFturn01, EFturn02, EFturn03,
                                             EFturn04, EFturn05, EFturn06,
                                             EFturn07, EFturn08, EFturn09,
                                             EFnes01, EFnes02, EFnes03,
                                             EFnes04, EFnes05, EFnes06, 
                                             EFnes07, EFnes08, EFnes09
                                             )
  # -- edit end
  names(ispline_uncertainty_all_thresholds) <- threshold_model_names
  rm(threshold_model_names, threshold_model_paths)
  rm(EFturn01, EFturn02, EFturn03,
         EFturn04, EFturn05, EFturn06,
         EFturn07, EFturn08, EFturn09,
         EFnes01, EFnes02, EFnes03,
         EFnes04, EFnes05, EFnes06, 
         EFnes07, EFnes08, EFnes09)
}
