# This is the nonpublic file for the results

# PACKAGE
# load functions from the package
devtools::load_all()
require(gitcreds)
# gitcreds::gitcreds_set()

# pathtoout <- "/run/user/1001/gvfs/smb-share:server=ipssmonstera.unibe.ch,share=nschenk/Documents/BEF/ANALYSIS_RESULTS"
# pathtoout <- "/run/user/1000/gvfs/smb-share:server=ipssmonstera.unibe.ch,share=nschenk/Documents/BEF/ANALYSIS_RESULTS"
#TODO make pathtoout with new folder structure?
pathtoout <- "~/Documents/github/BetaDivMultifun/vignettes/out"
pathtodata <- "~/IPS_SYNCFILES_BetaDivMultifun"

# get nicenames
#TODO add overview colors
#   - check if gray is working well
nicenames <- data.table::data.table(read.csv("vignettes/nicenames.csv"))

# smb://nas-ips/ips/groups/planteco/PROJECTS/Exploratories%20Synthesis/Research%20Projects/BetaDivMultifun



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
# funs <- "EFturnover_0.8"
# compon_lui <- "LUI" #"components"
# modelname <- paste("gdm", funs, compon_lui, sep = "_")
# gdminput <- readRDS(paste(pathtodata, paste("/analysis/output_datasets/gdm", funs, compon_lui, "input.Rds", sep = "_"), sep = ""))

