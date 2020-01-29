##################
#
# quick single functions heatmap
#
##################

funnames <- c("Biomass", "dung.removal", "Groundwater.recharge", "herbivory.20172018",
              "Litter.decomposition", "N_leaching_risk2015", "pathogen.infection",
              "Phosphatase", "Potential.nitrification", "caterpillars.predation",
              "Root.biomass", "Root.decomposition", "seed.depletion", "soilCflxs",
              "P_leaching_risk_comb", "soilNitrateflxs", "soilAmmoniaflxs")
i <- funnames[1]
# outname : gdm_dung.removal_LUI_output
mod <- readRDS(paste(pathtoout, "/GDM/gdm_", i, "_LUI_output.Rds", sep = ""))
mod <- gdm::isplineExtract(mod)
mod <- data.table(mod$y)
mod <- apply(mod, 2, max)
n <- copy(mod)

for(i in funnames[-1]){
  m <- readRDS(paste(pathtoout, "/GDM/gdm_", i, "_LUI_output.Rds", sep = ""))
  m <- gdm::isplineExtract(m)
  m <- data.table(m$y)
  m <- apply(m, 2, max)
  mod <- cbind(mod, m = m[names(n)])
  colnames(mod)[colnames(mod) == "m"] <- i
}

colnames(mod)[colnames(mod) == "mod"] <- "Biomass"

pheatmap::pheatmap(mod, display_numbers = T, cluster_rows = F, cluster_cols = F)
