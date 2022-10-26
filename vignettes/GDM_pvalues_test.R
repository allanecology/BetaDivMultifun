test <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/pvalue_calculation/gdm_EFdistance_LUI_perm.Rds")
test <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/old/gdm_EFdistance_LUI_permutation.Rds")
pvals <- test[[3]][,1]
plot(pvals)
test[[4]][,1]

# got maxsplines from script plot_GDM
plot(maxsplines)
df <- data.table(names = names(maxsplines), height = maxsplines)
df2 <- data.table(names = names(pvals), pval = pvals)
df <- merge(df, df2, by = "names", all.y = T)
rm(df2)


df[, sign := "n.s."]
df[pval < 0.05, sign := "*"]

df[is.na(height), height := 0.5]

new <- ggplot(df, aes(x = names, y = height, fill = sign)) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(data = df, aes(x = names, y = 0.3, label = sign))

plot_grid(old, new, nrow = 1)


##########
# compare old and new
# need names_trlevel from analysis_nonpublic.R
names_trlevel[grep("pollinat", names_trlevel$Trophic_level), final_name := "pollinator"]
names_trlevel <- names_trlevel[, .(Trophic_level, final_name)]
# names_trlevel <- names_trlevel[final_name != "", ]
names_trlevel[, Trophic_level.sne := paste(Trophic_level, ".beta.sne", sep = "")]
names_trlevel[, Trophic_level.sim := paste(Trophic_level, ".beta.sim", sep = "")]
names_trlevel[, names.sne := paste(final_name, ".beta.sne", sep = "")]
names_trlevel[, names.sim := paste(final_name, ".beta.sim", sep = "")]
names_trlevel[, Trophic_level := NULL]
names_trlevel[, final_name := NULL]
names_trlevel <- data.table(Trophic_level = c(names_trlevel$Trophic_level.sne, names_trlevel$Trophic_level.sim),
           names = c(names_trlevel$names.sne, names_trlevel$names.sim))

pval <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/pvalue_calculation/gdm_EFdistance_LUI_perm.Rds")
pval <- pval[[3]][,1]
new <- data.table(names = names(pval), pval_new = pval)
pval <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/old/gdm_EFdistance_LUI_permutation.Rds")
pval <- pval[[3]][,1]
old <- data.table(Trophic_level = names(pval), pval = pval)
old <- merge(old, names_trlevel, by = "Trophic_level", all.x = T)
old[is.na(names), names := Trophic_level]
old[, Trophic_level := NULL]

old_new <- merge(old, new, by = "names", all = T)
old_new <- melt(old_new, id.vars = "names")
old_new[value > 0.05, pval := 1]
old_new[value <= 0.05, pval := 0]

ggplot(old_new, aes(x = variable, y = names, fill = value)) + geom_tile() + scale_fill_distiller(palette = "RdPu") + 
  xlab("pvalues of old and new GDM model") +
  geom_text(aes(label = round(value, 2)))


############
# 1000 permutations
############
p100 <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/pvalue_calculation/gdm_EFdistance_LUI_perm.Rds")
p100 <- p100[[3]][,1]
p100 <- data.table(names = names(p100), pval_100 = p100)
p1000 <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/pvalue_calculation/gdm_EFdistance_LUI_1000perm.Rds")
p1000 <- p1000[[3]][,1]
p1000 <- data.table(names = names(p1000), pval_1000 = p1000)
perm <- merge(p100, p1000, by = "names", all = T)
rm(p100); rm(p1000)

perm <- melt(perm, id.vars = "names")
ggplot(perm, aes(x = names, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_hline(yintercept = 0.05) +
  coord_flip() +
  labs(title = "EFdistance LUI model", subtitle = "pvalues calculated with n = 100 and n = 1000 permutations")
# save as "GDM_pvalues_comparison_EFdist_100vs1000_permutations.pdf"


############
# take out artropodsoillarvae
############
# read in gdminput of EFdistance as "gdminput" as written in "analysis_nonpublic.R"
# gdminput_no_sc_arthro <- gdminput[, -grep("secondary.consumer.arthropodsoillarvae", names(gdminput))]
# saveRDS(gdminput_no_sc_arthro, file = "gdm_EFdistance_LUI_NO_sc_arthropodsoillarvae.Rds")

# read in input file constructed above, 100 permutations
perm <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/pvalue_calculation/gdm_EFdistance_LUI_NO_sc_arthropodsoillarvae_perm.Rds")
perm <- perm[[3]][,1]
perm <- data.table(names = names(perm), pval = perm)
# get maxsplines
in_arthro <- readRDS("~/IPS_SYNCFILES_BetaDivMultifun/analysis/output_datasets/gdm_EFdistance_LUI_NO_sc_arthropodsoillarvae_input.Rds")
gdm_output <- gdm::gdm(in_arthro, geo = T)# run GDM
saveRDS(gdm_output, file = "vignettes/out/gdm_EFdistance_LUI_NO_sc_arthropodsoillarvae_output.Rds") # save output to where it is always stored
# go to plot_GDM and plot the results


exSplines <- gdm::isplineExtract(gdm_output)
maxsplines <- apply(exSplines$y, 2, max, na.rm = TRUE)
maxsplines <- data.table(names = names(maxsplines), maxsplines = maxsplines)
rm(exSplines)


