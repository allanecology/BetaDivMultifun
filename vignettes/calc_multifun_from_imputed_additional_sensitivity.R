# 21-03-10
# Noelle Schenk
#
# SENSITIVITY - PCA AXES SELECTION¨
#
# Does it matter if I take different number of PCs?
# here, comparison of first 11, 13 and 14 axes.
#
# also contains useful pairs() function.
#
# load requirements
sections_to_be_loaded <- c()
source("vignettes/analysis_nonpublic.R")

# note : the below file was created by changing the number of selected PCA axes to
#    11, 13 and 14 and saving the output into a data.table.
#    in order to reproduce, use the file "calc_multifun_from_imputed.Rds" and
#    run everything until the pca, until the line : 
#    "saveRDS(EFmaster, "EFdistance.rds")" and instead of saving, keep the data.table
#    and rerun the above section with a different number than 11.
comparison <- readRDS(paste(pathtodata, "/data_assembly/output_data/21-03-10_calc_multifun_from_imputed_sensitivity_pca_axes_selection_impact_of_number_of_axes_on_EFdistance.RDS", sep = ""))

plot(comparison$EFdistance_11_axes, comparison$EFdistance_14_axes,
     main = "EFdist calculated on first 11 and first 14 axes")


panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(comparison[, .(EFdistance_11_axes, EFdistance_13_axes, EFdistance_14_axes)],
      diag.panel = panel.hist,
      upper.panel = panel.cor)

# result : the EFdist measures with 13 or 14 instead of 11 axes are highly correlated with
#    each other c > 0.98
#    note (limitation) : high number of replicates, therefore higher correlation expected.
# conclusion : the EFdist measure is robust to a deviating number of PC axes selected.
