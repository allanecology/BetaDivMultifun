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
