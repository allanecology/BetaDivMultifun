###############################
#
# SIMULATE BETADIVERSITY
#
###############################

# why ?
# The GDM input is ideally the individual plot level, so GDM calculates the differences between plots.
# For betadiversity, this is not possible, because the difference is not just substraction and the 
# diversity data per plot is not just one number.

require(betapart)
require(ggplot2)
require(cowplot)
require(data.table)
library(latex2exp)

# wie sind "common missing" Arten gewichtet?
# plot 1 :  A B C D E F G - - - - - - - - - -
# plot 2 :  - - - - - - - H I J K L M N - - -


betapart::beta.pair(beta.belowground.herbivore, index.family = "sorensen")
# rows are sites and columns are species

a <- paste("species", seq(1, 40), sep = "")
b <- c(rep(0, 10), rep(1, 10), rep(0, 20))
c <- c(rep(1, 10), rep(0, 30))
d <- rbind(b, c)
colnames(d) <- a


b <- c(rep(1, 20), rep(0, 10), rep(1, 10))
c <- c(rep(1, 30), rep(0, 10))
d <- rbind(b, c)
colnames(d) <- a

beta.pair(d, index.family = "sorensen")
beta.pair(d, index.family = "jaccard")

a <- paste("species", seq(1, 40), sep = "")
b <- c(rep(0, 10), rep(1, 10), rep(0, 20))
c <- c(rep(1, 10), rep(0, 30))
e <- rep(1, 40)
e <- rep(0, 40)
d <- rbind(b, c, e)
colnames(d) <- a



beta.multi(d, index.family = "sorensen")

# situation 2 : 
# comparison 1 : 40 species total, turnover of 10
# comparison 2 : 15 species total, turnover of 5
b <- c(rep(0, 10), rep(1, 30))
c <- c(rep(1, 10), rep(0, 10), rep(1, 20))
d1 <- rbind(b, c)
beta.pair(d1, index.family = "sorensen")

b <- c(rep(0, 5), rep(1, 8), rep(0, 27))
c <- c(rep(1, 5), rep(0, 5), rep(1, 3), rep(0, 27))
d2 <- rbind(b, c)
beta.pair(d2, index.family = "sorensen")

# situation 2, put to proportion of situation 1 (scaled to 40 species)
b <- c(rep(0, 15), rep(1, 25))
c <- c(rep(1, 15), rep(0, 15), rep(1, 10))
d2 <- rbind(b, c)
beta.pair(d2, index.family = "sorensen")


#########
# scale beta to number of species?
# in order to scale, I need to know how to scale.

# common presence

# create simulation dataset
b <- c(rep(0, 10), rep(1, 10))
c <- c(rep(1, 10), rep(0, 10))

# n_common <- 0
res <- data.table("n_common_species" = seq(0, 1000), "beta" = as.numeric(1000), "beta.to" = as.numeric(1000), "beta.nes" = as.numeric(1000))
for(n_common in seq(0, 1000)){
  d <- rbind(c(b, rep(1, n_common)), c(c, rep(1, n_common)))
  beta <- beta.pair(d, index.family = "sorensen")
  beta_sor <- beta$beta.sor
  beta_to <- beta$beta.sim
  beta_nes <- beta$beta.sne
  rm(beta)
  res[n_common_species == n_common, beta := beta_sor]
  res[n_common_species == n_common, beta.to := beta_to]
  res[n_common_species == n_common, beta.nes := beta_nes]
}
rm(n_common); rm(beta_nes); rm(beta_to); rm(beta_sor)

# rational function shape f(x) = 1/x
ggplot(res, aes(x = n_common_species, y = beta)) +
  geom_point() +
  labs(title = "scaling betadiversity", subtitle = "change or beta turnover with number of\nmutual species a",
       caption = TeX("$\\beta$ turnover can be scaled with : $\\beta_{to-scaled} = \\beta_{to} * \\frac{2a+b+c}{2a+2+b+c}$")) +
  xlab("n of mutual species")
plot(res$n_common_species, res$beta.to)
x <- seq(0, 1000)
points(res$n_common_species, res$beta.to * ((2*res$n_common_species)+10+10)/((2*res$n_common_species)+2+10+10), col = "red")
