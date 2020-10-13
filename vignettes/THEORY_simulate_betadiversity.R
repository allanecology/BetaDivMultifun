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


########
# alpha diversity
#
# are alpha and double 0 related? how to include any information of alpha diversity 
# in my analysis?

# use bird example dataset with turnover and nestedness
data(bbsData)
del <- beta.pair(bbs1980)

sim <- del$beta.sim
sim <- as.matrix(sim)
sim[!lower.tri(sim, diag = F)] <- 1000
sim <- reshape2::melt(sim, value.name = "beta.sim")
sim <- data.table(sim)
sim <- sim[beta.sim != 1000, ]

sne <- del$beta.sne
sne <- as.matrix(sne)
sne[!lower.tri(sne, diag = F)] <- 1000
sne <- data.table(reshape2::melt(sne, value.name = "beta.sne"))
sne <- sne[beta.sne != 1000, ]

simul_data <- merge(sim, sne, by = c("Var1", "Var2"))
rm(sne); rm(sim); rm(del)

# calc alpha diversity
# rowsums
alpha <- data.frame(rowSums(bbs1980))
colnames(alpha) <- "alpha1"
alpha$Var1 <- rownames(alpha)
alpha <- data.table(alpha)

simul_data <- merge(simul_data, alpha, by = "Var1")
setnames(alpha, old = c("Var1", "alpha1"), new = c("Var2", "alpha2"))
simul_data <- merge(simul_data, alpha, by = "Var2")

# mean alpha
simul_data[, mean_alpha := rowMeans(simul_data[,.(alpha1, alpha2)])]
# calc double 0s
# 
# length(intersect(which(bbs1980["AL", ] == 0), which(bbs1980["AK", ] == 0)))
# # second way of testing
# test <- rbind(bbs1980["AL", ], bbs1980["AK", ])
# sum(colSums(test) == 0)
simul_data[, "double_0" := -1]
for(i in 1:nrow(simul_data)){
  var1 <- as.character(simul_data[i, Var1])
  var2 <- as.character(simul_data[i, Var2])
  d0 <- length(intersect(which(bbs1980[var1, ] == 0), which(bbs1980[var2, ] == 0)))
  simul_data[i, double_0 := d0]
}
rm(var1); rm(var2); rm(i); rm(d0)



hist(simul_data$beta.sne)
hist(simul_data$beta.sim)

d0 <- ggplot(simul_data, aes(x = double_0, y = mean_alpha)) +
  geom_point()
d1 <- ggplot(simul_data, aes(x = double_0, y = alpha1)) +
  geom_point()
d2 <- ggplot(simul_data, aes(x = double_0, y = alpha2)) +
  geom_point()

abi <- ggplot(simul_data, aes(x = mean_alpha, y = beta.sim)) +
  geom_point()

abn <- ggplot(simul_data, aes(x = mean_alpha, y = beta.sne)) +
  geom_point()

abibn <- ggplot(simul_data, aes(x = mean_alpha, y = beta.sim, col = beta.sne)) +
  geom_point()

plot_grid(d0, d1, d2, abi, abn, abibn)
