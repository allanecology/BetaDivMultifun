##########################
#
# GDM Sensitivity
# order or variables
#
##########################

# AIM
# Does the order of variables affect the % variance explained in GDM?
# Assumption : GDM calculates % variance explained of variable 1, what is left is checked with variable 2.
#    that would mean the variable input order affects the % variance explained.

# REQUIREMENTS
require(data.table)
require(gdm)
require(cowplot)
# gdminput : dataset which is read in by "results_nonpublic.R"

# example 1
example_vars1 <- c("distance", "weights", "s1.xCoord", "s1.yCoord", "s2.xCoord", "s2.yCoord", 
                  "s1.autotroph.beta.sim", "s1.symbiont.soilfungi.beta.sim", "s1.LUI",
                  "s2.autotroph.beta.sim", "s2.symbiont.soilfungi.beta.sim", "s2.LUI")
sequence1 <- gdm.varImp(gdminput[, example_vars1], geo = T, nPerm = 2, fullModelOnly = T)
sequence1[2]

example_vars2 <- c("distance", "weights", "s1.xCoord", "s1.yCoord", "s2.xCoord", "s2.yCoord", 
                   "s1.symbiont.soilfungi.beta.sim", "s1.LUI", "s1.autotroph.beta.sim", 
                   "s2.symbiont.soilfungi.beta.sim", "s2.LUI", "s2.autotroph.beta.sim")
sequence2 <- gdm.varImp(gdminput[, example_vars2], geo = T, nPerm = 2, fullModelOnly = T)
sequence2[2]

variable_sequence_test <- rbindlist(list(data.table(t(sequence1[[2]][,1])), 
               data.table(t(sequence2[[2]][,1]))), use.names = T)
variable_sequence_test[, "sequence" := as.factor(c("A", "B"))]
variable_sequence_test <- melt(variable_sequence_test, value.name = "value", id.vars = "sequence")

example1 <- ggplot(variable_sequence_test, aes(x = variable, y = value, fill = sequence)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90))
# the sequence of variables does not matter


# example 2
example_vars1 <- c("distance", "weights", "s1.xCoord", "s1.yCoord", "s2.xCoord", "s2.yCoord", 
                   "s1.tertiary.consumer.beta.sne", "s1.edis_soil", "s1.plantparasite.protist.beta.sne",
                   "s2.tertiary.consumer.beta.sne", "s2.edis_soil", "s2.plantparasite.protist.beta.sne")
sequence1 <- gdm.varImp(gdminput[, example_vars1], geo = T, nPerm = 2, fullModelOnly = T)
sequence1[2]

example_vars2 <- c("distance", "weights", "s1.xCoord", "s1.yCoord", "s2.xCoord", "s2.yCoord", 
                   "s1.plantparasite.protist.beta.sne", "s1.tertiary.consumer.beta.sne", "s1.edis_soil",
                   "s2.plantparasite.protist.beta.sne", "s2.tertiary.consumer.beta.sne", "s2.edis_soil")
sequence2 <- gdm.varImp(gdminput[, example_vars2], geo = T, nPerm = 2, fullModelOnly = T)
sequence2[2]

variable_sequence_test <- rbindlist(list(data.table(t(sequence1[[2]][,1])), 
                                         data.table(t(sequence2[[2]][,1]))), use.names = T)
variable_sequence_test[, "sequence" := as.factor(c("A", "B"))]
variable_sequence_test <- melt(variable_sequence_test, value.name = "value", id.vars = "sequence")

example2 <- ggplot(variable_sequence_test, aes(x = variable, y = value, fill = sequence)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90))


plot_grid(example1, example2, labels = "AUTO")
