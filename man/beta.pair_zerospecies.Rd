% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preparing_diversity.R
\name{beta.pair_zerospecies}
\alias{beta.pair_zerospecies}
\title{beta.pair handle plots without species}
\usage{
beta.pair_zerospecies(x, index.family = "sorensen")
}
\arguments{
\item{x}{a data.set with same requirements as for beta.pair. *Note* for the project
BetaDivMultifun, the function `prepare_for_betapair` can be used. For more 
information, please read the documentation for beta.pair.}

\item{index.family}{is "sorensen" as set by default. Originally, it could be either
"sorensen" or "jaccard", but no special behaviour has been 
implemented for the "jaccard" index.}
}
\value{
same as for function beta.pair. A list with three dissimilarity matrices,
being beta.sim, beta.sne and beta.sor. For more information, please read the
documentation for beta.pair.
}
\description{
The function `betapart::beta.pair()` can not handle plot without species.
The turnover and nestedness components are set to NaN due to division by 
zero in the formula, in both cases that two plots are compared which both do not
contain any species and if one plot contains species and one does not.
This is mathematically correct, but these special cases can be handled 
differently to correspond better to biological interpretation.
}
\details{
This function aims to add this feature to beta.pair() without changing the 
overall behaviour of the function.

Betadiversity can be set to 0 or 1 in the described cases : 

If both plots do not contain any species, according to e.g. Carlo Ricotta
https://doi.org/10.1002/ece3.2980 , this can be interpreted as the
ultimate loss of betadiversity, and thus betadiversity can be set to 0.
e.g. Plot "P1" contains 3 species, "P2" and "P3" do not contain any species : 
P1 : 1 1 1 0 0 0 0
P2 : 0 0 0 0 0 0 0
P3 : 0 0 0 0 0 0 0

betadiversity(P2, P3) is set to beta.sor = 0 = beta.sne + beta.sim

If one plot contains species and the other not, the difference is 
pure nestedness, i.e. the maximal possible amount of nestedness, because
100% of the difference among plots is nestedness.
betadiversity(P1, P2) is set to beta.sne = 1, beta.sim = 0, beta.sor = 1

What is different compared to the beta.pair function?
Cases of plots with zero species are catched by the output of betapart.core.
The cases are set to 0 or 1, as described above.


note that some tests of the function are provided as example.
}
