#Function for performing pairwise comparisons using the function “adonis”  (R package vegan) direct on distance matrices
#From: Dias, R. A., Gianuca, A. T., Vizentin-Bugoni, J., Gonçalves, M. S. S., Bencke, G. A., & Bastazini, V. A. Livestock disturbance in Brazilian grasslands influences avian species diversity via turnover. Biodiversity and Conservation, 1-18.
#see msuplmentary material 2 for further details
###############################
install.packages("vegan")
require(vegan)

#R code:
PW.dist=pw.adonis.dist(as.matrix(x),factors,sim.method="bray",p.adjust.m = "bonferroni"); PW.dist

#arguments:
#x = community matrix
#factors = a column or vector with all factors to be tested pairwise
#sim.method = similarity function, one of the functions available in vegdist()
#p.adjust.m = the p.value correction method, one of the methods supported by p.adjust()
#the matrix must be transformed into a square matrix using the “as.matrix” function

#start of function
{
  library(vegan)
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  for(elem in 1:ncol(co)){
    ad = adonis(as.dist(x[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem])),factors %in% c(as.character(co[1,elem]),as.character(co[2,elem]))]) ~ 
                  factors[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem]))]);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}
#end of function
###############################

References:
  Baselga A, Orme D, Villeger S, De Bortoli J, Leprieur F (2015) Betapart. R package version 1.3. https://cran.r-project.org/web/packages/betapart/index.html. Accessed 11 May 2016
Oksanen J, Blanchet FG, Kindt R, Legendre P, Minchin PR, O'Hara RB, Simpson GL, Solymos P, Stevens MHH, Wagner H (2016) Vegan: community ecology package. R package version 2.3-5. https://cran.r-project.org/web/packages/betapart/index.html. Accessed 28 September 2016
ResearchGate (2016) Questions. How can I do PerMANOVA pairwise contrasts in R? https://www.researchgate.net/post/How_can_I_do_PerMANOVA_pairwise_contrasts_in_R#view=588791bcf7b67e4e3e034809. Accessed 01 February 2017
