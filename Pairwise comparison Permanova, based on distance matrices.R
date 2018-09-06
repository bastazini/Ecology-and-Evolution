###################### 
#Function for performing PerMANOVAs using the function “adonis”  (R package vegan) direct on distance matrices, intead of  community matrices
#From: Dias, R. A., Gianuca, A. T., Vizentin-Bugoni, J., Gonçalves, M. S. S., Bencke, G. A., & Bastazini, V.A.G.2017. Livestock disturbance in Brazilian grasslands influences avian species diversity via turnover. 
#Biodiversity and Conservation 26(10): 2473–2490. 
#DOI: https://doi.org/10.1007/s10531-017-1370-4
#see msuplmentary material 2 for further details
#This function was modified from  the function developed by Pedro Martinez Arbizu, which is available in the ResearchGate questions section (ResearchGate 2016; PM Arbizu in litt 2017).
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
pw.adonis.dist = function(x,factors, sim.method, p.adjust.m)
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

