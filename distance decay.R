### from:Bergamin, R. S., Bastazini, V. A. G., Vélez-Martin, E., Debastiani, V., Zanini, K. J., Loyola, R., & Müller, S. C. (2017). Linking beta diversity patterns to protected areas: lessons from the Brazilian Atlantic Rainforest. 
#Biodiversity and Conservation 26 (7): 1557–1568.
#DOI 10.1007/s10531-017-1315-y 
##code for fitting multiple models (i.e., intercept only, linear, exponential and  assintotic) for distance decay curves

install.packages("bbmle")
require(bbmle)

#data vectors  representing site similarity and geographic distances between sites
#similarity
#distance

#Models
# null
null=lm(similarity~1)
# linear
linear=lm(similarity~distance) 
# exponential
exponential=nls(similarity~a+b*log(distance),start = list(a=0.32,b=0))
# assintotic
assintotic=nls(similarity~a+b/(distance),start = list(a=0.32,b=0)) 

AICctab(null,linear,exponential,assintotic,nobs=100,weights = TRUE, delta = TRUE, base = TRUE)

plot(similarity~distance,xlab="Area ", ylab="Riqueza")
abline(null,col="green")
abline(linear,col="red")
lines(coefficients(exponential)[1]+(coefficients(exponential)[2])*log(1:max(distance)))
lines(coefficients(assintotic)[1]+(coefficients(assintotic)[2])/(1:max(distance)), col="blue")


