###################### 
#code for fitting multiple models (i.e., intercept only, linear, log, exponential and asymptotic model) for distance decay curves (i.e., the  decay of  ecological community similarity with geographic distance)
#####################

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
exponential =nls(similarity ~ a * exp(b * distance), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
exponential_log= =nls(similarity~a+b*log(distance),start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# asymptotic
asymptotic=nls(similarity~a+b/(distance),start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 


AICctab(null,linear,exponential,asymptotic,nobs=lentgh(x),weights = TRUE, delta = TRUE, base = TRUE)

#Plot data amd fitted models
plot(similarity~distance,xlab="Distance", ylab="Similarity")
abline(null,col="green")
abline(linear,col="red")
lines(coefficients(exponential)[1]+(coefficients(exponential)[2])*log(1:max(distance)))
lines(coefficients(asymptotic)[1]+(coefficients(asymptotic)[2])/(1:max(distance)), col="blue")


