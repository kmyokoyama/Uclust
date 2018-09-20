###########################################################
# Bootstrap to find sigma (variance of Bn)
#source("Bn.R")
library("robcor")
boot_sigma = function(ngv, numB = 2000, md)
{
    n = sum(ngv)
    if (n > 6) {
        B = rep(0, numB)
        for (i in 1:numB)
        {
            vaux1 = sample(n, n, replace = FALSE)
            mataux = md[vaux1, vaux1]
            B[i] = Bn(ngv, mataux)#Bn
        }
        varBboot = var(B)
    }
    else{
        B = vector()
        cb = combn(n, 2)
        for (i in 1:(dim(cb)[2]))
        {
            vaux1 = cb[, i]
            vaux2 = c(1:n)[-vaux1]
            vaux = c(vaux1, vaux2)
            mataux = md[vaux, vaux]
            B[i] = Bn(ngv, mataux)#Bn
        }
        a = robacf(B,
                   lag.max = 0,
                   type = "covariance",
                   plot = FALSE)
        varBboot = as.numeric(a$acf)
    }
    return(varBboot)
}
