###########################################################
# Bootstrap to find sigma (variance of Bn)
#library("robcor")
boot_sigma1 = function(ngv, md)
{
    n = sum(ngv)
    B = rep(0, n)
    for (i in 1:n)
    {
        vaux1 = i
        vaux2 = c(1:n)[-i]
        vaux = c(vaux1, vaux2)
        mataux = md[vaux, vaux]
        B[i] = Bn(c(1, n - 1), mataux)#Bn
    }
    a = robacf(B,
               lag.max = 0,
               type = "covariance",
               plot = FALSE)
    varBboot = as.numeric(a$acf)
    #varBboot=var(B)
    return(varBboot)
}
