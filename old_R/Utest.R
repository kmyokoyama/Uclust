#' Test for homogeneity of two groups.
#' 
#' @param ngv A numeric vector \code{c(n1, n2)} where \code{n1} is the number of
#'   elements in group 1 and \code{n2} is the number of elements in group 2.
#' @param numB Numeric scalar. The number of bootstraps. It's recommended
#'   \eqn{1000 < numB < 10000}.
#' @param md A n x n matrix of distances where \eqn{n = n1 + n2}.
#' @return p-value based on bootstraps.
Utest = function(ngv, numB, md)
{
    ng <- sum(ngv)
    comp <- Bn(ngv, md)
    vaux <- rep(0, ng)
    mataux <- matrix(0, ng, ng)
    absoluto <- rep(0, numB)
    
    for (i in 1:numB)
    {
        vaux <- floor(runif(ng, 1, (ng + 1)))
        mataux <- md[vaux, vaux] # Fill in the matrix with all possible combinations of vaux by rows.
        aux <- Bn(ngv, mataux)
        if (aux <= comp)
        {
            absoluto[i] = 1
        }
        else
        {
            absoluto[i] = 0
        }
    }
    
    p.value = 1 - (sum(absoluto))/numB
    
    p.value
}
