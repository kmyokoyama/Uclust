#' Test for group homegeneity.
#' 
#' Applies the maximum homogeneity test for the configuration of the max
#' padronized \code{Bn}. Repeat the optimization algorithm \code{rep} times and
#' chooses the maximum.
#' @param md A n x n matrix of distances where \eqn{n = n1 + n2}.
#' @param rep Numeric scalar. The number of independent runs of the algorithm to
#'   take care of local maximum.
#' @return A list [needs improvement].
homogeneity.max.test <- function(md, rep = 5) {
    Fobj <- vector()
    n <- dim(md)[1]
    grupo1 <- list()
    
    for (i in 1:rep) {
        ans <- optimBn(md)
        Fobj[i] <- ans$Fobj[length(ans$Fobj)]
        grupo1[[i]] <- ans$grupo1
    }
    minFobj <- min(Fobj)
    p.homo <- max_stdBn(minFobj, n)
    
    return(list(Fobj, "Fobj", grupo1, "grupo1", p.homo, "p.homo"))
}
