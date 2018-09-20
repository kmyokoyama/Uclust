# It works only when optimizing non-standardized Bn.
# Returns non-standardized Bn for a given assignment.
# It needs documentation.
objBn <- function(assign, mdm, varBn = NULL) {
    n <- length(assign)
    n1 <- sum(assign == 0)
    n2 <- n - n1

    if (n1 == 0 | n2 == 0) {
        return(Inf)
    }
    else {
        vaux1 <- which(assign == assign[1])
        vaux2 <- c(1:n)[-vaux1]
        n1 <- length(vaux1)
        n2 <- n - n1
        m <- mdm[c(vaux1, vaux2), c(vaux1, vaux2)]
        Bns <- Bn(c(n1, n2), m)

        Bn <- ifelse(is.null(varBn), -Bns, -Bns/sqrt(varBn[n1]))

        return(Bn)
    }
}
