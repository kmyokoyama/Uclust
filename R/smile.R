smile <- function(n) {
    Cn <- vector()

    # Cn[1] and C[n-1] are the same.
    Cn[1] <- Cn[n-1] <- (1) / ((n - 2) * n * (n - 1))

    for (n1 in 2:(n-2)) {
        n2 <- n - n1
        Cn[n1] <- (((n1*n2)/(n*(n-1))^2)*(2*n^2-6*n+4)/((n1-1)*(n2-1)))
    }

    return(Cn)
}
