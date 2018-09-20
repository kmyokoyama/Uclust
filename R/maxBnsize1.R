maxBnsize1 <- function(mdm) {
    n <- dim(mdm)[1]
    vecBn <- vector()

    for (i in 1:n) {
        vecBn[i] <- Bn(c(1, n - 1), mdm[c(i, c(1:n)[-i]), c(i, c(1:n)[-i])])
    }

    maxBn <- max(vecBn)
    ans <- list(maxBn, which(vecBn == maxBn))
    names(ans) <- c("maxBn", "grupo1")

    return(ans)
}
