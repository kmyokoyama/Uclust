#' Optimization of standardized \code{Bn}.
#'
#' @param dados Numeric vector.
#' @param itmax Numeric scalar. The maximum number of iterations.
#' @param centers Numeric scalar. The number of centers.
#' @return The group to which the objective function based on standardized
#'   \code{Bn} had been converged to a minimum.
optimBn <- function(dados, itmax = 200, centers = -1) {
    n <- dim(dados)[1]
    d <- dim(dados)[2]
    it <- 1
    ass <- vector()
    ass_old <- rep(2, n)
    ASS <- matrix(ncol = n, nrow = itmax)
    Fobj <- vector()
    # Smile function.
    Cn <- vector()
    varBn <- vector()
    numB <- 5000
    md <- as.matrix(dist(dados)^2)
    bootB <- boot_sigma(c(floor(n/2), (n - floor(n/2))), md, resampling_iter = numB) # Returns the variance of Bn.


    for (n1 in 2:(n - 2)) {
        n2 <- n - n1
        Cn[n1] <- (((4 * n1 * n2)/(n * (n - 1))^2) * (2 * n^2 - 6 * n + 4)/((n1 - 1) * (n2 - 1)))
    }
    for (n1 in 2:(n - 2)) {
        n2 <- n - n1
        varBn[n1] <- Cn[n1] * bootB/Cn[floor(n/2)]
    }

    if (centers == -1) {
        centers <- dados[sample(c(1:n), 2), ]
        cm <- colMeans(dados, na.rm = TRUE)
        vcm <- rbind(cm, cm)
        centers[is.na(centers)] <- vcm[is.na(centers)]
    }

    for (i in 1:n) {
        ass[i] <- (dist(t(cbind(dados[i, ], centers[1, ])))) > (dist(t(cbind(dados[i, ], centers[2, ]))))
    }
    ass
    ASS[1, ] <- ass


    # TRUE belongs to group 2.

    # Iterations.

    while (it < itmax && !prod(ass == ass_old)) {
        ass_old <- ass
        for (i in 1:n) {
            ass[i] <- 0
            f0 <- objstdBn(ass, varBn, md)
            ass[i] <- 1
            f1 <- objstdBn(ass, varBn, md)
            if (f0 < f1) {
                ass[i] <- 0
            }
        }

        Fobj[it] <- objstdBn(ass, varBn, md)
        it <- it + 1
        ASS[it, ] <- ass
    }

    ans <- list(which(ass == ass[1]), Fobj, it - 1, ASS[1:(it + 1), ], varBn)
    names(ans) <- c("grupo1", "Fobj", "numIt", "history", "varBn")

    ans
}
