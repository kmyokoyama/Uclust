#' Optimization of \code{Bn}.
#'
#' @param mdm Numeric vector.
#' @param itmax Numeric scalar. The maximum number of iterations.
#' @param centers Numeric scalar. The number of centers.
#' @param bootB Numeric scalar. The number of bootstrap iterations.
#' @return The group to which the objective function based on standardized
#'   \code{Bn} had been converged to a minimum.
optimBn <- function(mdm, itmax = 200, centers = -1, bootB = -1) {
    # Distance matrix.
    md <- mdm
    # Number of series.
    n <- dim(md)[1]

    # Set data structures.
    it <- 1
    ass <- vector()
    ass_old <- rep(2, n)
    # Keep track of the optimization procedure.
    ASS <- matrix(ncol = n, nrow = itmax)
    Fobj  vector()

    # Compute the smile function.
    Cn <- vector()
    varBn <- vector()
    numB <- 2000
    # Return the variance of Bn with group size c(floor(n/2), (n-floor(n/2)).
    if (bootB == -1) {
        bootB <- boot_sigma(c(floor(n / 2), (n - floor(n / 2))), numB, md)
    }

    # Cn[1] and C[n-1] are the same.
    Cn[1] <- Cn[n-1] <- (1) / ((n - 2) * n * (n - 1))

    # The others must be calculated.
    for (n1 in 2:(n-2)) {
        # TODO: We can optimize it.
        n2 <- n - n1
        Cn[n1] <- (((n1 * n2) / (n * (n - 1)) ^ 2) * (2 * n ^ 2 - 6 * n + 4) / ((n1 - 1) * (n2 - 1)))
    }

    for (n1 in 1:(n-1)) {
        # TODO: We can also optimize it.
        n2 <- n - n1
        varBn[n1] <- Cn[n1] * bootB / Cn[floor(n / 2)]
    }

    # Start optimization by initializing the parameters.

    # Initialize centers with random points from the sample if they were not defined.
    if (centers == -1) {
        centers <- sample(n, 2)
    }

    # Assign observations to the group with the closest center.
    for (i in 1:n) {
        ass[i] <- (md[i, centers[1]]) > (md[i, centers[2]])
    }

    ASS[1, ] <- ass

    # Start iterations.
    while (it < itmax && !prod(ass == ass_old)) {
        ass_old <- ass

        ord <- sample(n, n)
        for (i in ord) {
            ass[i] <- 0
            f0 <- objBn(ass, md)
            ass[i] <- 1
            f1 <- objBn(ass, md)
            if (f0 < f1) {
                ass[i] <- 0
            }
        }
        Fobj[it] <- objBn(ass, md)

        it <- it + 1
        ASS[it, ] <- ass
    }

    ans <- list(which(ass == ass[1]), Fobj, it - 1, ASS[1:(it+1), ], bootB)
    names(ans) <- c("grupo1", "Fobj", "numIt", "history", "bootB")

    ans
}
