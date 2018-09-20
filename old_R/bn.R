#' The test statistic \code{Bn}.
#'
#' A measure of distances between and within groups.
#'
#' It now allows for single element groups.
#'
#' @param ngv A numeric vector \code{c(n1, n2)} where \code{n1} is the number of
#'   elements in group 1 and \code{n2} is the number of elements in group 2.
#' @param distances A n x n matrix of distances where \eqn{n = n1 + n2}.
#' @return A value for the statistic \code{Bn}.
Bn <- function(ngv, distances)
{
    ng <- sum(ngv)

    # Total distance between groups.
    # It is always computed regardless of the size of groups 1 and 2.
    dist_between_groups <- 0.0
    for (i in 1:ngv[1])
    {
        for (j in (ngv[1] + 1):ng)
        {
            dist_between_groups <- dist_between_groups + distances[i, j]
        }
    }

    a1 <- (1 / (ngv[1] * ngv[2])) * dist_between_groups

    # Total distance within group 1.
    # It is only computed if group 1 has more than one element.
    if (ngv[1] > 1) {
        dist_within_group1 <- 0.0
        for (i in 1:(ngv[1] - 1))
        {
            for (j in (i + 1):ngv[1])
            {
                dist_within_group1 <- dist_within_group1 + distances[i, j]
            }
        }

        a2 <- (2 / (ngv[1] * (ngv[1] - 1))) * dist_within_group1
    }
    else {
        # It is used anyway to calculate Bn.
        a2 <- 0.0
    }

    # Total distance within group 2.
    # It is only computed if group 2 has more than one element.
    if (ngv[2] > 1) {
        dist_within_group2 <- 0.0
        for (i in (ngv[1] + 1):(ng - 1))
        {
            for (j in (i + 1):ng)
            {
                dist_within_group2 <- dist_within_group2 + distances[i, j]
            }
        }

        a3 <- (2 / (ngv[2] * (ngv[2] - 1))) * dist_within_group2
    }
    else {
        # It is used anyway to calculate Bn.
        a3 <- 0.0
    }

    # It is possible for a2 or a3 be zero.
    # TODO: What happens if both group 1 and group 2 are single element groups?
    Bn <- (ngv[1] * ngv[2] / (ng * (ng - 1))) * (2 * a1 - a2 - a3)

    return(Bn)
}

#' Bootstrap to find the variance of \code{Bn}.
#'
#' @return The variance of \code{Bn}.
boot_sigma <- function(ngv, md, sampling_method = "bootstrap", resampling_iter = 2000)
{
    n <- sum(ngv)

    if (sampling_method == "bootstrap") replacement <- TRUE
    else if (sampling_method == "jackknife") replacement <- FALSE
    else stop("Illegal sampling method.")

    if (n > 6) {
        B <- rep(0, resampling_iter)

        for (i in 1:resampling_iter)
        {
            vaux1 <- sample(n, n, replace = replacement)
            mataux <- md[vaux1, vaux1]
            B[i] <- Bn(ngv, mataux)#Bn
        }

        varBboot <- var(B)
    }
    else {
        B <- vector()
        cb <- combn(n, 2)

        for (i in 1:(dim(cb)[2]))
        {
            vaux1 <- cb[, i]
            vaux2 <- c(1:n)[-vaux1]
            vaux <- c(vaux1, vaux2)
            mataux <- md[vaux, vaux]
            B[i] <- Bn(ngv, mataux) # Bn.
        }

        a <- robcor::robacf(B, lag.max = 0, type = "covariance", plot = FALSE)

        varBboot <- as.numeric(a$acf)
    }

    return(varBboot)
}

# We need to merge it into boot_sigma function.
# I'll keep it in separate by now.
boot_sigma1 <- function(ngv, md)
{
    n <- sum(ngv)
    B <- rep(0, n)
    for(i in 1:n)
    {
        vaux1 <- i
        vaux2 <- c(1:n)[-i]
        vaux <- c(vaux1, vaux2)
        mataux <- md[vaux, vaux]
        B[i] <- Bn(c(1, n-1), mataux) # Bn.
    }
    a <- robcor::robacf(B, lag.max = 0, type = "covariance", plot = FALSE)

    varBboot <- as.numeric(a$acf)

    return(varBboot)
}
