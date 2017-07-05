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
