#' @export
is_homo <- function(mdm, rep = 50, test_max=TRUE) {
    Fobj <- vector()
    n <- dim(mdm)[1]
    grupo1 <- list()

    ans <- optimstdBn(mdm)
    Fobj[1] <- ans$Fobj[length(ans$Fobj)]
    grupo1[[1]] <- ans$grupo1
    bootB <- ans$bootB

    bootB1 <- ans$bootB1

    for (i in 2:rep) {
        ans <- optimstdBn(mdm, bootB = bootB, bootB1 = bootB1)
        Fobj[i] <- ans$Fobj[length(ans$Fobj)]
        grupo1[[i]] <- ans$grupo1
        #print(Fobj)
    }

    minFobj <- min(Fobj)
    g1 <- grupo1[[which(Fobj == min(Fobj))[1]]]
    g2 <- (1:n)[-g1]
    if (test_max == TRUE) {
        p.homo <- test_max_stdBn(minFobj, n)

        if (length(g1) == 1) {
            varBn <- bootB1
        }
        else {
            varBn <- bootB * smile(n)[length(g1)]/smile(n)[floor(n/2)]
        }

        ans <- list(minFobj, g1, g2, p.homo, Fobj,grupo1, bootB, bootB1, varBn)
        names(ans) <- list("minFobj", "grupo1", "grupo2", "p.MaxTest", "Fobj", "grupos1", "bootB", "bootB1", "varBn")
    }
    else {
        print("g1")
        print(g1)
        p.homo <- t.Bnbonf(-minFobj, n, length(g1), alpha = 0.05, bootB, bootB1, std = TRUE)
        alpha_b <- alpha/(2^(n-1)-1)
        ans <- list(minFobj, g1, g2, p.homo$p.value, alpha_b, Fobj, grupo1, bootB, bootB1, p.homo$varBn)
        names(ans) <- list("minFobj", "grupo1", "grupo2", "p.Bonferroni", "alpha_Bonferroni", "Fobj", "grupos1", "bootB", "bootB1", "varBn")
    }

    return(ans)
}
