#source("optimBn_restrict.R")

rep.optimBn_restrict <- function(mdm, n1_max, n1_min, rep = 15) {
    ans <- optimBn_restrict(mdm, n1_max, n1_min)
    Fobj <- vector()
    n <- dim(mdm)[1]
    grupo1 <- list()
    Fobj[1] <- ans$Fobj[length(ans$Fobj)]
    grupo1[[1]] <- ans$grupo1

    for (i in 2:rep) {
        ans <- optimBn_restrict(mdm, n1_max, n1_min)
        Fobj[i] <- ans$Fobj[length(ans$Fobj)]
        grupo1[[i]] <- ans$grupo1
    }

    minFobj <- min(Fobj)
    g1 <- grupo1[[which(Fobj == min(Fobj))[1]]]
    g2 <- (1:n)[-g1]

    ans <- list(minFobj, g1, g2, Fobj, grupo1)
    names(ans) <- list("minFobj", "grupo1", "grupo2", "Fobj", "grupos1")

    return(ans)
}
