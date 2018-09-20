#source("optimBn.R")

rep.optimBn = function(mdm, rep = 15, bootB = -1) {
    ans <- optimBn(mdm, bootB = bootB)
    Fobj <- vector()
    n <- dim(mdm)[1]
    grupo1 <- list()
    Fobj[1] <- ans$Fobj[length(ans$Fobj)]
    grupo1[[1]] <- ans$grupo1
    bootB <- ans$bootB

    for (i in 2:rep) {
        ans <- optimBn(mdm, bootB = bootB)
        Fobj[i] <- ans$Fobj[length(ans$Fobj)]
        grupo1[[i]] <- ans$grupo1
    }

    minFobj <- min(Fobj)
    g1 <- grupo1[[which(Fobj == min(Fobj))[1]]]
    g2 <- (1:n)[-g1]

    ans <- list(minFobj, g1, g2, Fobj, grupo1, bootB)
    names(ans) <- list("minFobj", "grupo1", "grupo2", "Fobj", "grupos1", "bootB")
    return(ans)
}
