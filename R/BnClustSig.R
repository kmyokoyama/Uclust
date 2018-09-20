uclust <- function(data, alpha = 0.05, from_distances = FALSE) {
    if (from_distances) {
        mdm <- data
    }
    else {
        mdm <- as.matrix(dist(data))
    }

    n <- dim(mdm)[1]

    is.h <- is_homo(rep = 15, mdm) # encontra o max do Bn pad e faz o Teste do Maximo

    homogeneous_test_output <- is.h   #tirar depois

    if (is.h$p.MaxTest < alpha) { # se nao homogeneo
        bootB <- is.h$bootB
        bootB1 <- is.h$bootB1
        oBn <- rep.optimBn(mdm, bootB = bootB) # encontra o maximo do Bn
        maxBn <- -oBn$minFobj

        n1 <- length(oBn$grupo1)
        is.group <- t.Bnbonf(maxBn, n, n1, alpha, bootB = bootB, bootB1 = bootB1)

        if (is.group$significant == TRUE) {
            clust <- oBn$grupo1
            p <- is.group$p.value
        }
        else {
            #1-faz a otimização primeiro c n1=1
            #2-faz a otimização restrita aos tamanhos de grupos centrais
            #3-faz o teste pro maior Bn entre 1 e 2 - se signif - para
            #caso contrario repete 2 e 3.
            n2 <- n-n1
            minsize <- (n1 == floor(n/2) || n2 == floor(n/2)) # os tamanhos com menor variancia para os quais nao queremos fazer otimizacao restrita
            oBn1 <- maxBnsize1(mdm)
            maxBn1 <- oBn1$maxBn

            if (minsize == TRUE) {
                clust <- oBn1$grupo1
                is.group <- t.Bnbonf(maxBn1, n, 1, alpha, bootB = bootB, bootB1 = bootB1)
                p <- is.group$p.value
                n1 <- 1
            }

            while (minsize == FALSE && is.group$significant == FALSE) { # para quando cegou no tamanho de grupo
                # centro da funcao smile ou quando achar um grupo significativo
                n1m <- min(n1,n2)
                n1_min <- n1m + 1
                n1_max <- n-n1_min
                oBn <- rep.optimBn_restrict(mdm, n1_max, n1_min)
                maxBn <- -oBn$minFobj # comparar com o Bn do grupo de tamanho 1
                maxBnmax <- max(maxBn1, maxBn)
                if (maxBnmax == maxBn1) {
                    n1 <- 1
                }
                else {
                    n1 <- length(oBn$grupo1) # definir o n1 com base no que tiver max Bn
                }

                is.group <- t.Bnbonf(maxBnmax, n, n1, alpha, bootB = bootB, bootB1 = bootB1)
                n2 <- n-n1
                minsize <- (n1 == floor(n/2) || n2 == floor(n/2))

                if (is.group$significant == TRUE) {
                    clust <- oBn$grupo1
                    p <- is.group$p.value
                }
                if (minsize == TRUE && is.group$significant == FALSE) {
                    clust <- oBn1$grupo1
                    is.group <- t.Bnbonf(maxBn1, n, 1, alpha, bootB = bootB, bootB1 = bootB1)
                    p <- is.group$p.value
                    n1 <- 1
                }
            }
        }
        alpha_correct <- alpha/(2^(n-1)-1)
    }
    else { # se homogeneo
        clust <- 1:n
        p <- is.h$p.MaxTest
        n1 <- n
        alpha_correct <- alpha
    }

    if (n1 != n) {
        ord <- c(clust,(1:n)[-clust])
        bn <- Bn(c(n1, (n-n1)), mdm[ord, ord])
        varBn <- is.group$varBn
        ishomo <- FALSE
    }
    else {
        n1 <- length(is.h$grupo1)
        ishomo <- TRUE
        bn <- Bn(c(length(is.h$grupo1), length(is.h$grupo2)), mdm[c(is.h$grupo1, is.h$grupo2), c(is.h$grupo1, is.h$grupo2)])
        varBn <- (bn/is.h$minFobj)^2
    }

    clust2 <- c(1:n)[-clust]
    ans <- list(clust, clust2, p, alpha_correct, n1, ishomo, bn, varBn, homogeneous_test_output)

    # cluster1, cluster2, p.value <- o que fica
    names(ans) <- c("cluster1", "cluster2", "p.value", "alpha_corrected", "n1", "ishomo", "Bn", "varBn", "homogeneous_test_output")

    return(ans)
}
