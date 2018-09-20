col1_50 <- matrix(rnorm(500, mean = 0, sd = 1), ncol = 50)
col51_100 <- matrix(rnorm(500, mean = 10, sd = 5), ncol = 50)
col101_150 <- matrix(rbeta(500, shape1 = 1.0, shape2 = 0.5), ncol = 50)
col151_200 <- matrix(rbeta(500, shape1 = 10.0, shape2 = 5), ncol = 50)

data1 <- cbind(col1_50, col51_100, col101_150, col151_200)
#data2 <- matrix(rnorm(200, mean = 1, sd = 2), ncol = 200)
data2 <- c(rnorm(50, mean = 10, sd = 1),
           rnorm(50, mean = 10, sd = 5),
           rbeta(50, shape1 = 1.0, shape2 = 0.5),
           rbeta(50, shape1 = 10.0, shape2 = 5))

data <- rbind(data1, data2)

mdm <- as.matrix(dist(data))

u_test_pvalue <- Utest(c(10, 1), 100, mdm)

print(u_test_pvalue)
