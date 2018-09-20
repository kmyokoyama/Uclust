data1 <- matrix(rnorm(5000, mean = 3, sd = 2), ncol = 500)
data2 <- matrix(rnorm(5000, mean = 3, sd = 2), ncol = 500)

data <- rbind(data1, data2)

mdm <- as.matrix(dist(data))

is_homogenous <- is_homo(mdm)

print(is_homogenous$p.MaxTest)
