readr::read_csv("~/codes/python/koizumi_faces.csv", col_names = FALSE)
koi <- readr::read_csv("~/codes/python/koizumi_faces.csv", col_names = FALSE)
koi <- as_data_frame(koi)
koi
str(koi)
mdm <- as.matrix(dist(koi))
devtools::load_all(".")
koi <- koi[1:5, ]
koi
is_homo(mdm = mdm, rep = 10)
mdm <- as.matrix(dist(koi))
is_homo(mdm = mdm, rep = 10)
bush <- readr::read_csv("~/codes/python/bush_faces.csv", col_names = FALSE)
bush <- as_data_frame(bush)
data <- rbind(koi, bush[1, ])
mdm <- as.matrix(dist(data))
Utest(ngv = c(5, 1), numB = 10, md = mdm)
koi <- readr::read_csv("~/codes/python/koizumi_faces.csv", col_names = FALSE)
koi <- as_data_frame(koi)
koi <- koi[1:10, ]
data <- rbind(koi, bush[1, ])
mdm <- as.matrix(dist(data))
Utest(ngv = c(10, 1), numB = 10, md = mdm)
koi <- readr::read_csv("~/codes/python/koizumi_faces.csv", col_names = FALSE)
koi <- as_data_frame(koi)
koi <- koi[1:15, ]
data <- rbind(koi, bush[1, ])
mdm <- as.matrix(dist(data))
Utest(ngv = c(10, 1), numB = 25, md = mdm)
Utest(ngv = c(10, 1), numB = 25, md = mdm)
Utest(ngv = c(10, 1), numB = 25, md = mdm)
is_homo(mdm = mdm, rep = 10)
is_homo(mdm = as.matrix(dist(koi)), rep = 10)
is_homo(mdm = as.matrix(dist(koi)), rep = 10)
koi5 <- koi[1:5, ]
is_homo(mdm = as.matrix(dist(koi5)), rep = 10)
is_homo(mdm = as.matrix(dist(rbin(koi5, bush[1, ]))), rep = 10)
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[1, ]))), rep = 10)
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[1, ]))), rep = 10)
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[2, ]))), rep = 10)
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[2, ]))), rep = 10)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[2, ]))), rep = 10)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[1, ]))), rep = 10)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[3, ]))), rep = 10)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[4, ]))), rep = 10)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[4, ]))), rep = 20)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[4, ]))), rep = 20)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[4, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[1, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[2, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi, bush[2, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi, bush[1, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi5)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi5)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi)), rep = 50)$p.MaxTest
koi_less <- koi[, 1:1000]
is_homo(mdm = as.matrix(dist(koi_less)), rep = 50)$p.MaxTest
koi5_less <- koi5[, 1:1000]
is_homo(mdm = as.matrix(dist(koi5_less)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi5_less)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi5)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(koi5)), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[1, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[2, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[3, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[4, ]))), rep = 50)$p.MaxTest
is_homo(mdm = as.matrix(dist(rbind(koi5, bush[5, ]))), rep = 50)$p.MaxTest
Utest(ngv = c(5, 1), numB = 10, md = as.matrix(dist(rbind(koi5, bush[5, ]))))
Utest(ngv = c(5, 1), numB = 10, md = as.matrix(dist(rbind(koi5, bush[5, ]))))
Utest(ngv = c(5, 1), numB = 10, md = as.matrix(dist(rbind(koi5, bush[5, ]))))
Utest(ngv = c(5, 1), numB = 10, md = as.matrix(dist(rbind(koi5, bush[5, ]))))
Utest(ngv = c(5, 1), numB = 100, md = as.matrix(dist(rbind(koi5, bush[5, ]))))
Utest(ngv = c(5, 1), numB = 100, md = as.matrix(dist(rbind(koi5, bush[1, ]))))
Utest(ngv = c(5, 1), numB = 100, md = as.matrix(dist(rbind(koi5, bush[1:2, ]))))
Utest(ngv = c(5, 1), numB = 100, md = as.matrix(dist(rbind(koi5, bush[1:3, ]))))
Utest(ngv = c(5, 1), numB = 100, md = as.matrix(dist(rbind(koi, bush[1:3, ]))))
savehistory("~/repositories/Uclust/scripts/faces.R")
