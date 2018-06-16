hilgard = readr::read_table2("data/hilgard.txt")
hilgard = readxl::read_xls("data/hilgard.xls")

n = hilgard$`Sample size`
n = (n/sqrt(n + 3))^2
z_transform = hilgard$`Fisher's Z`
z = z_transform*sqrt(n)
N = nrow(hilgard)
hilgard$t = hilgard$Correlation*sqrt((n-2)/(1 - hilgard$Correlation^2))

anderson_pb = data.frame(z = z,
                         M = n,
                         lower = rep(qnorm(0.95), N),
                         upper = rep(0, N),
                         dist_indices = rep(5, N),
                         outcome = as.factor(hilgard$Outcome),
                         best = as.factor(hilgard$`Best?`))

anderson_pb$dist_indices[anderson_pb$z < qnorm(0.95)] = 10

anderson_npb = anderson_pb
anderson_npb$dist_indices = 20