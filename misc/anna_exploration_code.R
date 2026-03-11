
devtools::load_all()
data("wpp", package = "Sta380Project")

grp1 <- wpp[wpp$dev_group == "Less developed", ]$birth_rate
grp2 <- wpp[wpp$dev_group == "More developed", ]$birth_rate

# looking at the ks.test (base R)
D0 <- suppressWarnings(ks.test(grp1, grp2)$statistic)

# ASIDE: some testing using an external package
library(DHARMa)
model <- lm(birth_rate ~ dev_group, data = wpp)
r <- simulateResiduals(model)
plot(r)

# actually simulating the KS by hand and also using the histogram...

z <- c(grp1, grp2)
n <- nrow(wpp)

R <- 10^4 - 1
D <- numeric(R)
for (i in 1:R) {
  k <- sample(1:n, size = n/2, replace = FALSE)
  xi <- z[k]
  yi <- z[-k]
  D[i] <- suppressWarnings(ks.test(xi, yi)$statistic)
}

p <- mean(c(D0, D) >= D0)
p

hist(D, xlim = c(0, 0.8))
abline(v=D0, col = "red")

