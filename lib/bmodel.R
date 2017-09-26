library(rjags)
library(lattice)

d <- read.table("res/full/summary")
d <- read.table("/home/runs/mfablet/te-insertions/res/full/summary")

d <- d[rowSums(d) != 0,]
d <- d[rowSums(d) < 500,]
y <- d$left_only
z <- d$right_only
hist((y), breaks=20)
hist(log(y), breaks=20)
hist((z), breaks=20)
hist(log(z), breaks=20)
x11()
plot(density(d$left_only/(d$left_only+d$left_with_match)))
lines(density(d$right_only/(d$right_only+d$right_with_match), na.rm=T), col="red")

model <- "model {
  for(i in 1:N) {
    y[i] ~ dpois(theta[i])
    z[i] ~ dpois(mu[i])
    mu[i] <- theta[i] * d[i] + (1 - d[i]) * theta[i] * (1 + nu)
    d[i] ~ dbern(p)
    logtheta[i] ~ dnorm(mu_logtheta, 1 / sd_logtheta ** 2)
    theta[i] <- 10 ** logtheta[i]
  }
  mu_logtheta ~ dunif(0,4)
  sd_logtheta ~ dunif(1,5)
  nu ~ dunif(0,2)
  p ~ dunif(0,1)
}
"

colnames(d)
N <- dim(d)[1]
d4j <- list(y = d$left_only + d$left_with_match, z = d$right_only + d$right_with_match, N = N)
inits <- list(
    list(mu_logtheta = 1, sd_logtheta = 2, d = rep(1,N), p=0.9, nu=1),
    list(mu_logtheta = 2, sd_logtheta = 3, d = rep(1,N), p=0.1, nu=1.5),
    list(mu_logtheta = 3, sd_logtheta = 1, d = rep(1,N), p=0.5, nu=0.5)
    )
m <- jags.model(file = textConnection(model), data = d4j, inits = inits, n.chains = 3, n.adapt = 1000)
update(m, 100000)
n.iter <- 10000
mcmc1 <- coda.samples(m, c("p", "mu_logtheta", "sd_logtheta", "nu"), n.iter = n.iter)
mcmc1_tot <- as.data.frame(as.matrix(mcmc1))

str(mcmc1)
colnames(mcmc1[[1]])
x11()
xyplot(mcmc1[[1]][,c("p", "mu_logtheta", "sd_logtheta", "nu")])


gelman.diag(mcmc1, multivariate=F)

## gelman.plot(mcmc1)
autocorr.plot(mcmc1[[1]][,"p"])
autocorr.plot(mcmc1[[1]][,"nu"])
cumuplot(mcmc1[[1]][,c("p","nu")])

mu_logtheta_tot <- mcmc1_tot[ , "mu_logtheta"]
sd_logtheta_tot <- mcmc1_tot[ , "sd_logtheta"]
p_tot <- mcmc1_tot[ , "p"]
nu_tot <- mcmc1_tot[ , "nu"]

table.sd.y <- vector(length = nrow(mcmc1_tot))
table.sd.z <- vector(length = nrow(mcmc1_tot))
for(i in 1 : nrow(mcmc1_tot)) {
    theta <- 10 ** rnorm(N,mu_logtheta_tot[i],sd_logtheta_tot[i])
    dreg <- rbinom(N, size = 1, prob = p_tot[i])
    table.sd.y[i] <- sd(rpois(N, theta))
    table.sd.z[i] <- sd(rpois(N, theta * (1 + nu_tot[i]) ** dreg))
}

summary(table.sd.y)
sd(y)

summary(table.sd.z)
sd(z)

xmax <- 500
plot( table(factor(y,levels=0:xmax)), type = "p", pch = 1, ylab = "fréquence", xlab = "nombre d'insertions")
for(i in 1:xmax) {
    quantiles <- quantile(table.y[, i] , prob = c(0.025, 0.975) )
    lines( c(i-1, i-1), c(quantiles[1], quantiles[2]) ,col = "blue" )
    median <- median(table.y[, i])
    points(i-1, median, pch = 19, col = "blue")
}
abline(h = 0)
