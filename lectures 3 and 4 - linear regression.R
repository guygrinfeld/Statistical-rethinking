library(rethinking)

data("Howell1")
d = Howell1

precis(d, hist = F)

data = d[d$age >18,]
dens(data$height)

##plot priors 
curve(dnorm(x, 178, 20), from=100, to=250)

## sampling from the prior - Simulation
sample_mu = rnorm(1e4, 178, 20)
sample_sigma = runif(1e4, 0,50)
prior_h = rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

## quadractic approximation
flist = alist( #make a list of priors and model defenitions
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0,50)
)

m1 = quap(flist, data = data) #fit

precis(m1)
vcov(m1)
cov2cor(vcov(m1))

post = extract.samples(m1, 1e4) # sample from the posterior (via simulation)
precis(post)


## linear model

### simulate priors
N = 100
a = rnorm(N, 178, 20) # intercept for centered data = prior for mean hight
b = rlnorm(N, 0, 1) # slope = prior in lognormal distribution
dens(b, xlim = c(0,5), adj= 0.1)

plot(NULL, xlim=range(data$weight), ylim=c(-100, 400),
     xlab = "weight", ylab = "height")
abline(h = 0, lty = 2)
abline(h=272, lty = 1, lwd =.5)
mtext("b ~ dlnorm(0,1)")
xbar = mean(data$weight)
for(i in 1:N) curve(a[i] + b[i]*(x-xbar),
                    from = min(data$weight), to = max(data$weight), add = T,
                    col = col.alpha("black", 0.2))

### model definition and fit
m2 = quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar), 
    a ~ dnorm(178, 20),
    b ~ dlnorm(0,1), 
    sigma ~ dunif(0,50)
  ), data = data  )
precis(m2)
cov2cor(vcov(m2))
pairs(m2)

#### check model prediction
plot(height ~ weight , data, col = rangi2)
post = extract.samples(m2)
a_map= mean(post$a)
b_map = mean(post$b)
curve(a_map + b_map*(x-xbar), add = T)

#### what the model knows about the value of 50?
mu_at_50 = post$a + post$b * (50 - xbar)
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu |weight = 50")
PI(mu_at_50)

#### repeat for every value in the sample using link
mu = link(m2)
mu = link(m2, data = data.frame(weight = seq(25, 70, 1)))

## plot
weight.seq = seq(25, 70,1)
plot(height ~ weight, data, type = "n")
for(i in 1:100)  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI, prob = 0.8)
plot(height ~ weight, data,col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq, col = col.alpha("black", 0.9))

### prediction intervals
sim.height = sim(m2, data = list(weight = weight.seq))
str(sim.height)

height.PI = apply(sim.height, 2, PI, prob = 0.89)
####plot
plot(height ~ weight, data,col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean) # MAP line
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq, col = col.alpha("black", 1))


# chapter 5

#prior = extract.prior(model)
#mu = link(model, post = prior, data = list(...) )
#plot(NULL, xlim = c(), ylim = c())
#for(i in 1:50 ) lines(xlim, mu[i,], col = col.alpha("black.0.4))

#Homework

data = d[d$age>=18,]

# 1
xbar = mean(data$height)
model = quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*(height - xbar), 
    a ~ dnorm(60, 20),
    b ~ dlnorm(0,1), 
    sigma ~ dunif(0,50)
  ), data = data  )

## check model prediction
height.seq = 50:200
mu = link(model, data = data.frame(height = height.seq))
mu.mean = apply(mu, 2, mean)
with(data, 
     plot(height, weight)
     )
lines(height.seq, mu.mean)

post1 = sim(model, data = list(height = c(140, 160, 175)))
apply(post1, 2, mean)
apply(post1, 2, PI, prob = .89)

#2
data = d[d$age < 13, ]

## prior checks

m_age = mean(data$age)
m_height = mean(data$height)
model2 = quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a +  b1*(age - m_age), 
    a ~ dnorm(15, 4),
    b1 ~ dlnorm(0,1), 
    sigma ~ dunif(0,50)
  ), data = data  )

model2.1 = quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a +  b1*(age - m_age) + b2*(height-m_height), 
    a ~ dnorm(15, 4),
    b1 ~ dlnorm(0,1),
    b2 ~ dlnorm(0,1),
    sigma ~ dunif(0,50),
    
    height ~ dnorm(mu_h, sigma_h),
    mu_h <- b_h0 + b_h1*(age-m_age),
    b_h0 ~ dnorm(100, 10),
    b_h1 ~ dlnorm(0, 1), 
    sigma_h ~ dunif(0,50)
  ), data = data  )

prior = extract.prior(model2)
mu = link(model2, post = prior, data = list(age = seq(0, 13, 0.5)) )
plot(NULL, xlim = c(0, 13), ylim = c(0, 50), xlab ="age", ylab="weight")
for(i in 1: 50) lines(seq(0, 13, 0.5), mu[i,], col = col.alpha("black",0.4))

precis(model2)
precis(model2.1)


##plot post

age.seq = seq(0, 13, .5)
mu = link(model2, data = data.frame(age = age.seq))
mu.mean = apply(mu, 2, mean)
with(data, 
     plot(age, weight)
)
lines(age.seq, mu.mean) # MAP line
mu.PI = apply(mu, 2, PI)
shade(mu.PI, age.seq)
sim.weight = sim(model2, data = list(age = age.seq))
weight.PI = apply(sim.weight, 2, PI)
shade(weight.PI, age.seq)


lines(height.seq, mu.mean)

post1 = sim(model, data = list(height = c(140, 160, 175)))
apply(post1, 2, mean)
apply(post1, 2, PI, prob = .89)

### use the counterfactual estimate
s = sim(model2.1, data=data.frame(age=0:13), vars = c("height", "weight"))
mean(s$weight[,2] - s$weight[,1])

#3
data$sex = data$male +1

m_age = by(data$age, data$sex, mean)


model3 = quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[sex] +  b1[sex]*(age - m_age[sex]), 
    a[sex] ~ dnorm(15, 4),
    b1[sex] ~ dlnorm(0,1), 
    sigma ~ dunif(0,50)
  ), data = data  )

precis(model3, depth = 2)
post = extract.samples(model3)
post$diff_fm = post$b1[,1] - post$b1[,2]
precis(post, depth = 2)

#Bonus
data("Oxboys")
data = Oxboys

library(tidyverse)

data = data %>% group_by(Subject) %>% mutate(delta = height - lag(height))
data = data %>% drop_na()

data$index = data$Occasion -1

model4 = quap(
  alist(
    delta ~ dlnorm(mu, sigma),
    mu <- delta[index],
    delta[index] ~ dlnorm(0,1),
    sigma ~ dunif(0,5)
  ), data = data )

precis(model4, depth = 2)

plot(data$Occasion, data$height)
mu = link(model4, data = list(Occasion=1:9))
mu.mean = apply(mu,2,mean)
lines(1:9, mu.mean)

model4.1 = quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- b0 + b1*Occasion,
    b0 ~ dnorm(120, 20),
    b1 ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ), data = data )
