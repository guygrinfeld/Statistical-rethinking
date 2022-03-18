library(rethinking)
### relationship approximation of binomial relationship

p_grid = seq(0,1, len = 1000)
prob_p = rep(1, 1000)
prob_data = dbinom(6,9, prob  = p_grid)
posterior = prob_data*prob_p
posterior = posterior/sum(posterior)

plot(p_grid, posterior)


### samples from the posterior 

samples = sample(p_grid, prob = posterior , size = 1e4, replace =T)
hist(samples)

w = rbinom(1e4, size = 9, prob = samples) 

### week 1 homework

#1
p_grid = seq(0,1, len = 1000)
prob_p = rep(1, 1000)
prob_data = dbinom(4,15, prob  = p_grid)
posterior = prob_data*prob_p
posterior = posterior/sum(posterior)

plot(p_grid, posterior, type = "l")

#2
prob_p = c(rep(0, 500), rep(1,500))
prob_data = dbinom(4,6, prob  = p_grid)
posterior = prob_data*prob_p
posterior = posterior/sum(posterior)

plot(p_grid, posterior)

samples = sample(p_grid, prob = posterior , size = 1e6, replace =T)
hist(samples)

#3
plot( samples , ylim=c(0,1))
quantile(samples, c(0.055, 0.945))
PI(samples, .89)
HPDI(samples, prob = .89)

#4 

W = rbinom(1e5, 1, prob= 0.7)
mistake = sample(which(W == 1), size = 0.2*length(which(W == 1)))
W[mistake] = 0

mean(W)

data = sample(W, 20)
prob_p = rep(1, 1000)
prob_data = dbinom(sum(data),length(data), prob  = p_grid)
posterior = prob_data*prob_p
posterior = posterior/sum(posterior)
samples = sample(p_grid, prob = posterior , size = 1e6, replace =T)
hist(samples)

