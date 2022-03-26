library(rethinking)
library(tidyverse)
data(foxes)
## milticollinearity 


##homework

data = foxes

data = data %>% mutate(F = scale(avgfood), A = scale(area), W = scale(weight), G = scale(groupsize))

##1
model1 = quap(
  alist(
    A ~ dnorm(mu, sd),
    mu <- a + b*F,
    a ~ dnorm(0, 0.2),
    b ~ dnorm(0, 1),
    sd ~ dexp(1)
  ), data = data 
)

precis(model1)

##2
model2 = quap(
  alist(
    W ~ dnorm(mu, sd),
    mu <- a + b*F,
    a ~ dnorm(0, 0.2),
    b ~ dnorm(0, 1),
    sd ~ dexp(1)
  ), data = data 
)

model2.1 = quap(
  alist(
    W ~ dnorm(mu, sd),
    mu <- a + b1*F +b2*G,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    sd ~ dexp(1)
  ), data = data 
)
precis(model2)
precis(model2.1)



##4
N = 300
A = rnorm(N)
U = rnorm(N)
S = rnorm(N, 0.5*A - 0.25*U)
X = rnorm(N, -0.2*S + A)
Y = rnorm(N, 0.3*X + S -0.5*A + 0.7*U)

d = data.frame(A, U, S, X ,Y)

model4 = quap(
  alist(
    Y ~ dnorm(mu, sd),
    mu <- a + bx*X + bs*S + ba*A, 
    a ~ dnorm(0, .2),
    bx ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sd ~exp(1)
  ), data = d
)

precis(model4)

