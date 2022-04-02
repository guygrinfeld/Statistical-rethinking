### over fitting
library(rethinking)
library(tidyverse)
#1 
d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)

d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1
m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.9,depth=2)

m6.10 <- quap(
               alist(
                 happiness ~ dnorm( mu , sigma ),
                 mu <- a + bA*A,
                 a ~ dnorm( 0 , 1 ),
                 bA ~ dnorm( 0 , 2 ),
                 sigma ~ dexp(1)
               ) , data=d2 )
precis(m6.10)
samples = extract.samples(m6.9)

simulation = sim(m6.10, data = list(A = seq(0,1, length = 20)), vars = "age")
plot(seq(18, 85, length =20),apply(simulation, 2, mean))



simulation %>% ggplot(aes(x = A, y = happiness, color = mid)) + geom_point() + geom_smooth()

c1 = compare(m6.9, m6.10, func = WAIC)
plot(c1)

c2 = compare(m6.9, m6.10, func = PSIS)


#2

data(foxes)

data = foxes

data = data %>% mutate(F = scale(avgfood), A = scale(area), W = scale(weight), G = scale(groupsize))

model1 = quap(
  alist(
    A ~ dnorm(mu, sd),
    mu <- a + b*F,
    a ~ dnorm(0, 0.2),
    b ~ dnorm(0, 1),
    sd ~ dexp(1)
  ), data = data 
)


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

compare(model1, model2, model2.1)


#3
data(cherry_blossoms)
d <- cherry_blossoms

d2 <- d[ complete.cases(d$doy) , ] # complete cases 
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

m3 <- quap( 
         alist(
           D ~ dnorm( mu , sigma ) ,
           mu <- a + B %*% w ,
           a ~ dnorm(100,10),
           w ~ dnorm(0,10),
           sigma ~ dexp(1)
         ), data=list( D=d2$doy , B=B ) ,
         start=list( w=rep( 0 , ncol(B) ) ) )

precis(m3, depth= 2)


m3.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + b1*year + b2*year^2,
    a ~ dnorm(100,10),
    c(b1, b2) ~ dnorm(0,10),
    sigma ~ dexp(1)),
    data=list( D=d2$doy , year=scale(d2$year, scale = F) 
  )
)

compare(m3, m3.1)
