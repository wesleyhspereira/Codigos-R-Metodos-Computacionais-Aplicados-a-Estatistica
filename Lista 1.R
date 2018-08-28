########### Aula 1 - Ex 1 ##########
f = function(x) exp(x + x^2)
integrate(f, lower = -2, upper = 2)

#3.3 exp(x + x^2) entre -2 e 2

#função transformada
h = function(y) {
  x = 4*y - 2
  exp(x + x^2)
} 

#integral
4*mean(h(runif(10000)))

#3.7 exp(-x^2) entre -inf e inf

#função transformada
h = function(y) {
  x = -log((1-y)/y)
  
  exp(-(x)^2)*((1+exp(-x))^2)/exp(-x)
  
}

mean(h(runif(10000)))

#3.9 exp(-x-y) 0 < y < x, 0 < x < inf

#função transformada
h = function(k, l) {
  x = l/(1-l)
  y = k*x
  
  exp(-(x+y))*x*(1+x)^2
}

mean(h(runif(100000), runif(100000)))


#

########### Aula 1 - Ex 3 ##########
#3.11
set.seed(10)
################
rmixp = function(n, p) {
  u = rbinom(n, prob = p, size=1)
  x = rnorm(n)
  y = rnorm(n, 3, 1)
  c(x[u==1], y[u==0])
}

dmixp = function(x, p) {
  p*dnorm(x) + (1-p)*dnorm(x, 3, 1)
}


pl = function(p, n=1000) {
  amostra = rmixp(n, p)
  x = seq(min(amostra)-1, max(amostra)+1, 0.001)
  hist(amostra, freq = F, main="Histrograma da amostra simulada")
  lines(x, dmixp(x, p), col='red')
  
}


pl(p=0.75)
pl(p=0.6)
pl(p=0.5)

#mais próximo de p=0.5 -> maior bimodalidade

#3.12
rmgp = function(n, a, b) {
  rgamma(n, a, b) %>% rpois(n, .)
}

rmgp(1000, 4, 2) %>% hist()




########### Aula 1 - Ex 4 ##########

fross4.2 = function(q)
{
  U = runif(1)
  i = 0
  p = exp(-q)
  FP = p
  while(U >= FP)
  {
    p = (q*p)/(i+1)
    FP = FP + p
    i = i + 1
  }
  return(i)
}

rpoisson = function(n,q)
{
  X = rep(q,n)
  return(sapply(X,fross4.2))
}

########### Aula 1 - Ex 5 ##########

fross4h.1 = function(v)
{
  N = length(v) - 1
  i = as.numeric(names(sort(v[match(names(v[v < 1/N]),names(v))])[1]))
  z = v[i] + v[-i]
  j = as.numeric(names(sort(v[match(names(z[z >= 1/N]),names(v))]))[1])
  k = as.numeric(match(i,as.numeric(names(v))))
  q = N*v[k]
  q[2] = 1 - q
  names(q) = c(i,j)
  return(q)
}

fross4h.2 = function(p)
{
  q = list()
  N = length(p)
  t = N - 1
  for(j in 1:(N-2))
  {
    q[[j]] = fross4h.1(p)
    i = match(names(q[[j]]),names(p))
    c = (1/t)*q[[j]]
    p[i[1]] = p[i[1]] - c[1]
    p[i[2]] = p[i[2]] - c[2]
    p = p[-which(p == 0)]*(t/(t-1))
    t = t - 1
  }
  q[[j+1]] = p
  return(q)
}

fross4h.3 = function(q,p)
{
  n = length(p)
  N = ceiling((n-1)*runif(1))
  u = runif(1)
  g = as.numeric((names(q[[N]])))
  x = ifelse(u<as.numeric(q[[N]][1]),g[1],g[2])
  return(x)
}

rmultimix = function(n,p)
{
  names(p) = 1:length(p)
  q = fross4h.2(p)
  x = replicate(n,fross4h.3(q,p))
  return(x)
}

