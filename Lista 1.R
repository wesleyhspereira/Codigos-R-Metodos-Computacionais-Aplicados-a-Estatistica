########### Aula 1 - Ex 3 ##########
#3.11
set.seed(10)
################
rmixp = function(n, p) {
  *rnorm(n) + *rnorm(n, 3, 1)
}

dmixp = function(x, p) {
  p*dnorm(x) + (1-p)*dnorm(x, 3, 1)
}


pl = function(p, n=1000) {
  amostra = mixp(n, p)
  x = seq(min(amostra)-1, max(amostra)+1, 0.001)
  hist(amostra, freq = F, main="Histrograma da amostra simulada")
  lines(x, dmixp(x, p), col='red')
  
}


pl(p=0.75)
pl(p=0.6)
pl(p=0.5)

#p=0.75
amostra = mixp(1000, 0.75)
x = seq(min(amostra), max(amostra), 0.001)
hist(amostra, freq = F)
lines(x, dmixp(x, 0.75))

#p=0.75
amostra = mixp(1000, 0.75)
x = seq(min(amostra), max(amostra), 0.001)
hist(amostra, freq = F)
lines(x, dmixp(x, 0.75))

#p=0.75
amostra = mixp(1000, 0.75)
x = seq(min(amostra), max(amostra), 0.001)
hist(amostra, freq = F)
lines(x, dmixp(x, 0.75))


hist(mixp(1000, 0.75))
lines(x, f(x))


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

