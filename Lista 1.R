########### Aula 1 - Ex 1 ##########

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


########### Aula 1 - Ex 6 ##########
#minha g será uma t com df baixo, para ter uma calda mais pesada

#densidade da skew normal 
dskew = function(x, ep, w, a) {
  (2/w)*dnorm((x-ep)/w)*pnorm(a*((x-ep)/w))
}

#usando SIR 
rskew = function(n, ep, w, a, m) {
  
  g = function(x, ep) dnorm(x, ep, sd=100)
  y = rnorm(m, mean = ep, sd=100)
  w = (dskew(y, ep=0, w=2, a=6)/g(y, ep=0))/sum((dskew(y, ep=0, w=2, a=6)/g(y, ep=0)))
  sample(y, size=n, prob = w, replace = T) 
  
  
}

########### Aula 2 - Ex 1 ##########

###### 9.3 #######
erg = function(x) cumsum(x)/(1:length(x))


set.seed(10)
n = 100000
burn = 1000
sigma2 = 20
#declarando x 
x = numeric(length = n)
#g( |X ) será normal(X, 100)
x[1] = rnorm(1, mean=0, sd = sigma2)

#f = cauchy padrão
f = function(x) dcauchy(x, 0, 1)

aceitacao = 0
for(i in 1:n) {
  y = rnorm(1, x[i], sigma2)
  u = runif(1)
  xt = x[i]
  r = (f(y)*dnorm(xt, y, sigma2))/(f(xt)*dnorm(y, mean=xt, sigma2))
  if(u <= r) {
    x[i+1] = y
    aceitacao = aceitacao + 1
  }else{
    x[i+1] = xt
  }
}

#taxa de aceitação
aceitacao/n

#traço da cadeia
plot(x, type='l')

#média ergódiga
plot(erg(x), type='l')


#burn-in
amostra = x[-c(1:burn)]

#gerando os decis
simulado = quantile(amostra, seq(0.1, 0.9, 0.1))
real = qcauchy(seq(0.1, 0.9, 0.1), 0, 1)

names(real) = names(simulado)

#tabela
data.frame(real, simulado)

plot(real, simulado)
abline(a=0, b=1)


###### 9.4 #######


rlaplace = function(n, init, sigma2) {
  #a distribuição laplace é proporcional a:
  f = function(x) exp(-abs(x))
  
  x = numeric(n)
  x[1] = rnorm(1, mean = init, sd = sigma2)
  accept = 0
  u = runif(n)
  for(i in 2:n) {
    y = rnorm(1, mean = x[i-1], sd = sigma2)
    xt = x[i-1]
    r = f(y)/f(xt)
    if(u[i] <= r) {
      x[i] = y
      accept = accept + 1
    }else{
      x[i] = xt
    }
  }
  
  return(list(cadeia = x, taxa = accept/n))
}

set.seed(28)
#gerando três cadeias
cad1 = rlaplace(1000, init = 1, sigma2 = 20)
cad2 = rlaplace(1000, init = 1, sigma2 = 10)
cad3 = rlaplace(1000, init = 1, sigma2 = 5)

#taxa de aceitação de cada cadeia
cad1$taxa
cad2$taxa
cad3$taxa


#trace plot de cada cadeia
cad1$cadeia %>% plot(type='l') #visita menos estados, hipótese de irredutibilidade fraca
cad2$cadeia %>% plot(type='l')
cad3$cadeia %>% plot(type='l')

###### 9.8 #######

rbg = function(n, m, a=10, b=10, init = list("x" = 0, "y" = 0.1)) {
  x = numeric(n)
  x[1] = init$x
  
  y = numeric(n)
  y[1] = init$y
  
  for(j in 2:n) {
    x[j] = rbinom(1, size = m, prob = y[j-1])
    y[j] = rbeta(1, x[j-1] + 1, m - x[j-1] + b)
  }
  
  return(cbind(x,y))
}

#gerando cadeis diferentes
cad1 = rbg(1000, m = 30, a = 30, b = 30)
cad2 = rbg(1000, m = 5, a = 1, b = 1)

#cadeia 1
cad1[,2] %>% plot(type='l') #traço
cad1[,2] %>% erg() %>% plot(type='l') #média ergódica

#cadeia 2
cad2[,2] %>% plot(type='l') #traço
cad2[,2] %>% erg() %>% plot(type='l') #média ergódiga

########### Aula 2 - Ex 2 ##########

###### 10.5 #######
#utilizando gibbs sampler, temos que:

#f(x|y) = c(y)exp(-xy), em que c(y) = y/(1-exp(-By)), x|y ~ exp(y)I(0<x<B)
#f(y|x) = c(x)exp(-xy), em que c(x) = x/(1-exp(-Bx)), y|x ~ exp(x)I(0<y<B)
#assumindo a priori B ~ exp(lambda)


#f(B|x, y) é exp(lambda)I(B>x+y)


#exponencial truncada em (0, B)
rtexp = function(n, lambda, B) {
  u = runif(n)
  qexp(u*pexp(B, rate=lambda), rate=lambda) %>% return()
}
#exponencial truncada em (x+y, Inf)
rtexpup = function(n, lambda, x, y) {
  u = runif(n)
  qexp(u*exp(-lambda*(x+y))+pexp(x+y,lambda), lambda)
}

init = list(x=4, y=3, B=6)
n = 10000
x = numeric(n); x[1] = init$x
y = numeric(n); y[1] = init$y
B = numeric(n); B[1] = init$B

for(i in 2:n) {
  x[i] = rtexp(1, y[i-1], B[i-1])
  y[i] = rtexp(1, x[i-1], B[i-1])
  B[i] = rtexpup(1, 1/4, x[i-1], y[i-1])
}

plot(erg(x), type='l')
plot(erg(y), type='l')
plot(erg(B), type='l')

burn = 1000
x = x[-c(1:1000)]
y = y[-c(1:1000)]
B = B[-c(1:1000)]

#E[x]
mean(x)

#E[XY]
mean(x*y)


###### 10.5 #######

#X|z,y ~ exp(1 + ay + bz)
#y|x,z ~ exp(1 + ax + cz)
#z|x,y ~ exp(1 + bx + cy)

init = list(x=1, y=1, z=1)
n = 3000
a = b = c = 1
x = numeric(n)
y = numeric(n)
z = numeric(n)
x[1] = init$x
y[1] = init$y
z[1] = init$z
for(i in 2:n) {
  x[i] = rexp(1, 1 + a*y[i-1] + b*z[i-1])
  y[i] = rexp(1, 1 + a*x[i-1] + c*z[i-1])
  z[i] = rexp(1, 1 + b*x[i-1] + c*y[i-1])
}


plot(erg(x), type='l') #x
plot(erg(y), type='l') #y
plot(erg(z), type='l') #z

burn = 1000
x = x[-c(1:1000)]
y = y[-c(1:1000)]
z = z[-c(1:1000)]

#E[XYZ]
mean(x*y*z)

########### Aula 2 - Ex 3 ##########
rweibull.slice = function(n, a, b) {
  x = numeric(n)
  x[1] = b*gamma(1 + 1/a)
  for(i in 2:n) {
    yr = runif(1,0, dweibull(x[i-1], a, b))
    f = function(x) {dweibull(x, a, b)-yr}
    ui = rootSolve::uniroot.all(f, lower = 0, upper = 10)
    x[i] = runif(1, ui[1], ui[2])
  }
  
  x
}

x = rweibull.slice(1000, 4, 3)
hist(x)

hist(x, freq = F)
lines(seq(0, max(x), 0.001), dweibull(seq(0, max(x), 0.001), 4, 3))


########### Aula 2 - Ex 4 ##########

#densidade 
dtnorm = function(x, mu, sigma, a, b) {
  dnorm((x-mu)/sigma, 0, 1)/(sigma*(pnorm((b-mu)/sigma)-pnorm((a-mu)/sigma)))
}


#gerando os números aleatórios
ftnorm = function(n, mu, sigma, a, b) {
  #f é proporcional a:
  f = function(x, mu, sigma) dnorm((x-mu)/sigma, mean=0, sd=1)
  #g será uma uniforme no intervalo (a,b)
  
  x = numeric(n)
  u = runif(n)
  x[1] = runif(1, a, b)
  accept = 0
  for(i in 2:n) {
    y = runif(1, a, b)
    xt = x[i-1]
    #como g é uniforme, g(Xt) == g(y)
    r = (f(y, mu, sigma)/f(xt, mu, sigma))
    if(u[i] <= r) {
      x[i] = y
      accept = accept + 1
    }else{
      x[i] = xt
    }
    
  }
  return(x)
}


#exemplo
hist(ftnorm(1000, mu=4, sigma=1, a=1, b=7), freq = F)
lines(seq(1, 7, 0.001), dtnorm(seq(1, 7, 0.001), 
                               mu=4, sigma=1, a=1, b=7))





