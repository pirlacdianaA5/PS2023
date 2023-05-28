#TEMA A
#A1
#a)
ex1a = function(lambda, p, k, n) {
  po = dpois(0:n, lambda)
  geo = dgeom(0:n, p)
  bi = dbinom(0:n, k, p)
  
  hist(po[k:n])
  hist(geo[k:n])
  hist(bi[k:n])
  
}
ex1a(3, 0.2, 5, 25)

## b
p=0.3
P_X_impar = dgeom(seq(1, 100000, 2), p)

P_X_mare_sau_egala_cu_4 = 1 - pgeom(3, p)

P_X_mica_sau_egala_cu_20 = pgeom(20, p)

sum(P_X_impar)
P_X_mare_sau_egala_cu_4
P_X_mica_sau_egala_cu_20

#c)  PROBLEME!!!!!!!!!
# Parametrii
lambda <- 2  # Parametrul lambda pentru distributia Poisson

# Determinarea celei mai mici valori k0 pentru care P(Y >= k0) < 10^(-7)
k0 <- 0  # Valoarea initiala pentru k0
while (ppois(k0, lambda) >= 10^(-7)) {
  k0 <- k0 + 1
}

# Afisarea rezultatului
print(paste("Cea mai mica valoare a lui k0:", k0))

#A2
ex2a=function(x) {
  yp = vector()
  ys = vector()
  yp[1] = median(p)
  yp[2] = mean(p)
  yp[3] = sd(p)
  yp[4] = as.vector(quantile(p))[1 + 1] # Q1
  yp[5] = as.vector(quantile(p))[1 + 2] # Q2
  yp[6] = as.vector(quantile(p))[1 + 3] # Q3
  
  ys[1] = median(s)
  ys[2] = mean(s)
  ys[3] = sd(s)
  ys[4] = as.vector(quantile(s))[1 + 1] # Q1
  ys[5] = as.vector(quantile(s))[1 + 2] # Q2
  ys[6] = as.vector(quantile(s))[1 + 3] # Q3
  
  print(yp);
  print(ys);
}
x=read.csv("note.csv", header=T, sep=",")
p=x[['P']]
s=x[['S']]
ex2a(x)
# sample=scan("note.txt")
summary(p)

ex2b = function(x) {
  n = length(x)
  medie = mean(x)
  s = sd(x) # devSt
  
  left = medie - s * 2
  right = medie + s * 2
  
  y = vector()
  z = vector()   # pentru a afisa valorile aberante
  p=0
  l = 0
  for(i in 1:n) {
    if(x[i] - left > 0 && x[i] - right < 0) {
      l = l + 1
      y[l] = x[i]
    }
    else {
      p=p+1
      z[p]=x[i]
    }
  }
  
  return (y)  #(returneaza esantionul fara valori aberante)
  
  # return (z) #(returneaza valorile aberante)
}
x=read.csv("note.csv", header=T, sep=",")
p=x[['P']]
s=x[['S']]
ex2b(p);
ex2b(s)

ex2c = function(x) {
  sample = c(ex2b(x)) # vectorul curatat
  C=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  #am scris numerele pe intervale de la 10 la 100 deoarece punctajele din note.txt apartin acestor intervale, valorile mai mici de 10 fiind valori aberante eiminate
  
  hist(sample, breaks=C,freq = F ,right = T)
}
x=read.csv("note.csv", header=T, sep=",")
p=x[['P']]
s=x[['S']]
ex2c(p)
ex2c(s)
