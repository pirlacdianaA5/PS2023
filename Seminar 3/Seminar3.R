#Laborator 3
#I
density_exponential = function(lambda, n, a) {
          x = seq(0, a, n);
          y = dexp(x, lambda);
          plot(x, y, type = 'l');
}

density_exponential(2.0,1,10)
#I.1.a)

density_gama = function(alpha,lambda, n, a) {
  x = seq(0, a, n);
  y = dexp(x, alpha, rate=lambda);
  plot(x, y, type = 'l');
}
density_gama(1,1/2,0.1,10)

#I.1.b)
density_student = function(r, n, a) {
  x = seq(0, a, n);
  y = dt(x, r);
  plot(x, y, type = 'l');
}
density_student(1,0.1,5)
density_student(10,0.1,5)
density_student(0.5,0.1,5)

#II.LNM

#Var1
LLN_Poisson = function(lambda, n) { #n este nr de generari
  sum = 0;
  for(i in 1:n) {
    u = rpois(1, lambda); #R generarea unei var aleatorii(random) si pois de poisson , 1 vine de la fptul ca generam cate un nr
    sum = sum + u;  #X1+X2+...+Xn
  }
  return(sum/n);
}
LLN_Poisson(2,100)
LLN_Poisson(2,10000)
LLN_Poisson(2,100000)

#Var2
LLN_Poisson = function(lambda, n) {
  return(mean(rpois(n, lambda))); 
}
LLN_Poisson(2,100000)


LLN_Gamma = function(alfa, lambda, n) {
  return(mean(rgamma(n, alfa, lambda)));
}
LLN_Gamma(1,2,100000)

#II.1.a)
LLN_Exp = function(lambda, n) {
  return(mean(rexp(n, lambda))); 
}
LLN_Exp(2,1000000)
 
#II.1.b)
LLN_Exp = function(m,p, n) {
  return(mean(rbinom(n, m,p))); 
}
LLN_Exp(5,0.2,100000000)


#III.TLC

CLT_Poisson = function(lambda, n, N, z) { # lambda distributie, N sa fie suficient de mare pt multe astfel de sume,
  expectation = lambda; #media teoretica a unei var aleatoare
  st_dev = lambda;
  upper_bound = z*st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) { #calculam P
    x_n = mean(rpois(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1; # P_N
    }
  }
  return(c(sum/N,pnorm(z))); #pt multi-argumets
  #X la k de i sunt valori simulate conform distributiei date Poisson ın cazul exercitiului de fata).Se compara apoi aceasta probabilitate cu pnorm(z).
}
CLT_Poisson(2,30,10000,1)
CLT_Poisson(2,1000,10000,1)

#III.1
CLT_Exp = function(lambda, n, N, z) { # lambda distributie, N sa fie suficient de mare pt multe astfel de sume,
  expectation = 1/lambda; 
  st_dev = 1/ lambda; 
  upper_bound = z*st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) { 
    x_n = mean(rexp(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1; 
    }
  } #Obtinem distributii exponentiale
  return(c(sum/N,pnorm(z))); #pt multi-argumets
}
CLT_Exp(2,30,100000,0)


#IV.M-L

#Standardizarea a unei var binomiale
binomial_probability = function(n, p, k) {
  expectation = n*p;##media pt distr liniara
  variance = n*p*(1 - p);  # X>k dispersia
  standard_deviation = sqrt(variance);
  q = (k + 0.5)/standard_deviation; #
  return(1 - pnorm(q));
}
binomial_probability(50,0.3,10)




#EXERCITII
#I.1.c)

density_norm=function(m,sigma,n,a)
{
  x = seq(0, a, n);
  y = dnorm(x,mean=m, sd= sigma)
  plot(x, y, type = "l",lwd=2, col="blue",  xlab="x", ylab="f(x)")
}
density_norm(0,1,3,100)

#II.2.
Student = function()
{
  # valori ale lui n si r
  n_values <- c(1000, 10000, 100000, 1000000)
  r_values <- c(1, 2, 3, 4, 5)
  
  for (r in r_values)
  { 
    if (r == 1 || r == 2)
      next 
    sigma <- sqrt(r/(r-2)) 
    for (n in n_values)
    {
      sigma_medie <- sigma/sqrt(n)  
      cat("Pentru n =", n, "si r =", r, ", deviatia standard a mediei aritmetice a", n, "variabile aleatoare Xi este", sigma_medie, "\n")
    }
  }
}
Student()

#III.2
verify_TLC= function(alpha, lambda, n, N, z) {
 
  data = rgamma(n * N, alpha, lambda)  # Generăm n * N variabile aleatoare Gamma(alpha, lambd)
  sample_mean = mean(data)  # Calculăm media 
  sample_sd = sd(data) #Calc deviația standard a datelor
  
  # Calculăm media și deviația standard pentru distribuția sumei de n variabile aleatoare Gamma(alpha, lambd)
  sum_mean = n * alpha / lambda
  sum_sd = sqrt(n * alpha / lambda^2)
  
  # Calculăm valoarea Z folosind formula standardizării
  z_value = (sample_mean - sum_mean) / (sum_sd / sqrt(n))
  
 
  if (z_value < z) {
    cat("Valoarea Z ", z_value, " este mai mică decât", z, "\n")
  } else {
    cat("Valoarea Z ", z_value, " este mai mare decât", z, "\n")
  }
}

verify_TLC(2, 0.5, 50, 10000, -1.5)


#IV.1.

binomial_probability = function(n, p, k) {
  expectation = n*p; #media pt distr liniara
  variance = n*p*(p-1);
  standard_deviation = sqrt(variance);
  q = (k + 0.5)/standard_deviation; #
  return(1 - pnorm(q));
}
binomial_probability()

