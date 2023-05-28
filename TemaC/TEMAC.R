#C1
graph_area = function(a, N) {
  count = 0;
  for(i in 1:N) {
    x1 = runif(1, -sqrt(a), sqrt(a))
    x2 = runif(1, -sqrt(a), sqrt(a))
    x3 = runif(1, 0, a)
    if(x3>x1^2+x2^2 && x3<=a)
      count = count+1
  }
  volum = 2*sqrt(a)*2*sqrt(a)*a
  return(volum*count/N)
}

rel_error = function(trueval, aprox)
{
  return(abs(trueval-aprox)/abs(trueval))
}

rel_error(pi*2^2/2, graph_area(2, 20000))
rel_error(pi*4^2/2, graph_area(4, 20000))
rel_error(pi*10^2/2, graph_area(10, 20000))

#C2
#y=0:3
#x=0:4
graph_area_T = function(N) {
  count = 0;
  for(i in 1:N) {
    x = runif(1, 0, 4)
    y = runif(1, 0, 3)
    if(3*y<=x+6 && y<=12-3*x)
      count = count+1
  }
  area = 4*3
  return(area*count/N)
}

graph_area_T(30000)#=9

#C3
#a)
MC_integration_a = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    sum = sum + (x+1)/sqrt(4-x^2)
  }
  return((1-(-1))*sum/N);
}

print(c(MC_integration_a(10000), pi/3))

#b)
MC_integration_b = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    x=-u
    sum = sum + (1/(x^2+4))/exp(-u);
  }
  return(sum/N);
}

print(c(MC_integration_b(10000), pi/4))
#c)
MC_integration_c = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    x=-u
    sum = sum + (x*exp(x))/exp(-u);
  }
  return(sum/N);
}

print(c(MC_integration_c(10000), -1))

#C4
#a)
Nr_days = function(m, n, p, q)
{
  nr_days=1
  fake=m
  while (fake>0) {
    
    new_fake_today=rbinom(1, n, p)
    fake = fake + new_fake_today
    
    deactivated_today = rbinom(1, fake, q)
    fake = fake - deactivated_today
    
    nr_days = nr_days+1
  }
  return(nr_days)
}
Nr_days(100, 500, 0.1, 0.9)

Nr_days_mean = function(m, n, p, q, N)
{
  sum=0
  for (i in 1:N)
    sum = sum + Nr_days(m, n, p, q)
  return(sum/N)
}

#pentru valorile 100000, 500, 0.5, 0.1 programul ruleaza foarte mult timp
Nr_days_mean(100, 500, 0.1, 0.9, 1000)

fakes_after_40 = function(m, n, p, q)
{
  fake=m
  for (i in 2:40) {
    
    new_fake_today=rbinom(1, n, p)
    fake = fake + new_fake_today
    
    deactivated_today = rbinom(1, fake, q)
    fake = fake - deactivated_today
    
  }
  return(fake)
}

fakes_after_40(100, 500, 0.5, 0.1)

#b)
prob_b = function(m, n, p, q, N)
{
  count = 0;
  for (i in 1:N)
  {
    x = fakes_after_40(m, n, p, q)
    if (x <= 2200)#3900 
      count = count + 1
  }
  return(count/N)
}

prob_b(100, 500, 0.5, 0.1, 1000)

#c)
prob_c = function(m, n, p, q, N0, eroare, niv_incredere)
{
  alfa = 1 - niv_incredere
  z = qnorm(alfa/2)
  epsilon = eroare
  p = prob_b(m, n, p, q, N0)
  N_min = p*(1 - p)*(z/epsilon)^2
  
  #return(N_min)
  return(prob_b(m, n, p, q, floor(N_min+1)))#de ce da 0?
}

prob_c(100, 500, 0.5, 0.1, 1000, 0.01, 0.99)
prob_b(100, 500, 0.5, 0.1, 10734)
