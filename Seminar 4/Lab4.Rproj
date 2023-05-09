#Laborator 4
#I. Estimarea ariilor si a volumelor

disc_area = function(N) { # imi da nr de pct pe care le generez 
  NC = 0;  #va numara pct care cad in interiorul cercului
  for(i in 1:N) {
    x = runif(1, -1, 1); # genereaza uniform 1 valoare intre -1 si 1
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1) #daca pct e in interior 
      NC = NC + 1;
  }
  return(4*NC/N); #o estimare a ariei cercului  4 este aria patratului in care am incadrat discul  
  } 

disc_area(10000)
disc_area(50000)

# Coloram cu rosu pct care sunt in int si albastru in ext

disc_area = function(N) { # imi da nr de pct pe care le generez 
  NC = 0;  #va numara pct care cad in interiorul cercului
  for(i in 1:N) {
    x = runif(1, -1, 1); # genereaza uniform 1 valoare intre -1 si 1
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1) #daca pct e in interior 
      NC = NC + 1;
  }
  return(4*NC/N); #o estimare a ariei cercului  4 este aria patratului in care am incadrat discul  
} 




#I.1.
Sphere_vol = function(N)
{
  NC=0;
  for(i in 1:N)
  {
    x=runif(1,-1,1)
    y=runif(1,-1,1)
    z=runif(1,-1,1)
    if(x*x + y*y + z*z <= 1)  #sa fie in interiorul sefrei
      NC=NC+1
  }
  return(8*NC/N);
}
  exactV=(4*pi)/3
  abs_err = abs(Sphere_vol(10000)-exactV)
  rel_err = abs_err/exactV*100
  
 print( cat("Volumul sferei unitate este ",exactV,", eroarea absoluta este ", abs_err,", iar eroarea relativa este ", rel_err))



I.2.
parabola = function(x) { # Definim functia parabolei
  return(-2*x^2 + 5*x - 2)
}
n <- 10000 #val uniforme
x <- runif(n, 0, 2)
y <- runif(n, 0, 2)
subparabola <- y <= parabola(x) #Verif punctele 
estimatA <- sum(subparabola) / n * 2 * max(parabola(x))
exactA <- abs(integrate(parabola, 0, 2)$value)
rel_err <- abs(estimatA - exactA) / exactA

cat("Aria estimata: ", aria_estimata, "\n")
cat("Aria exacta: ", aria_exacta, "\n")
cat("Eroarea relativa: ", rel_err, "\n")




#II.Integrarea MC 
#genereaza integrala unei  functii folosind generarea de numere aleatoare
#integrala de la a la b din h(X) dx = (b-a)/N *suma de la 1 la N de h(Ui)
#Ui: runif(1,a,b)

#Var1
MCintegration = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0; #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 0, 10); #Ui luam variabilele uniforme
    sum = sum + exp(-u*u/2); # suma din h(Ui)
  }
  return(10*sum/N); #(b-a)/N *suma de la 1 la N de h(Ui)
}
MCintegration(10000)

#Pentr a calcula o medie pentru k=30 astfel de aproximari si  deviatia standard 
MCintegraverage= function(k, N) {
  estimates =c() #initializare vector
  for(i in 1:k){
    estimates[i] = MCintegration(N);
  }
  print(mean(estimates)); #media tuturor estimarilor
  print(sd(estimates)); #deviatia standard a estimarilor
}
MCintegraverage(30, 20000)
MCintegraverage(30, 50000)


#Var2
# Cand nu luam variabile uniforme 1/N * suma de la 1 la N din h(Xi)/f(Xi)
#unde f: densitatea lui Xi
MCimprovedintegration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1); # Generăm un punct aleator 'u' dintr-o distribuție exponentială cu parametrul 1
    sum = sum + exp(-u*u)/exp(-u); # aici luam distributia exponentiala
  }
  return(sum/N);
}
#Ne spune cat de bune sunt estimarile (l am ales pe N)
MCimprvdintegraverage= function(k, N) {
  estimates = c( );
  for(i in 1:k)
    estimates[i] = MCimprovedintegration(N);
  print(mean(estimates)); #
  print(sd(estimates)); #cat de bune e estimarea
}
MCimprvdintegraverage(30, 20000)
MCimprvdintegraverage(30, 50000)

#II.1.a)

MCintegrationa = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0; #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 0, pi); #Ui luam variabilele uniforme
    sum = sum + sin(u)^2; # suma din h(Ui)
  }
  return(pi*sum/N); #(b-a)/N *suma de la 1 la N de h(Ui)
}
intMC=MCintegrationa(10000)
print(intMC)

print(cat("eroarea abs este ", abs(intMC-pi/2)))

print(cat("eroarea relativa", abs(intMC-pi/2)/(pi/2)))

      
#II.1.b)
MCimprovedintegrationb = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0; #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 1, 4); #a=1 si b=4
    sum = sum + exp(u); 
  }
  return(3*sum/N); #((b-a)*sum/N)
}
exact_values <- c(51.87987)
estimated_values <- MCimprovedintegrationb(50000)
# Calcularea erorilor absolute și relative
absolute_errors <- abs(estimated_values - exact_values)
relative_errors <- absolute_errors / exact_values
# Afișarea rezultatelor
print(cat("Valorile exacte ale integralelor:",exact_values,"\n"))
print(cat("Valorile estimate ale integralelor:", estimated_values,"\n"))
print(cat("Erorile absolute corespunzătoare:",absolute_errors,"\n"))
print(cat("Erorile relative corespunzătoare:",relative_errors,"\n"))

#II.1.d)
MCimprovedintegration = function(a, N) {
  sum = 0;
  for(i in 1:N) {
    u <- runif(1, a, a + 10)  # Folosim limita superioară a + 10 pentru a aproxima infinitul u = rexp(1, 1);
    sum = sum + 1/(4*u^2-1);
  }
  return(sum/N);
}
exact_values <- c(log(3/4))
estimated_values <- MCimprovedintegration(1,50000)

# Calcularea erorilor absolute și relative
absolute_errors <- abs(estimated_values - exact_values)
relative_errors <- absolute_errors / exact_values

# Afișarea rezultatelor
print("Valorile exacte ale integralelor:")
print(exact_values)
print("Valorile estimate ale integralelor:")
print(estimated_values)
print("Erorile absolute corespunzătoare:")
print(absolute_errors)
print("Erorile relative corespunzătoare:")
print(relative_errors)

II.2.
MCimprovedintegration <- function(k,N) {
  sum <- 0
  
  for (i in 1:N) {
    u <- rexp(1, k) #distributia exponentiala 3
    sum <- sum + exp(-2 * u^2)
  }
  
  return( sqrt(pi/8)*(sum / N))
}

MCimprovedintegration(3,50000) 



#III.Estimarea mediilor
#Modelul stochastic pentru numarul de erori (bug-uri) gasite ıntr-un
#nou produs software se poate descrie dupa cum urmeaza.
#Care este numarul mediu de zile ın care sunt detectate toate erorile?
Nrdays=function()
{ 
  nrdays=1 #var aleatoare
  lasterrors=c(27,31) #in primele 2 zile sunt gasite 27 si 31 de erori
  nrerrors=27 #nr de erori actual
  while(nrerrors>0) #mergem cat timp avem erori
  {
    lambda=min(lasterrors) #vect cu min de erori
    nrerrors=rpois(1,lambda) #distributie Poisson 
    lasterrors=c(nrerrors,lasterrors[1]) #update (nr de erori curente in ziua resp, orimul el din vect lasterrors) )
    nrdays=nrdays+1 # incrementam nr de zile
  } 
  return(nrdays)  #simulam var aleatoare
}
Nrdays()

MCnrdays=function(N)
{
  s=0
  for(i in 1:N)
    s=s+Nrdays()
  return(s/N)
}
MCnrdays(1000)



#III.1.
Nrdays_media = function()
{
  nrdays = 1
  lasterrors = c(9, 15, 13)
  nrerrors = sum(lasterrors) # suma celor 3 zile
  while (nrerrors > 0)
  {
    lambda = median(lasterrors) # media celor 3 zile
    nrerrors = rpois(1, lambda) 
    lasterrors = c(nrerrors, lasterrors[1:2]) 
    nrdays = nrdays + 1
  }
  return(nrdays)
}
Nrdays_media()

MCnrdays_media = function(N)
{
  s = 0
  for (i in 1:N)
    s = s + Nrdays_media()
  return(s/N)
}
MCnrdays_media(10000)


#III.2.

estimatemeanX = function(N)
{
  val = runif(1)
  if (val <= 0.75) # Daca val <= 3/4, alegem primul mecanic cu lambda = 4
    X = rexp(1, 4)
  else
    X = (rexp(1, 12) + 3)# Altfel, alegem al doilea mecanic cu lambda=12

  sum = 0
  for (i in 1:N)
    sum = sum + X
  return(sum / N) # se calc media
}
estimatemeanX(100)

#IV.Estimarea probabilitatilor

#Var 1
#a)Estimati probabilitatea de a mai avea inca erori dupa 21 de zile de teste folosind 500 de
#simulari MC. (In primele trei zile numarul de erori gasite este 28, 22 ¸si 18, respectiv.).

Nrdays=function()
{
  nrdays=2 
  lasterrors=c(18,22,28)
  nrerrors=18
  while(nrerrors>0)
  {
    lambda=min(lasterrors)
    nrerrors=rpois(1,lambda)
    lasterrors=c(nrerrors,lasterrors[1:2])
    nrdays=nrdays+1
  }
  return(nrdays) # nr de zile pana cand nu mai are erori
}
Nrdays()

MCnrdays21=function(N)
{
  s=0
  for(i in 1:N)
  {
    if(Nrdays()>21) # ma int daca dupa 21 de zile mai avem erori
      s=s+1 # mai avem erori
  } 
  return(s/N) # probabilitatea
}
MCnrdays21(10000)

 alfa = 1 - 0.95
 z = qnorm(alfa/2) #T.CENTRALA
 epsilon = 0.01 #eroare
 #Metoda 1
 p = 0.246 #prob
 Nmin = p*(1- p)*(z/epsilon)^2 #nr min de simulari
 Nmin
 MCnrdays21(Nmin + 1)
#Metoda 2
 Nmin = (1/4)*(z/epsilon)^2
 Nmin
 MCnrdays(Nmin + 1)
###TEMA###
##########
I.2.
parabola = function(x) { # Definim functia parabolei
  return(-2*x^2 + 5*x - 2)
}
n <- 10000 #val uniforme
x <- runif(n, 0, 2)
y <- runif(n, 0, 2)
subparabola <- y <= parabola(x) #Verif punctele 
estimatA <- sum(subparabola) / n * 2 * max(parabola(x))
exactA <- abs(integrate(parabola, 0, 2)$value)
rel_err <- abs(estimatA - exactA) / exactA

cat("Aria estimata: ", aria_estimata, "\n")
cat("Aria exacta: ", aria_exacta, "\n")
cat("Eroarea relativa: ", rel_err, "\n")


#II.1.b)
MCimprovedintegrationb = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0; #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 1, 4); #a=1 si b=4
    sum = sum + exp(u); 
  }
  return(3*sum/N); #((b-a)*sum/N)
}
exact_values <- c(51.87987)
estimated_values <- MCimprovedintegrationb(50000)
# Calcularea erorilor absolute și relative
absolute_errors <- abs(estimated_values - exact_values)
relative_errors <- absolute_errors / exact_values
# Afișarea rezultatelor
print(cat("Valorile exacte ale integralelor:",exact_values,"\n"))
print(cat("Valorile estimate ale integralelor:", estimated_values,"\n"))
print(cat("Erorile absolute corespunzătoare:",absolute_errors,"\n"))
print(cat("Erorile relative corespunzătoare:",relative_errors,"\n"))

#II.2.
MCimprovedintegration <- function(k,N) {
  sum = 0
  for (i in 1:N) {
    u <- rexp(1, k) #distributia exponentiala 3
    sum <- sum + exp(-2 * u^2)
  }
  
  return( sqrt(pi/8)*(sum / N))
}

MCimprovedintegration(3,50000) 

#III.2.
estimatemeanX = function(N)
{
  val = runif(1)
  if (val <= 0.75) # Daca val <= 3/4, alegem primul mecanic cu lambda = 4
    X = rexp(1, 4)
  else
    X = (rexp(1, 12) + 3)# Altfel, alegem al doilea mecanic cu lambda=12

  sum = 0
  for (i in 1:N)
    sum = sum + X
  return(sum / N) # se calc media
}
estimatemeanX(100)


#IV.2.
 Simulate_virus_infection = function(N, k) {
   infectedcomp = rep(0, 40)  # Vector pentru a urmări starea fiecărui computer (0 = curat, 1 = infectat)
   infectedcomp[1] = 1  # Setam primul computer ca infectat
   
   for (i in 2:N) {
     for (j in 1:40) {
       if (infectedcomp[j] == 1) {
         for (l in 1:40) {
           if (l != j && infectedcomp[l] == 0) {
             if (runif(1) < 0.2) {
               infectedcomp[l] <- 1
             }
           }
         }
       }
     }
     
     infected_indexes = which(infectedcomp == 1)
     if (length(infected_indexes) > k) {
       remove_indexes = sample(infected_indexes, k)
       infectedcomp[remove_indexes] = 0
     } else {
       infectedcomp[infected_indexes] = 0
     }
     
     if (all(infectedcomp == 1)) {
       return(TRUE)
     }
   }
   
   return(FALSE)
 }
 
 estimate_probability =function(N, k, event_func) {
   event_count = 0
   
   for (i in 1:N) {
     if (event_func(N, k)) {
       event_count = event_count + 1
     }
   }
   
   return(event_count / N)
 }
 
 N <- 10000
 k_values <- c(4, 6, 8, 10)
 
 #a)Estimati probabilitatea ca intr-o anumita zi toate computerele sa fie infectate.
 prob_allinfected <- estimate_probability(N, k_values[1], function(N, k) Simulate_virus_infection(N, k))
 print(cat(" Probabilitatea ca toate computerele să fie infectate într-o zi:", prob_allinfected))
 
 #b) Estimati probabilitatea ca ıntr-o anumita zi cel putin 15 computere sa fie infectate.
 prob_at_least_15_infected <- estimate_probability(N, k_values[1] function(N, k) {
   infected_count <- sum(Simulate_virus_infection(N, k))
   if (infected_count >= 15) {
     return(TRUE)
   } else {
     return(FALSE)
   }
 })
 print("Probabilitatea ca cel puțin 15 computere să fie infectate într-o zi:")
 print(prob_at_least_15_infected)
 
 #c)Estimati aceasta ultima probabilitate cu o eroare de ±0.01 cu probabilitea 0.95.

