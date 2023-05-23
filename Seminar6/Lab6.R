#Seminar 6

#I.Estimarea mediei unei populatii
selectionmean = function(filename) {
  x = scan(filename);
  m = mean(x)
}
selectionmean("sample.txt")

#I.1
selectionmean <- function(filename) {
  x <- scan(filename)
  m <- mean(x)
  return(m)
}

mean_value <- selectionmean("history.txt")
print(mean_value)


#II.Intervale de incredere pentru media unei populatii cu dispersia cunoscuta
#Val critica
#z∗ = qnorm(1 − α/2, mean = 0, sd = 1)
#xn = mean(date esantion)

alfa = 0.1
samplemean = 20 #media de viata a unui esantion de 100 de baterii 
n = 100 #de baterii
sigma = sqrt(9)  #dispersia e de 9 ore si devizatia standard 
criticalz = qnorm(1 - alfa/2, 0, 1) #
a = samplemean - criticalz*sigma/sqrt(n)#capetele intervalului
b = samplemean + criticalz*sigma/sqrt(n)
interval = c(a, b)
interval #probabilitatea ca media sa fie in intervalul acesta e de 90%

#II.1
zconfidence_interval = function(n, samplemean, alfa, sigma){
  criticalz = qnorm(1 - alfa/2, 0, 1) #pct critic
  a = samplemean - criticalz*sigma/sqrt(n)#capetele intervalului
  b = samplemean + criticalz*sigma/sqrt(n)
  interval = c(a, b)
  return(interval)
}
  zconfidence_interval(100,20,0.1,3)

#II.2
zconfidence_interval(25,67.53,0.1,10)

#II.3
zconfidence_interval(50,5,0.05,0.5)

#II.6 TEMA
zconfidence_intervalfis = function(filename, alfa, sigma ) {
  x = scan(filename)
  n = length(x) 
  mediaselectie = mean(x)  
  
  z <- qnorm((1 - alfa) / 2, 0, 1)# Calculul valorii critice Z pentru un nivel de încredere de 95%
  a <- mediaselectie - z * (sigma / sqrt(n))
  b <- mediaselectie + z * (sigma / sqrt(n))
  
  interval <- c(a, b)
  print(interval)
}
zconfidence_intervalfis("history.txt",0.05, 10)

#III. Intervale de incredere  => in loc de sigma vom folosi deviatia esantionului

#Ex rezolvat
 alfa = 0.05# niv de incredere
 samplemean = 3.3 #media
 n = 60
 s = 0.4 #deviatia standard
 se = s/sqrt(n)
 criticalt = qt(1 - alfa/2, n - 1) #pct critic
 a = samplemean - criticalt*se  #capetele
 b = samplemean + criticalt*se
 interval = c(a, b)
 interval
 
#III.1
 t_conf_interval = function(n, samplemean, s, alfa){
   se = s/sqrt(n)
   criticalt = qt(1 - alfa/2, n - 1) #pct critic
   a = samplemean - criticalt*se  #capetele
   b = samplemean + criticalt*se
   interval = c(a, b)
   return(interval)
 }
 
 t_conf_interval(60, 3.3, 0.4, 0.05)
 
 #III.2
 
 t_conf_interval(196, 44.65, sqrt(2.25), 0.01)
 
#III.3.a
t_conf_interval(49, 12, 1.75, 0.01)
t_conf_interval(49, 12, 1.75, 0.05) 

#III.3.b 
t_conf_interval(49, 13.5 , 1.25, 0.05)

#III.4
 selectionmean <- function(filename,alfa) {
   x <- scan(filename)
   n <- length(x)  # dim esantionului
   mediasel <- mean(x)  # media de selectie
   s <- sd(x)  # deviatia standard
   se <- s / sqrt(n)  # eroare standard
   critical_t <- qt(1 - alfa/2, n - 1) #pct critic
   
   a <- mediasel - critical_t * se
   b <- mediasel + critical_t * se
   
   interval <- c(a, b)
   return(interval)
 }
 selectionmean("history.txt",0.05)
  #III.5
 x <- c(12, 11, 12, 10, 11, 12, 13, 12, 11, 11, 13, 14, 10)
 samplemean <- mean(x)
 s <- sd(x)
 n <- length(x)
 
 critical_t1<- qt(0.05, n-1)
 critical_t2 <- qt(0.025, n-1)
 critical_t3 <- qt(0.005, n-1)
 
 # intervalele de incredere pentru media populației la nivelurile de semnificație
 alfa1 <- smplemean + critical_t1*(s/sqrt(n))
 alfa2 <- samplemean + critical_t2*(s/sqrt(n))
 alfa3 <- samplemean+ critical_t3*(s/sqrt(n))
 
 # afișam intervalele de incredere
 cat("Intervalul de incredere de 90% pentru media populatiei este [", alfa1[1], ",", alfa1[2], "]\n")
 cat("Intervalul de incredere de 95% pentru media populatiei este [", alfa2[1], ",", alfa2[2], "]\n")
 cat("Intervalul de incredere de 99% pentru media populatiei este [", alfa3[1], ",", alfa3[2], "]\n")
 
 #Testarea ipotezelor statistice
 #IV.Testul z asupra proportiilor
 #Ex re4zolvat
  alfa = 0.01 #pragul 1% nivel de semnificatie
  n = 100   
  succese = 63
  pprim = succese/n #provine din esantion
  p0 = 0.6 #ipoteza pe care o testez
  zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)  #scorul alegerii
  criticalz = qnorm(1 - alfa, 0, 1) #se va det val critica zscore*
  zscore
  criticalz
  
  
  #IV.1
  test_proportion =function( alfa, n, succese, p0, tip_ipoteza){
    pprim = succese/n
    zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)
    cat("zscore=",zscore)
    #In fct de tipul de ipoteza
    if(tip_ipoteza=="r"){
         criticalz = qnorm(1 - alfa, 0, 1)
         cat("criticalz=",criticalz)
         if(zscore < criticalz)
           print("HO nu se poate respinge")
         else 
           print("HO se respinge")
         }
    if(tip_ipoteza=="l"){
       criticalz = qnorm(alfa, 0, 1)
       cat("criticalz=",criticalz)
       if(zscore > criticalz)
           print("HO nu se poate respinge")
       else 
           print("HO se respinge")
        }
    if(tip_ipoteza=="s"){
      criticalz = qnorm(alfa, 0, 1)
      cat("criticalz=",criticalz)
         if(abs(zscore) <= criticalz)
            print("HO nu se poate respinge")
         else 
            print("HO se respinge")
    }
          
  }
  test_proportion(0.01, 100, 63, 0.6,"r")
  
  #IV.2
  
  test_proportion =function(alfa, n, succese, p0, tip_ipoteza) {
    
    pprim = succese/n
    zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)
    
    # În funcție de tipul de ipoteză
    if (tip_ipoteza == "r") {
      criticalz = qnorm(1 - alfa, 0, 1)
      if (zscore < criticalz)
        print("HO nu a crescut")
      else 
        print("HO a crescut")
    }
    if (tip_ipoteza == "l") {
      criticalz = qnorm(alfa, 0, 1)
      if (zscore > criticalz)
        print("HO nu a crescut")
      else 
        print("HO a crescut")
    }
    if (tip_ipoteza == "s") {
      criticalz = qnorm(alfa, 0, 1)
      if (abs(zscore) <= criticalz)
        print("HO nu a crescut")
      else 
        print("HO a crescut")
    }
    
    cat("zscore =", zscore, "\n")
    cat("criticalz =", criticalz, "\n")
  }
  test_proportion(0.05, 150, 20, 0.1,"r")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
