#TEMA E
#E1
# Numărul de măsurători= n
# Media măsurătorilor= media
# Deviația standard= sigma
# Nivelul de semnificație (1 - nivelul de încredere)= alpha

interval_X = function(n,media,alpha,sigma){
  z_alpha <- abs(qnorm(alpha/2))  # Valoarea critică a distribuției normale standard
  a= media - z_alpha * (sigma/sqrt(n))
  b= media + z_alpha * (sigma/sqrt(n))
  
  interval_x <- round(c(a,b), 2) #rotunjirea rezultatelor la 2 zecimale
  return(interval_x)
}
# Nivelul de încredere de 90%
interval_X(10,138,0.1,11)

# Nivelul de încredere de 95%
interval_X(10,138,0.5,11)

# Nivelul de încredere de 99%
interval_X(10,138,0.01,11)

#E2
#n= Numărul de indivizi din eșantion
#media_selectie = Media de selecție
#dispersia_esantion = Dispersia eșantionului
#alpha = Nivelul de semnificație (1 - nivelul de încredere)

interval_95 = function(n,media_selectie,dispersia_esantion,alpha){
  z_alpha <- abs(qnorm(alpha/2))  # Valoarea critică a distribuției normale standard
  a= media_selectie - z_alpha * sqrt(dispersia_esantion/n)
  b= media_selectie + z_alpha * sqrt(dispersia_esantion/n)
  interval <- round(c(a,b), 2)
  return(interval)
  }

interval_95(256,18,1.44,0.5)

#E3
test_proportion =function(alfa, n, succese, p0, tip_ipoteza) {

  pprim = succese/n
  zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)
  
  # În funcție de tipul de ipoteză
  if (tip_ipoteza == "r") {
    criticalz = qnorm(1 - alfa, 0, 1)
    if (zscore <= criticalz)
      print("HO nu se respinge")
    else 
      print("HO  se respinge")
  }
  if (tip_ipoteza == "l") {
    criticalz = qnorm(alfa, 0, 1)
    if (zscore >= criticalz)
      print("HO nu se respinge")
    else 
      print("HO  se respinge")
  }
  if (tip_ipoteza == "s") {
    criticalz = qnorm(1-alfa/2, 0, 1)
    if (abs(zscore) <= criticalz)
      print("HO nu respinge")
    else 
      print("HO  se respinge")
  }
  
  cat("zscore =", zscore, "\n")
  cat("criticalz =", criticalz, "\n")
}

# Test la nivel de semnificație de 1%
test_proportion(0.01, 153, 17, 0.12,"r")

# Test la nivel de semnificație de 5%
test_proportion(0.05, 153, 17, 0.12,"r")

