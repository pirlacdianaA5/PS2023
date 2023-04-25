x = c(11, 14, 21, 32, 17, 24, 21, 35, 52, 44, 21, 28, 36, 49, 41, 19, 20, 34, 37, 29)
stem(x)

sample = scan("sample1.txt")
min = min(sample)
max = max(sample)
interval = seq(40, 100, 10)
hist(sample, breaks = interval, right = F, freq = T)
a = 6
hist(sample, breaks = a, right = F, col = "blue")

frecv = c(9, 8, 12, 3, 17, 41, 29, 35, 32, 40, 19, 8)
barplot(frecv, space = 0)

#I.1
stem(sample)

#I.2
tablou = read.csv("unemploy2012.csv", header = T, sep = ';')
rate = tablou[['rate']]
interval = c(0,4,6,8,10,12,14,30)
hist(rate, breaks = interval, right = T, freq = F)

#II.1
mean(sample)
median(sample)

#II.2
tablou2= read.csv("life_expect.csv",header=T)
country =tablou[['country']]
female= tablou2[['female']]
male=tablou2[['male']]
mean(female)
median(female)
mean(male)
median(male)

sample= c(9, 8, 12, 3, 17, 41, 29, 35, 32, 40, 19, 8)
summary(sample)


 sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
 m = mean(sample)
 s = sd(sample)
 outliers = vector()
 j = 0
for(i in 1:length(sample))
   if(sample[i]< m - 2*s | sample[i] > m + 2*s) {
     j = j + 1
     outliers[j] = sample[i]
     }
 outliers
 
 #III.1
 outlier_mean =function(sample){
 m = mean(sample)
 s = sd(sample)
 outliers = vector()
 j = 0
 for(i in 1:length(sample))
   if(sample[i]< m - 2*s | sample[i] > m + 2*s) {
     j = j + 1
     outliers[j] = sample[i]
   }
 return(outliers)
 }
 outliers_mean(sample)
#III.2
 outliers_iqr =function(sample){
   q1 = as.vector(quantile(sample))[2]
   q3= as.vector(quantile(sample))[4]
   iqr=q3-q1
   outliers = vector()
   j = 0
   for(i in 1:length(sample))
     if(sample[i]< m - 2*s | sample[i] > m + 2*s) {
       j = j + 1
       outliers[j] = sample[i]
     }
   return(outliers)
 }
 
 outliers_iqr(sample)
 
 
 
 ##Exercitii lab
 
 ###I.3
 tablou = read.csv("life_expect.csv", header = T)
 country=tablou[['country']]
 male=tablou[['male']]
 female = tablou[['female']]
 hist(female, breaks = 7, freq=T, col='pink')
 hist(male, breaks = 7,freq=T,col='blue')

 
 ###II.3.Scrieti o functie care sa calculeze modul pentru un esantion dat.

 
 
 
 ###III.3 
 sample = scan("sample2.txt")
 summary(sample)
 outliers_mean(sample)
 outliers_iqr(sample)
 
 