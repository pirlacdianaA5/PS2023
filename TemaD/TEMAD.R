#D1
#a)
MC_M_element = function(x, k)
{
  n = length(x)
  for (i in 1:k)
  {
    elem = sample(x, 1) #selectează un singur element aleator din vectorul x
    count = 0;
    for (j in 1:n)
      if (x[j]==elem)
        count = count+1
    if (count>=n/2+1) #verificam daca e M-element
      return(elem)
  }
  return("x nu are M-element")
}

sample = c(1, 2, 3, 4, 2, 2, 1, 2, 2, 2, 2)
MC_M_element(sample, 5)

#b)
k = floor(log(10^-7 , base=0.5)+1)
MC_M_element(sample, k)

#D2
element_ith = function(i, A) {
  z = sample(A, 1)
  n = length(A)
  
  A_lt = vector()
  k = 1
  for (j in 1:n)
    if (A[j]<z)
    {
      A_lt[k] = A[j]
      k = k+1
    }
  
  A_gt = vector()
  k = 1
  for (j in 1:n)
    if (A[j]>z)
    {
      A_gt[k] = A[j]
      k = k+1
    }
  
  
  if(length(A_lt) > i)
    return(element_ith(i, A_lt))
  else
    if(n > i + length(A_gt))
      return(z)
  else
    return(element_ith(i - n + length(A_gt), A_gt))
}

A = seq(25, 3, -2)
element_ith(2-1, A)

#D3
#a)
MC_mediana = function(S, a)
{
  n = length(S)
  m = floor(a*log(n))
  S_ = sample(S, m)
  return(as.vector(quantile(S_))[2+1])
}
#b)
n = floor(sqrt(2/10^-7)+1)
n
sample = c(1, 2, 3, 4, 1, 3, 4, 3, 4)
MC_mediana(sample, 1)