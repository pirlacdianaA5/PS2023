#TEMA B
#B1
LLNgeom=function(n, p)
{
  y=mean(rgeom(n, p))
  err=abs(y-(1/p))
  return (c(y, err))
}
LLNgeom(5000, 0.2)
LLNgeom(10000, 0.6)
LLNgeom(100000, 0.6)
LLNgeom(500000, 0.8)


#B2
CLTstudent = function(r, n, N, z) {
  expectation = 0;
  st_dev = r/(r-2);
  upper_bound = z*st_dev/sqrt(n)+expectation;
  sum=0;
  for(i in 1:N)
  {
    xn = mean(rt(n, r));
    if(xn <= upper_bound)
      sum = sum + 1;
  }
  y=pnorm(z)
  
  return (abs((sum/N)-y));
}
CLTstudent(5, 50, 5000, -1.5)
CLTstudent(5, 50, 10000, 0)
CLTstudent(5, 50, 20000, -1.5)


#B3
bin_hXk=function(n, p, h, k)
{
  expectation=n*p
  var=n*p*(1-p)
  standard_deviation=sqrt(var)
  # Aproximarea probabilității utilizând Teorema de Moivre-Laplace
  HmicX=(h - 0.5 - expectation) / standard_deviation
  XmicK = (k - expectation)/(standard_deviation*sqrt(1-p))
  return (pnorm(XmicK)*pnorm(HmicX))

}
bin_hXk(10, 0.2, 2, 4)

