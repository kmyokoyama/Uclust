smile=function(n){
Cn=vector()
# Cn[1]=(1/(n*(n-1)))*L*2+ 1/(n*(n-1)*(n-2))*4*L
# Cn[n-1]=Cn[1]


for (n11 in 2:(n-2)){
  n22=n-n11
  Cn[n11]=(((n11*n22)/(n*(n-1))^2)*(2*n^2-6*n+4)/((n11-1)*(n22-1)))
}
return(Cn)
}