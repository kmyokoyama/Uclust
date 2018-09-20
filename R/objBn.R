#source("Bn.R")
objBn = function(assign,mdm){
  #funciona somente no contexto da otimização Bn nap padronizada
  # Retorna Bn nao padronizada para um dado assignement
  n=length(assign)
  n1=sum(assign==0)
  n2=n-n1
   if ( n1==0  |n2==0 ){
  #if ( n1==0 |n1==1 |n2==0 | n2==1){
    return(Inf)
  }
  else{
    vaux1=which(assign==assign[1])
    vaux2=c(1:n)[-vaux1]
    n1=length(vaux1)
    n2=n-n1
    m=mdm[c(vaux1,vaux2),c(vaux1,vaux2)]
    Bns=Bn(c(n1,n2),m)
    return(-Bns)
  }
}
