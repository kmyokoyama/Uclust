


# Gumbel Correction
gumbel_correction=function(nt,Fobj){
  #nt=2^(n-1)-1
  bn=sqrt(2*log(nt))- 0.5*log(4*pi*log(nt))/sqrt(2*log(nt)) 
  an=1/sqrt(2*log(nt))+40/(log(nt))
  bgumbel=an*(Fobj-bn)
  return(1-exp(-exp(-bgumbel))) #cdf gumbel 
}





#retorna o p-valor para Bn padronizada maxima
test_max_stdBn=function(Fobj,n,nt=0){
  if(nt==0){ #teste maximo normal sem restricao
    nt=2^(n-1)-1
  }
  if(nt<2^(28)){
# teste para Bn maxima (Fobj)
  p=1-exp((nt)*pnorm(-Fobj,log.p=TRUE) ) #p-valor
  }else{p=gumbel_correction(nt,-Fobj)}
  return(p)
}
