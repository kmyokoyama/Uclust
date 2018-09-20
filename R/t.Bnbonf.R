#source("boot_sigma.R")
#source("boot_sigma1.R")
t.Bnbonf=function(Bn,n,n1,alpha,bootB=-1,bootB1=-1,md=1,std=FALSE){
  if (std==FALSE){  # caso em que Bn nao eh padronizado std=FALSE
    Cn=vector()
    numB=2000
    if (n1==1||n1==(n-1)){
      if(bootB1==-1){
        bootB1=boot_sigma1(c(1,(n-1)),md)
      }
      varBn=bootB1
    }else{
      if(bootB==-1){
        bootB=boot_sigma(c(floor(n/2),(n-floor(n/2))),numB,md) # retorna a variancia do Bn com um esse c(floor(n/2),(n-floor(n/2)) tamamnho de grupo
    }
      n1b=floor(n/2)
      n2=n-n1b
      Cnd=(((n1b*n2)/(n*(n-1))^2)*(2*n^2-6*n+4)/((n1b-1)*(n2-1)))

      n1b=n1
      n2=n-n1b
      Cnn=(((n1b*n2)/(n*(n-1))^2)*(2*n^2-6*n+4)/((n1b-1)*(n2-1)))

      varBn=Cnn*bootB/Cnd #variancia do Bn para o teste
    }
      p=pnorm(Bn/sqrt(varBn),lower.tail = FALSE)
    }
    else{
      p=pnorm(Bn,lower.tail = FALSE) # aqui entra Bn padronizado. Nesse caso std=TRUE
    }
  significant=(p<(alpha/(2^(n-1)-1)))
  ans=list(p,significant,varBn)
  names(ans)=c("p.value","significant","varBn")
  return(ans)
}
