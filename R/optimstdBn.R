#############################################################
# funcao de otimizacao Bn padronizado
# Retorna o agrupamento para o qual a funcao objetivo baseada na Bn padronizada
# convergiu para um minimo

#source("boot_sigma.R")
#source("boot_sigma1.R")
#source("objstdBn.R")
optimstdBn=function(mdm,itmax=200, centers=-1,bootB=-1,bootB1=-1){
  md=mdm # matriz de distancias
  #encontra as dimensoes

  n=dim(md)[1] # n eh o numero de series


  #seta estruturas de dados para guardar assignments
  it=1
  ass=vector()
  ass_old=rep(2,n)
  ASS=matrix(ncol=n,nrow=itmax)   # essa matriz registra o historico da otimizacao
  #Bn=vector()
  Fobj=vector()


  #calcula a smaile function
  Cn=vector()
  varBn=vector()
  numB=2000
  # por enquanto mu4=2 (da normal) mu4=var((x-mu)^2)
  #mu4=2
  #Cn[1]=(1/(n*(n-1)))*L*mu4 + 1/(n*(n-1)*(n-2))*4*L
  #Cn[n-1]=Cn[1]
  if (bootB1==-1){
    bootB1=boot_sigma1(c(1,(n-1)),md)
  }


  if(bootB==-1){
     bootB=boot_sigma(c(floor(n/2),(n-floor(n/2))),numB,md) # retorna a variância do Bn com um esse c(floor(n/2),(n-floor(n/2)) tamamnho de grupo
  }


  for (n1 in 2:(n-2)){
    n2=n-n1
    Cn[n1]=(((n1*n2)/(n*(n-1))^2)*(2*n^2-6*n+4)/((n1-1)*(n2-1)))
  }
  varBn[1]=bootB1
  varBn[n-1]=bootB1

  for (n1 in 2:(n-2)){
    n2=n-n1
    varBn[n1]=Cn[n1]*bootB/Cn[floor(n/2)]
  }
  # plot(varBn)


  ### Comeca otimizacao inicializando os parametros

  # inicializa os centros com pontos aleatorios da amostra se nao foram definidos na funcao
  if (centers==-1){
   centers=sample(n,2)
  }
  #print(centers)
  # inicializa os assignments colocando as amostras no grupo de centro mais proximo

  for(i in 1:n){
    ass[i]=(md[i,centers[1]]) > (md[i,centers[2]])
    #ass[i]=(dist(t(cbind(dados[i,],centers[1,])))) > (dist(t(cbind(dados[i,],centers[2,]))))
  }
  ass

  ASS[1,]=ass
  #print(ass)

  # TRUE pertence ao grupo 2
  #### Comeca as iteracoes

  while (it<itmax && !prod(ass==ass_old)){

    ass_old=ass

    # centers[1,]=c(mean(dados[ass==FALSE,1]),mean(dados[ass==FALSE,2]))
    # centers[2,]=c(mean(dados[ass==TRUE,1]),mean(dados[ass==TRUE,2]))

    ord=sample(1:n)
    for (i in ord){
    #for(i in 1:n){
      ass[i]=0
      f0= objstdBn(ass,varBn,md)
      ass[i]=1
      f1 = objstdBn(ass,varBn,md)
      if(f0 < f1){
        ass[i]=0
      }
    }
    #Bn[it]=Bn(dados,which(ass==ass[1]))            # registra o valor de Bn para cada iteracao
    Fobj[it]= objstdBn(ass,varBn,md)

    it=it+1
    ASS[it,]=ass
    #print(it)

  }


  #monta resposta

  ans=list(which(ass==ass[1]),Fobj,it-1,ASS[1:(it+1),],varBn,bootB,bootB1)
  names(ans)=c("grupo1","Fobj", "numIt","history","varBn","bootB","bootB1")

  #plot(dados,col=ass+1) # funciona para 2 D

  ans

}
