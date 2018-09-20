#############################################################
# funcao de otimizacao Bn padronizado

#source("objBn.R")
optimBn_restrict=function(mdm,n1_max,n1_min,itmax=200){
  if(n1_max<n1_min){
    print("ERROR: n1_max  must be larger than n1_min")
  }else{
  md=mdm # matriz de distancias
  #encontra as dimensoes
  n=dim(md)[1] # n eh o numero de series

  #seta estruturas de dados para guardar assignments
  it=1
  ass_old=rep(2,n)
  ASS=matrix(ncol=n,nrow=(itmax+1))   # essa matriz registra o historico da otimizacao
  #Bn=vector()
  Fobj=vector()
  ### Comeca otimizacao inicializando os parametros
  # inicializa os centros com pontos aleatorios da amostra se nao foram definidos na funcao

  ass=rep(0,n)
  ass[sample(n,floor(n/2))]=1
  ASS[1,]=ass
  #print(ass)
  # TRUE pertence ao grupo 2
  #### Comeca as iteracoes
  if (n1_max==n1_min){ # caso  n1_max==n1_min
    count=0
    while (it<itmax && count<max(n/2,10)){
      ass_old=ass
      ord=sample(n,n)
      for (i in ord){
        v=(1:n)[-i]
        j=sample(v,1)
        f0= objBn(ass,md)
        temp_ass=ass
        ass[j]=ass[i]
        ass[i]=temp_ass[j]

        if (sum(ass)<=n1_max && sum(ass)>=n1_min){
          f1 = objBn(ass,md)
        }else{
          f1=Inf
        }

        if(f0 < f1){
          ass=temp_ass
        }
      }
      Fobj[it]= objBn(ass,md)

      it=it+1
      ASS[it,]=ass
      if(prod(ass==ass_old)){
        count=count+1
      }
      else{
        count=0
      }
    }



  }else{ ################################    A partir daqui faz o caso n1_max diferente do n1_min


  while (it<itmax ){#&& !prod(ass==ass_old)){
    ass_old=ass
    ord=sample(n,n)
    for (i in ord){
      ass[i]=0
      if (sum(ass)<=n1_max && sum(ass)>=n1_min){
         f0= objBn(ass,md)
      }else{
        f0=Inf
      }

        ass[i]=1
      if (sum(ass)<=n1_max && sum(ass)>=n1_min){
      f1 = objBn(ass,md)
      }else{
        f1=Inf
      }

      if(f0 < f1){
        ass[i]=0
      }
    }
    Fobj[it]= objBn(ass,md)

    it=it+1
    ASS[it,]=ass
  }

  }
  #monta resposta

  ans=list(which(ass==ass[1]),Fobj,it-1,ASS[1:(it+1),])
  names(ans)=c("grupo1","Fobj", "numIt","history")

  #plot(dados,col=ass+1) # funciona para 2 D

  ans

  }
}
