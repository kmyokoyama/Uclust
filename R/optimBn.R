#############################################################
# funcao de otimizacao Bn padronizado
# Retorna o agrupamento para o qual a funcao objetivo baseada na Bn padronizada
# convergiu para um minimo

#source("boot_sigma.R")
#source("objBn.R")
optimBn=function(mdm,itmax=200, centers=-1,bootB=-1){
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

    ord=sample(n,n)
    for (i in ord){
    #for(i in 1:n){
      ass[i]=0
      f0= objBn(ass,md)
      ass[i]=1
      f1 = objBn(ass,md)
      if(f0 < f1){
        ass[i]=0
      }
    }
    #Bn[it]=Bn(dados,which(ass==ass[1]))            # registra o valor de Bn para cada iteracao
    Fobj[it]= objBn(ass,md)

    it=it+1
    ASS[it,]=ass
    #print(it)

  }


  #monta resposta

  ans=list(which(ass==ass[1]),Fobj,it-1,ASS[1:(it+1),],bootB)
  names(ans)=c("grupo1","Fobj", "numIt","history","bootB")

  #plot(dados,col=ass+1) # funciona para 2 D

  ans

}
