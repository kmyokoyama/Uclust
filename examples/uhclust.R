source("BnClustSig.R")
uhclust=function(X,alpha=0.05,rep=10){ 
#entrar com o X (em colunas ou seja cada coluna eh uma variavel)

if(is.null(rownames(X))){

  rownames(X)=1:(dim(X)[1])
}


labels=rownames(X)

md1=as.matrix(dist(X))
md=md1^2

n=dim(md)[1]
grupo=list()
grupo[[1]]=labels
clusters=list()
pvalues=vector()
taux=vector() #faz ou nao faz o teste
taux[1]=TRUE

merge=matrix(rep(0),ncol=2,nrow=(n-1))
height=vector()
ind.grupo=1




########### Realiza o teste para a primeira divisao

ht=BnClustSig(md)
pvalues[[1]]=ht$p.value
height[n-1]=1   #
if(ht$p.value>alpha){                            # todo grupo homogeneo
  ans=list("homogeneous",ht$p.value)
  names(ans)=c("homogeneous","pvalue")
  print(ans)
  #return(ans) 
  
  
}else{                                            #Caso contrario, primeira divisao faz separado
                                                  #organiza a primeira componente
  ind.aux1=ind.grupo+1
  
  
  
  if(length(ht$cluster1)==1){                # se um dos grupos tem tamanho 1
    #taux[ind.aux1]=FALSE 
    merge[n-1,1]=-ht$cluster1
    
  }else{
    grupo[[ind.aux1]]=labels[ht$cluster1]
    merge[n-1,1]=n-ind.aux1 # soh faz sentido testar grupos maiores que 3
    taux[ind.aux1]=(length(grupo[[ind.aux1]])>3)  #avisa se este novo grupo deve ser testado por ter mais de 3 elementos
    height[n-ind.aux1]=2
    }
                                                 # repete o procedimento para a segunda componente
  ind.aux2=ind.grupo+length(ht$cluster1)
  grupo[[ind.aux2]]=labels[ht$cluster2]
  
  
  if(length(ht$cluster2)==1){
    merge[n-1,2]=-ht$cluster2
    #taux[ind.aux2]=FALSE
    
  }else{
    merge[n-1,2]=n-ind.aux2
    taux[ind.aux2]=(length(grupo[[ind.aux2]])>3)
    height[n-ind.aux2]=2
  }
}



############################# Continua o algoritmo para as divisoes seguintes

if(ht$p.value<alpha){
  ind.grupo=2
}
while (ind.grupo<n){
#  print(grupo)
  
  
  if(taux[ind.grupo]==TRUE){                         # Se o teste deve ser realizado (nao eh homo e n>3)
  
  indx.esses=match(grupo[[ind.grupo]],labels)       ## encontrando os indices correspondentes as labels do grupo
  ht=BnClustSig(md[indx.esses,indx.esses])
  pvalues[ind.grupo]=ht$p.value
  
  if(ht$p.value>alpha){                             #se eh homogeneo  
    merge[n-ind.grupo,1]=-indx.esses[ht$cluster1[1]]
    merge[n-ind.grupo,2]=n-(ind.grupo+1)
    grupo[[ind.grupo+1]]=labels[indx.esses[ht$cluster1[-1]]]
    taux[ind.grupo+1]=FALSE
    height[n-(ind.grupo+1)]=height[n-ind.grupo]
    ind.grupo=ind.grupo+1
  }else{                                            #se todo o grupo nao eh homogeneo
    
    ind.aux1=(ind.grupo+1)                           #organiza o grupo 1
    
    if(length(ht$cluster1)==1){               #ve se o grupo 1 so tem 1 componente
      merge[n-ind.aux1,1]=-indx.esses[ht$cluster1]
     # taux[ind.aux1]=FALSE
    }else{                                          #  se o gruo 1 tiver mais de um componente                 
                            
    merge[n-ind.grupo,1]=n-ind.aux1
    height[n-ind.aux1]=height[n-ind.grupo]+1
    grupo[[ind.aux1]]= labels[indx.esses[ht$cluster1]]
    taux[ind.aux1]=(length(grupo[[ind.aux1]])>3)  #avisa se este novo grupo deve ser testado por ter mais de 3 elementos
    }
                                                    
    ind.aux2=ind.grupo+length(ht$cluster1)                          #organiza o grupo 2
    
    if(length(ht$cluster2)==1){               #ve se o grupo 2 so tem 1 componente
      merge[n-ind.aux2,1]=-indx.esses[grupo[[ind.aux2]]]
      #taux[ind.aux2]=FALSE
    }else{   
    
    merge[n-ind.grupo,2]=n-ind.aux2
    height[n-ind.aux2]=height[n-ind.grupo]+1
    grupo[[ind.aux2]]= labels[indx.esses[ht$cluster2]]
    taux[ind.aux2]=(length(grupo[[ind.aux2]])>3)  #avisa se este novo grupo deve ser testado por ter mais de 3 elementos
                                                    
    
    
  print("divide")
    ind.grupo=ind.grupo+1
  }
    
  }    
    
  }else{                                #se o teste nao deve ser realizado
    ng=length(grupo[[ind.grupo]])
    while(ng>1){
      indx.esses= match(grupo[[ind.grupo]],labels) 
      if(ng==2){
        merge[n-ind.grupo,1]=-match(grupo[[ind.grupo]][1],labels)
        merge[n-ind.grupo,2]=-match(grupo[[ind.grupo]][2],labels)
        taux[ind.grupo]=FALSE
        ind.grupo=ind.grupo+1
        ng=1
      }else{
        merge[n-ind.grupo,1]=-match(grupo[[ind.grupo]][1],labels)
        merge[n-ind.grupo,2]=n-(ind.grupo+1)
        height[n-(ind.grupo+1)]=height[n-ind.grupo]
        grupo[[ind.grupo+1]]=grupo[[ind.grupo]][-1]
        ind.grupo=ind.grupo+1
        ng=ng-1
        taux[ind.grupo]=FALSE
      }
    }
  }
}


############

#verificar

height=1+max(height)-height
order=c(t(merge))
order=-order[order<0]

ans=list(merge,height, labels,order )
names(ans)=c("merge", "height","labels","order")
class(ans)="hclust"

plot(ans,hang=-1)

return(ans)
}