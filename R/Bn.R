############################################
#Calcula a estatistica Bn
Bn = function(ngv,md)                                                        ##ngv ? um vetor com o tamanho de cada grupo, m ? a matriz de dist?ncias
{ # calcula a Bn
  ng=sum(ngv)
  maux1=matrix(0, nrow=ng, ncol=ng)
  if (min(ngv)>1){
     for (i in 1:ngv[1])
     {
       for (j in (ngv[1]+1):ng)                                                    ##Dist?ncia entre os grupos
       {
          maux1[i,j] = md[i,j] #D12
       }
     }

     maux2=matrix(0, ngv[1], ngv[1])
     for (i in 1:(ngv[1]-1))
     {
       for (j in (i+1):ngv[1])
       {
         maux2[i,j] = md[i,j] # D11                                                      ##Dist?ncia dentro do grupo 1
       }
     }

  maux3=matrix(0, ng, ng)
  for (i in (ngv[1]+1):(ng-1))
  {
    for (j in (i+1):ng)
    {
      maux3[i,j] = md[i,j]    #D22                                                    ##Dist?ncia dentro do grupo 2
    }
  }

  a1 = (1/(ngv[1]*ngv[2]))*sum(maux1)
  a2 = (2/(ngv[1]*(ngv[1]-1)))*sum(maux2)
  a3 = (2/(ngv[2]*(ngv[2]-1)))*sum(maux3)
  sBn = (ngv[1]*ngv[2]/(ng*(ng-1)))*(2*a1-a2-a3)
  }
  else{
      maux1=matrix(0, nrow=ng, ncol=ng)
      if(ngv[1]==1){
         for (i in 1:ngv[1])
         {
          for (j in (ngv[1]+1):ng)                                                    ##Dist?ncia entre os grupos
           {
            maux1[i,j] = md[i,j] #D12
           }
        }

#     maux2=matrix(0, ngv[1], ngv[1])
#     for (i in 1:(ngv[1]-1))
#     {
#       for (j in (i+1):ngv[1])
#       {
#         maux2[i,j] = md[i,j] # D11                                                      ##Dist?ncia dentro do grupo 1
#       }
#     }

    maux3=matrix(0, ng, ng)
    for (i in (ngv[1]+1):(ng-1))
    {
      for (j in (i+1):ng)
      {
        maux3[i,j] = md[i,j]    #D22                                                    ##Dist?ncia dentro do grupo 2
      }
    }

    a1 = (1/(ngv[1]*ngv[2]))*sum(maux1)
    a2 = 0#     (2/(ngv[1]*(ngv[1]-1)))*sum(maux2)
    a3 = (2/(ngv[2]*(ngv[2]-1)))*sum(maux3)
    sBn = (ngv[1]*ngv[2]/(ng*(ng-1)))*(a1-a2-a3)
  }
      else{
        maux1=matrix(0, nrow=ng, ncol=ng)
        for (i in 1:ngv[1])
        {
          for (j in (ngv[1]+1):ng)                                                    ##Dist?ncia entre os grupos
          {
            maux1[i,j] = md[i,j] #D12
          }
        }

            maux2=matrix(0, ngv[1], ngv[1])
            for (i in 1:(ngv[1]-1))
            {
              for (j in (i+1):ngv[1])
              {
                maux2[i,j] = md[i,j] # D11                                                      ##Dist?ncia dentro do grupo 1
              }
            }

#         maux3=matrix(0, ng, ng)
#         for (i in (ngv[1]+1):(ng-1))
#         {
#           for (j in (i+1):ng)
#           {
#             maux3[i,j] = md[i,j]    #D22                                                    ##Dist?ncia dentro do grupo 2
#           }
#         }

        a1 = (1/(ngv[1]*ngv[2]))*sum(maux1)
        a2 =(2/(ngv[1]*(ngv[1]-1)))*sum(maux2)
        a3 = 0#(2/(ngv[2]*(ngv[2]-1)))*sum(maux3)
        sBn = (ngv[1]*ngv[2]/(ng*(ng-1)))*(a1-a2-a3)
      }
      }
  sBn
}
