# applies the maximun homogeneity test for the configuration of the max padronized Bn
# repeat the optimization algorithm n times and chooses the maximum 
source("test_max_stdBn.R")
source("optimstdBn.R")
source("smile.R")
require("reliaR")
is_homo=function(rep,mdm,test_max=TRUE){
   Fobj=vector()
   n=dim(mdm)[1]
   grupo1=list()

   
   
   ans=optimstdBn(mdm)
   Fobj[1]=ans$Fobj[length(ans$Fobj)]
   grupo1[[1]]=ans$grupo1
   bootB=ans$bootB
   bootB1=ans$bootB1
   
    for (i in 2:rep){
        ans=optimstdBn(mdm,bootB=bootB,bootB1=bootB1)
        Fobj[i]=ans$Fobj[length(ans$Fobj)]
        grupo1[[i]]=ans$grupo1
        #print(Fobj)
   }
   minFobj=min(Fobj)
   g1=grupo1[[which(Fobj==min(Fobj))[1]]]
   g2=(1:n)[-g1]
   if(test_max==TRUE){ 
     if(n>30){
       p.homo=gumbel_correction(n,minFobj)
     }
     else{
     p.homo=test_max_stdBn(minFobj,n)
     }
     if(length(g1)==1){ 
       varBn=bootB1
     }
     else{ 
       varBn= bootB*smile(n)[length(g1)]/smile(n)[floor(n/2)]
     }
     ans=list(minFobj,g1,g2,p.homo,Fobj,grupo1,bootB,bootB1,varBn)
     names(ans)=list("minFobj","grupo1","grupo2","p.MaxTest","Fobj","grupos1","bootB","bootB1","varBn")
   }
  else{
    print("g1")
    print(g1)
    p.homo=t.Bnbonf(-minFobj,n,length(g1),alpha=0.05,bootB,bootB1,std=TRUE)    
    alpha_b=alpha/(2^(n-1)-1)
    ans=list(minFobj,g1,g2,p.homo$p.value,alpha_b,Fobj,grupo1,bootB,bootB1,p.homo$varBn)
    names(ans)=list("minFobj","grupo1","grupo2","p.Bonferroni","alpha_Bonferroni","Fobj","grupos1","bootB","bootB1","varBn")
  }

   return(ans)
    }




#Gumbel Correction
gumbel_correction=function(n,Bnpad){
nt=2^(n-1)-1
bn=sqrt(2*log(nt))- 0.5*log(4*pi*log(nt))/sqrt(2*log(nt)) - n^(3/2)/log(nt)
# corection factor - n^(3/2)/log(nt) 
an=1/sqrt(2*log(nt))
bgumbel=an*(-Bnpad-bn)
return(1-pgumbel(bgumbel,0,1))
}