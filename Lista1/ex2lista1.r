############ CÓDIGO FONTE###########################
n=20
a=0
b=1
p=0
h=(b-a)/(n)
i=seq(0,n,1)
matriz <- matrix( data=0,nrow = (n-1), ncol = (n-1))

mindiag=function(q){
  
  f= function(q){
    
    #Vetor de termos independentes:
    x= a+i*h
    y=t(x)
  
    
    #Criando a matriz Ah:
    
    for(i in 1:(n-1)){
      matriz[i,i]= 2+((h^2)*q)
    }
    
    for(i in 1:(n-2)){
      matriz[(i+1),i]=-1-(h/2)*p
      matriz[i,(i+1)]=-1+(h/2)*p
    }
   
    
    #Eliminação de gauss
    gauss = function(matriz,y,m){
      for(j in 1:(m-1)){
        if(matriz[j,j]==0){
          for(k in 1:(m-1)){
            if(matriz[k,j]!=0){
              a=matriz[j,]
              matriz[j,]=matriz[k,]
              matriz[k,]=a
            }
          }	
        }
        
        for(i in ((j+1):m)){
          u=-(matriz[i,j]/matriz[j,j])	
          for(k in j:m){
            matriz[i,k]= matriz[i,k]+(u*matriz[j,k])
          }
          y[i]=y[i]+(u*y[j])
        }
        
      }
      
      #Resolver o sistema triangular resultante
      for(i in m:1){
        x[i]=y[i]
        if(i<m){
          for(j in ((i+1):m)){  
            x[i]=x[i]-(matriz[i,j]*x[j])
          }
        }
        x[i]=(x[i]/matriz[i,i])
      }
      
      #retorna Ah*:
      return(matriz)
    }
    
    #Método para determinar o valor minimo da  diagonal de Ah*
    diagonal=function(matriz_asterisco,m){
      
      diag <- matrix(nrow = 1, ncol = m)
      for(i in 1:(m)){
        diag[i]=matriz_asterisco[i,i]
      }
      
      return(min(diag))
    }
    
    matriz_asterisco=gauss(matriz,y,n-1)
    a=diagonal(matriz_asterisco,n-1)
    return(a)

      }
  
  #Vetor com os valores minimos das diagonais de Ah* para valores distintos de q
  c <- matrix(nrow = 1, ncol = length(q))
  
  for(i in 1:length(q)){
  
    c[i]=f(q[i])
  }
  return(c)
  
}



#############EXERCICIO 2.1################################

options(digits=15)

q=seq(-10,0,1)

mindiag(q)

###########################################################

###########EXERCICIO 2.2################################

q=seq(-10,0,0.1)

plot(q,mindiag(q),type="l",col="red",lwd=3)
grid(lwd=3)

#####################################################

########################EXERCICIO 2.3#######################################

q_0=-10
q_1=-9.9
prec=10^-8
n_max=100
errof=(q_1-q_0)


met_secantes=function(mindiag,q_0,q_1,prec,n_max,errof){
  iter=1
  
while((abs(errof)>prec)&&(iter<n_max)){
  if(abs(q_0)==abs(q_1)){
    break
  }else{
  q_2=((mindiag(q_1)*q_0)-(mindiag(q_0)*q_1))/(mindiag(q_1)-mindiag(q_0))
  erro=q_1
  errof=q_1-q_0
  iter=iter+1
  q_0=q_1
  q_1=q_2
  cat("iter",(iter-1),q_2,"\n")
  }
}
cat("Número de iterações:",(iter-1))
}


options(digits=15)
met_secantes(mindiag,q_0,q_1,prec,n_max,errof)

###############################################






 
  