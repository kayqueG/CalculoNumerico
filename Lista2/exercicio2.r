
###################EX2.1##############################
#############DECLARANDO AS VARIAVEIS###################
t=c(0,0.125,0.250,0.375,0.5,0.065,0.750,0.875,1)
x=c(0.7416,0.2685,0.3333,0.3982,-0.0749,-0.3089,0.3333,0.9756,0.7416)
y=c(0.7416,0.9756,0.3333,-0.3089,-0.0749,0.3982,0.3333,0.2685,0.7416)
z=c(-0.4832,-0.2441,0.3334,0.9107,1.1498,0.9107,0.3334,-0.2441,-0.4832)
j=seq(0,50,1)
t_asterisco=j/50


##########CRIANDO UM MÉTODO PARA p_x(t)##################
p_x=function(o){
  vet<- matrix(nrow = 1, ncol = 51)
  
  
k=1
while(k<=51){
  soma=0
for(i in 1:length(t)){
  u=1
  r=1

  for(j in 1:length(t)){
    if(j!=i){
      u=u*(o[k]-t[j])
      r=r*(t[i]-t[j])
    }
  }
  
  soma=soma+(u/r*x[i])
}
  vet[k]=soma
  k=k+1
}
  
  return(vet)
}


##########CRIANDO UM MÉTODO PARA p_y(t)##################
p_y=function(o){
  vet<- matrix(nrow = 1, ncol = 51)
  matriz<- matrix(nrow = 1, ncol = 51)

  k=1
  while(k<=51){
    soma=0
    for(i in 1:length(t)){
      u=1
      r=1
      
      for(j in 1:length(t)){
        if(j!=i){
          u=u*(o[k]-t[j])
          r=r*(t[i]-t[j])
        }
      }
      
      soma=soma+(u/r*y[i])
    }
    vet[k]=soma
    k=k+1
  }
  
  return(vet)
}


##########CRIANDO UM MÉTODO PARA p_z(t)##################
p_z=function(o){
  vet<- matrix(nrow = 1, ncol = 51)
  
 
  k=1
  while(k<=51){
    soma=0
    for(i in 1:length(t)){
      u=1
      r=1
      
      for(j in 1:length(t)){
        if(j!=i){
          u=u*(o[k]-t[j])
          r=r*(t[i]-t[j])
        }
      }
      
      soma=soma+(u/r*z[i])
    }
    vet[k]=soma
    k=k+1
  }
  
  return(vet)
}

##########DEFININDO UMA MATRIZ PARA SALVAR OS VALORES############
tabela<- matrix(nrow = 3, ncol = 51)
tabela <-rbind(p_x(t_asterisco),p_y(t_asterisco),p_z(t_asterisco))

tabela

#################EXERCICIO 2.2############################
p_x(t_asterisco)+p_y(t_asterisco)+p_z(t_asterisco)

################EXERCICIO 2.3###################

install.packages("scatterplot3d") # Instalando o pacote para plotar o gráfico 3D
library("scatterplot3d")
p_x=p_x(t_asterisco)
p_y=p_y(t_asterisco)
 p_z=p_z(t_asterisco)
scatterplot3d(p_x[1,],p_y[1,],p_z[1,],pch=16,color="blue",type="h","Gráfico 3D dos polinômios",xlab="p_x(t*)",ylab="p_y(t*)",zlab="p_z(t*)",angle=135)
##OBS: É possivel rotacionar o grafico adicionando o parametro angle!!!!!#############







