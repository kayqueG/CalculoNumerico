
####################EXERCICIO 3.1 #############################################

n=10
a=0
b=1
h=(b-a)/(n)
i=seq(0,n,1)
q=(pi)^2
p=-2*((pi)^2)



#Vetor de termos independentes:
matriz<- matrix( data=0,nrow = (n-1), ncol = n-1)
x=seq(0.1,0.9,0.1)
y=t(x)

r=-2*(pi^2)*sin(pi*x)
y=-r*(h^2)

#Criando a matriz Ah:

for(i in 1:(n-1)){
  matriz[i,i]= 2+((h^2)*q)
}

for(i in 1:(n-2)){
  matriz[(i+1),i]=-1-(h/2)*p
  matriz[i,(i+1)]=-1+(h/2)*p
}

x_0<- matrix( data=0,nrow = n-1, ncol = 1)
n_max=7
k=0
x_old=x_0
x_new=x_0
w=y
m=n-1  
z=1
o<- matrix( nrow = n-1, ncol = n_max) #Matriz para guardar os valores de x_new

seidel=function(matriz,w,m,x_0,n_max){
while(k<n_max){
x_new[1]=(1/(2+((h^2)*q)))*(w[1]+x_old[2])
for(i in 1:(m-1)){
  if((i !=1)&&(i!=9)){
    x_new[i]=(1/(2+((h^2)*q)))*(w[i]+(x_new[i-1])+(x_old[i+1]))
  }
  
}

x_new[m]=(1/2)*((w[m]+x_new[m-1]))
o[,z]=x_new
z=z+1
x_old=x_new
k=k+1
}
return(o)
}

seidel(matriz,w,m,x_0,n_max)
##################################################






    