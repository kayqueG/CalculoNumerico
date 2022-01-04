###########Tarefa1############
options(digits=16)

binomial = function(n,l){
  return(factorial(n)/(factorial(l)*factorial(n-l)))
}

legendre = function(x,k = 3){
  P = array(0,c(k,length(x)))
  for(j in 0:(k-1)){
    soma = array(0,c(1,length(x)))
    for(i in 0:(j)){
      soma = soma + binomial(j,i)*binomial((j+i),i)*((x-1)/2)^i
    }
    P[j+1,] = soma
  }
  
  return(P)
}

k = 10
x = 0.7688
p = legendre(x,k)
print("Questao 1 tarefa 1:")
print(p)

###########Tarefa 2.1############
options(digits=16)
#Questao 1 Tarefa 2.1
trapezio = function(f,a,b,n,k,j){
  h = (b-a)/n
  
  #x = seq(a,b,h)
  # soma = 0
  # for(i in 2:n){
  #   soma = soma + f(x[i],j,k)
  # }
  
  x = seq(a+h,b-h,h)
  soma = 0
  soma = soma + sum(f(x,j,k))
  
  soma = soma + 0.5*f(a,j,k) + 0.5*f(b,j,k)
  return(h*soma)
}

#testando para func exp
#trapezio(exp,0,1,1000)
#print(exp(1)-exp(0))

fdl = function(x){
  return(-(pi^2)*(sin(pi*x)+cos(pi*x)))
}

f = function(x,j,k){
  return(fdl(x) * (legendre(x,k)[j,]))
}

coef_trap = function(m,k,a,b){
  alpha = 1:k
  for (j in 1:k){
    alpha[j] = ((2*j-1)/2) * trapezio(f,a,b,m,k,j)
  }
  return(alpha)
}

m = 10^4
k=9
a=-1
b=1

alpha = coef_trap(m,k,a,b)

print("Questao 1 tarefa 2.1")
print(alpha)

###########Tarefa 2.2############
eliminaGauss = function(A,y){
  m = length(y)
  #Passo1
  for (j in 1:(m-1)){
    if (A[j,j] == 0){
      k = j
      while(A[k,j] == 0){
        k = k + 1
        if(k > m){
          print("A matriz eh singular")
          return()
        }
        if(A[k,j] != 0){
          tempA = A[j,]
          tempy = y[j]
          A[j,] = A[k,]
          y[j] = y[k]
          A[k,] = tempA
          y[k] = tempy
          break
        }
      }
    }
    for (i in (j+1):m){
      u = -(A[i,j]/A[j,j])
      A[i,] = A[i,] + u*A[j,]
      y[i] = y[i] + u*y[j]
    }
  }
  #Passo2
  x = array(0,m)
  for(i in m:1){
    x[i] = y[i]
    if(i < m){
      for(j in (i+1):m){
        x[i] = x[i]-A[i,j]*x[j]
      }
    }
    x[i] = x[i]/A[i,i]
  }
  return(x)
}

legendreInt2 = function(x, k=3, somaExpr){
  P = array(0,c(k,length(x)))
  for(j in 0:(k-1)){
    soma = array(0,c(1,length(x)))
    for(i in 0:(j)){
      soma[1,] = soma[1,] + 4*binomial(j,i)*binomial((j+i),i)*((x-1)/2)^(i+2)*(1/(i+1))*(1/(i+2)) + somaExpr
    }
    P[j+1,] = soma
  }
  return(P)
}

fdlInt2 = function(x,alpha,k,somaExpr){
  soma = 0
  for(i in 1:k){
    soma = soma + legendreInt2(x,k,somaExpr)[i,]*alpha[i]
  }
  return(soma)
}

Gk = function(x,alpha,a,b,k){
  A = array(0,c(2,2))
  be = array(0,c(2,1))
  #resolvendo para os valores do extremo para descobrir o valor das constantes de integração
  A[1,] = c(1,-1)
  A[2,] = c(1,1)
  be[1,1] = a-fdlInt2(c(-1),alpha,k,0)
  be[2,1] = b-fdlInt2(c(1),alpha,k,0)
  resol = eliminaGauss(A,be)
  exprInt = resol[1] + resol[2]*x
  soma = array(0,c(1,length(x)))
  soma = soma + (a-fdlInt2(array(-1,length(x)),alpha,k,exprInt))*((1-x)/2)
  soma = soma + (b-fdlInt2(array(1,length(x)),alpha,k,exprInt))*((1+x)/2)
  soma = soma + fdlInt2(x,alpha,k,exprInt)
  return(soma)
}

y = function(x){return(sin(pi*x)+cos(pi*x))}

m = 10^4
k=7
a=-1
b=1
x=c(-1,-0.7,0,0.3,1)
#alpha = coef_trap(m,k,a,b)
resposta = Gk(x,alpha,-1,-1,k)
print("Questao 1 Tarefa 2.2")
print(resposta)

###########Tarefa 2.3############
erroETrap = function(m,k,a,b,n){
  alpha = -1
  beta = -1
  erro = array(0,length(k))
  
  h = (b-a)/m
  
  x = seq(a,b,h)

  for (i in (1:length(k))){
    
    coef = coef_trap(n,k[i],a,b)
    W = array(0,length(m+1))
    G = array(0,length(m+1))
    
    W = y(x)
    G = Gk(x,coef,alpha,beta,k[i])

    
    erro[i] = max(abs(W-G))
    #print(erro[i])
  }
  return(erro)
}

m=10117
k=seq(3,31,1)
a = -1
b = 1

erroTrap1 = erroETrap(m,k,a,b,10^4)
print("Questao 1 tarefa 2.3")
print(erroTrap1)

###########Tarefa 3.1############
m=10117
k=seq(3,31,1)
a = -1
b = 1

erroTrap2 = erroETrap(m,k,a,b,10^5)
print("Questao 1 tarefa 3.1")
print(erroTrap2)

###########Tarefa 3.2############
simpson = function(f,a,b,n,k,j){
  h = (b-a)/n
  
  x1 = seq(a+h,b-h,2*h)
  x2 = seq(a+2*h,b-h*2,2*h)
  soma = 0
  soma = soma + 4*sum(f(x1,j,k))
  soma = soma + 2*sum(f(x2,j,k))
  soma = soma + f(a,j,k) + f(b,j,k)
  
  return(h*(soma/3))
}

coef_simp = function(m,k,a,b){
  alpha = 1:k
  for (j in 1:k){
    alpha[j] = ((2*j-1)/2) * simpson(f,a,b,m,k,j)
  }
  return(alpha)
}

erroESimp = function(m,k,a,b,n){
  alpha = -1
  beta = -1
  erro = array(0,length(k))
  
  h = (b-a)/m
  
  x = seq(a,b,h)

  for (i in (1:length(k))){
    coef = coef_simp(n,k[i],a,b)
    W = array(0,length(m+1))
    G = array(0,length(m+1))
    
    W = y(x)
    G = Gk(x,coef,alpha,beta,k[i])

    
    erro[i] = max(abs(W-G))
    #print(erro[i])
  }
  return(erro)
}

m = 10117
k = seq(3,31,1)
a=-1
b=1

# m = 10^4
# k=6
# a=-1
# b=1

# alpha = coef_simp(m,k,a,b)
# 
# print("Alphas novos")
# print(alpha)

erroSimp = erroESimp(m,k,a,b,10^5)
print("Questao 1 tarefa 3.2")
print(erroSimp)

###########Tarefa 3.3############
print("Questao 1 tarefa 3.3")
k = seq(2,30,1)
plot(k, log(erroTrap1), 'b',col = "red", xlab = "k", ylab = "log(Em,k)",lwd = 6)
lines(k, log(erroTrap2),'b', col = "blue",lwd = 3)
lines(k, log(erroSimp),'b', col = "black",lwd = 1)
legend("top", legend=c("erroTrap1", "erroTrap2", "erroSimp"),
       col=c("red", "blue", "black"), lty = 1:1, cex=0.5)

###########Tarefa 4############
#erroSimp = c(6,5,7,3,5,7,3,2,7,1,3,7,3,4,1,9)

k = seq(2,16,1)
erro = log(erroSimp)[1:15]

A = array(0,c(3,3))
b = array(0,c(3,1))

f1 = function(x){return(1)}
f2 = function(x){return(x)}
f3 = function(x){return(x^2)}

f = function(x,i){
  if(i==1){
    return(f1(x))
  } else if (i==2){
    return(f2(x))
  } else {
    return(f3(x))
  }
}

for (i in 1:3){
  for (j in 1:3){
    temp = 0
    for (l in 1:length(k)){
      temp = temp + f(k[l],i) * f(k[l],j)
    }
    A[i,j] = temp
  }
}

y = array(0,c(3,1))
for (j in 1:3){
  temp = 0
  for (l in 1:length(k)){
    temp = temp + f(k[l],j) * erro[j]
  }
  y[j] = temp
}

print("Tarefa 4 - 1")
print(A)
print(y)

print("Tarefa 4 - 2")
b = eliminaGauss(A,y)
print(b)

fmq = function(x){
  return(b[1] + b[2]*x + b[3]*(x^2))
}

o = fmq(k)

print("Tarefa 4 - 3")
plot(k, o, 'b',col = "red", xlab = "k", ylab = "log(Em,k)")
lines(k, erro,'b',col = "blue")
legend("top", legend=c("Func min qud", "dados amostrais"),
       col=c("red", "blue"), lty = 1:1, cex=0.5)

