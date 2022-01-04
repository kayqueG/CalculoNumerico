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

montaMatrizAh = function(n,h,p,q){
  A = matrix(0,n-1,n-1)
  for(i in 1:(n-1)){
    A[i,i] = 2 + (h**2)*q
  }
  for(i in 1:n-2){
    A[i+1,i] = -1 -(h/2)*p
    A[i,i+1] = -1 +(h/2)*p
  }
  return(A)
}

montaVetorVh = function(n,h,alpha,beta,p,q,r){
  xn = array(0,(n-1))
  for (i in 1:(n-1)){
    xn[i] = alpha + i*h
  }
  vh = array(0,(n-1))
  for (i in 1:(n-1)){
    vh[i] = -r(xn[i])
  }
  vh[1] = -r(xn[i]) + (1/h**2 + (1/2*h)*p)*alpha
  vh[n-1] = -r(xn[n-1]) + (1/h**2 - (1/2*h)*p)*beta
  return (h**2 * vh)
}

montaXn = function(n,a,h){
  #como x0 nao esta sendo utilizado, apenas crio os x1-xn-1
  xn = array(0,(n-1))
  for(i in 1:(n-1)){xn[i] = alpha + i*h}
  return (xn)
}

montaProj = function(xn){
  n = length(xn)
  proj = array(0,n)
  for(i in 1:n){proj[i] = y(xn[i])}
  return (proj)
}

erroN = function(proj,yh){return (max(abs(proj-yh)))}

a=0
b=1
alpha = 0
beta = 0
p=0
q=pi**2
r = function(x){return (-2*(pi**2)*sin(pi*x))}
y = function(x){return (sin(pi*x))}

n=c(100,200,300,400,500,600,700,800,900,1000)

resposta = matrix(0,length(n),2)

for(i in 1:length(n)){
  h = (b-a)/n[i]
  A = montaMatrizAh(n[i],h,p,q)
  vh = montaVetorVh(n[i],h,alpha,beta,p,q,r)
  xn = montaXn(n[i],a,h)
  projh = montaProj(xn)
  yh = eliminaGauss(A,vh)
  resposta[i,1] = n[i]
  resposta[i,2] = erroN(projh,yh)
  #print(i)
  #print(A)
  #print(vh)
  #print(xn)
  #print(projh)
  #print(yh)
  #print(erroN(projh,yh))
}
options(digits = 16)
print(resposta)