phi = function(x){2 - (3/4)/x}

k = 65

options(digits = 16)
a = matrix(0,(k+1),1)
a[1] = 2
for (i in 2:(k+1)){
  a[i] = phi(a[i-1])
}
print(a)