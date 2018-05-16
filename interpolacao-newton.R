#Interpola��o de Newton

interpolacao_newton <- function(X,Y){
n <- length(X)
i <- n
j <- 1
coeficientes <- c()

while(i > 1){
  coeficientes <-c(coeficientes, Y[1])
  z <- c()
  k <- 1
  cat("Divis�es:\n")
  while(k < i){
    cat(Y[1+k],"-",Y[k],"/",X[j+k],"-",X[k],"\n")
    z <- c(z, ((Y[1+k]-Y[k])/(X[j+k]-X[k])))    
    k <- k+1 
  }
  cat("resultados:\n", z, "\n\n")
  Y <- z
  j <- j+1
  i <- i-1
}
cat("Coeficientes:", coeficientes)
return(coeficientes)
}

polinomio <- function(coeficientes, X, x){
  i <- 1
  resultado <- 0
  while( i < length(X)){
    j <- 1
    z <- 1
    while (j < i){
      z <- z*(x-X[j])
      j <- j+1
    }
    resultado <- resultado + coeficientes[i]*z
    i <- i+1
  }
  cat("\n\nP(",x,") =",resultado)
  return(resultado)
  
}
