##Interpolação de Newton

interpolacao_newton <- function(X,Y){
  n <- length(X)
  i <- n
  j <- 1
  coeficientes <- c()
  
  while(i > 1){
      cat(".")
    coeficientes <-c(coeficientes, Y[1])
    z <- c()
    k <- 1
    while(k < i){
      z <- c(z, ((Y[1+k]-Y[k])/(X[j+k]-X[k])))
      k <- k+1 
    }
    Y <- z
    j <- j+1
    i <- i-1
    
  }
  return(coeficientes)
}

polinomio <- function(C, X, x){
  i <- 1
  S <- c()
  M <- c(1)
  
  while(i <= length(C)){
    if(i == 1){
      S <- c(S,C[i])
    }
    else{
      M <- c(M,M[i-1]*(x - X[i-1]))
      S <- c(S,S[i-1]+M[i]*C[i])
    }
    i <- i+1
  }
  
  return(S[i-1])
}
