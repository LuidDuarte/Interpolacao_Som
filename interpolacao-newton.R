##Interpolação de Newton

interpolacao_newton <- function(X,Y){
  n <- length(X)
  i <- n
  j <- 1
  coeficientes <- c()
  
  while(i >= 1){
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



matriz <-read.table(file="D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/a1s.txt", sep="\t")
X <- matriz$V1
Y <- matriz$V2

#A interpolação de newton com graus maiores chegará a resultados perto de infinito, e acabará nos levando a resultados incertos. 
#A partir de testes, verifiquei que seria melhor dividir os vetores de 30 em 30, não gerando os erros de -Inf.
n <- 1
novo_Y_total <- c()
while(n <= 20970){
  m <- n+29
  X <- matriz$V1[n:m]
  Y <- matriz$V2[n:m]
  
  novo_Y <- c()
  novo_X <- seq(0, X[length(X)], length.out = length(X))
  coeficientes <- interpolacao_newton(X,Y)
  a <- 1
  while(a <= length(X)){
    novo_Y <- c(novo_Y, polinomio(coeficientes, X, X[a]))
    a<- a+1
  }
  novo_Y_total <- c(novo_Y_total, novo_Y)
  n <- n+30
}

write(novo_Y_total, file="D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/teste(2).txt", sep="\n")