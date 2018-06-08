Lagrange <- function(x,X, n){
  L <- 1
  novo_X <- X[! X %in% X[n]]
  
  for (i in novo_X){
    L <- L*((x-i)/(X[n]-i))
  }
  
  return(L)
} 

Polinomio <- function(X,Y,x){
  P <- 0
  n <- 1
  for (i in Y){
    P <- P +(i*Lagrange(x,X,n))
    n <- n+1
  }
  return(P)
}

GeraNovo_Y <- function(X,Y,novo_X){
  novo_Y <- c()
  i <- 1
  for(x in novo_X){
    novo_Y <- c(novo_Y, Polinomio(X,Y,x))
  }
  return(novo_Y)
}

geraY_lagrange <- function(nome_nota, grau, nome_resultado){
  caminho <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/"
  matriz <-read.table(file=paste(caminho,nome_nota, sep=""), sep="\t")
  X <- matriz$V1
  Y <- matriz$V2
 
  n <- 1
  i <- 1
  frequencia <- 2.2675736961451E-5 #44100Hz, como n???o ??? passado tempo para o audacity, ele reconhece essa frequ???ncia.
  
  ##??? necess???rio que arredonde para 5 casas decimais para igualar com o txt criado pelo audacity
  novo_X <- seq(0.00001, X[length(X)], frequencia)
  aux <- 1
  while(aux <= length(novo_X)){
    novo_X[aux] <- round(novo_X[aux], digits=5)
    aux <- aux+1
  }
  
  novo_Y_total <- c()
  while(n+grau <= length(matriz$V1)){
    m <- n+grau
    X <- matriz$V1[n:m]
    Y <- matriz$V2[n:m]
    
    novo_Y <- c()
    a <- 1
    while(a <= length(X)){
      if(novo_X[i] <= X[grau]){
        novo_Y <- c(novo_Y, Polinomio(X, Y, novo_X[i]))
        i <- i+1
      }
      a<- a+1
      
    }
    novo_Y_total <- c(novo_Y_total, novo_Y)
    n <- n+grau+1
  }
  caminhoResultado <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/resultados-lagrange/"
  
  write(novo_Y_total, file=paste(caminhoResultado,nome_resultado, sep=""), sep="\n")
  
}