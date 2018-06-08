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

geraY_newton <- function(nome_nota, grau, nome_resultado){
  caminho <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/"
  matriz <-read.table(file=paste(caminho,nome_nota, sep=""), sep="\t")
  X <- matriz$V1
  Y <- matriz$V2
  
  #A interpolação de newton com graus maiores chegará a resultados perto de infinito, e acabará nos levando a resultados incertos. 
  #A partir de testes, verifiquei que seria melhor dividir os vetores de 30 em 30, não gerando os erros de -Inf.
  #Apesar de não ir à inifito de 30 em 30, fica muito incerto os resultados não chegando nem próximo a um som, diminuindo o grau para 10, 6, 4, percebe-se uma grande melhora.
  n <- 1
  i <- 1
  frequencia <- 2.2675736961451E-5 #44100Hz, como não é passado tempo para o audacity, ele reconhece essa frequência.
  
  ##É necessário que arredonde para 5 casas decimais para igualar com o txt criado pelo audacity
  novo_X <- seq(0.00001, X[length(X)], 2.2675736961451E-5)
  aux <- 1
  while(aux <= length(novo_X)){
    novo_X[aux] <- round(novo_X[aux], digits=5)
    aux <- aux+1
  }
  
  write (novo_X, file="D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/novo_X.txt", sep="\n")
  novo_Y_total <- c()
  while(n+grau <=  length(matriz$V1)){
    m <- n+grau
    X <- matriz$V1[n:m]
    Y <- matriz$V2[n:m]
    
    novo_Y <- c()
    coeficientes <- interpolacao_newton(X,Y)
    a <- 1
    while(a <= length(X)){
      if(novo_X[i] <= X[grau+1]){
        novo_Y <- c(novo_Y, polinomio(coeficientes, X, novo_X[i]))
        i <- i+1
      }
      a<- a+1
      
    }
    novo_Y_total <- c(novo_Y_total, novo_Y)
    n <- n+grau+1
  }
  caminhoResultado <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/resultados-newton/"
  
  write(novo_Y_total, file=paste(caminhoResultado,nome_resultado, sep=""), sep="\n")
  
}