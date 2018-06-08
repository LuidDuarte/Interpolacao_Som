## CÓDIGO DE JOÃO GABRIEL DAMASCENO

#Na função mmq é gerado Au = b, e achado os valores dos elementos da matriz u que são os
#coeficientes do polinomio
mmq <- function(x, y, grau){
    
  n <- length(x) 
  mt <- matrix(c(0),grau+1,grau+1);
  
  #primeira matriz (A), no metodo mmq polinomial a matriz A utiliza os elementos do vetor x sendo que
  #o primeiro elemento A[1,1] é o numero de elementos do vetor x, e o modelo da matriz resumidamente
  # todos os elementos são somatorias dos elementos do vetor x elevado cada um, sendo que 
  #o expoente é definido pela posição do elemento da matriz, por exemplo numa matriz 3x3(A só é
  #matriz quadrada) na primeira o grau dos expoentes são (3) 1 2 
  #na segunda aumenta em 1 o grau que começa por tanto 1 2 3
  #na terceira linha 2 3 4
  #ou seja, na posição A[1,2] seria a somatoria de cada elemento de x elevado à 1 , somados
  
  i<-1
  j<-1
  p <- 0
  
  while(i <= grau+1)
  { 
    
    j <- 1
    
    if(i==1 && j==1){
          g <- 1
        }else{
          g <- 0
        }
    
    while(j <= grau+1)
    {
      #se for a primeira posição da matriz coloca o numero de elementos de X
      if(i==1 && j==1){
        mt[i,j] <- n
      }else{
        #faz a soma dos elementos elevados ao expoente referente de sua posição
        soma <- 0 
        for(z in 1:n)
        {
          soma <- (x[z]^(p+g)) + soma
        }
        mt[i,j] <- soma
        g <- g + 1
      }
      j <- j + 1
    }
    p<- p + 1
    i<- i +1
    
  }
  
  
  #A segunda matriz (b), ela tem uma coluna apenas e as linhas são iguais ao valor do grau+1
  #seus elementos são a somatoria de Xn^(linha-1)*Yn
  ms <- matrix(c(0),grau+1,1);
  e <- 0
  a <- 1
  b<-1
  
    while(b<=grau+1){
      soma <- 0
      
      for(c in 1:length(y)){
        soma <- ((y[c])*(x[c]^(e))) + soma
      }
      
      ms[b,a] <- soma
      e <- e+1
      b<- b+1
    }
  
  #matriz polinomio(u) sendo inicializada
  
  mr <- matrix(c(0),grau+1,1);
  
  #eliminaçao de gauss
  k <- 1
  m <- 0
  for(k in 1:grau)
  {
    for(i in (k+1):(grau+1))
    {
      m <- mt[i,k]/mt[k,k]
      mt[i,k] <- 0
      for(j in (k+1):(grau+1))
      {
        mt[i,j] <-mt[i,j] - m*mt[k,j]
      }
      ms[i,1] <- ms[i,1] - m*ms[k,1]
    }
  }
  
  
  mr[grau+1,1] <- ms[grau+1,1]/mt[grau+1,grau+1]
  for(i in (grau):1)
  {
    soma<-0
    for(j in (i+1): (grau+1))
    {
      soma <- soma + mt[i,j]*mr[j,1]
      
    }
    mr[i,1] <- (ms[i,1] - soma)/mt[i,i]
  }
  return (mr)
}

#F(X), nessa função é feito a equação referente aos coeficientes do polinomio e seu grau com o
#valor do elemento x referente
y_gerado <- function(polinomio, x, grau){
  GY <- numeric(length(x))
  
  for(a in 1:length(x))
  {
    soma <- 0
    for(b in 1:length(polinomio))
    {
      soma <- polinomio[b]*(x[a]^(b-1))+ soma
    }
    GY[a] <- soma
  }
  return(GY)
}

#Essa função calcula o desvio que cada grau gera
desv <- function(NOVOY, y){
  
  d <- numeric(length(NOVOY))
  soma <- 0
  for(a in 1:length(NOVOY))
  {
    d[a] <- (y[a] - NOVOY[a])^2
    soma <- soma + d[a]
  }
  
  return(soma)
}

geraY_mmq <- function(nome_nota, qntd, nome_resultado){
  caminho <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/"
  matriz <-read.table(file=paste(caminho,nome_nota, sep=""), sep="\t")
  novo_Y_total <- c()
  l <- 1
  grau <- 0
  desvio1 <- 2000
  desvio2 <- 1
  
  while(desvio1 > desvio2){
  grau <- grau+1
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
  
  novo_Y_linha <- c()

  while(n+qntd <= length(matriz$V1)){
      m <- n+qntd
      X <- matriz$V1[n:m]
      Y <- matriz$V2[n:m]
    
      novo_Y <- c()
      a <- 1
      while(a <= length(X)){
        if(novo_X[i] <= X[qntd]){
          novo_Y <- c(novo_Y, y_gerado(mmq(X,Y,grau), novo_X[i], k))
        i <- i+1
        }
        a<- a+1
      
      }
      novo_Y_linha <- c(novo_Y_linha, novo_Y)
      n <- n+qntd+1
  }
  novo_Y_total <- rbind(novo_Y_total, novo_Y_linha)
  if(l > 1)
    desvio1  <- desv(novo_Y_total[l-1],matriz$V2)
  
  desvio2 <- desv(novo_Y_total[l],matriz$V2)
  l <- l+1
  }
  caminhoResultado <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/resultados-mmq/"
  nome_resultado <- paste(nome_resultado,grau-1,sep="")
  nome_resultado <- paste(nome_resultado,".txt", sep="")
  write(novo_Y_total[l-1,], file=paste(caminhoResultado,nome_resultado, sep=""), sep="\n")
  
}
