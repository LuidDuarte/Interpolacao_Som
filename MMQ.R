# Teste código final MMQ

#Na função MMQ é gerado A*C = S e encontrado os valores dos elementos da matriz C (coeficientes do polinômio)

CalculoMMQ <- function(x_dado, y_dado, grau)
{
  #BEGIN(3) - Monta a matrizA
  qnt_pontos <- length(x_dado)
  
  #Dimensão da matrizA = (grau+1)X(grau+1) = linhaXcoluna
  matrizA <- matrix(c(0),grau+1,grau+1);
  
  linha <- 1
  coluna <- 1
  grauLinha <- 0
  
  while(linha <= grau+1)
  {
    coluna <- 1
    
    if(linha==1 && coluna==1)
    {
      grauConsecutivo <- 1
    }
    else
    {
      grauConsecutivo <- 0
    }
    
    while(coluna <= grau+1)
    {
      
      if(linha==1 && coluna==1)
      {
        #MatrizA[1,1] = número de pontos dados
        matrizA[linha,coluna] <- qnt_pontos
      }
      
      else
      {
        #Somatória dos elementos elevados ao expoente referente a sua posição
        soma <- 0 
        
        for(z in 1:qnt_pontos)
        {
          soma <- (x_dado[z]^(grauLinha+grauConsecutivo)) + soma
        }
        
        matrizA[linha,coluna] <- soma
        grauConsecutivo <- grauConsecutivo + 1 #Varia os graus de uma mesma linha
      }
      
      coluna <- coluna + 1
    }
    
    grauLinha <- grauLinha+1
    linha <- linha+1
  }
  #END(3)
  
  
  #BEGIN(4) - Monta a matrizS (da "solução"). Seus elementos são a somatoria de Xn^(linha-1)*Yn
  matrizS <- matrix(c(0),grau+1,1); #matriz vazia de dimensão (grau+1)X(1)
  e <- 0 #Representa o expoente do x
  fixo <- 1
  l <- 1
  
  while(l <= grau+1) #Quantidade de linhas(l) = grau+1
  {
    soma <- 0
    
    for(c in 1:length(y_dado)) #c representa x e y que estão sendo usados
    {
      soma <- ((y_dado[c])*(x_dado[c]^(e))) + soma
    }
    
    matrizS[l,fixo] <- soma
    e <- e+1 #Atualiza o expoente de x
    l <- l+1 #Atualiza a linha da matriz
  }
  #END(4)
  
  
  #MatrizC (dos coeficientes) sendo inicializada
  matrizC <- matrix(c(0),grau+1,1);
  
  
  #BEGIN(Eliminação de Gauss) - Insere na matrizC os valores calculados dos coeficientes
  #BEGIN(5) - Transforma a matrizA em uma matriz triangular superior
  k <- 1
  m <- 0
  
  for(k in 1:grau)
  {
    for(i in (k+1):(grau+1))
    {
      m <- matrizA[i,k]/matrizA[k,k]
      matrizA[i,k] <- 0
      
      for(j in (k+1):(grau+1))
      {
        matrizA[i,j] <-matrizA[i,j] - m*matrizA[k,j]
      }
      matrizS[i,1] <- matrizS[i,1] - m*matrizS[k,1]
    }
  }
  #END(5)
  
  #BEGIN(6) - Calcula os valores de todos os coeficientes
  #Calcula valor do último coeficiente
  matrizC[grau+1,1] <- matrizS[grau+1,1]/matrizA[grau+1,grau+1]
  
  for(i in (grau):1)
  {
    soma<-0
    for(j in (i+1): (grau+1))
    {
      soma <- soma + matrizA[i,j]*matrizC[j,1]
    }
    matrizC[i,1] <- (matrizS[i,1] - soma)/matrizA[i,i]
  }
  #END(6)
  
  #Retorna a matriz dos coeficientes com seus valores calculados
  return (matrizC)
  
  #END(Eliminação de Gauss)
}
#Fim função CalculoMMQ


#Calcula os novos valores f(x) (usando o polinômio com os coeficientes encontrados pelo MMQ)
CalculoNovoY <- function(coeficientes, x_dado, grau)
{
  #Cria um vetor de tamanho = quantidade de pontos dados
  y_gerados <- numeric(length(x_dado))
  
  for(posicao_variavel in 1:length(x_dado))
  {
    soma <- 0
    
    for(posicao_coeficiente in 1:length(coeficientes))
    {
      soma <- coeficientes[posicao_coeficiente]*(x_dado[posicao_variavel]^(posicao_coeficiente-1))+soma
    }
    
    y_gerados[posicao_variavel] <- soma
  }
  
  return(y_gerados)
}
#Fim função CalculoNovoY


CalculoDesvio <- function(novo_y, y_dado)
{
  #Cria o vetor desvio de tamanho = quantidade de y calculados
  desvio <- numeric(length(novo_y))
  desvio_total <- 0
  
  for(posicao in 1:length(novo_y))
  {
    desvio[posicao] <- (y_dado[posicao] - novo_y[posicao])^2
    desvio_total <- desvio_total + desvio[posicao]
  }
  
  return(desvio_total)
}
#Fim função CalculoDesvio

GeraY_MMQ <- function(nome_nota, qntd, nome_resultado)
{
  caminho <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/"
  
  #Armazena os valores da nota do piano
  matriz <- read.table(file=paste(caminho,nome_nota, sep=""), sep="\t")
  
  novoY_total <- c()
  l <- 1
  grau <- 0
  desvio1 <- 2000
  desvio2 <- 1
  
  while(desvio1 > desvio2)
  {
    grau <- grau+1
    X <- matriz$V1
    Y <- matriz$V2
    n <- 1
    i <- 1
    
    #Como não é passado tempo para o audacity, ele reconhece a frequênci 44100Hz
    frequencia <- 2.2675736961451E-5
    
    novo_X <- seq(0.00001, X[length(X)], frequencia)
    aux <- 1
    
    #Arredonda para 5 casas decimais para igualar com o txt criado pelo audacity
    while(aux <= length(novo_X))
    {
      novo_X[aux] <- round(novo_X[aux], digits=5)
      aux <- aux+1
    }
    
    novo_Y_linha <- c()
    
    while(n+qntd <= length(matriz$V1))
    {
      m <- n+qntd
      X <- matriz$V1[n:m]
      Y <- matriz$V2[n:m]
      
      novo_Y <- c()
      a <- 1
      
      while(a <= length(X))
      {
        if(novo_X[i] <= X[qntd])
        {
          novo_Y <- c(novo_Y, CalculoNovoY(CalculoMMQ(X,Y,grau), novo_X[i], k))
          i <- i+1
        }
        
        a<- a+1
      }
      
      novo_Y_linha <- c(novo_Y_linha, novo_Y)
      n <- n+qntd+1
    }
    
    novoY_total <- rbind(novoY_total, novo_Y_linha)
    if(l > 1)
      desvio1  <- CalculoDesvio(novoY_total[l-1],matriz$V2)
    
    desvio2 <- CalculoDesvio(novoY_total[l],matriz$V2)
    l <- l+1
  }
  
  caminhoResultado <- "D:/3o\ Semestre/Calculo\ Numerico/Trabalho/notas\ piano/arquivos-texto/resultados-mmq/"
  nome_resultado <- paste(nome_resultado,grau-1,sep="")
  nome_resultado <- paste(nome_resultado,".txt", sep="")
  write(novoY_total[l-1,], file=paste(caminhoResultado,nome_resultado, sep=""), sep="\n")
}
#Fim função GeraY_MMQ
