source("D:/3o\ Semestre/Calculo\ Numerico/Trabalho/lagrange.R")
source("D:/3o\ Semestre/Calculo\ Numerico/Trabalho/interpolacao-newton.R")
source("D:/3o\ Semestre/Calculo\ Numerico/Trabalho/MMQ.R")

geraY_lagrange("a1s.txt", 4,"a1s_grau4.txt")
geraY_lagrange("a1s.txt", 5,"a1s_grau5.txt")
geraY_lagrange("a1s.txt", 6,"a1s_grau6.txt")
geraY_lagrange("a1s.txt", 7,"a1s_grau7.txt")
geraY_lagrange("a1s.txt", 8,"a1s_grau8.txt")

geraY_newton("a1s.txt", 4,"a1s_grau4.txt")
geraY_newton("a1s.txt", 5,"a1s_grau5.txt")
geraY_newton("a1s.txt", 6,"a1s_grau6.txt")
geraY_newton("a1s.txt", 7,"a1s_grau7.txt")
geraY_newton("a1s.txt", 8,"a1s_grau8.txt")

##O método MMQ decidira o melhor grau, portanto passamos apenas o tamanho dos intervalos.
geraY_mmq("a1s.txt", 4, "(4)a1s_grau")
geraY_mmq("a1s.txt", 5, "(5)a1s_grau")
geraY_mmq("a1s.txt", 6, "(6)a1s_grau")
geraY_mmq("a1s.txt", 7, "(7)a1s_grau")
geraY_mmq("a1s.txt", 8, "(8)a1s_grau")