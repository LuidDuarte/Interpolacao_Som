
pontos_l <-read.table(file="D:\\3o Semestre\\Calculo Numerico\\Trabalho\\00070-00080-l.txt", sep="\t")
plot(pontos_l$V1, pontos_l$V2, type="l", col="black")

pontos_d <-read.table(file="D:\\3o Semestre\\Calculo Numerico\\Trabalho\\00070-00080.txt", sep="\t")
plot(pontos_d$V1, pontos_d$V2, type="l", col="red")
