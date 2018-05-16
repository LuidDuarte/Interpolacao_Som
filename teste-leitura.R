
pontos_l <-read.table(file="D:\\3o Semestre\\Calculo Numerico\\Trabalho\\00070-00080-l.txt", sep="\t")
plot(pontos_l$V1, pontos_l$V2, type="l", col="black")

pontos_d <-read.table(file="D:\\3o Semestre\\Calculo Numerico\\Trabalho\\00070-00080.txt", sep="\t")
plot(pontos_d$V1, pontos_d$V2, type="l", col="red")

exemploBruna <-read.table(file="D:\\3o Semestre/Calculo Numerico/Trabalho/exemploBruna.txt", sep="\t")
plot(exemploBruna$V1, exemploBruna$V2, type="l", col="blue")

parteBruna <-read.table(file="D:\\3o Semestre/Calculo Numerico/Trabalho/parteBruna.txt", sep="\t")
plot(parteBruna$V1, parteBruna$V2, type="l", col="blue")
plot(parteBruna$V1, parteBruna$V2, col="red")


exemploPdf <-read.table(file="D:/3o Semestre/Calculo Numerico/Trabalho/arquivo1.txt", sep="\t")
length(exemploPdf$V1)

exemploPdf

exemploPdf2 <-read.table(file="D:/3o Semestre/Calculo Numerico/Trabalho/arquivo2.txt", sep="\t")
length(exemploPdf2$V1)

exemploPdf