library(moments)
skewness(base_simetrica$ALUNADO)
sort(table(base_simetrica$ALUNADO))
base_simetrica <- read_csv("Documentos/MBA/Estatística Aplicada/base_simetrica.csv", locale = locale(date_names = "pt"))
summary(base_simetrica$ALUNADO)
histograma_alunado <- hist(base_simetrica$ALUNADO, xlab = "Alunado", ylab = "Frequência" , main = "Histograma de Alunado", breaks = 15, xlim = c(0 ,max(base_simetrica$ALUNADO)), ylim = c(0, 5))

sd(base_simetrica$ALUNADO)
mean(base_simetrica$ALUNADO)
median(base_simetrica$ALUNADO)
pnorm(120, mean = mean(base_simetrica$ALUNADO) , sd = sd(base_simetrica$ALUNADO), lower=F)

qtd_total_total_investido <- length(base_simetrica$TOTAL_RECEBIDO_ADESAO)
qtd_total_total_investido

base_mais_10000 <- subset(x = base_simetrica$TOTAL_RECEBIDO_ADESAO, subset = base_simetrica$TOTAL_RECEBIDO_ADESAO > 10000)
qtd_mais_10000 <- length(base_mais_10000)

base_menos_igual_10000 <- subset(x = base_simetrica$TOTAL_RECEBIDO_ADESAO, subset = base_simetrica$TOTAL_RECEBIDO_ADESAO <= 10000)
qtd_menos_igual_10000 <- length(base_menos_igual_10000)

prob_mais_10000 <- qtd_mais_10000 / qtd_total_total_investido
prob_menos_igual_10000 <- qtd_menos_igual_10000 / qtd_total_total_investido

prob_5_10_mais_10000 <-pbinom(5, size = 10, prob = prob_mais_10000)
prob_5_10_mais_10000

base_trabalhada <- read_csv("Documentos/MBA/Estatística Aplicada/base_trabalhada.csv", locale = locale(date_names = "pt"))

png("/home/maycon/Documentos/MBA/Estatística Aplicada/histograma_alunado_geral.png")
hist(base_trabalhada$ALUNADO, breaks = 30 , ylim= c(0, 15000), xlim = c(0, max(base_trabalhada$ALUNADO)), col = c("blue", "black"), main = "Histograma de Alunado", ylab = "Frequência", xlab = "Alunado")
dev.off()

png("/home/maycon/Documentos/MBA/Estatística Aplicada/histograma_total_recebido_geral.png")
hist(base_trabalhada$TOTAL_RECEBIDO_ADESAO, breaks = 30 , ylim= c(0, 13000), xlim = c(0, max(base_trabalhada$TOTAL_RECEBIDO_ADESAO)), col = c("blue", "black"), main = "Histograma de Total Recebido", ylab = "Frequência", xlab = "Total Recebido")
dev.off()