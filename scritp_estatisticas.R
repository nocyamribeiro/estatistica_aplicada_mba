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

plot(x = base_trabalhada$ALUNADO, y = base_trabalhada$TOTAL_RECEBIDO_ADESAO)
install.packages("ggplot2")
library(plyr)
library(ggplot2)

scatter.smooth(base_trabalhada$ALUNADO, base_trabalhada$TOTAL_RECEBIDO_ADESAO, pch='.', col = "blue")
grafico <- ggplot(base_trabalhada, aes(ALUNADO, TOTAL_RECEBIDO_ADESAO), pch = '.')
grafico <- grafico + geom_point(col = "blue" , size = 1)
grafico <- grafico + geom_smooth(col = "black")
grafico <- grafico + labs(x = "Alunado")
grafico <- grafico + labs(y = "Total Recebido")

png("/home/indra/Documentos/estatistica_aplicada_mba/relacao_alunado_total_investido.png")
grafico
dev.off()


## Cálculos da base toda ##################################################################
library(moments)
skewness(base_trabalhada$ALUNADO)
kurtosis(base_trabalhada$ALUNADO)
tabela_alunado <- sort(table(base_trabalhada$ALUNADO))
names(table(base_trabalhada$ALUNADO))[table(base_trabalhada$ALUNADO) == max(table(base_trabalhada$ALUNADO))]
amplitude <- max(base_trabalhada$ALUNADO) - min(base_trabalhada$ALUNADO)
amplitude
mad(base_trabalhada$ALUNADO)
sd(base_trabalhada$ALUNADO)
quantile(base_trabalhada$ALUNADO)
var(base_trabalhada$ALUNADO)
max(tabela_alunado)
base_trabalhada <- read_csv("Documentos/MBA/Estatística Aplicada/base_simetrica.csv", locale = locale(date_names = "pt"))
summary(base_trabalhada$ALUNADO)
histograma_alunado <- hist(base_trabalhada$ALUNADO, xlab = "Alunado", ylab = "Frequência" , main = "Histograma de Alunado", breaks = 15, xlim = c(0 ,max(base_trabalhada$ALUNADO)), ylim = c(0, 5))


qtd_total_total_investido <- length(base_trabalhada$TOTAL_RECEBIDO_ADESAO)
qtd_total_total_investido

base_mais_10000 <- subset(x = base_trabalhada$TOTAL_RECEBIDO_ADESAO, subset = base_trabalhada$TOTAL_RECEBIDO_ADESAO > 10000)
qtd_mais_10000 <- length(base_mais_10000)

base_menos_igual_10000 <- subset(x = base_trabalhada$TOTAL_RECEBIDO_ADESAO, subset = base_trabalhada$TOTAL_RECEBIDO_ADESAO <= 10000)
qtd_menos_igual_10000 <- length(base_menos_igual_10000)

prob_mais_10000 <- qtd_mais_10000 / qtd_total_total_investido
prob_menos_igual_10000 <- qtd_menos_igual_10000 / qtd_total_total_investido
prob_mais_10000

prob_5_10_mais_10000 <-pbinom(5, size = 10, prob = prob_mais_10000)
prob_5_10_mais_10000

pnorm(150, mean = mean(base_trabalhada$ALUNADO) , sd = sd(base_trabalhada$ALUNADO), lower=F)



#####################Tarefa 5 - Inferência


ic.m <- function(x, conf = 0.95){
     n <- length(x)
     media <- mean(x)
     variancia <- var(x)
     quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
     ic <- media + quantis * sqrt(variancia/n)
     return(ic)
}

ic.m(base_trabalhada$ALUNADO)
t.test(base_trabalhada$ALUNADO)
intervalo_confianca <- mean(base_trabalhada$ALUNADO) + qt(c(0.025, 0.975), df = length(base_trabalhada$ALUNADO)-1) * sqrt(var(base_trabalhada$ALUNADO)/length(base_trabalhada$ALUNADO))
intervalo_confianca

####Correlação entre Alunado e Total Recebido

correlacao_alunado_total_recebido <- cor(base_trabalhada$ALUNADO,base_trabalhada$TOTAL_RECEBIDO_ADESAO)
correlacao_alunado_total_recebido


regressao <- lm(base_trabalhada$ALUNADO ~ base_trabalhada$TOTAL_RECEBIDO_ADESAO)
summary(regressao)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(regressao) ~ fitted(regressao), pch = 19)
abline(h = 0, lty = 2)
x <- base_trabalhada$ALUNADO;
y <- base_trabalhada$TOTAL_RECEBIDO_ADESAO
dados <- data.frame(x, y)
regressao <- lm(y ~ x, data = dados)
#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(y~x)
abline(regressao,lty=2)

descobrirY <- function(x) {
  a <- 160.3
  b <- 4078.6 
  return (a * x) + b

}

descobrirY(1430)
library(plyr)
library(ggplot2)
grafico <- ggplot(base_trabalhada, aes(ALUNADO, TOTAL_RECEBIDO_ADESAO), pch = '.')
grafico <- grafico + geom_point(col = "blue" , size = 0.8)
grafico <- grafico + geom_smooth(method='lm',formula=y~x, col = "black", size = 0.5)
grafico <- grafico + labs(title = "Total Recebido por Alunos", x = "Alunado", y = "Total Recebido")
grafico <- grafico + theme(aspect.ratio = 0.5)
grafico
media_alunos_por_estado_localizacaco <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$UF_ESCOLA, base_trabalhada$LOCALIZACAO_ESCOLA), mean, na.rm = T)
media_alunos_por_estado <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$UF_ESCOLA), mean, na.rm = T)

pct <- round(media_alunos_por_estado$x/sum(media_alunos_por_estado$x)*100)
labels <- media_alunos_por_estado$Group.1
lbls <- paste(labels, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="")
pie <- pie(media_alunos_por_estado$x, labels = lbls)

soma_alunos_por_estado <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$UF_ESCOLA), sum, na.rm = T)
soma_alunos_por_estado

pct <- round(soma_alunos_por_estado$x/sum(soma_alunos_por_estado$x)*100, digits = 2)
labels <- soma_alunos_por_estado$Group.1
lbls <- paste(labels, pct) # add percents to labels

lbls <- paste(lbls,"%",sep="")

pie <- pie(soma_alunos_por_estado$x, radius=1, labels = soma_alunos_por_estado$x, col=rainbow(length(lbls)), main="Alunado por estado")
legend("bottom", bty = "n", yjust=0, xjust = 0.5, ncol = 10,lbls, cex=0.6,fill=rainbow(length(soma_alunos_por_estado$x)))


# Create a basic bar
valor_legenda <- paste0(soma_alunado_estado$Group.1 ," ", round(soma_alunos_por_estado$x/sum(soma_alunos_por_estado$x)*100, digits = 2), "%")
pie = ggplot(soma_alunos_por_estado, aes(x="", y=x, fill=valor_legenda)) + geom_bar(stat="identity", width=1)

#valor_legenda <- paste0(round(soma_alunos_por_estado$x/sum(soma_alunos_por_estado$x)*100, digits = 2), "%")
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = ""), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
#pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Alunos por estado")

# Limpa os dados de fora
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

?legend
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


par(resetPar())

r_norte <- c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')
r_centro_oeste <- c('GO', 'DF', 'MT', 'MS')
r_sul <- c('PR', 'SC', 'RS')
r_nordeste <- c('AL', 'BA', 'CE', 'MA', 'PB', 'PI', 'PE', 'RN', 'SE')
r_sudeste <- c('SP', 'MG', 'RJ', 'ES')


base_trabalhada$REGIAO <- ifelse(base_trabalhada$UF_ESCOLA %in% r_norte, "Norte", base_trabalhada$REGIAO)
base_trabalhada$REGIAO <- ifelse(base_trabalhada$UF_ESCOLA %in% r_centro_oeste, "Centro Oeste", base_trabalhada$REGIAO)
base_trabalhada$REGIAO <- ifelse(base_trabalhada$UF_ESCOLA %in% r_sul, "Sul", base_trabalhada$REGIAO)
base_trabalhada$REGIAO <- ifelse(base_trabalhada$UF_ESCOLA %in% r_sudeste, "Sudeste", base_trabalhada$REGIAO)
base_trabalhada$REGIAO <- ifelse(base_trabalhada$UF_ESCOLA %in% r_nordeste, "Nordeste", base_trabalhada$REGIAO)


soma_alunos_regiao <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$REGIAO), sum, na.rm = T)
soma_alunos_regiao
sum(soma_alunos_regiao$x)
sum(base_trabalhada$ALUNADO)
View(base_trabalhada)

pct_regiao <- round(soma_alunos_regiao$x/sum(soma_alunos_regiao$x)*100, digits = 2)
labels_regiao <- soma_alunos_regiao$Group.1
lbls_regiao <- paste(labels_regiao, pct_regiao) # add percents to labels
lbls_regiao <- paste(lbls_regiao,"%",sep="")
label_pizza_regiao <- soma_alunos_regiao$x
pie_regiao <- pie(soma_alunos_regiao$x, radius=1, labels = label_pizza_regiao, col=rainbow(length(lbls_regiao)), main="Alunado por Região")
legend("topright", ncol = 1,lbls_regiao, cex=0.7,fill=rainbow(length(soma_alunos_regiao$x)))

soma_recebimentos_regiao <- aggregate(base_trabalhada$TOTAL_RECEBIDO_ADESAO, list(base_trabalhada$REGIAO), sum, na.rm = T)
soma_recebimentos_regiao[order(soma_recebimentos_regiao$x), ]


soma_recebimentos_estado <- aggregate(base_trabalhada$TOTAL_RECEBIDO_ADESAO, list(base_trabalhada$UF_ESCOLA), sum, na.rm = T)
soma_recebimentos_estado[order(soma_recebimentos_estado$x), ]

soma_alunado_estado <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$UF_ESCOLA), sum, na.rm = T)
soma_alunado_estado[order(soma_alunado_estado$x), ]

base_trabalhada$RECEBIMENTO_POR_ALUNO <- base_trabalhada$TOTAL_RECEBIDO_ADESAO / base_trabalhada$ALUNADO
media_recebimentos_por_aluno <- aggregate(base_trabalhada$RECEBIMENTO_POR_ALUNO , list(base_trabalhada$UF_ESCOLA), mean, na.rm = T)
media_recebimentos_por_aluno[order(media_recebimentos_por_aluno$x), ]

summary(base_trabalhada$RECEBIMENTO_POR_ALUNO)
max(soma_alunado_estado$x)

soma_recebimento_por_localizacao <- aggregate(base_trabalhada$TOTAL_RECEBIDO_ADESAO, list(base_trabalhada$LOCALIZACAO_ESCOLA), sum, na.rm = T)
soma_recebimento_por_localizacao[order(soma_recebimento_por_localizacao$x),]

media_recebimento_por_localizacaco <- aggregate(base_trabalhada$TOTAL_RECEBIDO_ADESAO, list(base_trabalhada$LOCALIZACAO_ESCOLA), mean, na.rm = T)
media_recebimento_por_localizacaco[order(media_recebimento_por_localizacaco$x),]

media_recebimento_por_estado <- aggregate(base_trabalhada$TOTAL_RECEBIDO_ADESAO, list(base_trabalhada$UF_ESCOLA), mean, na.rm = T)
media_recebimento_por_estado[order(media_recebimento_por_estado$Group.1),]

write.csv(media_recebimento_por_estado, file = "/home/maycon/Documentos/MBA/Estatística Aplicada/media_recebimento_por_estado.csv")

media_alunado_por_localizacaco <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$LOCALIZACAO_ESCOLA), mean, na.rm = T)
media_alunado_por_localizacaco[order(media_alunado_por_localizacaco$x),]

media_recebimento_por_aluno_por_localizacao <- aggregate(base_trabalhada$RECEBIMENTO_POR_ALUNO, list(base_trabalhada$LOCALIZACAO_ESCOLA), mean, na.rm = T)
media_recebimento_por_aluno_por_localizacao[order(media_recebimento_por_aluno_por_localizacao$x),]

media_alunado_por_estado <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$UF_ESCOLA), mean, na.rm = T)
media_alunado_por_estado[order(media_alunado_por_estado$Group.1),]

View(media_alunado_por_estado)

write.csv(base_trabalhada, file = "/home/maycon/Documentos/MBA/Estatística Aplicada/base_trabalhada_completa.csv")

View(subset(base_trabalhada, subset = base_trabalhada$ALUNADO == max(base_trabalhada$ALUNADO)))
sum(base_trabalhada$ALUNADO)

media_alunado_por_localizacaco_estado <- aggregate(base_trabalhada$ALUNADO, list(base_trabalhada$LOCALIZACAO_ESCOLA, base_trabalhada$UF_ESCOLA), mean, na.rm = T)
media_alunado_por_localizacaco_estado[order(media_alunado_por_localizacaco_estado$Group.2),]

names(media_alunado_por_localizacaco_estado) <- c("LOCALIZACAO", "UF", "MEDIA_ALUNADO")
media_alunado_por_localizacaco_estado

write.csv(media_alunado_por_localizacaco_estado, file = "/home/maycon/Documentos/MBA/Estatística Aplicada/media_aluno_por_estado_localizacao.csv")
soma_alunos_regiao