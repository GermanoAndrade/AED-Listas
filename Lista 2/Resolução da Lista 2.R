#' ---
#' title: "Resolução da Lista 2"
#' author: "Germano Andrade Brandão - 2017080008"
#' date: "07/04/2020"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(ggpubr)
setwd("C:/Graduação - Data Science/1º Período/Análise Explorátoria de Dados e Visualização/AED - Listas/AED-Listas/Lista 2")
#Carregando os dados
load("./Databases/dados.RData")
cd_salarios <- read.csv("./Databases/cd_salarios.csv", sep=";")

#' 
#' ## Nota inicial
#' 
#' Para a resolução dos exercícios, foram utilizados os pacotes "ggplot2" (para construção de gráficos) e "ggpubr" (para realocar os gráficos na última questão da lista).
#' 
#' # Capítulo 2
#' 
#' ### Questão 3
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 2/Questão_3.PNG")

#' 
#' ##### ºPopulação Urbana
## ------------------------------------------------------------------------------
dadosPop <- cd_brasil$pop_urbana
dadosPop <- dadosPop[!is.na(dadosPop)]
Frequencia_ni <- table(cut(dadosPop, b=c(-Inf,700000, 2000000,4000000,6000000,8000000,Inf), dig.lab = 8))
Frequencia_fi <- Frequencia_ni/sum(Frequencia_ni)
Porcentagem_100fi <- Frequencia_fi*100
Tabela_Pop <- matrix(c(Frequencia_ni, Frequencia_fi, Porcentagem_100fi), nrow = 6, ncol=3)
Tabela_Pop <- rbind(Tabela_Pop, colSums(Tabela_Pop[,1:3]))
colnames(Tabela_Pop) <- c("Frequência ni","Frequência fi","Porcentagem 100fi")
row.names(Tabela_Pop) <- c("Menor que 700.000", "Entre 700.000 e 2.000.000", "Entre 2.000.000 e 4.000.000", "Entre 2.000.000 e 4.000.000", "Entre 6.000.000  e 8.000.000", "Maior que 8.000.000", "Total")
Tabela_Pop

#' 
#' ##### ºDensidade Populacional
## ------------------------------------------------------------------------------
dadosDensi <- cd_brasil$densidade
dadosDensi <- as.numeric(sub(",", ".", dadosDensi))
dadosDensi <- dadosDensi[!is.na(dadosDensi)]
Frequencia_ni <- (table(cut(dadosDensi, b=c(-Inf, 8, 25, 50, 70, 90, Inf))))
Frequencia_fi <- Frequencia_ni/sum(Frequencia_ni)
Porcentagem_100fi <- Frequencia_fi*100
Tabela_Densi <- matrix(c(Frequencia_ni, Frequencia_fi, Porcentagem_100fi), nrow = 6, ncol = 3)
Tabela_Densi <- rbind(Tabela_Densi, colSums(Tabela_Densi[,1:3]))
colnames(Tabela_Densi) <- c("Frequência ni", "Frequência fi", "Porcentagem 100fi")
row.names(Tabela_Densi) <- c("Menor que 8", "Entre 8 e 25", "Entre 25 e 50", "Entre 50 e 70", "Entre 70 e 90", "Maior que 90", "Total")
Tabela_Densi

#' 
#' ### Questão 8
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 2/Questão_8.PNG")

#' 
#' ##### ºHistograma
## ------------------------------------------------------------------------------
ggplot(data = cd_municipios, aes(x = populacao)) +
geom_histogram(breaks=seq(0,1000, by=200), col="gray", fill="blue") + labs(title="Histograma da distribuição das populações", x="Populações (em 10.000 habitantes)", y="Frequência") + scale_x_continuous(breaks=seq(0,1000,200))

#' 
#' ##### ºRamo-e-folhas
## ------------------------------------------------------------------------------
stem(cd_municipios$populacao, scale =16)

#' 
#' ##### ºGráfico de dispersão unidimensional
## ------------------------------------------------------------------------------
ggplot(data = cd_municipios, aes(populacao,0)) + geom_point(col='blue', size =2) + labs(title="Gráfico de dispersão das populações", x="Populações (em 10.000 habitantes)",y=" ") + geom_line(col = "red")

#' 
#' ### Questão 20
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 2/Questão_20.PNG")

#' 
#' ##### ºRamo-e-folhas
## ------------------------------------------------------------------------------
stem(cd_poluicao$co)

#' 
#' # Capítulo 3
#' 
#' ### Questão 18
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_18.PNG")

#' 
## ------------------------------------------------------------------------------
Pop <- cd_municipios$populacao
#Ordenando o vetor em  ordem decrescente:
Pop <- sort(Pop,decreasing = TRUE)
#Ordenando o novo vetor que contém as 15 maiores.
Pop <- sort(Pop[1:15])
# Novo conjunto de dados
Pop

# Quantis q(0,1), q(0,2), q1=q(0,25), q2=q(0,5)=Mediana e q3=q(0,75)
quantile(Pop, probs = c(0.1,0.2,0.25,0.5,0.75))

#' 
#' ### Questão 42
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_42.PNG")

#' 
#' ##### Para essa e outras questões, vamos precisar do conceito de desvio absoluto mediano (dam):
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Desvio_absoluto_mediano.PNG")

#' 
#' ##### ºPopulação Urbana
## ------------------------------------------------------------------------------
Pop_Urb <- cd_brasil$pop_urbana
Pop_Urb <- Pop_Urb[!is.na(Pop_Urb)]
Desv_Abs_Median <- median(abs(Pop_Urb-median(Pop_Urb)))
Desv_Abs_Median

#' 
#' ##### ºPopulação Rural
## ------------------------------------------------------------------------------
Pop_Rur <- cd_brasil$pop_rural
Pop_Rur <- sort(Pop_Rur[!is.na(Pop_Rur)])
Desv_Abs_Median <- median(abs(Pop_Rur-median(Pop_Rur)))
Desv_Abs_Median

#' 
#' ### Questão 43
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_43-a.PNG")

#' 
#' ##### (a)
#' 
#' ##### ºMedidas de posição
## ------------------------------------------------------------------------------
Mono_Carb <- cd_poluicao$co
#Média
mean(Mono_Carb)

#Mediana
median(Mono_Carb)

#Moda
#OBS.: O próprio conjunto de dados fornecido ("dados.RData") nos dá uma função "moda2" que calcula a moda de um conjunto, mas não vamos considerar essa função para a resolução da questão. Ex.:
moda2(Mono_Carb)

#Para calcular a moda, pelo fato de o R não possuir uma função built-in para tal, assim como tem para calcular a média ou a mediana, vamos utilizar outro método. Aqui, pegamos as repetições das observações com a função "table"
table(Mono_Carb)

#Aqui, já conseguimos observar que o número 6.2 foi o que se repetiu mais vezes (9), mas queremos que o R busque isso para a gente.
#Então, o que queremos é o nome da "coluna" que corresponde ao valor máximo de repetições [table(Mono_Carb)]
names(table(Mono_Carb)[table(Mono_Carb)==max(table(Mono_Carb))])

#' 
#' ##### ºMedidas de dispersão
## ------------------------------------------------------------------------------
#Desvio Médio
#Igual ao somatório do valor absoluto das distâncias de cada observação à média, dividido pelo total de observações.
print(sum(abs(Mono_Carb-mean(Mono_Carb)))/length(Mono_Carb))

#variância
#Aqui foi preciso converter a Variância Amostral para Variância Populacional multiplicando por (n-1) e dividindo por (n).
var(Mono_Carb)*(length(Mono_Carb)-1)/length(Mono_Carb)

#Desvio Padrão
#Igual à raiz quadrada da Variância.
Var <- var(Mono_Carb)*(length(Mono_Carb)-1)/length(Mono_Carb)
print(Var^(1/2))

#Média Aparada [x(0,10)]
#Vamos ordenar as observações em ordem crescente:
Mono_Carb_O <- sort(Mono_Carb)
#Agora, vamos calcular a quantidade de observações (100α%, com α=0,10) a qual vamos precisar remover essa quantidade das menores observações e essa quantidade das maiores:
Qtd_Observ <- (length(Mono_Carb_O)*0.10)
Qtd_Observ

#Agora, removemos 12 das menores e das maiores observções, e calculamos a média (assim, teremos a média aparada):
Mono_Carb_R <- Mono_Carb_O[(Qtd_Observ+1):(length(Mono_Carb_O)-Qtd_Observ)]
mean(Mono_Carb_R)

#' 
#' ##### (b)
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_43-b.PNG")

#' 
#' ##### ºMedidas de posição
## ------------------------------------------------------------------------------
Salarios <- cd_salarios$Mecânico
#Média
mean(Salarios)

#Mediana
median(Salarios)

#Moda
#Usaremos o mesmo método do item (a):
names(table(Salarios)[table(Salarios)==max(table(Salarios))])

#' 
#' ##### ºMedidas de dispersão
## ------------------------------------------------------------------------------
#Desvio Médio
#Igual ao somatório do valor absoluto das distâncias de cada observação à média, dividido pelo total de observações.
print(sum(abs(Salarios-mean(Salarios)))/length(Salarios))

#variância
#Aqui foi preciso converter a Variância Amostral para Variância Populacional multiplicando por (n-1) e dividindo por (n).
var(Salarios)*(length(Salarios)-1)/length(Salarios)

#Desvio Padrão
#Igual à raiz quadrada da Variância.
Var <- var(Salarios)*(length(Salarios)-1)/length(Salarios)
print(Var^(1/2))

#Média Aparada [x(0,10)]
#Vamos ordenar as observações em ordem crescente:
Salarios_O <- sort(Salarios)
#Agora, vamos calcular a quantidade de observações (100α%, com α=0,10) a qual vamos precisar remover essa quantidade das menores observações e essa quantidade das maiores:
Qtd_Observ <- (length(Salarios_O)*0.10)
Qtd_Observ

#Agora, removemos 3 das menores e das maiores observções, e calculamos a média (assim, teremos a média aparada):
Salarios_R <- Salarios_O[(Qtd_Observ+1):(length(Salarios_O)-Qtd_Observ)]
mean(Salarios_R)

#' 
#' ##### (c)
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_43-c.PNG")

#' 
#' ##### ºMedidas de posição
## ------------------------------------------------------------------------------
Preco <- cd_veiculos$preco
#Média
mean(Preco)

#Mediana
median(Preco)

#Moda
#Utilizaremos o mesmo metódo descrito no item (a):
names(table(Preco)[table(Preco)==max(table(Preco))])

#percebemos que a Moda é igual ao número total de observações:
length(Preco)==length(names(table(Preco)[table(Preco)==max(table(Preco))]))

#Logo, concluímos que a distribuição não tem um valor que se repete, ou seja, não possui uma moda.

#' 
#' ##### ºMedidas de dispersão
## ------------------------------------------------------------------------------
#Desvio Médio
#Igual ao somatório do valor absoluto das distâncias de cada observação à média, dividido pelo total de observações.
print(sum(abs(Preco-mean(Preco)))/length(Preco))

#variância
#Aqui foi preciso converter a Variância Amostral para Variância Populacional multiplicando por (n-1) e dividindo por (n).
var(Preco)*(length(Preco)-1)/length(Preco)

#Desvio Padrão
#Igual à raiz quadrada da Variância.
Var <- var(Preco)*(length(Preco)-1)/length(Preco)
print(Var^(1/2))

#Média Aparada [x(0,10)]
#Vamos ordenar as observações em ordem crescente:
Preco_O <- sort(Preco)
#Agora, vamos calcular a quantidade de observações (100α%, com α=0,10) a qual vamos precisar remover essa quantidade das menores observações e essa quantidade das maiores:
Qtd_Observ <- (length(Preco_O)*0.10)
Qtd_Observ

#Agora, removemos 3 das menores e das maiores observções, e calculamos a média (assim, teremos a média aparada):
Preco_R <- Preco_O[(Qtd_Observ+1):(length(Preco_O)-Qtd_Observ)]
mean(Preco_R)

#' 
#' ### Questão 44
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_44.PNG")

#' 
#' ##### ºMonóxido de Carbono (CO)
## ------------------------------------------------------------------------------
ggplot(data = cd_poluicao, aes(x = co)) + geom_histogram(breaks = seq(4, 13), col="gray1", fill = "turquoise2") + labs(title = "Histograma (CO)", x = "Monóxido do Carbono (CO)", y = "Frequência") +
    scale_x_continuous(breaks=seq(4,13)) + theme_classic()

#' 
#' Ramo-e-folhas (CO)
## ------------------------------------------------------------------------------
stem(cd_poluicao$co)

#' 
#' Desenho esquemático
## ------------------------------------------------------------------------------
ggplot(data = cd_poluicao, aes(y=co)) + geom_boxplot(outlier.colour = "black", col = "firebrick4", fill = "darkorchid3") + labs(title="Box Plot (CO)", y = "Monóxido de Carbono (CO)") + theme_classic()

#' 
#' ##### ºSalário de Mecânicos
## ------------------------------------------------------------------------------
ggplot(data = cd_salarios, aes(x = Mecânico)) + geom_histogram(fill = "deeppink", col = "cyan1", breaks = seq(0,40,10)) + labs(title = "Histograma Salário de Mecânicos", x = "Salario dos mecânicos(em francos suiços)", y = "Frequência") + theme_classic()

#' 
#' Ramo-e-folhas
## ------------------------------------------------------------------------------
stem(cd_salarios$Mecânico, scale = 4)

#' 
#' Desenho esquemático
## ------------------------------------------------------------------------------
ggplot(data = cd_salarios, aes(y = Mecânico)) + geom_boxplot(fill = "forestgreen", colour = "lightseagreen") + labs(title = "Box Plot Salário de Mecânicos", y = "Salario dos mecânicos(em francos suiços)") + theme_classic()

#' 
#' ##### ºPreço dos Veículos
## ------------------------------------------------------------------------------
ggplot(data = cd_veiculos, aes(x = preco)) + geom_histogram(linetype="dotted",col = "dodgerblue3",fill = "tomato", breaks = seq(0,40000,10000)) + labs(title = "Histogama Preços", x = "Preço dos veículos (em dólares)", y = "Freqûencia") + theme_classic()

#' 
#' Ramo-e-folhas
## ------------------------------------------------------------------------------
stem(cd_veiculos$preco, scale = 4)

#' 
#' Desenho esquemático
## ------------------------------------------------------------------------------
ggplot(data = cd_veiculos, aes(y = preco)) + geom_boxplot(fill = "wheat3", col = "black",outlier.color = "red", outlier.shape = 8, outlier.size = 3) + labs(title = "Box Plot Preço de Veículos", y = "Preço dos veículos (em dólares)") + theme_classic()

#' 
#' ### Questão 46
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 3/Questão_46.PNG")

#' 
## ------------------------------------------------------------------------------
Ubatuba <- cd_temperaturas$ubatuba
Ubatuba <- sort(Ubatuba)
{ggplot(data = cd_temperaturas, aes(x = seq(0,1,length.out = 120), y = Ubatuba)) + 
  geom_point(shape = 23, fill = "midnightblue", size = 3.8) + 
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  labs(title = "Gráfico de Quantis", x = " ", y = "Temperatura") + 
  theme(plot.background = element_rect(fill = "beige"), panel.background = element_rect(fill = "beige"), panel.grid = element_line(linetype = "longdash", colour = "gray30"), panel.grid.minor = element_line(colour = "beige"))}

#' 
## ---- include=FALSE------------------------------------------------------------
u = seq(0, 8.1, 0.1)
v = u

#' 
#' 
## ------------------------------------------------------------------------------
#Vamos usar que ui = q2(mediana) - xi e vi = x[n+1-i] - q2
#Então como i vai do primeiro elemento (x1) à mediana, vamos considerar i como de 1 até metade das observações (i = n/2)
i <- 1:(length(Ubatuba)/2)
#Temos que  1≤i≤60
i

#Agora, vamos calcular os ui
ui <- median(Ubatuba)-Ubatuba[i]
#Para calcular os vi, vamos apenas reordernar o vetor em ordem decrescente e naturalmente obteremos o resultado que se quer.
Ubatuba_D <- sort(Ubatuba, decreasing = TRUE)
vi <- Ubatuba_D[i]-median(Ubatuba_D)

#Por fim, podemos fazer o gráfico de simetria em relação à reta u=v (em vermelho)
ggplot(data = NULL, aes(x = ui, y = vi)) +
  geom_point(shape = 19, colour = "green2", size = 4) +
  geom_line(data = NULL, aes(x = u, y = v), col = "red", size = 1.2) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "white"), panel.grid.minor = element_line(linetype = "longdash", colour = "gray40"), panel.background = element_rect(fill = "white"), axis.title = element_text(face = "bold")) +
  labs(title = "Gráfico de Simetria", x = "u", y = "v")

#' 
#' # Capítulo 4
#' 
#' ### Questão 37
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 4/Questão_37.PNG")

#' 
#' A partir da análise dos Box Plots das regiões geográficas brasileiras, notamos a região sudeste com uma distribuição anormal em relação às demais e isso se deve ao fato de conter o Estado de São Paulo, que possui uma população bastante acima da média da sua região e da média nacional. Também observamos que a região Norte é a que possui uma distribuição mais simétrica dentre as regiões, e que a região Sul, ao contrário da Nordeste e Centro-Oeste, tem uma distribuição assimétrica à esquerda.
#' 
## ---- warning=FALSE------------------------------------------------------------
ggplot(data = cd_brasil, aes(x = regiao, y = total)) + 
  geom_boxplot(outlier.colour = "maroon1", outlier.shape = 8, fill = "yellow1", col = "turquoise4") + 
  labs(title = "Box Plot da População Brasileira por Região", x = "Região", y = "População") +
  scale_y_continuous(breaks = seq(0,35000000,5000000)) +
  theme_classic()

#' 
#' ### Questão 39
## ---- echo=FALSE, out.width = '80%'--------------------------------------------
knitr::include_graphics("./Questões/Capítulo 4/Questão_39.PNG")

#' 
#' Analisando os Gráficos de dispersão, percebe-se que existe associação entre as variáveis em questão. Uma vez que, por exemplo, se a Temperatura está alta, a Umidade também está, tomando o Gráfico 2 como exemplo.
## ------------------------------------------------------------------------------
CO <- sort(cd_poluicao$co)
Temperatura <- sort(cd_poluicao$temp)
Umidade <- sort(cd_poluicao$umid)
  
GDP1 <- ggplot(data = cd_poluicao, aes(x = CO, y = temp)) +
  geom_point(shape = 23, fill = "yellow1", size = 3) +
  scale_x_continuous(breaks = seq(0,14, 2),limits = c(0,14)) +
  scale_y_continuous(limits = c(0,25)) +
  labs(title = "Gráfico de dispersão 1: Monóxido de Carbono (CO) X Temperatura", x = "Monóxido de Carbono (CO)", y = "Temperaura (°C)") +
  theme_classic() +
  theme(panel.grid = element_line(linetype = "dashed", colour = "white"), panel.grid.minor = element_line(linetype = "longdash", colour = "gray40"))


GDP2 <- ggplot(data = cd_poluicao, aes(x = Temperatura, y = umid)) +
  geom_point(shape = 24, fill = "magenta1", size = 3) +
  scale_x_continuous(breaks = seq(0,25,5), limits = c(0,25)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(title = "Gráfico de dispersão 2: Temperatura X Umidade", x = "Temperatura (°C)", y = "Umidade") +
  theme_classic() +
  theme(panel.grid = element_line(linetype = "dashed", colour = "white"), panel.grid.minor = element_line(linetype = "longdash", colour = "gray40"))

GDP3 <- ggplot(data = cd_poluicao, aes(x = Umidade, y = co)) +
  geom_point(shape = 25, fill = "mediumspringgreen", size = 3) +
  scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  scale_y_continuous(breaks = seq(0,14, 2),limits = c(0,14)) + 
  labs(title = "Gráfico de disersão 3: Umidade X Monóxido de Carbono (CO)", x = "Umidade", y = "Monóxido de Carbono (CO)") + 
  theme_classic() +
  theme(panel.grid = element_line(linetype = "dashed", colour = "white"), panel.grid.minor = element_line(linetype = "longdash", colour = "gray40"))

#' 
#' 
## ------------------------------------------------------------------------------
ggarrange(GDP1, GDP2, GDP3,
          ncol = 2, nrow = 2, heights = 2, widths = 15, vjust = -5)

#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
