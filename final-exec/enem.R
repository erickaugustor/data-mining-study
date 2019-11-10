#####################################################################
# For big tables

library(bigmemory)
library(biganalytics)
library(bigtabulate)

# Plots
library(ggplot2)

#####################################################################
# Local file

getwd()
location <- getwd()
setwd(location)

#####################################################################

enemDataset2014  <- read.csv("enem-2014.csv", nrows=100000, header=TRUE, sep=",")

str(enemDataset2014)

# Removendo alunos que não declararam o tipo de escola
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$TP_ESCOLA),]

# Removendo alunos que não tem notas nas provas
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CH),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CN),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_LC),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_MT),]

# Transformando a coluna de Tipo de Escola em Factor
enemDataset2014$TP_ESCOLA <- as.factor(enemDataset2014$TP_ESCOLA)


enemDataset <- enemDataset2014

# Excluindo variaveis
rm(enemDataset2014)


#####################################################################
tipoEscola <- data.frame(table(enemDataset$TP_ESCOLA))

histTipoEscola <- ggplot(tipoEscola, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Tipo de escola",
    y="Quantidade de aluno",
    title="Histograma de Quantidade de aluno por tipo de escola"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip();

#####################################################################

notaEscola <- subset(enemDataset,select = c(
  "TP_ESCOLA",
  "NOTA_CN",
  "NOTA_CH",
  "NOTA_LC",
  "NOTA_MT",
  "NU_NOTA_REDACAO"))

histNota_CH_LC <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = TP_ESCOLA)) +
  ggtitle("Notas de Ciências Humanas e Linguagem e Comunicação")

histNota_MT_CN <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ciências da Natureza e Matemática") 

#####################################################################

histREDACAO <- ggplot(notaEscola, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

lineREDACAO <- ggplot(notaEscola, aes(NU_NOTA_REDACAO)) +
  geom_freqpoly(
    aes(color = TP_ESCOLA, linetype = TP_ESCOLA),
    bins = 30,
    size = 1.5
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################

maioresNotasRedacao <- subset(notaEscola, NU_NOTA_REDACAO > 700)

histMaioresNotasREDACAO <- ggplot(maioresNotasRedacao, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    bins = 30,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################

menoresNotasREDACAO <- subset(notaEscola, NU_NOTA_REDACAO < 600)

histMenoresNotasREDACAO <- ggplot(menoresNotasREDACAO, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    bins = 30,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################




