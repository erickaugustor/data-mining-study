# PCA
# An�lise explorat�ria de dados.

# Classifica��o, problema para verificar se os dados est�o correlatos.

# O componente principal � uma vari�ncia de v�rias


# O dataset ser� transformado, os dados ser�o mudados em rela��o as coisas

dataset <- mtcars

dataset.pca <- prcomp(dataset[,c(1:7, 10, 11)], center = TRUE, scale = TRUE)
# Calculando o valor da PCA, indicando as colunas de 1 a 7, coluna 10 e 11.
# center e scale, o scale diz respeito a padroniza��o dos valores
# � preciso normalizar os dados pois o PCA � sensivel ao valor da escala

summary(dataset.pca)
# Sumariza��o dele mostra que existem 9 componentes principais, 9 colunas!
# As duas ultimas linhas mostra que o PC1 mostra que 62% do dataset existe uma classifica��o
# O PC2 tem 23%, que somado ao anterior chega com 85% de considera��o

# O PC3 e adiante contribuem menos com o role, pois eles est�o dispersos.