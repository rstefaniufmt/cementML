library(readxl)
library(caret)

library(DALEX)

dados_limpos <- read_excel("dados_limpos_cls.xlsx")
str(dados_limpos)

dados_limpos$HEALING[dados_limpos$HEALING == 1] <- "SIM"
dados_limpos$HEALING[dados_limpos$HEALING == 0] <- "NAO"
 
#dividir os dados
intrain <- createDataPartition(y = dados_limpos$HEALING, p= 0.7, list = FALSE)
treino <- dados_limpos[intrain,]
teste <- dados_limpos[-intrain,]
str(treino)
str(teste)

summary(dados_limpos)

#categoriza??o
treino[["HEALING"]] = factor(treino[["HEALING"]])
teste[["HEALING"]] = factor(teste[["HEALING"]])

# Verificando a distribuição dos dados originais e das partições
prop.table(table(teste$HEALING)) * 100
prop.table(table(treino$HEALING)) * 100
summary(treino)

# Correlação entre as variáveis preditoras
descrCor <- cor(treino[,names(treino) != "HEALING"])
descrCor


# Função de Normalização
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}

# Removendo a variável target dos dados de treino e teste
numeric.vars_treino <- colnames(treinoX <- treino[,names(treino) != "HEALING"])
numeric.vars_teste <- colnames(testeX <- teste[,names(teste) != "HEALING"])

# Aplicando normalização às variáveis preditoras de treino e teste
treino_scaled <- scale.features(treino, numeric.vars_treino)
teste_scaled <- scale.features(teste, numeric.vars_teste)


# Fazer com cada um dos modelos melhores (com os tr?s)
trctrl <- trainControl(method = "repeatedcv",  repeats = 3)

sv <- train(HEALING ~., data = treino_scaled, method = "knn", trControl=trctrl,
            tuneLength = 10)




print(sv)


#analise dos dados

test_pred <- predict(sv, newdata = teste_scaled)
print(test_pred)
plot(test_pred)

confusionMatrix(table(test_pred, teste$HEALING))

# Arquivo de controle
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# Treinamento do modelo


knn_v2 <- train(HEALING ~ ., 
                data = treino_scaled, 
                method = "knn", 
                trControl = ctrl, 
                metric = "ROC",
               tuneLength = 20)

# Modelo KNN
knn_v2

# Número de Vizinhos x Acurácia
plot(knn_v2, print.thres = 0.5, type="S")

# Fazendo previsões
knnPredict <- predict(knn_v2, newdata = teste_scaled)

plot(knnPredict)
# Criando a Confusion Matrix
confusionMatrix(knnPredict, teste$HEALING)



# ANALISE COM GRAFICO

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(HEALING ~., data = treino, method = "svmLinear", trControl=trctrl, preProcess = c("center", "scale"), tuneGrid = grid, tuneLength = 10)
print(svm_Linear_Grid)
print(svm_Linear_Grid$bestTune)
plot(svm_Linear_Grid)
plot(svm_Linear_Grid$finalModel, margin =0.2)

test_pred_grid <- predict(svm_Linear_Grid, newdata = teste)
print(teste$HEALING)
print(test_pred_grid)
plot(test_pred_grid)
confusionMatrix(table(test_pred_grid, teste$HEALING))


#radial

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Radial_Grid <- train(HEALING ~., data = treino, method = "svmRadial", trControl=trctrl, preProcess = c("center", "scale") , tuneLength = 10)
print(svm_Radial_Grid)
print(svm_Radial_Grid$bestTune)


test_pred_grid <- predict(svm_Radial_Grid, newdata = teste)
print(teste$HEALING)
print(test_pred_grid)
plot(test_pred_grid)
confusionMatrix(table(test_pred_grid, teste$HEALING))



#gravando os dados reais, previstos e residuais
#Se voc? tiver dificuldade ou preferir fazer os gr?ficos eestatisticas no excel, pode gravar a planilha a seguir: 
#a primeira coluna s?o os valores reais, a segunda os previstos e a ultima os residuais.

#dadosFinais <- data.frame(Real = dados_limpos$HEALING, Previsto = pred, Residual =  sv$best.model$residuals)
#write.table(dadosFinais, file="dadosFinais.csv", sep=";")

