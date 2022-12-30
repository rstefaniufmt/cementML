library(readxl)
library(caret)
library(e1071)

dados_limpos <- read_excel("dados_limpos_cls.xlsx")

summary(dados_limpos)
dados_limpos$HEALING <- as.factor(dados_limpos$HEALING)

fit = svm(`HEALING`~., data = dados_limpos, scale = FALSE, type = "C-classification", kernel = "linear", cost = 5)


plot(fit, formula = `HEALING`~., dados_limpos, margin = 0.2)

summary(fit)


#analise dos dados
#analise dos dados
print("Valores Previstos:")
pred <- predict(fit, dados_limpos)
summary(sv$fitted)
summary(pred)
print("R2: ")
print(cor(dados_limpos$HEALING, pred))
print(pred)
plot(pred)

#gravando os dados reais, previstos e residuais
#Se voc? tiver dificuldade ou preferir fazer os gr?ficos eestatisticas no excel, pode gravar a planilha a seguir: 
#a primeira coluna s?o os valores reais, a segunda os previstos e a ultima os residuais.

dadosFinais <- data.frame(Real = dados_limpos$HEALING, Previsto = pred, Residual =  sv$best.model$residuals)
write.table(dadosFinais, file="dadosFinais.csv", sep=";")

