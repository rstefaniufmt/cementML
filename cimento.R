library(readxl)
library(e1071)

dados_limpos <- read_excel("dados_limpos.xlsx")
View(dados_limpos)

# Fazer com cada um dos modelos melhores (com os tr?s)


sv = tune.svm(`HEALING`~., data = dados_limpos, gamma = c(0.01,0.1,1), cost = 10^(-2:2), 
              type = 'eps-regression', kernel = 'sigmoid', coef0 = c(0, 0.1, 1), 
              tunecontrol=tune.control(cross=10))

summary(sv)
print("Residuais:")
print(sv$best.model$residuals)

#analise dos dados
print("Valores Previstos:")
pred <- predict(sv$best.model, dados_limpos)

summary(sv$best.model$fitted)
summary(pred)
print("R2: ")
print(cor(dados_limpos$HEALING, pred))

rmse <- function(error)
{
  sqrt(mean(error^2))
}


#RMSE ? Root-Mean-Square Error. Quanto menor, melhor - fazer pra cada um dos modelos
erro = rmse(sv$best.model$residuals)

#graficos

#O primeiro gr?fico ? o gr?fico de valores reais vs. previsto
# r2 ? para formatar a correla??o R2 dos valores reais e previstos

#plota o gr?fico
r2 <- sprintf("R2 = %f", cor(pred, dados_limpos$HEALING))
plot(dados_limpos$HEALING, dados_limpos$HEALING, xlab="Valor Real",ylab = "Previsto",
     ylim= c(0.5,1.1), xlim= c(0.5,1.1), pch=16)
points(dados_limpos$HEALING, pred, col = "blue", pch=4)
mtext(r2, side = 3)

#residuais (gr?fico)
#agora o gr?fico dos residuais
plot(pred, sv$best.model$residuals, xlab="Previsto",ylab = "Residuais", 
     xlim = c(0.7,1.1), col= "red", pch=1)

#gravando os dados reais, previstos e residuais
#Se voc? tiver dificuldade ou preferir fazer os gr?ficos eestatisticas no excel, pode gravar a planilha a seguir: 
#a primeira coluna s?o os valores reais, a segunda os previstos e a ultima os residuais.

dadosFinais <- data.frame(Real = dados_limpos$HEALING, Previsto = pred, Residual =  sv$best.model$residuals)
write.table(dadosFinais, file="dadosFinais.csv", sep=";")

