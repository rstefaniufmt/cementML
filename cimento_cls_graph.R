library(readxl)
library(caret)
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(mlbench)        
library(RColorBrewer) # customized coloring of plotslibrary(tidyverse)    
library(corrplot) # data manipulation and visualization
library(gclus)
library(PerformanceAnalytics)

dados_limpos <- read_excel("dados_limpos_cls.xlsx")
str(dados_limpos)

#dividir os dados
intrain <- createDataPartition(y = dados_limpos$HEALING, p= 0.7, list = FALSE)
treino <- dados_limpos[intrain,]
teste <- dados_limpos[-intrain,]
str(treino)
str(teste)

summary(dados_limpos)

#categoriza??o
tm <- treino
treino[["HEALING"]] = factor(treino[["HEALING"]])
summary(treino)
# Fazer com cada um dos modelos melhores (com os tr?s)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#analise de Correlação

correlationMatrix <- cor(tm)
print(correlationMatrix)
corrplot(correlationMatrix)
corrplot(correlationMatrix, method = "number")

corr <- abs(correlationMatrix)
cl <- dmat.color(corr)
order <- order.single(corr)
pairs(dados_limpos,                    # Data frame of variables
      order,                # Order of the variables
      panel.colors = colors,   # Matrix of panel colors
      border.color = "grey70", # Borders color
      gap = 0.45,              # Distance between subplots
      main = "", # Main title
      show.points = TRUE,      # If FALSE, removes all the points
      pch = 21,
      bg = rainbow(3)[dados_limpos$HEALING])

chart.Correlation(dados_limpos, histogram = TRUE, method = "pearson")

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.70)
print(highlyCorrelated)

# prepara para classificacao

model_cor <- train(`HEALING` ~., data=tm, preProcess="scale", trControl=trctrl)
importance = varImp(model_cor, scale=FALSE)
print(importance)
plot(importance)

#prepara treino
 
Argamassa = treino$AGUA +treino$LACTATO  + treino$FATOR + treino$TEMPO +  treino$FISSURA + treino$AGREGADO + treino$AREIA + treino$PLASTIFICANTE
Bacterias = treino$BACTERIA
healing = treino$HEALING

traindata = data.frame(Argamassa, Bacterias, healing) #dados de treino

#prepara teste

d = teste$AGUA + teste$LACTATO + teste$FISSURA + teste$FATOR + teste$TEMPO
e = teste$AGREGADO + teste$AREIA + teste$PLASTIFICANTE + teste$BACTERIA
f = teste$HEALING
testdata = data.frame(d, e, f) #dados de teste

#classifica;'ao
model.svm = ksvm(healing ~ Argamassa + Bacterias, data = treino, type = "C-svc")
plot(model.svm, data = treino)
print(model.svm)

#linear
sv <- svm(healing ~ Argamassa + Bacterias, data = traindata, kernel = "linear", type = "C-classification", scale = TRUE)
plot(sv, traindata)


print(sv)

#analise dos dados

test_pred <- predict(sv, newdata = testdata)
print(test_pred)

confusionMatrix(table(test_pred[1:11], testdata$f))

# ANALISE COM CV

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(HEALING ~., data = treino, method = "svmLinear", trControl=trctrl, preProcess = c("center", "scale"), tuneGrid = grid, tuneLength = 10)
print(svm_Linear_Grid)
print(svm_Linear_Grid$bestTune)
plot(svm_Linear_Grid)

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
plot(svm_Radial_Grid)

test_pred_grid <- predict(svm_Radial_Grid, newdata = teste)
test_pred_grid


print(teste$HEALING)
print(test_pred_grid)
plot(test_pred_grid)
confusionMatrix(table(test_pred_grid, teste$HEALING))

