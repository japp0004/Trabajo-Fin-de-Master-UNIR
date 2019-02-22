# Implementación del Modelo ORIENTAL I a una semana
rm(list =ls()) 
# Carga de datos

load("./Datos/DATASET_ORIENTAL_I_SEMANA_1.RData")

DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C = ifelse(DATASET_ORIENTAL_I_SEMANA_I$OUTPUT>3,2,DATASET_ORIENTAL_I_SEMANA_I$OUTPUT)
DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C = ifelse(DATASET_ORIENTAL_I_SEMANA_I$OUTPUT<=1,0,DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C)
DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C = ifelse((DATASET_ORIENTAL_I_SEMANA_I$OUTPUT>1 & DATASET_ORIENTAL_I_SEMANA_I$OUTPUT<=3) ,1,DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C)



# Eliminación de las variables
# "PARCELA"         "FECHA"           "FECHA_timestamp" "UTMX"            "UTMY"            "SUELO"           "VARIEDAD"       
# "MARCO"           "DIAMETRO_COPA"   "ALTURA_COPA"     "ESTACION"        "OUTPUT_C"        "LONGITUD"        "LATITUD"    
data1<-DATASET_ORIENTAL_I_SEMANA_I[,-c(1,2,3,5,6,8,10,11,14,15,16,108,106,107)]

#Transformación de variables categóricas
library(dummies)
data<-dummy.data.frame(data1,sep=".")


library(xgboost)
# La ultima columna del dataset es la salida (OUT)
data$OUT=data$OUTPUT
data$OUTPUT<-NULL
data$OUTPUT<-NULL

# Preparar los datos para entrenar xgboost
train.x<- as.matrix(data, rownames.force=NA)
train.x <- as(train.x, "sparseMatrix")

# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train.x[,1:(ncol(data)-1)], label = train.x[,"OUT"])

# Elección de parámetros de xgboost
All_rmse<- c()
Param_group<-c()
error_idx=data.frame(error=vector(),nround=vector())

# PARTE 1: ESCOGER LOS PARAMETROS DEL MODELO

for (iter in 1:50) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                booster = "gbtree",
                max_depth = sample(5:15, 1),
                eta = runif(1, 0.01, 0.4),
                gamma = runif(1, 0.0, 0.3),
                subsample = runif(1, 0.5, 0.9),
                colsample_bytree = runif(1, 0.5, 1)
                
  )
  cv.nround = 1000
  cv.nfold = 5
  mdcv <- xgb.cv(data=train_Data, params = param, nthread=6,
                 nfold=5, nrounds=1000,
                 verbose = T,early_stopping_rounds = 60)
  # Least Mean_Test_RMSE as Indicator #
  min_rmse<- min(mdcv$evaluation_log[,test_rmse_mean])
  min_logloss_index = which.min(mdcv$eval$test_rmse_mean)
  error_idx = rbind(error_idx,data.frame(error=min_rmse,nround=min_logloss_index))
  All_rmse<-append(All_rmse,min_rmse)
  Param_group<-append(Param_group,param)
  # Select Param
}

param<-Param_group[((which.min(All_rmse)-1)*8+1):((which.min(All_rmse)-1)*8+8)]
nround<-error_idx$nround[which.min(error_idx$error)]

# param
# $objective
# [1] "reg:linear"
# 
# $eval_metric
# [1] "rmse"
# 
# $booster
# [1] "gbtree"
# 
# $max_depth
# [1] 6
# 
# $eta
# [1] 0.04412901
# 
# $gamma
# [1] 0.2975884
# 
# $subsample
# [1] 0.741792
# 
# $colsample_bytree
# [1] 0.5228301

# nround
# [1] 982

# PARTE 2: BUSCAR LAS VARIABLES MAS IMPORTANTES SEGUN EL MODELO
# Entrenamiento del modelo para escoger las variables más importantes
Training <-
  xgb.train(params = param,
            data = train_Data,
            nrounds = nround,
            watchlist = list(train = train_Data),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 6)

# Variables importantes para el modelo
names <- names(data[,1:(ncol(data)-1)])
importanceMatrix <- xgb.importance(names, 
                                   model = Training)
xgb.plot.importance(importanceMatrix[1:(ncol(data)-1),])

# Escogemos las 25 variables más importantes
features<-importanceMatrix$Feature[1:25]

indice=vector()
n=1
for (k in 1:ncol(data)){
  for (p in 1:length(features)){
    if(names(data)[k]==features[p]){
      indice[n]=k
      n=n+1
    }
  }
}

data_feat=data[,c(indice,ncol(data))]

# PROBAR EL MODELO CON LAS VARIABLES MAS IMPORTANTES ESCOGIDAS ANTERIORMENTE
# Dividimos el conjunto de datos en train y test
year_test <- "2013"
train <- data_feat[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")!=year_test,]
test <- data_feat[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test,]

train.x<- as.matrix(train, rownames.force=NA)
test.x<- as.matrix(test, rownames.force=NA)
train.x <- as(train.x, "sparseMatrix")
test.x <- as(test.x, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train.x[,1:(ncol(data_feat)-1)], label = train.x[,"OUT"])
test_data <- xgb.DMatrix(data = test.x[,1:(ncol(data_feat)-1)])


modelo_1W <-
  xgb.train(params = param,
            data = train_Data,
            nrounds = nround,
            watchlist = list(train = train_Data),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 6)


require(caret)

prediction <- predict(modelo_1W, test_data)


test$pred=prediction
test$fecha=DATASET_ORIENTAL_I_SEMANA_I$FECHA[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test]
test$OUTPUT_C=DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test]

library(plotly)
 p <- plot_ly(test, x = ~fecha, y = ~OUT, name = 'real', type = 'scatter') %>%
   add_trace(y = ~pred, name = 'pred')  %>%
   add_trace(y = rep(1,nrow(test)), name = '1%', mode = 'lines', line=list(dash='dot', width=1))  %>%
   add_trace(y = rep(3,nrow(test)), name = '3%', mode = 'lines', line=list(dash='dot', width=1)) 
 
 ggplotly(p)

test$pred_C = ifelse(test$pred>3,2,test$pred)
test$pred_C = ifelse(test$pred<=1,0,test$pred_C)
test$pred_C = ifelse((test$pred>1 & test$pred<=3) ,1,test$pred_C)

library(caret)
mtab<-table(test$pred_C,test$OUTPUT_C) 

print(confusionMatrix((mtab), mode = "everything")) # RESULTADO CON LAS N VARIABLES MAS IMPORTANTES



# PARTE 3: ESCOGER VARIABLES MANUALMENTE

# Variables finales
n1=which.max(names(data)=="PICADAS_MED_1M_PARCELA")
n2=which.max(names(data)=="ALTITUD")
n3=which.max(names(data)=="HR_MAX_1M")
n4=which.max(names(data)=="IF_MED_1M_PARCELA")
n5=which.max(names(data)=="NMOSCAS_MOSQ_MED_1M_PARCELA")
n6=which.max(names(data)=="NMOSCAS_PLACA_MED_1Q_PARCELA")
n7=which.max(names(data)=="DENSIDAD")
n8=which.max(names(data)=="DIAJUL")
n9=which.max(names(data)=="MOSCA_VIVA_MED_1M_PARCELA")
n10=which.max(names(data)=="PRECIP1Q")
n11=which.max(names(data)=="VV_MED_1M")
n12=which.max(names(data)=="T_MED_1Q")
n13=which.max(names(data)=="TSUP30_1M")
n14=which.max(names(data)=="T_BETW_20_30_1M")
n15=which.max(names(data)=="TINF10_1W")
n16=which.max(names(data)=="NMOSCAS_MOSQ_MED_1M")
n17=which.max(names(data)=="NMOSCAS_PLACA_MED_1M")
n18=which.max(names(data)=="PICADAS_MED_1M")
n19=which.max(names(data)=="IF_MED_1M")
n20=which.max(names(data)=="ORIFICIOS_MED_1M")




no=which.max(names(data)=="OUT")

data_final<-data[,c(n1,n2,n3,n4,n5,n6,n7,n9,n10,n11,n13,n14,n15,n16.n17,n18,n19,n20,no)]


# Dividimos el conjunto de datos en train y test
year_test <- "2013"
train <- data_final[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")!=year_test,]
test <- data_final[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test,]

train.x<- as.matrix(train, rownames.force=NA)
test.x<- as.matrix(test, rownames.force=NA)
train.x <- as(train.x, "sparseMatrix")
test.x <- as(test.x, "sparseMatrix")


# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train.x[,1:(ncol(data_final)-1)], label = train.x[,"OUT"])
test_data <- xgb.DMatrix(data = test.x[,1:(ncol(data_final)-1)])


modelo_1W <-
  xgb.train(params = param,
            data = train_Data,
            nrounds = nround,
            watchlist = list(train = train_Data),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 6)


require(caret)

prediction <- predict(modelo_1W, test_data)


test$pred=prediction
test$fecha=DATASET_ORIENTAL_I_SEMANA_I$FECHA[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test]
test$OUTPUT_C=DATASET_ORIENTAL_I_SEMANA_I$OUTPUT_C[format(DATASET_ORIENTAL_I_SEMANA_I$FECHA, "%Y")==year_test]

test$pred_C = ifelse(test$pred>3,2,test$pred)
test$pred_C = ifelse(test$pred<=1,0,test$pred_C)
test$pred_C = ifelse((test$pred>1 & test$pred<=3) ,1,test$pred_C)

library(caret)
mtab<-table(test$pred_C,test$OUTPUT_C) 

print(confusionMatrix((mtab), mode = "everything")) # RESULTADO ESCOGIENDO VARIABLES "A MANO"

