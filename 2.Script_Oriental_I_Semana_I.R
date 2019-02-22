## Script para generear el Data Frame para oriental I 1 semana

## Packages
install.packages("lubridate")
install.packages("xray")
library(lubridate)
library(xray)
## carga de datos
load("./Datos/Datos.RData")

## Carga de funciones
load("./Funciones/UTM2LatLong.RData")

#---------- FILTRADO DE DATOS ----------
## Filtrado de datos de campaña oriental I
D_PARCELAS_oriental_I<-subset(D_PARCELAS,DATO56=="CO/OL/08 CAMPIÑA ALTA ORIENTAL I")

#### Filtrado T_muestreo (Obtenemos el T_muestreo de )
T_MUESTREOS_CAMP_I=data.frame()
for (parcela in unique(D_PARCELAS_oriental_I$CODIGOPT)){
  muestreo = subset(T_MUESTREOS,CODIGOPT==parcela)
  T_MUESTREOS_CAMP_I=rbind(T_MUESTREOS_CAMP_I,muestreo)
} 

#### filtrado d_opc (Seleccionamos campiña oriental I)
D_OPC_CAMP_I=data.frame()
for (parcela in unique(D_PARCELAS_oriental_I$CODIGOPT)){
  muestreo = subset(D_OPC,CODIGOPT==parcela)
  D_OPC_CAMP_I=rbind(D_OPC_CAMP_I,muestreo)
}

T_MUESTREOS_CAMP_I$Fecha<-as.Date(T_MUESTREOS_CAMP_I$DIAJUL-1, origin = paste(T_MUESTREOS_CAMP_I$ANIO,"-01-01",sep=""))
T_MUESTREOS_CAMP_I$timestamp<-as.numeric(as.POSIXlt(T_MUESTREOS_CAMP_I$Fecha, format = "%Y-%m-%d"))

#######   seleccionamos mosca (bactrocera oleae)
TRAT_oriental_I=subset(D_OPC_CAMP_I,DATO9=="Mosca ( Bactrocera oleae )")
datos_climaticos_oriental_I$timestamp <- as.numeric(as.POSIXlt(as.character(datos_climaticos_oriental_I$FECHA), format = "%d-%m-%y"))

# Check days within meteo stations
print("datos_climaticos_oriental_I")
for (year in 2006:2017){
  t1=as.numeric(as.POSIXlt(paste("01/01/",as.character(year),sep=""), format = "%d/%m/%Y "))
  t2=as.numeric(as.POSIXlt(paste("31/12/",as.character(year),sep=""), format = "%d/%m/%Y "))
  print(paste(year,"->",nrow(datos_climaticos_oriental_I[datos_climaticos_oriental_I$timestamp>=t1 & datos_climaticos_oriental_I$timestamp<=t2,]),"days"))
}

T_MUESTREOS_CAMP_I$timestamp<-T_MUESTREOS_CAMP_I$timestamp-7200 #(7200 = 2h)

cont=0
DATASET_ORIENTAL_I_SEMANA_I<- data.frame()
for(parcela in unique(D_PARCELAS_oriental_I$CODIGOPT)){
  cont=cont+1;
  # Variables de parcela (fijas)
  UTMX=as.numeric(gsub(",",".",D_PARCELAS_oriental_I$DATO11[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]))
  UTMY=as.numeric(gsub(",",".",D_PARCELAS_oriental_I$DATO37[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]))
  ALTITUD=as.numeric(D_PARCELAS_oriental_I$DATO12[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  SUELO=D_PARCELAS_oriental_I$DATO14[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]
  RIEGO=D_PARCELAS_oriental_I$DATO15[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]
  VARIEDAD=D_PARCELAS_oriental_I$DATO18[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]
  MARCO=D_PARCELAS_oriental_I$DATO22[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]
  MARCO=gsub("X","x",MARCO)
  MARCO=gsub("tresbolillo","",MARCO)
  MARCO=gsub("TRESBOLILLO","",MARCO)
  DENSIDAD=as.numeric(D_PARCELAS_oriental_I$DATO29[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  DIAMETRO_COPA=as.numeric(D_PARCELAS_oriental_I$DATO47[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  ALTURA_COPA=as.numeric(D_PARCELAS_oriental_I$DATO57[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  ESTACION=D_PARCELAS_oriental_I$DATO25[D_PARCELAS_oriental_I$CODIGOPT==parcela][1]
  
  T_SUB_MUESTREO_PARCELA=subset(T_MUESTREOS_CAMP_I,(CODIGOPT==parcela & !is.na(DATO5)))
  for (k in 1:nrow(T_SUB_MUESTREO_PARCELA)){
    YEAR = T_SUB_MUESTREO_PARCELA$ANIO[k]
    DIAJUL = T_SUB_MUESTREO_PARCELA$DIAJUL[k]
    fecha_timestamp=T_SUB_MUESTREO_PARCELA$timestamp[k]
    fecha_timestamp2 = fecha_timestamp-7*24*3600
    DIA1ENERO = as.numeric(as.POSIXlt(paste("01/01/",as.character(YEAR),sep=""), format = "%d/%m/%Y"))
    INICIO_INVIERNO = as.numeric(as.POSIXlt(paste("21/12/",as.character(YEAR-1),sep=""), format = "%d/%m/%Y"))
    FIN_INVIERNO = as.numeric(as.POSIXlt(paste("20/03/",as.character(YEAR),sep=""), format = "%d/%m/%Y"))
    
    #Variables climáticas
    TEMP_ACUM = datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp>=DIA1ENERO & datos_climaticos_oriental_I$timestamp<fecha_timestamp2]
    GRADOS_DIA = sum(TEMP_ACUM[TEMP_ACUM>=9])
    T_MED_1W = mean(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    T_MED_1Q= mean(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    T_MED_1M = mean(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    T_MAX_1W = max(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    T_MAX_1Q= max(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    T_MAX_1M = max(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    T_MIN_1W = min(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    T_MIN_1Q = min(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    T_MIN_1M = min(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    TSUP32_1W = sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]>=32)
    TSUP32_1Q = sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]>=32)
    TSUP32_1M= sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]>=32)
    TSUP30_1W = sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]>=30)
    TSUP30_1Q = sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]>=30)
    TSUP30_1M = sum(datos_climaticos_oriental_I$T_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]>=30)
    TINF10_1W = sum(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]<=10)
    TINF10_1Q = sum(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]<=10)
    TINF10_1M = sum(datos_climaticos_oriental_I$T_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]<=10)
    T_BETW_20_30_1W = sum(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]<=30 &
                            datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]>=20)
    T_BETW_20_30_1Q = sum(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]<=30 &
                            datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]>=20)
    T_BETW_20_30_1M = sum(datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]<=30 &
                            datos_climaticos_oriental_I$T_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]>=20)
    
    HR_MED_1W = mean(datos_climaticos_oriental_I$H_R_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    HR_MED_1Q = mean(datos_climaticos_oriental_I$H_R_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    HR_MED_1M = mean(datos_climaticos_oriental_I$H_R_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    HR_MAX_1W = max(datos_climaticos_oriental_I$H_R_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    HR_MAX_1Q = max(datos_climaticos_oriental_I$H_R_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    HR_MAX_1M = max(datos_climaticos_oriental_I$H_R_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    HR_MIN_1W= min(datos_climaticos_oriental_I$H_R_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    HR_MIN_1Q = min(datos_climaticos_oriental_I$H_R_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    HR_MIN_1M = min(datos_climaticos_oriental_I$H_R_MIN[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    RANGOHR_1W = HR_MAX_1W-HR_MIN_1W
    RANGOHR_1Q = HR_MAX_1Q-HR_MIN_1Q
    RANGOHR_1M = HR_MAX_1M-HR_MIN_1M
    
    PRECIP1W=sum(datos_climaticos_oriental_I$LLUVIA[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)],na.rm=T)
    PRECIP1Q=sum(datos_climaticos_oriental_I$LLUVIA[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)],na.rm=T)
    PRECIP1M=sum(datos_climaticos_oriental_I$LLUVIA[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)],na.rm=T)
    PRECIP_INVIERNO = sum(datos_climaticos_oriental_I$LLUVIA[datos_climaticos_oriental_I$timestamp<=FIN_INVIERNO & datos_climaticos_oriental_I$timestamp>=INICIO_INVIERNO],na.rm=T)
    
    VV_MED_1W = mean(as.numeric(datos_climaticos_oriental_I$V_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]),na.rm=T)
    VV_MED_1Q = mean(as.numeric(datos_climaticos_oriental_I$V_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]),na.rm=T)
    VV_MED_1M = mean(as.numeric(datos_climaticos_oriental_I$V_MED[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]),na.rm=T)
    #VV_MAX_1W = max(as.numeric(datos_climaticos_oriental_I$V_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]),na.rm=T)
    #VV_MAX_1Q = max(as.numeric(datos_climaticos_oriental_I$V_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]),na.rm=T)
    #VV_MAX_1M = max(as.numeric(datos_climaticos_oriental_I$V_MAX[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]),na.rm=T)
    
    RAD_MED_1W = mean(as.numeric(datos_climaticos_oriental_I$RADIACION[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-7*24*3600)]),na.rm=T)
    RAD_MED_1Q = mean(as.numeric(datos_climaticos_oriental_I$RADIACION[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-15*24*3600)]),na.rm=T)
    RAD_MED_1M = mean(as.numeric(datos_climaticos_oriental_I$RADIACION[datos_climaticos_oriental_I$timestamp<fecha_timestamp2 & datos_climaticos_oriental_I$timestamp>=(fecha_timestamp2-30*24*3600)]),na.rm=T)
    
    # Variables de plaga en la zona biológica
    
    PICADAS_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    PICADAS_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    PICADAS_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    
    NMOSCAS_MOSQ_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    NMOSCAS_MOSQ_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    NMOSCAS_MOSQ_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IF_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IF_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IF_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IR_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IR_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    IR_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    NMOSCAS_PLACA_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    NMOSCAS_PLACA_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    NMOSCAS_PLACA_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    MOSCA_VIVA_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    MOSCA_VIVA_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    MOSCA_VIVA_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    ORIFICIOS_MED_1W = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    ORIFICIOS_MED_1Q = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    ORIFICIOS_MED_1M = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2],na.rm=T)
    
    # Variables de plaga
    
    PICADAS_MED_1w_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I $timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    PICADAS_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I $timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    PICADAS_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO5[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I $timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    NMOSCAS_MOSQ_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    NMOSCAS_MOSQ_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    NMOSCAS_MOSQ_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO1[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    IF_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    IF_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    IF_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO2[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    IR_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela ],na.rm=T)
    IR_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    IR_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO3[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    NMOSCAS_PLACA_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    NMOSCAS_PLACA_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    NMOSCAS_PLACA_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO4[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela ],na.rm=T)
    
    MOSCA_VIVA_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    MOSCA_VIVA_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    MOSCA_VIVA_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO6[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    ORIFICIOS_MED_1W_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-7*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    ORIFICIOS_MED_1Q_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-15*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    ORIFICIOS_MED_1M_PARCELA = mean(T_MUESTREOS_CAMP_I$DATO7[T_MUESTREOS_CAMP_I$timestamp>=(fecha_timestamp2-30*24*3600) & T_MUESTREOS_CAMP_I$timestamp<fecha_timestamp2 & T_MUESTREOS_CAMP_I$CODIGOPT==parcela],na.rm=T)
    
    
    
    # Tratamientos 
    TRAT = subset(TRAT_oriental_I,(CODIGOPT==parcela & ANIO ==YEAR))
    if(nrow(TRAT)>0){
      DIAS_ATRAS_TRAT=yday(as.POSIXct(fecha_timestamp2,origin="1970-01-01"))-TRAT$DIAJUL
      TRATAMIENTOS=sum(DIAS_ATRAS_TRAT>0)
    }else TRATAMIENTOS=0
    
    # Variable de salida
    OUTPUT = T_SUB_MUESTREO_PARCELA$DATO5[k]
    
    # Creación del dataset
    DATASET_PARCELA = data.frame(
      PARCELA = parcela,
      FECHA =  as.POSIXct(fecha_timestamp, origin="1970-01-01"),
      FECHA_timestamp = fecha_timestamp,
      DIAJUL = DIAJUL,
      UTMX=UTMX,
      UTMY=UTMY,
      ALTITUD=ALTITUD,
      SUELO=SUELO,
      RIEGO=RIEGO,
      VARIEDAD=VARIEDAD,
      MARCO=MARCO,
      MARCO_M2=eval(parse(text=gsub("x","*",MARCO))),
      DENSIDAD=DENSIDAD,
      DIAMETRO_COPA=DIAMETRO_COPA,
      ALTURA_COPA=ALTURA_COPA,
      ESTACION=ESTACION,
      GRADOS_DIA = GRADOS_DIA,
      T_MED_1W = T_MED_1W,
      T_MED_1Q = T_MED_1Q,
      T_MED_1M = T_MED_1M,
      T_MAX_1W = T_MAX_1W,
      T_MAX_1Q = T_MAX_1Q,
      T_MAX_1M = T_MAX_1M,
      T_MIN_1W = T_MIN_1W,
      T_MIN_1Q = T_MIN_1Q,
      T_MIN_1M = T_MIN_1M,
      TSUP32_1W = TSUP32_1W,
      TSUP32_1Q = TSUP32_1Q,
      TSUP32_1M = TSUP32_1M,
      TSUP30_1W = TSUP30_1W,
      TSUP30_1Q = TSUP30_1Q,
      TSUP30_1M = TSUP30_1M,
      TINF10_1W = TINF10_1W,
      TINF10_1Q = TINF10_1Q,
      TINF10_1M = TINF10_1M,
      T_BETW_20_30_1W = T_BETW_20_30_1W,
      T_BETW_20_30_1Q = T_BETW_20_30_1Q,
      T_BETW_20_30_1M = T_BETW_20_30_1M,
      HR_MED_1W = HR_MED_1W,
      HR_MED_1Q = HR_MED_1Q,
      HR_MED_1M = HR_MED_1M,
      HR_MAX_1W = HR_MAX_1W,
      HR_MAX_1Q = HR_MAX_1Q,
      HR_MAX_1M = HR_MAX_1M,
      HR_MIN_1W = HR_MIN_1W,
      HR_MIN_1Q = HR_MIN_1Q,
      HR_MIN_1M = HR_MIN_1M,
      RANGOHR_1W = RANGOHR_1W,
      RANGOHR_1Q = RANGOHR_1Q,
      RANGOHR_1M = RANGOHR_1M,
      PRECIP1W = PRECIP1W,
      PRECIP1Q = PRECIP1Q,
      PRECIP1M = PRECIP1M,
      PRECIP_INVIERNO = PRECIP_INVIERNO,
      VV_MED_1W = VV_MED_1W,
      VV_MED_1Q = VV_MED_1Q,
      VV_MED_1M = VV_MED_1M,
      #VV_MAX_1W = VV_MAX_1W,
      #VV_MAX_1Q = VV_MAX_1Q,
      #VV_MAX_1M = VV_MAX_1M,
      RAD_MED_1W = RAD_MED_1W,
      RAD_MED_1Q = RAD_MED_1Q,
      RAD_MED_1M = RAD_MED_1M,
      PICADAS_MED_1W = PICADAS_MED_1W,
      PICADAS_MED_1Q = PICADAS_MED_1Q,
      PICADAS_MED_1M = PICADAS_MED_1M,
      PICADAS_MED_1W_PARCELA = PICADAS_MED_1w_PARCELA,
      PICADAS_MED_1Q_PARCELA = PICADAS_MED_1Q_PARCELA,
      PICADAS_MED_1M_PARCELA = PICADAS_MED_1M_PARCELA,
      NMOSCAS_MOSQ_MED_1W_PARCELA = NMOSCAS_MOSQ_MED_1W_PARCELA,
      NMOSCAS_MOSQ_MED_1Q_PARCELA = NMOSCAS_MOSQ_MED_1Q_PARCELA,
      NMOSCAS_MOSQ_MED_1M_PARCELA = NMOSCAS_MOSQ_MED_1M_PARCELA,
      IF_MED_1W_PARCELA = IF_MED_1W_PARCELA,
      IF_MED_1Q_PARCELA = IF_MED_1Q_PARCELA,
      IF_MED_1M_PARCELA = IF_MED_1M_PARCELA,
      IR_MED_1W_PARCELA = IR_MED_1W_PARCELA,
      IR_MED_1Q_PARCELA = IR_MED_1Q_PARCELA,
      IR_MED_1M_PARCELA = IR_MED_1M_PARCELA,
      NMOSCAS_PLACA_MED_1W_PARCELA = NMOSCAS_PLACA_MED_1W_PARCELA,
      NMOSCAS_PLACA_MED_1Q_PARCELA = NMOSCAS_PLACA_MED_1Q_PARCELA,
      NMOSCAS_PLACA_MED_1M_PARCELA = NMOSCAS_PLACA_MED_1M_PARCELA,
      MOSCA_VIVA_MED_1W_PARCELA = MOSCA_VIVA_MED_1W_PARCELA,
      MOSCA_VIVA_MED_1Q_PARCELA = MOSCA_VIVA_MED_1Q_PARCELA,
      MOSCA_VIVA_MED_1M_PARCELA = MOSCA_VIVA_MED_1M_PARCELA,
      ORIFICIOS_MED_1W_PARCELA = ORIFICIOS_MED_1W_PARCELA,
      ORIFICIOS_MED_1Q_PARCELA = ORIFICIOS_MED_1Q_PARCELA,
      ORIFICIOS_MED_1M_PARCELA = ORIFICIOS_MED_1M_PARCELA,
      
      NMOSCAS_MOSQ_MED_1W = NMOSCAS_MOSQ_MED_1W,
      NMOSCAS_MOSQ_MED_1Q = NMOSCAS_MOSQ_MED_1Q,
      NMOSCAS_MOSQ_MED_1M = NMOSCAS_MOSQ_MED_1M,
      IF_MED_1W = IF_MED_1W,
      IF_MED_1Q = IF_MED_1Q,
      IF_MED_1M = IF_MED_1M,
      IR_MED_1W = IR_MED_1W,
      IR_MED_1Q = IR_MED_1Q,
      IR_MED_1M = IR_MED_1M,
      NMOSCAS_PLACA_MED_1W = NMOSCAS_PLACA_MED_1W,
      NMOSCAS_PLACA_MED_1Q = NMOSCAS_PLACA_MED_1Q,
      NMOSCAS_PLACA_MED_1M = NMOSCAS_PLACA_MED_1M,
      MOSCA_VIVA_MED_1W = MOSCA_VIVA_MED_1W,
      MOSCA_VIVA_MED_1Q = MOSCA_VIVA_MED_1Q,
      MOSCA_VIVA_MED_1M = MOSCA_VIVA_MED_1M,
      ORIFICIOS_MED_1W = ORIFICIOS_MED_1W,
      ORIFICIOS_MED_1Q = ORIFICIOS_MED_1Q,
      ORIFICIOS_MED_1M = ORIFICIOS_MED_1M,
      TRATAMIENTOS=TRATAMIENTOS,
      OUTPUT = OUTPUT)
    
    DATASET_ORIENTAL_I_SEMANA_I = rbind(DATASET_ORIENTAL_I_SEMANA_I,DATASET_PARCELA)
  }
  print(paste("---- PROCESSED ",round(cont/length(unique(D_PARCELAS_oriental_I$CODIGOPT))*100),"% ------"))
}

#--- Insertamos la altitud que se encuentra vacía
df_parcelas=data.frame(CODPARCELA=vector(),UTMX=vector(),UTMY=vector(),Latitud=vector(),Longitud=vector(),Altitud=vector())
for (parcela in unique(D_PARCELAS_oriental_I$CODIGOPT)){
  x=as.numeric(D_PARCELAS_oriental_I$DATO11[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  y=as.numeric(D_PARCELAS_oriental_I$DATO37[D_PARCELAS_oriental_I$CODIGOPT==parcela][1])
  lst=UTM2LatLong(x,y)
  df_parcelas<-rbind(df_parcelas,data.frame(CODPARCELA=parcela,UTMX=x,UTMY=y,Latitud=lst$latitude,Longitud=lst$longitude,Altitud=lst$elevation))
}

for(K in 1:nrow(DATASET_ORIENTAL_I_SEMANA_I)){
  DATASET_ORIENTAL_I_SEMANA_I$ELEVATION[K]=df_parcelas$Altitud[df_parcelas$CODPARCELA==DATASET_ORIENTAL_I_SEMANA_I$PARCELA[K]]
  DATASET_ORIENTAL_I_SEMANA_I$LATITUD[K]=df_parcelas$Latitud[df_parcelas$CODPARCELA==DATASET_ORIENTAL_I_SEMANA_I$PARCELA[K]]
  DATASET_ORIENTAL_I_SEMANA_I$LONGITUD[K]=df_parcelas$Longitud[df_parcelas$CODPARCELA==DATASET_ORIENTAL_I_SEMANA_I$PARCELA[K]]
}

for (k in 1:nrow(DATASET_ORIENTAL_I_SEMANA_I)){
  if(is.na(DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[k])){
    x=as.numeric(DATASET_ORIENTAL_I_SEMANA_I$UTMX[k])
    y=as.numeric(DATASET_ORIENTAL_I_SEMANA_I$UTMY[k])
    lst=UTM2LatLong(x,y)
    DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[k]=lst$elevation
  }
}

########---------------------------
DATASET_ORIENTAL_I_SEMANA_I$RIEGO[DATASET_ORIENTAL_I_SEMANA_I$RIEGO=="Secano"]="SECANO"
DATASET_ORIENTAL_I_SEMANA_I$RIEGO[DATASET_ORIENTAL_I_SEMANA_I$RIEGO=="Regadío"]="REGADÍO"
DATASET_ORIENTAL_I_SEMANA_I$RIEGO[is.na(DATASET_ORIENTAL_I_SEMANA_I$RIEGO)]="SECANO"

DATASET_ORIENTAL_I_SEMANA_I$SUELO[is.na(DATASET_ORIENTAL_I_SEMANA_I$SUELO)]="FRANCO"
DATASET_ORIENTAL_I_SEMANA_I$SUELO[DATASET_ORIENTAL_I_SEMANA_I$SUELO=="Franco"]="FRANCO"
DATASET_ORIENTAL_I_SEMANA_I$SUELO[DATASET_ORIENTAL_I_SEMANA_I$SUELO=="SIN DEFINIR"]="FRANCO"

DATASET_ORIENTAL_I_SEMANA_I$DENSIDAD[is.na(DATASET_ORIENTAL_I_SEMANA_I$DENSIDAD)]=100

DATASET_ORIENTAL_I_SEMANA_I$VARIEDAD[DATASET_ORIENTAL_I_SEMANA_I$VARIEDAD=="Picudo"]="PICUDO"
DATASET_ORIENTAL_I_SEMANA_I$VARIEDAD[is.na(DATASET_ORIENTAL_I_SEMANA_I$VARIEDAD)]="Picual, Marteño"

DATASET_ORIENTAL_I_SEMANA_I$MARCO[DATASET_ORIENTAL_I_SEMANA_I$MARCO_M2==100]="10x10"
DATASET_ORIENTAL_I_SEMANA_I$MARCO[is.na(DATASET_ORIENTAL_I_SEMANA_I$MARCO)]="10x10"

DATASET_ORIENTAL_I_SEMANA_I$ESTACION="BAENA"

#DATASET_ORIENTAL_I_SEMANA_I$MARCO=DATASET_ORIENTAL_I_SEMANA_3$MARCO
#DATASET_ORIENTAL_I_SEMANA_I$MARCO_M2=DATASET_ORIENTAL_I_SEMANA_3$MARCO_M2
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-007-038-00009-01"]=370
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-900-048-00024-01"]=231
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-007-009-00006-00-00"]=325
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-019-031-00168-01-00"]=306
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-007-003-00046-10"]=343
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-900-048-00024-01"]=231
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-063-004-00107-02-00"]=298
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-046-003-00057-00-00"]=265.6117
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-039-017-00009-01"]=426
DATASET_ORIENTAL_I_SEMANA_I$ALTITUD[DATASET_ORIENTAL_I_SEMANA_I$PARCELA=="14-007-003-00021-15-00"]=335

######## sustitución de valores vacios
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W  			 = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q   			 = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q)                      
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M              = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W         = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q         = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M         = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M                   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W        = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q        = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M        = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W            = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q            = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M            = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M)            
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W_PARCELA      = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q_PARCELA      = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M_PARCELA      = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$PICADAS_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W_PARCELA = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q_PARCELA = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M_PARCELA = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_MOSQ_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IF_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M_PARCELA           = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$IR_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W_PARCELA= ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q_PARCELA= ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M_PARCELA= ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$NMOSCAS_PLACA_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W_PARCELA   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q_PARCELA   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M_PARCELA   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$MOSCA_VIVA_MED_1M_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W_PARCELA    = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1W_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q_PARCELA    = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1Q_PARCELA)            
DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M_PARCELA   = ifelse(is.na(DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M_PARCELA),0,DATASET_ORIENTAL_I_SEMANA_I$ORIFICIOS_MED_1M_PARCELA)      


xray::anomalies(DATASET_ORIENTAL_I_SEMANA_I)
sapply(DATASET_ORIENTAL_I_SEMANA_I,function(x) sum(is.na(x)))

#DATASET_ORIENTAL_I_SEMANA_I$ALTITUD<-DATOS_I$ALTITUD
#DATASET_ORIENTAL_I_SEMANA_I$UTMX<-DATOS_I$UTMX
#DATASET_ORIENTAL_I_SEMANA_I$UTMY<-DATOS_I$UTMY
#DATASET_ORIENTAL_I_SEMANA_I$SUELO<-DATOS_I$SUELO
#DATASET_ORIENTAL_I_SEMANA_I$RIEGO<-DATOS_I$RIEGO
#DATASET_ORIENTAL_I_SEMANA_I$VARIEDAD<-DATOS_I$VARIEDAD
#DATASET_ORIENTAL_I_SEMANA_I$MARCO<-DATOS_I$MARCO
#DATASET_ORIENTAL_I_SEMANA_I$MARCO_M2<-DATOS_I$MARCO_M2
#DATASET_ORIENTAL_I_SEMANA_I$DENSIDAD<-DATOS_I$DENSIDAD
#DATASET_ORIENTAL_I_SEMANA_I$DIAMETRO_COPA<-DATOS_I$DIAMETRO_COPA
#DATASET_ORIENTAL_I_SEMANA_I$ALTURA_COPA<-DATOS_I$ALTURA_COPA
#DATASET_ORIENTAL_I_SEMANA_I$ESTACION<-DATOS_I$ESTACION
#DATASET_ORIENTAL_I_SEMANA_I$LONGITUD<-DATOS_I$LONGITUD
#DATASET_ORIENTAL_I_SEMANA_I$LATITUD<-DATOS_I$LATITUD
#DATASET_ORIENTAL_I_SEMANA_I$ELEVATION<-DATOS_I$ELEVATION









