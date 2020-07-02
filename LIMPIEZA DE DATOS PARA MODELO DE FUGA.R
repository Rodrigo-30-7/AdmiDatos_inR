
#########################################################################################
#########################################################################################
####################### LIMPIEZA DE DATOS PARA MODELO DE FUGA ###########################
#########################################################################################
#########################################################################################
{
library(data.table)
  
setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Plan Vital - Fuga/Ultimos modelos")
  
clientes = fread("CLIENTES.csv", sep=",")
saldos = fread("SALDOS.csv", sep=",")
reclamos = fread("RECLAMOS.csv", sep=",")
desafiliados = fread("DESAFILIADOS.csv", sep=",")
  
View(clientes)


#############################  
#### BORRAR FILAS VACÍAS ####
#############################
  
# clientes #
dim(clientes)
clientes <- subset(clientes, region != "")
dim(clientes)
clientes <- subset(clientes, cuenta_obligatoria != "")
dim(clientes)
clientes <- subset(clientes, usa_web != "")
dim(clientes)
clientes <- subset(clientes, visit_web != "")
dim(clientes)
clientes <- subset(clientes, visit_sucursal != "")
dim(clientes)
clientes <- subset(clientes, usa_app != "")
dim(clientes)
clientes <- subset(clientes, empleador_paga != "")
dim(clientes)
clientes <- subset(clientes, antiguedad != "")
dim(clientes)
clientes <- subset(clientes, genero != "")
dim(clientes)
clientes <- subset(clientes, edad != "")
dim(clientes)
clientes <- subset(clientes, id_persona != "")
dim(clientes)

# saldos #
saldos <- subset(saldos, tipo_producto != "")
dim(saldos)
saldos <- subset(saldos, tipo_fondo != "")
dim(saldos)
saldos <- subset(saldos, val_cuo_saldo != "")
dim(saldos)
saldos <- subset(saldos, id_persona != "")
dim(saldos)

# reclamos #
reclamos <- subset(reclamos, tipo_reclamo != "")
dim(reclamos)
reclamos <- subset(reclamos, id_persona != "")
dim(reclamos)


head(clientes)
head(saldos)
head(reclamos)
head(desafiliados)




###########################
#### QUITAR DUPLICADOS ####
###########################

# reclamos #
reclamos <- reclamos[!duplicated(id_persona),]
dim(reclamos)


# saldos #
saldos <- saldos[!duplicated(id_persona),]
dim(saldos)


###############################
#### DICOTOMIZAR VARIABLES ####
###############################

# region #
clientes$region[clientes$region == "REGION METROPOLITANA"] <- "0"
clientes$region[clientes$region != "0"] <- "1"

# genero #
clientes$genero[clientes$genero == "FEMENINO"] <- "0"
clientes$genero[clientes$genero != "0"] <- "1"

# visit_web #
clientes$visit_web[clientes$visit_web > 0 & clientes$visit_web < 100] <- "1"
clientes$visit_web[clientes$visit_web > 100] <- "2"

# antiguedad #
#clientes$visit_web[clientes$visit_web > 0 & churn$visit_web < 100] <- "1"
#clientes$visit_web[clientes$visit_web > 99] <- "2"

# edad #
#clientes$visit_web[clientes$visit_web > 0 & churn$visit_web < 100] <- "1"
#clientes$visit_web[clientes$visit_web > 99] <- "2"


#######################
#### CRUZAR TABLAS ####
#######################

churn <- merge(clientes,saldos, by = "id_persona", all.x = T)
#View(churn)

churn <- merge(churn,reclamos, by = "id_persona", all.x = T)
#View(churn)

churn <- merge(churn,desafiliados, by = "id_persona", all.x = T)
#View(churn)


#######################################  
#### BORRAR FILAS VACÍAS DEL CRUCE ####
#######################################


sapply(churn, function(x) sum(is.na(x)))

dim(churn)
churn <- subset(churn, tipo_producto != "")
dim(churn)
churn <- subset(churn, tipo_fondo != "")
dim(churn)
churn <- subset(churn, val_cuo_saldo != "")
dim(churn)

names(churn)
################################################## 
#### CREAR VARIABLES NUEVAS DE RECLAMO Y FUGA ####
##################################################


churn$tipo_reclamo=ifelse(churn$tipo_reclamo %in% c("NORMATIVOS","NO NORMATIVOS"), 1,0)
churn$flag_desafiliacion=ifelse(churn$flag_desafiliacion %in% c("TRAS OUT","DEVOLUCION DE FONDOS EXTRANJEROS"), 1,0)

sapply(churn, function(x) sum(is.na(x)))

churn$cuenta_obligatoria<-NULL # Son puros 1 la borro

### tipo producto es nominal voy a dejar k-1 dummy en este caso 2 dummy
churn$producto_CAV<- ifelse(churn$tipo_producto=="CAV",1,0)
churn$producto_CCV<- ifelse(churn$tipo_producto=="CCV",1,0)
churn$tipo_producto<-NULL # La borro la voy a ocupar como dummy

#tipo fondo a numero
churn$tipo_fondo<-ifelse(churn$tipo_fondo=="A",1,ifelse(churn$tipo_fondo=="B",2,ifelse(churn$tipo_fondo=="C",3,ifelse(churn$tipo_fondo=="D",4,ifelse(churn$tipo_fondo=="E",5,6)))))

write.table(churn, "churn_prueba.csv", sep=";", row.names = F)

View(churn)

}
