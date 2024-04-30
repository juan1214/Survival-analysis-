library(readxl)
library(ggplot2)
library(visdat)
library(survival)
library(naniar)
library(missForest)
library(survminer)
library(xtable)
BD <- read_excel("C:/Users/UNALMED/Desktop/One Health/Tesis doctora Olga/1. BASE DE RESULTADOS PROYECTO GOBERNACIÓN_CyC_14122023.xlsx")
BD1 <- read_excel("C:/Users/UNALMED/Desktop/One Health/Tesis doctora Olga/BDAnalisis_hospitalizados_2024.xlsx")

# Definir las variables específicas a convertir a numérico
variables <- c("IL-6...19", "IL-10...21", "IL-6...52", "IL-10...54", "IL-6...80", "IL-10...82", "IL-6...114", "IL-10...116")

convertir_a_numerico <- function(data, variables) {
  for (col in variables) {
    data[[col]] <- as.numeric(data[[col]])
  }
  return(data)
}

# Llamar a la función con las variables específicas
BD <- convertir_a_numerico(BD, variables)


              ##################################################
              ###ANÁLISIS DESCRIPTIVO VARIABLES DE EXPOSICIÓN###
              ##################################################


#Análisis descriptivo para la variable IL-6 por muestra
summary(BD[, variables[c(TRUE, FALSE)]])

#Análisis descriptivo para la variable IL-10 por muestra
summary(BD[ ,variables[c(FALSE, TRUE)]])






              #####################################
              ###ANÁLISIS VARIABLES DESCRIPTIVAS###
              #####################################

variables_descriptivas <- c("qPCR...4","VARIANT...5","ELISA IgG...7",
                            "ARCHITECT IgG...8","ARCHITECT IgG CONCENTRATION...9",
                            "ARCHITECT IgM...10","IgG ANTI-NUCLEOCAPSID...11",
                            "IgG ANTI-NUCLEOCAPSID Ratio...12")

variables_numericas <- c("ARCHITECT IgG CONCENTRATION...9","IgG ANTI-NUCLEOCAPSID Ratio...12")

variables_factor <- c("qPCR...4","VARIANT...5","ELISA IgG...7","ARCHITECT IgG...8","ARCHITECT IgM...10","IgG ANTI-NUCLEOCAPSID...11")


#convertir las variables a numéricas y factores según especificación
convertir_variables_especifico <- function(variables_numericas, variables_factor, BD) {
  for (variable in variables_numericas) {
    if (variable %in% names(BD)) {
      BD[[variable]] <- as.numeric(BD[[variable]])
      cat(paste("La variable", variable, "ha sido convertida a numérica\n"))
    } else {
      cat(paste("La variable", variable, "no se encuentra en la base de datos\n"))
    }
  }
  
  for (variable in variables_factor) {
    if (variable %in% names(BD)) {
      BD[[variable]] <- as.factor(BD[[variable]])
      cat(paste("La variable", variable, "ha sido convertida a factor\n"))
    } else {
      cat(paste("La variable", variable, "no se encuentra en la base de datos\n"))
    }
  }
  
  return(BD)
}


# Llamar a la función con las variables específicas
BD <- convertir_variables_especifico(variables_numericas, variables_factor, BD)



#################################################################################
# Definir la función para verificar la naturaleza de las variables
verificar_naturaleza <- function(variables_descriptivas, BD) {
  for (variable in variables_descriptivas) {
    if (variable %in% names(BD)) {
      tipo <- typeof(BD[[variable]])
      cat(paste("La variable", variable, "es de tipo", tipo, "\n"))
    } else {
      cat(paste("La variable", variable, "no se encuentra en la base de datos\n"))
    }
  }
}

# Llamar a la función con las variables descriptivas la base de datos
verificar_naturaleza(variables_descriptivas, BD)


              ##############################################
              ###Análisis descriptivo variables numericas###
              ##############################################


# Realizar la prueba de Shapiro-Wilk y crear gráficos de densidad para las variables numéricas
for (variable in variables_numericas) {
  if (variable %in% names(BD)) {
    shapiro_test <- shapiro.test(BD[[variable]])
    p_valor <- shapiro_test$p.value
    
    # Crear el gráfico de densidad
    density_plot <- ggplot(BD, aes(x = .data[[variable]], fill = "Density")) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Densidad de", variable, " - p-valor:", round(p_valor)))
    
    print(density_plot)
  } else {
    cat(paste("La variable", variable, "no se encuentra en la base de datos\n"))
  }
}


#Análisis descriptivo 
summary(BD[ ,variables_numericas])

#Análisis descriptivo variables cualitativas 
summary(BD[ ,variables_factor])


            #############################
            ###IMPUTACIÓN DE VARIABLES###
            #############################

library(zoo)
library(stats)

###########################################
###IMPUTACIÓN DE VARIABLES DE EXPOSICIÓN###
###########################################


#Graficos de densidad y prueba shapiro-wilks

# Realizar la prueba de Shapiro-Wilk y crear gráficos de densidad para las variables numéricas
for (variable in variables) {
  if (variable %in% names(BD)) {
    shapiro_test <- shapiro.test(BD[[variable]])
    p_valor <- shapiro_test$p.value
    
    # Crear el gráfico de densidad
    density_plot <- ggplot(BD, aes(x = .data[[variable]], fill = "Density")) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Densidad de", variable, " - p-valor:", round(p_valor)))
    
    print(density_plot)
  } else {-
    cat(paste("La variable", variable, "no se encuentra en la base de datos\n"))
  }
}

#crear una base de datos con las variables a imputar
BDIMP <- data.frame(BD[, c("IL-6...19", "IL-6...52", "IL-6...80","IL-10...21","IL-10...54","IL-10...82")])

#imputación por bosques aleatorios
BDIMP_BA <- missForest(BDIMP)
# Guardar la base de datos imputada en una nueva variable
BDIMP_imputado <- BDIMP_BA$ximp


BDIMP <- sapply(BDIMP, function(x) na.aggregate(x, FUN = median))
BDIMP <- data.frame(BDIMP)

#Análisis descriptivo variables imputadas
summary(BDIMP)

 #Prueba Kolmogorov-smirnov para diferencia

ks.test(BD$`IL-6...80`,BDIMP$IL.6...80)

#Graficos de densidad de variables originales vs variables imputadas

par(mfrow=c(1,3))
plot(density(BD$`IL-6...19`,na.rm = T),col=2,main="IL-6 sample 1") 
lines(density(BDIMP$IL.6...19),col=3)  
plot(density(BD$`IL-6...52`,na.rm = T),col=2,main="IL-6 sample 2")
lines(density(BDIMP$IL.6...52),col=3)
plot(density(BD$`IL-6...80`,na.rm = T),col=2,main="IL-6 sample 3")
lines(density(BDIMP$IL.6...80),col=3)

par(mfrow=c(1,3))
plot(density(BD$`IL-10...21`,na.rm = T),col=2,main="IL-10 sample 1")
lines(density(BDIMP$IL.10...21),col=3)
plot(density(BD$`IL-10...54`,na.rm = T),col=2,main="IL-10 sample 2")
lines(density(BDIMP$IL.10...54),col=3)
plot(density(BD$`IL-10...82`,na.rm = T),col=2,main="IL-10 sample 3")
lines(density(BDIMP$IL.10...82),col=3)

##########################################
###IMPUTACIÓN DE VARIABLES DESCRIPTIVAS###
##########################################

#Imputacióon de variables descriptivas numericas
va_num_descrip <- BD[, variables_numericas]
va_num_descrip <- sapply(va_num_descrip, function(x) na.aggregate(x, FUN = median))
va_num_descrip <- data.frame(va_num_descrip)

summary(va_num_descrip)

#Graficos de densidad de las variables descriptivas numericas originales vs imputadas
par(mfrow=c(1,2))
plot(density(BD$`ARCHITECT IgG CONCENTRATION...9`,na.rm = T),col=2,main="ARCHITECT IgG CONCENTRATION") 
lines(density(BDIMP$ARCHITECT.IgG.CONCENTRATION...9),col=3)  
plot(density(BD$`IgG ANTI-NUCLEOCAPSID Ratio...12`,na.rm = T),col=2,main="IgG ANTI-NUCLEOCAPSID Ratio")
lines(density(BDIMP$IgG.ANTI.NUCLEOCAPSID.Ratio...12),col=3)

#agregar a la BDIMP las variable imputadas
BDIMP <- cbind(BDIMP, va_num_descrip)

#Imputación de variables cualitativas
va_cuali_descrip <- BD[, variables_factor]

#R esta tomando los valores N/A como una cualidad, primero debes de convertir la cualidad N/A  valores faltantes

# Función para convertir N/A a valores faltantes en una variable
convertir_na <- function(x) {
  if (is.factor(x)) {
    levels(x)[levels(x) == "N/A"] <- NA
    x
  } else {
    replace_na(x, NA)
  }
}

# Aplicar la función a todas las variables categóricas
va_cuali_descrip <- lapply(va_cuali_descrip[, sapply(va_cuali_descrip, is.factor)], convertir_na)



            ###############################
            ###GRAFICOS DE SUPERVIVENCIA###
            ###############################

#Evento: muerte
#Sensura: Vivo
#Variable evento: `fallece_dia 28`
#Tiempo de evento: 2020 al 2023, variables : fecha_dx_covid, fecha_ultimo_contacto(fecha_fallece, fecha_ultimo contacto a los 28 del diagnotico por covid    )
#Para calcular el tiempo del evento calculamos el tiempo entre la variable fec_ing_hosp y fech_falle

####cREAR NUEVA VARIABLE FECHA DE TERMINACIÓN DEL ESTUDIO A LOS 28 DIAS DESDE EL DX_COVID#####

# Convertir la columna "dx_covid" y "fecha_falle" a un objeto de fecha
BD1$dx_covid <- as.Date(BD1$dx_covid)
BD1$fech_falle <- as.Date(BD1$fech_falle)

# Sumar 45 días a la fecha de dx_covid y asignarla a la columna fecha_fin_stu
BD1$fecha_fin_stu_45 <- BD1$dx_covid + 45


###agregar las fechas de fallecimiento de los 14 pacientes a la nueva variable de terminación de estudio###

BD1$fecha_fin_stu_45[!is.na(BD1$fech_falle)] <- BD1$fech_falle[!is.na(BD1$fech_falle)]





# Crear un objeto de tipo "Surv" con los tiempos de supervivencia y el evento
tiempo_evento <- with(BD1, as.numeric(difftime(fecha_fin_stu_45, dx_covid, units = "days")))
evento <- BD1$`fallece_dia 28`
surv_obj <- Surv(time = tiempo_evento, event = evento)

# Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier
km_fit <- survfit(surv_obj ~ 1)

summary(km_fit)

# Convertir el objeto survfit en un data frame
km_df <- data.frame(time = km_fit$time, n.risk = km_fit$n.risk, n.event = km_fit$n.event, surv = km_fit$surv, std.err = km_fit$std.err)

# Graficar el Kaplan-Meier
plot(km_fit, xlab = "Tiempo (días)", ylab = "Probabilidad de supervivencia", main = "Gráfico de Supervivencia Kaplan-Meier", ylim = c(0.8, 1))

#grafico mas elaborado
# Crear el gráfico con ggsurvplot
ggsurvplot(km_fit, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 45),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           ggtheme = theme_minimal(),  # Establecer el tema del gráfico
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados", # Establecer el título del gráfico
           ylim = c(0.85,1)
)



#######################################################
###Análisis de supervincia a los 28 días del estudio###
#######################################################

# Sumar 28 días a la fecha de dx_covid y asignarla a la columna fecha_fin_stu
BD1$fecha_fin_stu_28 <- BD1$dx_covid + 28

###agregar las fechas de fallecimiento de los 14 pacientes a la nueva variable de terminación de estudio###
BD1$fecha_fin_stu_28[!is.na(BD1$fech_falle)] <- BD1$fech_falle[!is.na(BD1$fech_falle)]

tiempo_evento_28 <- with(BD1, as.numeric(difftime(fecha_fin_stu_28, dx_covid, units = "days")))
evento <- BD1$`fallece_dia 28`


# Aplicar la condición para censurar los datos
tiempo_censurado <- ifelse(tiempo_evento_28 > 28 & evento == 1, 28, tiempo_evento_28)
evento_censurado <- ifelse(tiempo_evento_28 > 28 & evento == 1, 0, evento)

surv_obj_censurado <- Surv(time = tiempo_censurado, event = evento_censurado)

# Realizar el análisis de supervivencia con los datos censurados según la condición

km_fit_censurado <- survfit(surv_obj_censurado ~ 1)

summary(km_fit_censurado)

# Convertir el objeto survfit en un data frame
km_df_censurado <- data.frame(time = km_fit_censurado$time, n.risk = km_fit_censurado$n.risk, n.event = km_fit_censurado$n.event, surv = km_fit_censurado$surv, std.err = km_fit_censurado$std.err)


ggsurvplot(km_fit_censurado, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           ggtheme = theme_minimal(),  # Establecer el tema del gráfico
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados", # Establecer el título del gráfico
           ylim = c(0.85,1)
)


#########################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por grupo de género###
#########################################################################################
km_fit_gen <- survfit(surv_obj_censurado ~ genero , data = BD1)

summary(km_fit_gen)



# Graficar el Kaplan-Meier por grupo de género con escala de eje y desde 0.5
plot(km_fit_gen, xlab = "Tiempo (días)", ylab = "Probabilidad de supervivencia", main = "Gráfico de Supervivencia Kaplan-Meier por Género", ylim = c(0.8, 1))

#Grafico mas elaborado
# Crear el gráfico con ggsurvplot por género
ggsurvplot(km_fit_gen, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por Género",
           ylim = c(0.8, 1),  # Ajustar límites del eje y
           legend.title = "Género",
           legend.labs = c("Femenino", "Masculino"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados por género",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia,
)

# Realizar la prueba de log-rank
logrank_test_result_gen <- survdiff(surv_obj_censurado ~ genero, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_result_gen)

################################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por grupo de ingreso a UCI###
################################################################################################

km_fit_ing_uci <- survfit(surv_obj_censurado ~ ing_uci , data = BD1)

summary(km_fit_ing_uci)

# Crear el gráfico con ggsurvplot por ingreso a UCI
ggsurvplot(km_fit_ing_uci, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por Género",
           ylim = c(0.8, 1),  # Ajustar límites del eje y
           legend.title = "Género",
           legend.labs = c("Femenino", "Masculino"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados por ingreso a UCI",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia,
)

# Realizar la prueba de log-rank
logrank_test_result_ing_uci <- survdiff(surv_obj_censurado ~ ing_uci, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_result_ing_uci)

###############################################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por grupo de clasificación de enfermedad###
###############################################################################################################

km_fit_class <- survfit(surv_obj_censurado ~ Clasificacion , data = BD1)

summary(km_fit_class)

# Crear el gráfico con ggsurvplot por ingreso a UCI
ggsurvplot(km_fit_class, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por clasificación enfermedad",
           ylim = c(0.85, 1),  # Ajustar límites del eje y
           legend.title = "Clasificación enfermedad",
           legend.labs = c("Critica", "Grave", "Moderado"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados \n por clasificación de enfermedad",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia
)

# Realizar la prueba de log-rank
logrank_test_class <- survdiff(surv_obj_censurado ~ pcr_covid, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_class)


########################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por grupo de edad ###
########################################################################################

###creemos una variable que agrupe las edades


f_grupo_edad <- function(x, edad_final = 95, grupo = 20, ...) {
  # Creamos un vector númerico desde el cero hasta la edad con una amplitud
  seq_edades <- seq(0, edad_final, grupo)
  # Número de grupos que se crearon
  n <- length(seq_edades)
  # Creamos un vector para las edades inferiores de cada intervalo de edad
  seq_edades_sup <- c(seq_edades + grupo - 1)[-n]
  # Creamos los labels de los grupos! 
  labs <-  c(paste0(seq_edades[-n], "-", seq_edades_sup), paste0(seq_edades[n], "+"))
  
  # Colocamos todo en su lugar
  cut(x, c(seq_edades, 200),  
      labels = labs, right = FALSE, include.lowest = TRUE, ...)
}


BD1$grupo_edad <- f_grupo_edad(BD1$edad...9, 
                                edad_final = 95, # edad final
                                grupo = 20 # amplitud de los grupos de edad quinquenales
)

summary(BD1$grupo_edad)

#Creamos el objeto tipo survfit

km_fit_edad <- survfit(surv_obj_censurado ~ grupo_edad, data = BD1)

summary(km_fit_edad)

# Crear el gráfico con ggsurvplot por ingreso grupo de edad
ggsurvplot(km_fit_edad, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por clasificación de edad",
           ylim = c(0.85, 1),  # Ajustar límites del eje y
           legend.title = "Grupo de edades",
           legend.labs = c("20-39", "40-59", "60-79","80+"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados \n por clasificación de edad",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia
)

# Realizar la prueba de log-rank
logrank_test_edad <- survdiff(surv_obj_censurado ~ grupo_edad, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_edad)



#########################################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por clasificación Puntaje Charlson ###
#########################################################################################################

#Crear nueva variable ClasificacionPCharlson

# Crear nueva variable ClasificacionPCharlson basada en PuntajeCharlson
BD1$ClasificacionPCharlson <- ifelse(BD1$PuntajeCharlson %in% 0:1, "Ausencia de comorbilidad",
                                     ifelse(BD1$PuntajeCharlson == 2, "Comorbilidad baja",
                                            ifelse(BD1$PuntajeCharlson >= 3, "Comorbilidad alta", NA)))

#Creamos el objeto tipo survfit

km_fit_Cla_p_Charlson <- survfit(surv_obj_censurado ~ ClasificacionPCharlson, data = BD1)

summary(km_fit_Cla_p_Charlson)

# Crear el gráfico con ggsurvplot por ingreso grupo de edad
ggsurvplot(km_fit_Cla_p_Charlson, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por clasificación de puntaje de Charlson",
           ylim = c(0.85, 1),  # Ajustar límites del eje y
           legend.title = "Clasificación puntaje de Charlson",
           legend.labs = c("Ausencia de comorbilidad", "Comorbilidad alta", "Comorbilidad baja"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados \n por clasificación de puntaje de Charlson",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia
)


# Realizar la prueba de log-rank
logrank_test_Cla_p_Charlson <- survdiff(surv_obj_censurado ~ ClasificacionPCharlson, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_Cla_p_Charlson)



#########################################################################################################
###Crear un objeto de tipo "Survfit" con el modelo de Kaplan-Meier por clasificación Puntaje News-2   ###
#########################################################################################################

#Crear nueva variable ClasificacionPCharlson

# Crear nueva variable ClasificacionPCharlson basada en PuntajeCharlson
BD1$ClasificacionPNews2 <- ifelse(BD1$PuntajeNews <= 4, "Observacion",
                                     ifelse(BD1$PuntajeNews %in% 5:6, "Aviso medico",
                                            ifelse(BD1$PuntajeNews >= 7, "Urgente avisar UCI", NA)))


#Creamos el objeto tipo survfit

km_fit_Cla_p_News2 <- survfit(surv_obj_censurado ~ ClasificacionPNews2, data = BD1)

summary(km_fit_Cla_p_News2)

# Crear el gráfico con ggsurvplot por ingreso grupo de edad
ggsurvplot(km_fit_Cla_p_News2, 
           data = BD1,
           risk.table = FALSE,
           pval = TRUE,
           conf.int = TRUE,
           xlim = c(0, 30),
           break.time.by = 5,
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de supervivencia",
           main = "Gráfico de Supervivencia Kaplan-Meier por clasificación de puntaje News2",
           ylim = c(0.85, 1),  # Ajustar límites del eje y
           legend.title = "Clasificación puntaje News2",
           legend.labs = c("Aviso medico", "Observacion", "Urgente avisar UCI"),  # Etiquetas en la leyenda
           risk.table.title = "Tabla de Riesgos",
           risk.table.col = "strata",  # Colorear la tabla por grupo
           tables.theme = theme_cleantable(),  # Tema de la tabla,
           title = "Gráfico de Supervivencia Kaplan-Meier para la mortalidad de los 131 pacientes hospitalizados \n por clasificación de puntaje News2",
           surv.plot.height = 0.7  # Ajustar la altura del gráfico de supervivencia
)

# Realizar la prueba de log-rank
logrank_test_Cla_p_News2 <- survdiff(surv_obj_censurado ~ ClasificacionPNews2, data = BD1)

# Mostrar el resultado de la prueba
print(logrank_test_Cla_p_News2)

