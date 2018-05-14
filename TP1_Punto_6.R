#PUNTO 6

rm(list=ls())

setwd("E:/UBA/Aprendizaje Automático/TP1/Árboles Punto 6")
a_train_original_data <- "train_original.csv"
a_test_original_data <- "test_original.csv"
library("RWeka")
library("partykit")
library("ggplot2")

options(warn=-1)  #Suprimimos los Warnings

#Importo los archivos

df_train_original <- read.csv(a_train_original_data, 
                              header = TRUE, 
                              sep = ",", 
                              colClasses=c('numeric','factor','factor','factor','factor','factor','factor','factor','factor','factor','factor'), 
                              check.names = FALSE)

df_test_original <- read.csv(a_test_original_data, 
                             header = TRUE, 
                             sep = ",", 
                             colClasses=c('numeric','factor','factor','factor','factor','factor','factor','factor','factor','factor','factor'), 
                             check.names = FALSE)

disc_col_name <- "age"
bins_min <- 1
bins_max <- 20
bins_step <- 1

# Cantidad de archivos a generar por tipo de imputación.
N <- round((bins_max - bins_min) /  bins_step, 0)

# Variaciones del CF
CF_min <- 0
CF_max <- 0.5
CF_step <- 0.05

# Cantidad de corridas
M <- round((CF_max - CF_min) /  CF_step, 0)

for (i in 0:N){
  
  bins <- bins_min + i*bins_step
  
  # Partimos del data set puro.
  df_train_aux_igual_ancho <- df_train_original
  df_train_aux_igual_cantidad <- df_train_original
  df_test_aux_igual_ancho <- df_test_original
  df_test_aux_igual_cantidad <- df_test_original
  
  # Calculamos los bins de igual ancho.  
  min <- min(rbind(df_train_original[, disc_col_name], df_test_original[, disc_col_name]))
  max <- max(rbind(df_train_original[, disc_col_name], df_test_original[, disc_col_name]))
  ancho <- (max - min)
  ancho_bin <- ceiling(ancho/bins)
  min_modif <- floor((min+max)/2 - ceiling(ancho/2))
  max_modif <- ceiling((min+max)/2 + ceiling(ancho/2))
  bins_igual_ancho <- seq(min_modif, max_modif, ancho_bin)
  bins_igual_ancho_labels <- paste("[", as.character(round(bins_igual_ancho[1],0)), "-" ,as.character(round(bins_igual_ancho[2],0)), "]", sep = "")
  for(k in 1:(bins-1)){
    
    bins_igual_ancho_labels <- c(bins_igual_ancho_labels,
                                 paste("(", as.character(round(bins_igual_ancho[1+k],0)), "-" ,as.character(round(bins_igual_ancho[2+k],0)), "]", sep = ""))
    
  }
  
  # Calculamos los bins de igual cantidad.
  bins_igual_cantidad <- quantile(rbind(df_train_aux_igual_cantidad$age, df_test_aux_igual_cantidad$age), seq(0,1,1/bins))
  bins_igual_cantidad_labels <- paste("[", as.character(round(bins_igual_cantidad[1],0)), "-" ,as.character(round(bins_igual_cantidad[2]),0), "]", sep = "")
  for(k in 1:(bins-1)){
    
    bins_igual_cantidad_labels <- c(bins_igual_cantidad_labels,
                                    paste("(", as.character(round(bins_igual_cantidad[1+k],0)), "-" ,as.character(round(bins_igual_cantidad[2+k]),0), "]", sep = ""))
    
  }
  
  df_train_aux_igual_ancho$age <- as.factor(bins_igual_ancho_labels[.bincode(df_train_aux_igual_ancho$age, bins_igual_ancho, TRUE, TRUE)])
  df_train_aux_igual_cantidad$age <- as.factor(bins_igual_cantidad_labels[.bincode(df_train_aux_igual_cantidad$age, bins_igual_cantidad, TRUE, TRUE)])
  
  df_test_aux_igual_ancho$age <- as.factor(bins_igual_ancho_labels[.bincode(df_test_aux_igual_ancho$age, bins_igual_ancho, TRUE, TRUE)])
  df_test_aux_igual_cantidad$age <- as.factor(bins_igual_cantidad_labels[.bincode(df_test_aux_igual_cantidad$age, bins_igual_cantidad, TRUE, TRUE)])
  
  if (i==3) {
    check_train_bin4_igualcant<-df_train_aux_igual_cantidad
    check_test_bin4_igualcant<-df_test_aux_igual_cantidad  
  }
  #Corro los árboles
  
  # Hacemos variar el CF.
  for (j in 1:M){
    
    CF <- CF_min + j*CF_step
    
    #Igual_ancho
    tree_train_igual_ancho<-J48(as.factor(y)~., df_train_aux_igual_ancho,control=Weka_control(M=2,C=CF))
    validacion_test_igual_ancho<-evaluate_Weka_classifier(tree_train_igual_ancho,newdata=df_test_aux_igual_ancho)
    p_igual_ancho <- as.party(tree_train_igual_ancho)
    
    #Igual_Cantidad
    tree_train_igual_cantidad<-J48(as.factor(y)~., df_train_aux_igual_cantidad,control=Weka_control(M=2,C=CF))
    validacion_test_igual_cantidad<-evaluate_Weka_classifier(tree_train_igual_cantidad,newdata=df_test_aux_igual_cantidad)
    p_igual_cantidad <- as.party(tree_train_igual_cantidad)
    
    if (i==0 & j==1) {
      df_punto_7_igual_ancho <- data.frame(Corrida=i,
                                           Cant_bins=bins,
                                           Confidence_factor=CF,
                                           Performance_train=summary(tree_train_igual_ancho)$details[1],
                                           Size_tree=width(p_igual_ancho),
                                           Num_leaves=length(p_igual_ancho),
                                           Prof_Tree=depth(p_igual_ancho),
                                           Performance_test=validacion_test_igual_ancho$details[1])
      
      df_punto_7_igual_cantidad <- data.frame(Corrida=i,
                                              Cant_bins=bins,
                                              Confidence_factor=CF,
                                              Performance_train=summary(tree_train_igual_cantidad)$details[1],
                                              Size_tree=width(p_igual_cantidad),
                                              Num_leaves=length(p_igual_cantidad),
                                              Prof_Tree=depth(p_igual_cantidad),
                                              Performance_test=validacion_test_igual_cantidad$details[1])
      
    } else {
      df_punto_7_igual_ancho <- rbind(df_punto_7_igual_ancho,
                                      data.frame(
                                        Corrida=i,
                                        Cant_bins=bins,
                                        Confidence_factor=CF,
                                        Performance_train=summary(tree_train_igual_ancho)$details[1],
                                        Size_tree=width(p_igual_ancho),
                                        Num_leaves=length(p_igual_ancho),
                                        Prof_Tree=depth(p_igual_ancho),
                                        Performance_test=validacion_test_igual_ancho$details[1]))
      
      df_punto_7_igual_cantidad <- rbind(df_punto_7_igual_cantidad,
                                         data.frame(Corrida=i,
                                                    Cant_bins=bins,
                                                    Confidence_factor=CF,
                                                    Performance_train=summary(tree_train_igual_cantidad)$details[1],
                                                    Size_tree=width(p_igual_cantidad),
                                                    Num_leaves=length(p_igual_cantidad),
                                                    Prof_Tree=depth(p_igual_cantidad),
                                                    Performance_test=validacion_test_igual_cantidad$details[1]))
      
    }
  }
}


#Discretización supervisada de Weka

df_total_discretizacion_weka<-rbind(
  data.frame(df_train_original,Base='Train'),
  data.frame(df_test_original,Base='Test'))

#Es indistinto que especifique age pq es la única numérica. Da los mismos resultados (chequeado). Dejo el punto para no perder la base y dsp abrirla entre train y test
df_total_discretizacion_weka_discretizado<-Discretize(formula=y~.,df_total_discretizacion_weka)

df_weka_discretizado_train<-df_total_discretizacion_weka_discretizado[df_total_discretizacion_weka_discretizado$Base=='Train',c(1:10,12)]
df_weka_discretizado_test<-df_total_discretizacion_weka_discretizado[df_total_discretizacion_weka_discretizado$Base=='Test',c(1:10,12)]

for (j in 1:M){
  
  CF <- CF_min + j*CF_step
  
  #Igual_ancho
  tree_weka_discretizado_train<-J48(as.factor(y)~., df_weka_discretizado_train,control=Weka_control(M=2,C=CF))
  validacion_test_weka_discretizado<-evaluate_Weka_classifier(tree_weka_discretizado_train,newdata=df_weka_discretizado_test)
  #p_weka_discretizado <- as.party(tree_weka_discretizado_train)
  
  if (j==1) {
    df_punto_7_discret_weka <- data.frame(Corrida=j,
                                          Cant_bins='3',
                                          Confidence_factor=CF,
                                          Performance_train=summary(tree_weka_discretizado_train)$details[1],
                                          Size_tree='',#width(p_weka_discretizado),
                                          Num_leaves='',#length(p_weka_discretizado),
                                          Prof_Tree='',#depth(p_weka_discretizado),
                                          Performance_test=validacion_test_weka_discretizado$details[1])
    
  } else {
    df_punto_7_discret_weka <- rbind(df_punto_7_discret_weka,
                                     data.frame(Corrida=j,
                                                Cant_bins='3',
                                                Confidence_factor=CF,
                                                Performance_train=summary(tree_weka_discretizado_train)$details[1],
                                                Size_tree='',#width(p_weka_discretizado),
                                                Num_leaves='',#ength(p_weka_discretizado),
                                                Prof_Tree='',#depth(p_weka_discretizado),
                                                Performance_test=validacion_test_weka_discretizado$details[1]))
    
  }
}

############################
##########GRÁFICOS##########
############################    

#Gráfico igual ancho 

df_punto_7_igual_ancho$Confidence_factor<-as.factor(df_punto_7_igual_ancho$Confidence_factor)

tiff(filename="7_Tamaño_igual_ancho_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_ancho, 
       aes(Cant_bins, Size_tree, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Tamaño del arbol") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()


#CF de 0.1 seq
tiff(filename="7_Tamaño_igual_ancho_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_ancho[df_punto_7_igual_ancho$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Size_tree, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Tamaño del arbol") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()

#CF todos- PerformanceTest
tiff(filename="7_PerfTest_igual_ancho_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_ancho, 
       aes(Cant_bins, Performance_test, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance validación") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#5CF - PerformanceTest
tiff(filename="7_PerfTest_igual_ancho_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_ancho[df_punto_7_igual_ancho$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Performance_test, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance validación") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#CF todos- PerformanceTrain
tiff(filename="7_PerfTrain_igual_ancho_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_ancho, 
       aes(Cant_bins, Performance_train, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance entrenamiento") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#5CF - PerformanceTest
tiff(filename="7_PerfTrain_igual_ancho_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_ancho[df_punto_7_igual_ancho$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Performance_train, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance entrenamiento") + 
  ggtitle("Discretización - Intervalos de igual ancho")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()



#Gráfico igual cantidad

df_punto_7_igual_cantidad$Confidence_factor<-as.factor(df_punto_7_igual_cantidad$Confidence_factor)


tiff(filename="7_Tamaño_igual_cantidad_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_cantidad, 
       aes(Cant_bins, Size_tree, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Tamaño del arbol") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#CF de 0.1 seq
tiff(filename="7_Tamaño_igual_cantidad_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_cantidad[df_punto_7_igual_cantidad$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Size_tree, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Tamaño del arbol") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()

#CF todos- PerformanceTest
tiff(filename="7_PerfTest_igual_cantidad_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_cantidad, 
       aes(Cant_bins, Performance_test, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance validación") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#5CF - PerformanceTest
tiff(filename="7_PerfTest_igual_cantidad_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_cantidad[df_punto_7_igual_cantidad$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Performance_test, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance validación") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#CF todos- PerformanceTrain
tiff(filename="7_PerfTrain_igual_cantidad_TodosCF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
#Todos los cf
ggplot(df_punto_7_igual_cantidad, 
       aes(Cant_bins, Performance_train, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance entrenamiento") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()


#5CF - PerformanceTest
tiff(filename="7_PerfTrain_igual_cantidad_5CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_7_igual_cantidad[df_punto_7_igual_cantidad$Confidence_factor %in% c('0.1','0.2','0.3','0.4','0.5'),], 
       aes(Cant_bins, Performance_train, group = Confidence_factor, color = Confidence_factor)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Cantidad de bins") + 
  ylab("Performance entrenamiento") + 
  ggtitle("Discretización - Intervalos de igual cantidad")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
dev.off()




write.table(x = df_punto_7_igual_ancho,
            file = "Resultados_punto_7_Igual_ancho.csv", 
            sep = ",", 
            quote = FALSE,
            row.names = FALSE)

write.table(x = df_punto_7_igual_cantidad,
            file = "Resultados_punto_7_Igual_cantidad.csv",
            sep = ",", 
            quote = FALSE,
            row.names = FALSE)

