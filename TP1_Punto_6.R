  
  
  rm(list=ls())
  
  setwd("E:/UBA/Aprendizaje Automático/TP1/Árboles Punto 6")
  
  library("RWeka")
  library("partykit")
  library("ggplot2")
  #install.packages("libcoin")
  
  #Cargo datos de test
  a_test_original_data <- "test_original.csv"
  df_test_original <- read.csv(a_test_original_data, 
                               header = TRUE, 
                               sep = ",", 
                               colClasses=c('numeric','factor','factor','factor','factor','factor','factor','factor','factor','factor','factor'), 
                               check.names = FALSE)
  
  porc_min_ruido <- 0
  porc_max_ruido <- 0.35
  porc_step_ruido <- 0.01
  
  # Cantidad de archivos a generar por tipo de ruido.
  N <- round((porc_max_ruido - porc_min_ruido) /  porc_step_ruido, 0)
  
  noise_level <- 35
  
  for (i in 0:3){
  
  
  }  
  #Cargo datos de train
  nombre_train_set <- paste("06.- data_script/dataset_ruido_",noise_level,"_.csv", sep = "")
  a_train_ruido_data <- nombre_train_set
  df_train_ruido <- read.csv(a_train_ruido_data, 
                                header = TRUE, 
                                sep = ",", 
                                colClasses=c('numeric','factor','factor','factor','factor','factor','factor','factor','factor','factor','factor'), 
                                check.names = FALSE)
  
  
  #Genera árbol train
  tree_train <- J48(as.factor(y)~., df_train_ruido,control=Weka_control(M=2,C=0.25))
  summary(tree_train)
  performance_train<-summary(tree_train)$details[1]
  
  #Acá evalúa con la base de TEST
  validacion_test<-evaluate_Weka_classifier(tree_train,newdata=df_test_original)
  performance_test<-validacion_test$details[1]
  
  #Dimensión del árbol
  p <- as.party(tree_train)
  leaves<-length(p)
  ## [1] 9
  size<-width(p)
  ## [1] 5
  prof<-depth(p)
  ## [1] 4
  
  
  
  #Genera el árbol con la base de TRAIN
  
  datos_train<-length(df_train_ruido$y)
  M_porc<-list(seq(0.05,0.5,0.05))
  M_par<-lapply(1*seq(0.05,0.5,0.05),identity)
  hasta<-length(M_par)
  #Genero los árboles
  
  
  
  
  
  
  
  for (i in 1:hasta){
    tree_train<-J48(as.factor(y)~., df_train_ruido,control=Weka_control(M=2,C=M_par[i]))
    validacion_test<-evaluate_Weka_classifier(tree_train,newdata=df_test_original)
    p <- as.party(tree_train)
    
    if (i==1) {
      df_punto_6 <- data.frame(
                            Corrida=i,
                            M_porcentaje=M_porc[[1]][i],
                            M_num=M_par[[i]],
                            Performance_train=summary(tree_train)$details[1],
                            Size_tree=width(p),
                            Num_leaves=length(p),
                            Prof_Tree=depth(p),
                            Performance_test=validacion_test$details[1],
                            Noise_Percentage = noise_level)
    } else {
      df_punto_6 <- rbind(df_punto_6,
                    data.frame(
                            Corrida=i,
                            M_porcentaje=M_porc[[1]][i],
                            M_num=M_par[[i]],
                            Performance_train=summary(tree_train)$details[1],
                            Size_tree=width(p),
                            Num_leaves=length(p),
                            Prof_Tree=depth(p),
                            Performance_test=validacion_test$details[1],
                            Noise_Percentage = noise_level,
                            check.names=FALSE))
    }
  }
  
  nombre_grafico_tamanio <- paste("6_Tamanio_vs_CF_",noise_level,".jpeg", sep = "")
  
  tiff(filename= nombre_grafico_tamanio, units="in",width=6.5,height=4,res=300,compression='lzw')
  ggplot(df_punto_6, 
         aes(M_porcentaje, Size_tree)) + 
    geom_line(color='dark blue') + 
    geom_point(color='dark blue') + 
    theme_minimal() +
    xlab("Porcentaje del Confidence Factor") + 
    ylab("Tamaño del árbol") + 
    ggtitle(paste("Tamaño según variación del CF", "Noise %", noise_level))+
    theme_linedraw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
  dev.off()
  
  df_graf<-rbind(data.frame(df_punto_6[,c(1:3)],Performance=df_punto_6[,4],Base=rep("Train",20)),
                 data.frame(df_punto_6[,c(1:3)],Performance=df_punto_6[,8],Base=rep("Test",20)))
  
  nombre_grafico_accuracy <- paste("6 _Accuracy_vs_CF_",noise_level,".jpeg", sep = "")
  
  tiff(filename= nombre_grafico_accuracy ,units="in",width=6.5,height=4,res=300,compression='lzw')
  ggplot(df_graf, 
         aes(M_porcentaje, Performance, group = Base, color = Base)) + 
    geom_line() + 
    geom_point() + 
    theme_minimal() +
    xlab("Porcentaje del Confidence Factor") + 
    ylab("Accuracy") + 
    ggtitle(paste("Accuracy según variación del CF","Noise %",noise_level))+
    theme_linedraw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
  dev.off()
  
  write.table(x = df_punto_6,
             file = "Resultados_punto_6_CF.csv", 
             sep = ",", 
             quote = FALSE,
             append = TRUE,
             row.names = FALSE,
             col.names = FALSE)

  #############################Gráficos###############################################3
  rm(list=ls())
  setwd("E:/UBA/Aprendizaje Automático/TP1/Árboles Punto 6")
  
  test_ruido_data <- "Resultados_punto_6_CF.csv"
  list_test_ruido <- read.csv(test_ruido_data, 
                               header = TRUE, 
                               sep = ",", 
                               colClasses=c('numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'), 
                               check.names = FALSE)
  
  
  list_test_ruido$M_porcentaje<-as.factor(list_test_ruido$M_porcentaje)
  
  ggplot(list_test_ruido, 
         aes(Noise_Percentage, Performance_train, group =M_porcentaje, color = M_porcentaje)) + 
    geom_line() + 
    geom_point() + 
    theme_minimal() +
    xlab("Porcentaje Faltantes") + 
    ylab("Performance") +  
    ggtitle("Performance Train con Ruido ")
  
  ggplot(list_test_ruido, 
         aes(Noise_Percentage, Performance_test, group =M_porcentaje, color = M_porcentaje)) + 
    geom_line() + 
    geom_point() + 
    theme_minimal() +
    xlab("Porcentaje Faltantes") + 
    ylab("Performance") +  
    ggtitle("Performance Test con Ruido ")
  
  