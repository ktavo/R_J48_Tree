

rm(list=ls())

setwd("E:/UBA/Aprendizaje Automático/TP1/Árboles Punto 6")
a_train_original_data <- "train_original.csv"
a_test_original_data <- "test_original.csv"
library("RWeka")
library("partykit")
#install.packages("libcoin")
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


#Genera árbol train
tree_train <- J48(as.factor(y)~., df_train_original,control=Weka_control(M=2,C=0.25))
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

datos_train<-length(df_train_original$y)
M_porc<-list(seq(0.05,0.5,0.05))
M_par<-lapply(1*seq(0.05,0.5,0.05),identity)
hasta<-length(M_par)
#Genero los árboles




for (i in 1:hasta){
  tree_train<-J48(as.factor(y)~., df_train_original,control=Weka_control(M=2,C=M_par[i]))
  validacion_test<-evaluate_Weka_classifier(tree_train,newdata=df_test_original)
  p <- as.party(tree_train)
  
    if (i==1) {
    df_punto_3 <- data.frame(Corrida=i,
                                   M_porcentaje=M_porc[[1]][i],
                                   M_num=M_par[[i]],
                                   Performance_train=summary(tree_train)$details[1],
                                   Size_tree=width(p),
                                   Num_leaves=length(p),
                                   Prof_Tree=depth(p),
                                   Performance_test=validacion_test$details[1])
    } else {
  df_punto_3 <- rbind(df_punto_3,
                  data.frame(Corrida=i,
                                  M_porcentaje=M_porc[[1]][i],
                                  M_num=M_par[[i]],
                                  Performance_train=summary(tree_train)$details[1],
                                  Size_tree=width(p),
                                  Num_leaves=length(p),
                                  Prof_Tree=depth(p),
                                  Performance_test=validacion_test$details[1],
                                  check.names=FALSE))
    }
}



tiff(filename="3_Tamanio_vs_CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_punto_3, 
       aes(M_porcentaje, Size_tree)) + 
  geom_line(color='dark blue') + 
  geom_point(color='dark blue') + 
  theme_minimal() +
  xlab("Porcentaje del Confidence Factor") + 
  ylab("Tamaño del árbol") + 
  ggtitle("Tamaño según variación del CF")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

df_graf<-rbind(data.frame(df_punto_3[,c(1:3)],Performance=df_punto_3[,4],Base=rep("Train",20)),
               data.frame(df_punto_3[,c(1:3)],Performance=df_punto_3[,8],Base=rep("Test",20)))

tiff(filename="3_Accuracy_vs_CF.jpeg",units="in",width=6.5,height=4,res=300,compression='lzw')
ggplot(df_graf, 
       aes(M_porcentaje, Performance, group = Base, color = Base)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  xlab("Porcentaje del Confidence Factor") + 
  ylab("Accuracy") + 
  ggtitle("Accuracy según variación del CF")+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

write.table(x = df_punto_3,
            file = "Resultados_punto_3_CF.csv", 
            sep = ",", 
            quote = FALSE,
            row.names = FALSE)
