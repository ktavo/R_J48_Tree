# ----------------------- Descripción del script --------------------------
# Título:             data script - Ej 6
# ----------------------- Generación de variables -------------------------

# Borramos todos los objetos.
rm(list=ls())
options(warn=-1)  #Suprimimos los Warnings

a_train_original_data <- "train_original.csv"
s_dataset_faltantes <- "dataset_ruido_"
s_csv <- "_.csv"

df_train_original <- read.csv(a_train_original_data, 
                              header = TRUE, 
                              sep = ",", 
                              as.is = TRUE, 
                              check.names = FALSE)
porc_min_ruido <- 0
porc_max_ruido <- 0.35
porc_step_ruido <- 0.01

# Cantidad de archivos a generar por tipo de ruido.
N <- round((porc_max_ruido - porc_min_ruido) /  porc_step_ruido, 0)

for (i in 1:N){
  
  porc_ruido <- porc_min_ruido + i*porc_step_ruido
  
  # Partimos del data set puro.
  df_train_aux <- df_train_original
  
  # Tomamos el procentaje de filas a nulear al azar.
  df_train_aux_rows <- sample(1:nrow(df_train_aux), porc_ruido*nrow(df_train_aux))
  
  # Agregamos el ruido
  df_train_aux[df_train_aux_rows, ][df_train_aux[df_train_aux_rows,]$y == "yes", "y"] <- "n"
  df_train_aux[df_train_aux_rows, ][df_train_aux[df_train_aux_rows,]$y == "no", "y"] <- "y"
  
  df_train_aux[df_train_aux$y == "n", "y"] <- "no"
  df_train_aux[df_train_aux$y == "y", "y"] <- "yes"
  
  write.table(x = df_train_aux,
              file = paste(s_dataset_faltantes, as.character(porc_ruido*100), s_csv, sep = ''), 
              sep = ",", 
              quote = FALSE,
              row.names = FALSE)
  
}
  
  




