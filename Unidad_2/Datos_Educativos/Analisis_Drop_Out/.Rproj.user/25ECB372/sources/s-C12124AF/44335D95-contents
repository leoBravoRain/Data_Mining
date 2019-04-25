# Set working directory
setwd("/home/leo/Escritorio/magister_uach/MAD/Unidad 2/Datos_Educativos/Analisis_Drop_Out/")

# Install pacakage for read excel file
# install.packages("readxl")

# Load library
# Plot library
library('ggplot2')

# Load library for read excel file
library("readxl")

# library for multiple plots
library(magrittr)
library(ggpubr)

# Read excel file
file_name    <- "estudiantes_dropout.xlsx"
df <- read_excel(file_name, col_names = TRUE )

# Drop na rows
df <- df[complete.cases(df),]

# Get courses
courses = unique(df$Asignatura)

# iterate over each course
for(course in courses){

  # Get data from specific course
  df_tmp = df[ (df$Asignatura == course), ]
  
  # title
  title = paste('Course', course, sep = ' ')
  
  # Mean plot with its errors
  mean_plot <- ggerrorplot(df_tmp, x = "Agno", y = "Calificacion", 
              desc_stat = "mean_se",
              error.plot = "errorbar",            # Change error plot type
              add = "dotplot",
              add.params = list(color = "darkgray")
              
          ) + ggtitle(title)
  
  # # Scatter plot
  # scat <- ggplot(df_tmp, aes(x = Agno, y = Calificacion, color = Agno)) + ggtitle(title) + geom_point()
    
  # Plot histogram for analyse distribution of each clasificaction plot_hist <-
  hist <- ggplot(df_tmp, aes(x = Calificacion)) + geom_histogram(binwidth = 0.1) + ggtitle(title)

  # PLot califications v/s students for each course
  plot_box <- ggplot(df_tmp, aes(x = Agno, y = Calificacion, color = Agno, group = cut_width(Agno, 1))) +  geom_boxplot() + ggtitle(title)

  # Displayy multiple plots
  plot(ggarrange(hist, plot_box, mean_plot,
                 ncol = 1, nrow = 3))
}
  