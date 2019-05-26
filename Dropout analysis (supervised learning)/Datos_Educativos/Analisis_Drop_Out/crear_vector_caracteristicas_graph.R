library(dplyr)
library(ggplot2)
library(igraph)
library(stringr)

df_malla <- read.csv('MALLA_DATASET.csv', header = TRUE,sep = ';')

g_malla.nodes <- data.frame(
  id = df_malla[!duplicated(df_malla$Requisito),c('Requisito')],
  name = df_malla[!duplicated(df_malla$Requisito),c('Requisito')],
  level = df_malla[!duplicated(df_malla$Requisito),c('Semestre')]
)
g_malla.edges <- data.frame(
  from = df_malla[df_malla$Siguiente != '',c('Requisito')],
  to = df_malla[df_malla$Siguiente != '',c('Siguiente')]
)

grafo_malla <- graph_from_data_frame(g_malla.edges, directed=TRUE, vertices=g_malla.nodes)

V(grafo_malla)$level <- g_malla.nodes$level
V(grafo_malla)$level

l <- layout_with_sugiyama(grafo_malla, layers = -V(grafo_malla)$level)$layout

estructura <- function(grafo, estudiante){
  wtc <-cluster_walktrap(grafo)
  g<- data.frame(estudiante = estudiante, 
                 nodos = vcount(grafo),
                 aristas = ecount(grafo),
                 densidad = round(graph.density(grafo), 2),
                 diametro = diameter(grafo),
                 long.med.cam = round(average.path.length(grafo),2),
                 grado_medio = round(mean(degree(grafo)),2),
                 modularidad = round(modularity(grafo,membership(wtc)),2),
                 coeficiente_cluster = round(transitivity(grafo),2)
                 )
  return(g)
}

#relaciones <-function(grafo){
#  r<-data.frame(relaciones =c(
#    "potenciales", "presentes",
#    "% presentes"),
#    valor =c((vcount(grafo)*(vcount(grafo)-1))/2,
#             ecount(grafo),
#             round(graph.density(grafo)*100,2)))
#  return(r)
#}

df_estudiantes <- read.csv('estudiantes.csv', header = TRUE)
#cambiar codigo de cursos mal codificados ( A000XX -> A00XX, A000XXX -> A0XXX)
df_estudiantes$curso <- lapply(df_estudiantes$curso, function(x){ 
  if(str_length(x)==6){str_replace(x,"A000","A00")}
  else if(str_length(x)==7){str_replace(x,"A000","A0")}
  else{str_replace(x,'A00','A00')}
})
#Quitar filas de asignaturas que no estan en la malla ( asignaturas con codigo > A0060)
df_estudiantes <- df_estudiantes[as.integer(substr(df_estudiantes$curso,3,5))<=60,]
#Quitar estudiantes que no cursaron el primer semestre en el periodo de tiempo
validos <- unique(as.character(df_estudiantes[as.integer(substr(df_estudiantes$curso,3,5))<=4,'estudiante']))
df_estudiantes <- df_estudiantes[df_estudiantes$estudiante %in% validos,]
df_estudiantes<- as.data.frame(lapply(df_estudiantes, unlist))
repetidos <- df_estudiantes[,c('estudiante','curso')] %>% group_by(estudiante,curso) %>% tally()
repetidos <- repetidos[as.integer(repetidos$n) > 1,]


todos <- unique(as.character(df_estudiantes$estudiante))
largo <- length(unique(as.character(df_estudiantes$estudiante)))
y <- c("estudiante","nodos", "aristas", "densidad","diametro","long.med.cam","grado_medio","modularidad","coeficiente_cluster")
vector_caracteristicas <- data.frame(matrix(ncol= length(y), nrow = 0))
colnames(vector_caracteristicas)<-y






for(i in 1:largo){
  seleccionado <-todos[i]
  cursado <-df_estudiantes[df_estudiantes$estudiante == seleccionado,c('curso')]
  estudiante <- g_malla.nodes
  cursado
  
  estudiante1.edges <- g_malla.edges
  #estudiante1.edges$color <- as.character(estudiante1.edges$color)
  estudiante1.edges[(estudiante1.edges$from %in% cursado & estudiante1.edges$to %in% cursado),c('color')] <- '#000000'
  
  estudiante.edges <- estudiante1.edges[!is.na(estudiante1.edges$color),c('from','to')]
  sin_repetir <- estudiante.edges
  
  if(seleccionado %in% repetidos$estudiante){
    temp <- repetidos[repetidos$estudiante == seleccionado,]
    for (i in 1:nrow(temp[temp$estudiante == seleccionado,])) {
      for(j in 1:as.integer(temp[i,'n'] %>% .$n)){
        estudiante.edges <- rbind(estudiante.edges,data.frame(
          from=as.character(temp[i,'curso'] %>% .$curso),
          to=as.character(temp[i,'curso'] %>% .$curso)
        )
        )
      }
    }
  }
  estudiante.edges
  
  
  repetidos$estudiante
  
  grafo_estudiante <- graph_from_data_frame(estudiante.edges, directed=TRUE, vertices=estudiante)
  vector_caracteristicas <- rbind(vector_caracteristicas,estructura(grafo_estudiante,seleccionado))
}

View(vector_caracteristicas)



