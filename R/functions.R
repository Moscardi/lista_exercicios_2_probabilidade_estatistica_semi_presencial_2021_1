require("tidyverse")
require("ggplot2")
require("e1071")

# Delete all previous data
rm(
  list = ls()
)

# Set work directory to current directory
setwd(
  dir = getwd()
)

initialization <- function() {
  
  #  set disciplina data
  disciplina <<- c(8, 5, 5, 7, 9, 7, 8, 9, 6, 8, 6, 6, 7, 7, 4, 3)
  
  # define image size
  image_size <<- 480
}

compute_Y_for_histogram_vertical_lines <- function() {
  frequencias <- c(rep.int(x = 0, times = max(disciplina) + 1 ))
   
  for (disc in disciplina) {
    frequencias[disc+1] = frequencias[disc+1] + 1
  }
  
  return(frequencias)
}

variancia_amostral <- function() {
  n <- length(disciplina) - 1
  media <- mean(x = disciplina)
  
  somatoria <- 0
  
  for (disc in disciplina) {
    somatoria <- somatoria + (disc - media)^2
  }
  
  return(somatoria/n)
}

desvio_padrao_amostral <- function() {
  
  return(sqrt(x = variancia_amostral()))
}

coeficiente_variacao <- function() {
  return(
    desvio_padrao_amostral()/mean(disciplina)*100
  )
}

escore <- function() {
  s <- desvio_padrao_amostral()
  media <- mean(x = disciplina)
  
  index = 1
  
  escores = c(rep(0,length(seq(0,9,1))))
  
  for (disc in seq(0,9,1)) {
    escores[index] = (disc - media)/s
    index = index + 1
  }
  
  return(escores)
}