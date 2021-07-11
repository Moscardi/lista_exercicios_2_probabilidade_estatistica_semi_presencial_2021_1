# Set work directory to current directory
setwd(
  dir = getwd()
)

source(file = "./functions.R")

initialization()

jpeg(
  filename = "../fig/d-graph-3.jpeg",
  width = image_size,
  height = image_size/2
)

Y_axis <-compute_Y_for_histogram_vertical_lines()

par(bty="o")
plot.default (
  x = c(0:max(disciplina)),
  y = Y_axis, 
  type = "p", 
  xlim = c(0,max(disciplina)), 
  ylim = c(0,max(Y_axis)),
  xlab = "Nº de disciplinas matriculadas", 
  ylab = "Frequência",
  main = "Diagrama de frequência para dados não agrupados em classe"
)

dev.off()

jpeg(
  filename = "../fig/d-graph-4.jpeg",
  width = image_size,
  height = image_size/2
)

par(bty="o")
plot (
  x = c(0:max(disciplina)),
  y = Y_axis, 
  type = "h", 
  xlim = c(0,max(disciplina)), 
  ylim = c(0,max(Y_axis)),
  xlab = "Nº de disciplinas matriculadas", 
  ylab = "Frequência"
)

dev.off()

# Create graph 5 for d) question
jpeg(
  filename = "../fig/d-graph-5.jpeg",
  width = image_size,
  height = image_size/2
)

par(bty="o")
boxplot(
  x = disciplina,
  horizontal = TRUE,
  xlab = 'N° de disciplinas matriculadas',
  range = 1.5,
  pch = 8,
  ylim = c(0, max(disciplina) + min(disciplina))
)

dev.off()

percentis <- quantile(
  x = sort.default(x = disciplina),
  probs = seq(0, 1, 0.1),
  type = 1
)

quartis <-quantile(
  x = sort.default(x = disciplina),
  probs = seq(0, 1, 0.25),
  type = 1
)

p10 <- percentis[1]
p90 <- percentis[10]
q1 <- quartis[1]
q2 <- quartis[2]
q3 <- quartis[3]
q4 <- quartis[4]


media <- mean(x = disciplina)

variancia <- variancia_amostral()

desvio_padrao <- desvio_padrao_amostral()

variacao <- coeficiente_variacao()

assimetria <- skewness(x = disciplina, type = 1)

escores <- escore()








