library(dplyr)
library("ggpubr")
library(plyr)
library(ggplot2)
library(plotly)
library(rjson)

Encoding("UTF-8")

dados_Algar <- read.csv2("Cobertura_ALGAR.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Claro <- read.csv2("Cobertura_CLARO.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Ligue <- read.csv2("Cobertura_LIGUE.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Nextel <- read.csv2("Cobertura_NEXTEL.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Oi <- read.csv2("Cobertura_OI.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Sercomtel <- read.csv2("Cobertura_SERCOMTEL.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Tim <- read.csv2("Cobertura_TIM.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")
dados_Vivo <- read.csv2("Cobertura_VIVO.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")


UFs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
Regioes <- c("Centro-oeste","Nordeste","Norte","Sudeste","Sul")
Tech <- c("4G","3G","2G")
Setor <- c("Urbano","Rural")
Operadoras <- c("Algar","Claro","Ligue","Nextel","Oi","Sercomtel","Tim","Vivo")
Operadoras2 <- c("Claro","Nextel","Oi","Tim","Vivo")


m <- matrix(rnorm(216), nrow = 27, ncol = 8)

# Algar
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Algar %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) {
    result <- 0
    }
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,1] <- result

}

# CLARO
i <- 0
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Claro %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,2] <- result
  
}

# Ligue
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Ligue %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,3] <- result
  
}

# Nextel
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Nextel %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,4] <- result
  
}

# Oi
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Oi %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,5] <- result
  
}

# Sercomtel
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Sercomtel %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,6] <- result
  
}

# Tim
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Tim %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,7] <- result
  
}

# Vivo
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Vivo %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,8] <- result
  
}



fig1 <- plot_ly(
  x = Operadoras, y = UFs,
  z = m, type = "heatmap",
  coloraxis = 'coloraxis',
  hovertemplate = paste("Estado: %{y}<br> Operadora: %{x} </br> Porcentagem(%): %{z} <extra></extra>")
)

fig1 <- fig1 %>% layout(coloraxis=list(colorscale='Jet'))

#SEGUNDO HEATMAP

m <- matrix(rnorm(135), nrow = 27, ncol = 5)


# CLARO
i <- 0
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Claro %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,1] <- result
  
}


# Nextel
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Nextel %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,2] <- result
  
}

# Oi
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Oi %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,3] <- result
  
}

# Tim
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Tim %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,4] <- result
  
}

# Vivo
i <- 1
for(i in 1:length(UFs)){
  
  dados_filtrados <- dados_Vivo %>% filter(UF==UFs[i])
  if(length(dados_filtrados$UF)==0) result <- 0
  else{
    result <- (sum(dados_filtrados$n_morador_coberto) * 100) / sum(dados_filtrados$Moradores)
  }
  
  m[i,5] <- result
  
}



fig2 <- plot_ly(
  x = Operadoras2, y = UFs,
  z = m, type = "heatmap",
  coloraxis = 'coloraxis',
  hovertemplate = paste('Estado: %{y}<br> Operadora: %{x} </br> Porcentagem(%): %{z} <extra></extra>')
)

fig2 <- fig2 %>% layout(coloraxis=list(colorscale='Jet'))


fig1
fig2

saida <- paste("Heatmap_4G_Todas",".html", sep = "")
htmlwidgets::saveWidget(as_widget(fig1), saida)
saida <- paste("Heatmap_4G_maior_cobertura",".html", sep = "")
htmlwidgets::saveWidget(as_widget(fig2), saida)


