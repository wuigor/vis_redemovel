library(dplyr)
library("ggpubr")
library(plyr)
library(ggplot2)
library(plotly)
library(rjson)

Encoding("UTF-8")
dados_todos <- read.csv2("dados_2G_3G_4G.csv", header=TRUE, sep=";", dec = ",", encoding="UTF-8")

UFs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
Regioes <- c("Centro-oeste","Nordeste","Norte","Sudeste","Sul")

# Regi„o 4G
dados_4G <- dados_todos
dados_4G["area_4G_coberta_km2"] <- (dados_4G$area_4G * dados_4G$area_km2) / 100
dados_4G["populacao_4G_coberta"] <- (dados_4G$moradores * dados_4G$moradores_4G) / 100

dados_4G_centro_oeste <- dados_4G %>% filter(regiao=="Centro-oeste")
dados_4G_Nordeste <- dados_4G %>% filter(regiao=="Nordeste")
dados_4G_Norte <- dados_4G %>% filter(regiao=="Norte")
dados_4G_Sudeste <- dados_4G %>% filter(regiao=="Sudeste")
dados_4G_Sul <- dados_4G %>% filter(regiao=="Sul")


dados_populacao_regiao_4G <- data.frame(
  "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
  "moradores_coberta"=c(
    (sum(dados_4G_centro_oeste$populacao_4G_coberta) * 100) / sum(dados_4G_centro_oeste$moradores), 
    (sum(dados_4G_Nordeste$populacao_4G_coberta) * 100) / sum(dados_4G_Nordeste$moradores),
    (sum(dados_4G_Norte$populacao_4G_coberta) * 100) / sum(dados_4G_Norte$moradores), 
    (sum(dados_4G_Sudeste$populacao_4G_coberta) * 100) / sum(dados_4G_Sudeste$moradores),
    (sum(dados_4G_Sul$populacao_4G_coberta) * 100) / sum(dados_4G_Sul$moradores) 
  )
)

dados_populacao_regiao_4G1 <- arrange(dados_populacao_regiao_4G,desc(dados_populacao_regiao_4G$moradores_coberta))
fig <- plot_ly(x = dados_populacao_regiao_4G1$moradores_coberta, y = dados_populacao_regiao_4G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regi√µes"), xaxis=list(title = "% Moradores cobertos pela tecnologia 4G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_moradores_4G.html")


# Regi„o 3G
dados_3G <- dados_todos
dados_3G["area_3G_coberta_km2"] <- (dados_3G$area_3G * dados_3G$area_km2) / 100
dados_3G["populacao_3G_coberta"] <- (dados_3G$moradores * dados_3G$moradores_3G) / 100

dados_3G_centro_oeste <- dados_3G %>% filter(regiao=="Centro-oeste")
dados_3G_Nordeste <- dados_3G %>% filter(regiao=="Nordeste")
dados_3G_Norte <- dados_3G %>% filter(regiao=="Norte")
dados_3G_Sudeste <- dados_3G %>% filter(regiao=="Sudeste")
dados_3G_Sul <- dados_3G %>% filter(regiao=="Sul")


dados_populacao_regiao_3G <- data.frame(
  "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
  "moradores_coberta"=c(
    (sum(dados_3G_centro_oeste$populacao_3G_coberta) * 100) / sum(dados_3G_centro_oeste$moradores), 
    (sum(dados_3G_Nordeste$populacao_3G_coberta) * 100) / sum(dados_3G_Nordeste$moradores),
    (sum(dados_3G_Norte$populacao_3G_coberta) * 100) / sum(dados_3G_Norte$moradores), 
    (sum(dados_3G_Sudeste$populacao_3G_coberta) * 100) / sum(dados_3G_Sudeste$moradores),
    (sum(dados_3G_Sul$populacao_3G_coberta) * 100) / sum(dados_3G_Sul$moradores) 
  )
)

dados_populacao_regiao_3G1 <- arrange(dados_populacao_regiao_3G,desc(dados_populacao_regiao_3G$moradores_coberta))
fig <- plot_ly(x = dados_populacao_regiao_3G1$moradores_coberta, y = dados_populacao_regiao_3G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regi√µes"), xaxis=list(title = "% Moradores cobertos pela tecnologia 3G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_moradores_3G.html")


# Regi„o 2G
dados_2G <- dados_todos
dados_2G["area_2G_coberta_km2"] <- (dados_2G$area_2G * dados_2G$area_km2) / 100
dados_2G["populacao_2G_coberta"] <- (dados_2G$moradores * dados_2G$moradores_2G) / 100

dados_2G_centro_oeste <- dados_2G %>% filter(regiao=="Centro-oeste")
dados_2G_Nordeste <- dados_2G %>% filter(regiao=="Nordeste")
dados_2G_Norte <- dados_2G %>% filter(regiao=="Norte")
dados_2G_Sudeste <- dados_2G %>% filter(regiao=="Sudeste")
dados_2G_Sul <- dados_2G %>% filter(regiao=="Sul")


dados_populacao_regiao_2G <- data.frame(
  "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
  "moradores_coberta"=c(
    (sum(dados_2G_centro_oeste$populacao_2G_coberta) * 100) / sum(dados_2G_centro_oeste$moradores), 
    (sum(dados_2G_Nordeste$populacao_2G_coberta) * 100) / sum(dados_2G_Nordeste$moradores),
    (sum(dados_2G_Norte$populacao_2G_coberta) * 100) / sum(dados_2G_Norte$moradores), 
    (sum(dados_2G_Sudeste$populacao_2G_coberta) * 100) / sum(dados_2G_Sudeste$moradores),
    (sum(dados_2G_Sul$populacao_2G_coberta) * 100) / sum(dados_2G_Sul$moradores) 
  )
)

dados_populacao_regiao_2G1 <- arrange(dados_populacao_regiao_2G,desc(dados_populacao_regiao_2G$moradores_coberta))
fig <- plot_ly(x = dados_populacao_regiao_2G1$moradores_coberta, y = dados_populacao_regiao_2G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regi√µes"), xaxis=list(title = "% Moradores cobertos pela tecnologia 2G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_moradores_2G.html")



# Cidades 

dados_4G <- dados_todos
dados_4G["populacao_4G_coberta"] <- (dados_4G$area_4G * dados_4G$area_km2) / 100
dados_4G["populacao_4G_coberta"] <- (dados_4G$moradores * dados_4G$moradores_4G) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"populacao_4G_coberta"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_4G_filtrados <- dados_4G %>% filter(UF==UFs[i])
    if(dados_4G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_4G_filtrados$populacao_4G_coberta) * 100) / sum(dados_4G_filtrados$moradores)
      
      linha <- data.frame("UF"=c(UFs[i]),"populacao_4G_coberta"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
    tabela_estados <- data.frame("cidade"=c(0),"populacao_4G_coberta"=c(0))
    
    numero_cidades <- length(dados_4G_filtrados$regiao)
    if(numero_cidades > 10){
      numero_cidades <- 10
    }
    
    dados_4G_filtrados <- dados_4G_filtrados[order(dados_4G_filtrados$moradores_4G, decreasing = TRUE),]
    
    z <- 1
    for(z in 1:numero_cidades){
      
      linha <- data.frame("cidade"=c(dados_4G_filtrados$cidade[z]),"populacao_4G_coberta"=c(dados_4G_filtrados$moradores_4G[z]))
      tabela_estados <- rbind.fill(tabela_estados, linha)
      
    }
    
    tabela_estados <- tabela_estados[-1, ]
    fig <- plot_ly(x = tabela_estados$populacao_4G_coberta, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
    fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Moradores cobertos pela tecnologia 4G", range=c(0,100)))
    fig
    saida <- paste("dados_cidades_morador_4G_",UFs[i],".html", sep="")
    htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$populacao_4G_coberta, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Moradores cobertos pela tecnologia 4G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_morador_4G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}





# 3G
dados_3G <- dados_todos
dados_3G["populacao_3G_coberta"] <- (dados_3G$area_3G * dados_3G$area_km2) / 100
dados_3G["populacao_3G_coberta"] <- (dados_3G$moradores * dados_3G$moradores_3G) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"populacao_3G_coberta"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_3G_filtrados <- dados_3G %>% filter(UF==UFs[i])
    if(dados_3G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_3G_filtrados$populacao_3G_coberta) * 100) / sum(dados_3G_filtrados$moradores)
      
      linha <- data.frame("UF"=c(UFs[i]),"populacao_3G_coberta"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
      tabela_estados <- data.frame("cidade"=c(0),"populacao_3G_coberta"=c(0))
      
      numero_cidades <- length(dados_3G_filtrados$regiao)
      if(numero_cidades > 10){
        numero_cidades <- 10
      }
      
      dados_3G_filtrados <- dados_3G_filtrados[order(dados_3G_filtrados$moradores_3G, decreasing = TRUE),]
      
      z <- 1
      for(z in 1:numero_cidades){
        
        linha <- data.frame("cidade"=c(dados_3G_filtrados$cidade[z]),"populacao_3G_coberta"=c(dados_3G_filtrados$moradores_3G[z]))
        tabela_estados <- rbind.fill(tabela_estados, linha)
        
      }
      
      tabela_estados <- tabela_estados[-1, ]
      fig <- plot_ly(x = tabela_estados$populacao_3G_coberta, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
      fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Moradores cobertos pela tecnologia 3G", range=c(0,100)))
      fig
      saida <- paste("dados_cidades_morador_3G_",UFs[i],".html", sep="")
      htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$populacao_3G_coberta, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Moradores cobertos pela tecnologia 3G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_morador_3G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}



#2G
dados_2G <- dados_todos
dados_2G["populacao_2G_coberta"] <- (dados_2G$area_2G * dados_2G$area_km2) / 100
dados_2G["populacao_2G_coberta"] <- (dados_2G$moradores * dados_2G$moradores_2G) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"populacao_2G_coberta"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_2G_filtrados <- dados_2G %>% filter(UF==UFs[i])
    if(dados_2G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_2G_filtrados$populacao_2G_coberta) * 100) / sum(dados_2G_filtrados$moradores)
      
      linha <- data.frame("UF"=c(UFs[i]),"populacao_2G_coberta"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
      tabela_estados <- data.frame("cidade"=c(0),"populacao_2G_coberta"=c(0))
      
      numero_cidades <- length(dados_2G_filtrados$regiao)
      if(numero_cidades > 10){
        numero_cidades <- 10
      }
      
      dados_2G_filtrados <- dados_2G_filtrados[order(dados_2G_filtrados$moradores_2G, decreasing = TRUE),]
      
      z <- 1
      for(z in 1:numero_cidades){
        
        linha <- data.frame("cidade"=c(dados_2G_filtrados$cidade[z]),"populacao_2G_coberta"=c(dados_2G_filtrados$moradores_2G[z]))
        tabela_estados <- rbind.fill(tabela_estados, linha)
        
      }
      
      tabela_estados <- tabela_estados[-1, ]
      fig <- plot_ly(x = tabela_estados$populacao_2G_coberta, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
      fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Moradores cobertos pela tecnologia 2G", range=c(0,100)))
      fig
      saida <- paste("dados_cidades_morador_2G_",UFs[i],".html", sep="")
      htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$populacao_2G_coberta, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Moradores cobertos pela tecnologia 2G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_morador_2G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}





