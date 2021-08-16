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

# Região 4G
dados_4G <- dados_todos
dados_4G["area_4G_coberta_km2"] <- (dados_4G$area_4G * dados_4G$area_km2) / 100

dados_4G_centro_oeste <- dados_4G %>% filter(regiao=="Centro-oeste")
dados_4G_Nordeste <- dados_4G %>% filter(regiao=="Nordeste")
dados_4G_Norte <- dados_4G %>% filter(regiao=="Norte")
dados_4G_Sudeste <- dados_4G %>% filter(regiao=="Sudeste")
dados_4G_Sul <- dados_4G %>% filter(regiao=="Sul")


dados_area_regiao_4G <- data.frame(
   "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
   "area_coberta"=c(
     (sum(dados_4G_centro_oeste$area_4G_coberta_km2) * 100) / sum(dados_4G_centro_oeste$area_km2), 
     (sum(dados_4G_Nordeste$area_4G_coberta_km2) * 100) / sum(dados_4G_Nordeste$area_km2),
     (sum(dados_4G_Norte$area_4G_coberta_km2) * 100) / sum(dados_4G_Norte$area_km2), 
     (sum(dados_4G_Sudeste$area_4G_coberta_km2) * 100) / sum(dados_4G_Sudeste$area_km2),
     (sum(dados_4G_Sul$area_4G_coberta_km2) * 100) / sum(dados_4G_Sul$area_km2) 
     )
)
#dados_area_regiao_4G1 <- dados_area_regiao_4G[order(dados_area_regiao_4G$area_coberta),]
dados_area_regiao_4G1 <- arrange(dados_area_regiao_4G,desc(dados_area_regiao_4G$area_coberta))
fig <- plot_ly(x = dados_area_regiao_4G1$area_coberta, y = dados_area_regiao_4G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regiões"), xaxis=list(title = "% Área coberta pela tecnologia 4G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_area_4G.html")


# Regigão 3G
dados_3G <- dados_todos
dados_3G["area_3G_coberta_km2"] <- (dados_3G$area_3G * dados_3G$area_km2) / 100

dados_3G_centro_oeste <- dados_3G %>% filter(regiao=="Centro-oeste")
dados_3G_Nordeste <- dados_3G %>% filter(regiao=="Nordeste")
dados_3G_Norte <- dados_3G %>% filter(regiao=="Norte")
dados_3G_Sudeste <- dados_3G %>% filter(regiao=="Sudeste")
dados_3G_Sul <- dados_3G %>% filter(regiao=="Sul")


dados_area_regiao_3G <- data.frame(
  "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
  "area_coberta"=c(
    (sum(dados_3G_centro_oeste$area_3G_coberta_km2) * 100) / sum(dados_3G_centro_oeste$area_km2), 
    (sum(dados_3G_Nordeste$area_3G_coberta_km2) * 100) / sum(dados_3G_Nordeste$area_km2),
    (sum(dados_3G_Norte$area_3G_coberta_km2) * 100) / sum(dados_3G_Norte$area_km2), 
    (sum(dados_3G_Sudeste$area_3G_coberta_km2) * 100) / sum(dados_3G_Sudeste$area_km2),
    (sum(dados_3G_Sul$area_3G_coberta_km2) * 100) / sum(dados_3G_Sul$area_km2) 
  )
)

dados_area_regiao_3G1 <- arrange(dados_area_regiao_3G,desc(dados_area_regiao_3G$area_coberta))
fig <- plot_ly(x = dados_area_regiao_3G1$area_coberta, y = dados_area_regiao_3G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regiões"), xaxis=list(title = "% Área coberta pela tecnologia 3G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_area_3G.html")




# Região 2G
dados_2G <- dados_todos
dados_2G["area_2G_coberta_km2"] <- (dados_2G$area_2G * dados_2G$area_km2) / 100

dados_2G_centro_oeste <- dados_2G %>% filter(regiao=="Centro-oeste")
dados_2G_Nordeste <- dados_2G %>% filter(regiao=="Nordeste")
dados_2G_Norte <- dados_2G %>% filter(regiao=="Norte")
dados_2G_Sudeste <- dados_2G %>% filter(regiao=="Sudeste")
dados_2G_Sul <- dados_2G %>% filter(regiao=="Sul")


dados_area_regiao_2G <- data.frame(
  "Regiao"=c("Centro-oeste","Nordeste","Norte","Sudeste","Sul"),
  "area_coberta"=c(
    (sum(dados_2G_centro_oeste$area_2G_coberta_km2) * 100) / sum(dados_2G_centro_oeste$area_km2), 
    (sum(dados_2G_Nordeste$area_2G_coberta_km2) * 100) / sum(dados_2G_Nordeste$area_km2),
    (sum(dados_2G_Norte$area_2G_coberta_km2) * 100) / sum(dados_2G_Norte$area_km2), 
    (sum(dados_2G_Sudeste$area_2G_coberta_km2) * 100) / sum(dados_2G_Sudeste$area_km2),
    (sum(dados_2G_Sul$area_2G_coberta_km2) * 100) / sum(dados_2G_Sul$area_km2) 
  )
)

dados_area_regiao_2G1 <- arrange(dados_area_regiao_2G,desc(dados_area_regiao_2G$area_coberta))
fig <- plot_ly(x = dados_area_regiao_2G1$area_coberta, y = dados_area_regiao_2G1$Regiao, type = 'bar', orientation = 'h')
fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Regiões"), xaxis=list(title = "% Área coberta pela tecnologia 2G",range=c(0,100)))

fig
htmlwidgets::saveWidget(as_widget(fig), "dados_regiao_area_2G.html")




# Estados
dados_4G <- dados_todos
dados_4G["area_4G_coberta_km2"] <- (dados_4G$area_4G * dados_4G$area_km2) / 100


k <- 0
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"area_4G_coberta_km2"=c(0))
  
  i <- 0
  for(i in 1:length(UFs)){
    
    dados_4G_filtrados <- dados_4G %>% filter(UF==UFs[i])
    if(dados_4G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_4G_filtrados$area_4G_coberta_km2) * 100) / sum(dados_4G_filtrados$area_km2)
      
      linha <- data.frame("UF"=c(UFs[i]),"area_4G_coberta_km2"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    
    
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$area_4G_coberta_km2, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Área coberta pela tecnologia 4G"))
  
  fig
  
  saida <- paste("dados_regiao_area_4G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)

}






# Cidades

dados_4G <- dados_todos
dados_4G["area_4G_coberta_km2"] <- (dados_4G$area_4G * dados_4G$area_km2) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"area_4G_coberta_km2"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_4G_filtrados <- dados_4G %>% filter(UF==UFs[i])
    if(dados_4G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_4G_filtrados$area_4G_coberta_km2) * 100) / sum(dados_4G_filtrados$area_km2)
      
      linha <- data.frame("UF"=c(UFs[i]),"area_4G_coberta_km2"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
    tabela_estados <- data.frame("cidade"=c(0),"area_4G_coberta_km2"=c(0))
    
    numero_cidades <- length(dados_4G_filtrados$regiao)
    if(numero_cidades > 10){
      numero_cidades <- 10
    }
    
    dados_4G_filtrados <- dados_4G_filtrados[order(dados_4G_filtrados$area_4G, decreasing = TRUE),]
    
    z <- 1
    for(z in 1:numero_cidades){
      
      linha <- data.frame("cidade"=c(dados_4G_filtrados$cidade[z]),"area_4G_coberta_km2"=c(dados_4G_filtrados$area_4G[z]))
      tabela_estados <- rbind.fill(tabela_estados, linha)
      
    }
    
    tabela_estados <- tabela_estados[-1, ]
    fig <- plot_ly(x = tabela_estados$area_4G_coberta_km2, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
    fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Área coberta pela tecnologia 4G", range=c(0,100)))
    fig
    saida <- paste("dados_cidades_area_4G_",UFs[i],".html", sep="")
    htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$area_4G_coberta_km2, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Área coberta pela tecnologia 4G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_area_4G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}


#3G
dados_3G <- dados_todos
dados_3G["area_3G_coberta_km2"] <- (dados_3G$area_3G * dados_3G$area_km2) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"area_3G_coberta_km2"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_3G_filtrados <- dados_3G %>% filter(UF==UFs[i])
    if(dados_3G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_3G_filtrados$area_3G_coberta_km2) * 100) / sum(dados_3G_filtrados$area_km2)
      
      linha <- data.frame("UF"=c(UFs[i]),"area_3G_coberta_km2"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
      tabela_estados <- data.frame("cidade"=c(0),"area_3G_coberta_km2"=c(0))
      
      numero_cidades <- length(dados_3G_filtrados$regiao)
      if(numero_cidades > 10){
        numero_cidades <- 10
      }
      
      dados_3G_filtrados <- dados_3G_filtrados[order(dados_3G_filtrados$area_3G, decreasing = TRUE),]
      
      z <- 1
      for(z in 1:numero_cidades){
        
        linha <- data.frame("cidade"=c(dados_3G_filtrados$cidade[z]),"area_3G_coberta_km2"=c(dados_3G_filtrados$area_3G[z]))
        tabela_estados <- rbind.fill(tabela_estados, linha)
        
      }
      
      tabela_estados <- tabela_estados[-1, ]
      fig <- plot_ly(x = tabela_estados$area_3G_coberta_km2, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
      fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Área coberta pela tecnologia 3G", range=c(0,100)))
      fig
      saida <- paste("dados_cidades_area_3G_",UFs[i],".html", sep="")
      htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$area_3G_coberta_km2, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Área coberta pela tecnologia 3G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_area_3G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}

#2G
dados_2G <- dados_todos
dados_2G["area_2G_coberta_km2"] <- (dados_2G$area_2G * dados_2G$area_km2) / 100


k <- 1
for(k in 1:length(Regioes)){
  
  tabela_regiao <- data.frame("UF"=c(0),"area_2G_coberta_km2"=c(0))
  
  i <- 1
  for(i in 1:length(UFs)){
    
    dados_2G_filtrados <- dados_2G %>% filter(UF==UFs[i])
    if(dados_2G_filtrados$regiao[1]==Regioes[k]){  
      result <- (sum(dados_2G_filtrados$area_2G_coberta_km2) * 100) / sum(dados_2G_filtrados$area_km2)
      
      linha <- data.frame("UF"=c(UFs[i]),"area_2G_coberta_km2"=c(result))
      tabela_regiao <<- rbind.fill(tabela_regiao, linha)
      
    }
    
    if(k==1){
      tabela_estados <- data.frame("cidade"=c(0),"area_2G_coberta_km2"=c(0))
      
      numero_cidades <- length(dados_2G_filtrados$regiao)
      if(numero_cidades > 10){
        numero_cidades <- 10
      }
      
      dados_2G_filtrados <- dados_2G_filtrados[order(dados_2G_filtrados$area_2G, decreasing = TRUE),]
      
      z <- 1
      for(z in 1:numero_cidades){
        
        linha <- data.frame("cidade"=c(dados_2G_filtrados$cidade[z]),"area_2G_coberta_km2"=c(dados_2G_filtrados$area_2G[z]))
        tabela_estados <- rbind.fill(tabela_estados, linha)
        
      }
      
      tabela_estados <- tabela_estados[-1, ]
      fig <- plot_ly(x = tabela_estados$area_2G_coberta_km2, y = tabela_estados$cidade, type = 'bar', orientation = 'h')
      fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Cidades"), xaxis=list(title = "% Área coberta pela tecnologia 2G", range=c(0,100)))
      fig
      saida <- paste("dados_cidades_area_2G_",UFs[i],".html", sep="")
      htmlwidgets::saveWidget(as_widget(fig), saida)
    } 
  }
  
  tabela_regiao <- tabela_regiao[-1, ]
  fig <- plot_ly(x = tabela_regiao$area_2G_coberta_km2, y = tabela_regiao$UF, type = 'bar', orientation = 'h')
  fig <- fig%>% layout(yaxis=list(categoryorder = "sum ascending", title = "Estados"), xaxis=list(title = "% Área coberta pela tecnologia 2G", range=c(0,100)))
  
  fig
  
  saida <- paste("dados_regiao_area_2G_",Regioes[k],".html", sep="")
  htmlwidgets::saveWidget(as_widget(fig), saida)
  
}

