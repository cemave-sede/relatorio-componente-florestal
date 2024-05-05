# Gráficos de dados Borboletas- Protocolo básico
# Numero de UC's e EA's implementadas 214-2022
# Script: Isabela Freitas Oliveira

library(tidyverse)
library(ggplot2)
library(stringr)

dados_borboletas <- read.table("dados_analise_borboletas_Isabela_V2.txt", header = TRUE)
dados_borboletas$ano <- factor(dados_borboletas$ano)
dados_borboletas <- dados_borboletas[dados_borboletas$ano != "2023", ]

# Preparar os dados para o gráfico
contagem_uc_ea <- dados_borboletas %>%
  group_by(ano) %>%
  summarise(
    num_uc = n_distinct(uc),
    num_ea = n_distinct(uc_ea)
  )

dados_long <- contagem_uc_ea %>%
  pivot_longer(
    cols = -ano,
    names_to = "categoria",
    values_to = "contagem"
  )

dados_long$categoria <- factor(dados_long$categoria, levels = c("num_uc","num_ea"))

ggplot(dados_long, aes(x = ano, y = contagem, fill = categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = contagem), 
    position = position_dodge(width = 0.9), # ajustar 'width' para alinhar com as barras
    vjust = -0.25, # ajustar 'vjust' para mover o texto um pouco acima das barras
    size = 5
  ) +
  scale_fill_manual(
    values = c("num_uc" = "#35978f", "num_ea" = "#c7eae5"),
    labels = c("N˚de UC's", "N˚de EA's")) +
  labs(
    x = "",
    y = "",
    fill = "",
    title = str_wrap("Número de UC's e EA's implementadas ao longo dos anos", width = 35) # Quebra o título em duas linhas
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, hjust = 0, lineheight = 1.3),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 18), 
    axis.text.x = element_text(size = 16),  
    legend.position = "bottom",
    axis.ticks.y = element_blank(), # remove ticks do eixo Y
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),  # remove as linhas principais da grade
    panel.grid.minor = element_blank()# remove texto do eixo Y
  )
