##Arrumando o dataframe para calcular o esforço de cada campanha
#Script: Isabela Freitas Oliveira

library(dplyr)

# Ler e limpar dados_borboletas
dados_borboletas <- read.table("dados_analise_borboletas_Isabela_V2.txt", header = TRUE)
dados_borboletas <- dados_borboletas %>%
  filter(ano != "2023", !is.na(tribo))



# Calcular o esforço amostral para cada combinação de uc_ea, expedicao e ano
esforco_por_expedicao <- dados_borboletas %>%
  group_by(uc_ea, uc, expedicao, ano) %>%
  summarize(
    max_dia_amostragem = max(dia_amostragem) - 1,
    .groups = 'drop'
  ) %>%
  mutate(
    esforco_amostral = 16 * max_dia_amostragem
  )

# Agregar o esforço de amostragem para cada uc_ea e ano
esforco_total_por_ea_ano <- esforco_por_expedicao %>%
  group_by(uc_ea, ano) %>%
  summarize(
    esforco_total_ano = sum(esforco_amostral),
    .groups = 'drop'
  )

# Agregar o esforço de amostragem para cada uc e ano
esforco_total_por_uc_ano <- esforco_por_expedicao %>%
  group_by(uc, ano) %>%
  summarize(
    esforco_total_ano = sum(esforco_amostral),
    .groups = 'drop'
  )


# Agregação para obter o esforço total por ano por uc
esforco_por_ano_uc <- esforco_total_por_uc_ano %>%
  group_by(ano,uc) %>%
  summarize(
    esforco = sum(esforco_total_ano),
    .groups = 'drop'
  )

# Agregação para obter o esforço total por ano
esforco_por_ano <- esforco_total_por_uc_ano %>%
  group_by(ano) %>%
  summarize(
    esforco = sum(esforco_total_ano),
    .groups = 'drop'
  )

# Agregação para obter o número total de indivíduos por ano
num_ind_por_ano <- dados_borboletas %>%
  group_by(ano) %>%
  summarize(
    num_ind = sum(individuos, na.rm = TRUE),
    .groups = 'drop'
  )


# Juntar as duas tabelas agregadas
tabela_final_esfoco <- left_join(esforco_por_ano, num_ind_por_ano, by = "ano")
#write.csv(tabela_final_esfoco, "tabela_final_esfoco.csv", row.names = FALSE)


#####Fazer o gráfico de Abundancia e esforço
# Definindo os dados para cada série separadamente
dados_esforco <- tabela_final_esfoco[, c("ano", "esforco")]
names(dados_esforco)[2] <- "valor"
dados_esforco$tipo <- "Esforço"

dados_abundancia <- tabela_final_esfoco[, c("ano", "num_ind")]
names(dados_abundancia)[2] <- "valor"
dados_abundancia$tipo <- "Abundância"

# Juntando os dados
dados_plot <- rbind(dados_esforco, dados_abundancia)

# Criando o gráfico
grafico <- ggplot(data = dados_plot, aes(x = ano, y = valor, colour = tipo, linetype = tipo, shape = tipo)) +
  geom_line(size = 1.8) +
  geom_point(size = 5) +
  scale_colour_manual(values = c("Esforço" = "#35978f", "Abundância" = "#dfc27d")) +
  scale_linetype_manual(values = c("Esforço" = "solid", "Abundância" = "solid")) +
  scale_shape_manual(values = c("Esforço" = 16, "Abundância" = 17)) +
  scale_y_continuous(
    "Esforço Amostral e Abundância",
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(0, 25000)
  ) +
  scale_x_continuous(breaks = tabela_final_esfoco$ano) +
  labs(
    title = "",
    x = "Ano",
    y = "Esforço Amostral e Abundância",
    colour = "",
    shape = "", # Isso ajuda a garantir que as formas correspondam na legenda
    linetype = "" # Isso ajuda a garantir que os tipos de linha correspondam na legenda
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = -0.5, size = 18, lineheight = 1.5), # Ajuste o lineheight conforme necessário
    legend.position = c(0.05, 0.95), # Define a posição da legenda no canto superior esquerdo
    legend.justification = c(0, 1), # Define a justificação da legenda para o canto superior esquerdo
    legend.box.just = "left", # Alinha a legenda à esquerda
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(margin = margin(r = 12)),
    axis.text.x = element_text(margin = margin(r = 12)),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    axis.line = element_line(colour = "gray40"),
    legend.text = element_text(size = 18),
    axis.line.x = element_blank()
  )

print(grafico)
