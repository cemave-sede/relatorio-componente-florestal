##### Índices de abundância de tribos ao longo dos anos - Mata Atlântica
## Script: Isabela Freitas Oliveira

library(dplyr)
library(tidyr)


# Definir o diretório de trabalho
setwd("~/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela")
df_indice<-read.csv("resultado_final_com_esforco.csv", header=T, sep = ';') # dataframe para fazer os gráficos
df_indice <- df_indice[!is.na(df_indice$tribo), ]

dados_regiao_especifica <- df_indice %>%
  filter(bioma == "Amazonia", regiao == "sudeste")

# Agregar soma_individuos por tribo e ano
soma_individuos_por_tribo_ano <- dados_regiao_especifica %>%
  filter(soma_individuos > 0 ) %>%
  group_by(tribo, ano) %>%
  summarize(soma_individuos = sum(soma_individuos, na.rm = TRUE)) %>%
  ungroup()

# Verificar o esforço individual por uc_ea e expedicao
esforco_individual <- dados_regiao_especifica %>%
  group_by(uc, uc_ea, ano, expedicao) %>%
  summarize(esforco_individual = unique(esforco_amostral), na.rm = TRUE) %>%
  ungroup()

# Somar os esforços por expedicao
esforco_por_expedicao <- esforco_individual %>%
  group_by(uc, ano, expedicao) %>%
  summarize(esforco_total_expedicao = sum(esforco_individual, na.rm = TRUE)) %>%
  ungroup()

# Calcular o esforço total anual para cada ano
esforco_total_anual <- esforco_por_expedicao %>%
  group_by(ano) %>%
  summarize(esforco_total_anual = sum(esforco_total_expedicao, na.rm = TRUE)) %>%
  ungroup()

# Juntar com soma_individuos_por_tribo_ano
df_agregado <- soma_individuos_por_tribo_ano %>%
  left_join(esforco_total_anual, by = "ano") %>%
  ungroup()

# Calcular o índice de abundância
df_agregado <- df_agregado %>%
  mutate(frequencia_corrigida = (soma_individuos + 0.00001) / esforco_total_anual,
         log_frequencia_corrigida = log10(frequencia_corrigida)) %>%
  group_by(tribo) %>%
  arrange(ano) %>%
  mutate(indice_abundancia = if_else(row_number() == 1, 1, 
                                     log_frequencia_corrigida / lag(log_frequencia_corrigida))) %>%
  ungroup()

# Reestruturar para visualização
df_tribo_ano_largo <- df_agregado %>%
  pivot_wider(names_from = ano, values_from = indice_abundancia)

# Reestruturar o dataframe para o formato largo # tabela geral dos índices
df_tribo_ano_largo <- df_agregado %>%
  select(tribo, ano, indice_abundancia) %>% # Selecionar as colunas relevantes
  pivot_wider(names_from = ano, values_from = indice_abundancia) # Transformar para o formato largo

# Limitar os valores a 3 casas decimais
df_tribo_ano_largo <- df_tribo_ano_largo %>%
  mutate_if(is.numeric, ~ round(., 3))
#write.csv(df_tribo_ano_largo, "tabela_índice_tribo_ano_AM_sudeste.csv", row.names = FALSE)

############
#Gráficos
library(ggplot2)

# Definir as cores e tipos de linha para cada tribo
cores_linhas <- c("Brassolini" = "#003C30", "Morphini" = "#23867E", "Haeterini" = "#97D6CD")
tipos_linhas <- c("Brassolini" = "solid", "Morphini" = "dashed", "Haeterini" = "twodash")

# Filtrar tribos florestais
tribos_selecionadas <- df_tribo_ano_largo %>%
  filter(tribo %in% c("Brassolini", "Morphini", "Haeterini")) %>%
  mutate(tribo = factor(tribo, levels = c("Brassolini", "Morphini", "Haeterini")))

# Transformar 'ano' para o formato longo e garantir que é numérico
dados_longos <- tribos_selecionadas %>%
  pivot_longer(cols = -tribo, names_to = "ano", values_to = "indice_abundancia")

# Certifique-se de que cada 'tribo' tem um 'tipo_linha' correspondente
dados_longos$tipo_linha <- ifelse(dados_longos$tribo == "Brassolini", "solid",
                                  ifelse(dados_longos$tribo == "Morphini","dashed","twodash"))

# Crie o gráfico
gg_florestal <- ggplot(dados_longos, aes(x = ano, y = indice_abundancia, group = tribo, color = tribo, linetype = tribo)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.5,2))+
  scale_color_manual(values = cores_linhas) +
  scale_linetype_manual(values = tipos_linhas) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", size = 1) +
  theme_minimal() +
  labs(title = "Região Climática Sudeste Amazônica",
       subtitle = "Borboletas florestais",
       x = "Ano",
       y = expression("Índice de Abundância (" * italic("i") * ")"),
       color = "Tribo",
       linetype = "Tribo") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.line = element_line(color = "black"),
        axis.line.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = tipos_linhas)))

# Exibir o gráfico
print(gg_florestal)

# Salvar a imagem usando ggsave()
ggsave(filename = "indice de abundancia AM_Sudeste_florestal_linhas_.png",  # Escolha um nome adequado para o arquivo
       plot = gg_florestal,  # O objeto ggplot
       device = "png",  # Pode ser "jpeg", "tiff", etc.
       path = "/Users/isabelafreitasoliveira/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela",  # Especifique o diretório onde você deseja salvar o gráfico
       dpi = 300,  # Define a resolução (DPI)
       width = 12, height = 8, units = "in")  # Define o tamanho da imagem
######
#Gráficos Borboletas de areas abertas

# Definir as cores e tipos de linha para cada tribo
cores_linhas <- c("Callicorini" = "#CFA154", "Biblidini" = "#824B09", "Ageroniini" = "#543005")
tipos_linhas <- c("Callicorini" = "twodash", "Biblidini" = "dashed", "Ageroniini" = "solid")

# Filtrar tribos florestais
tribos_selecionadas <- df_tribo_ano_largo %>%
  filter(tribo %in% c("Callicorini", "Biblidini", "Ageroniini")) %>%
  mutate(tribo = factor(tribo, levels = c("Callicorini", "Biblidini", "Ageroniini")))

# Transformar 'ano' para o formato longo e garantir que é numérico
dados_longos <- tribos_selecionadas %>%
  pivot_longer(cols = -tribo, names_to = "ano", values_to = "indice_abundancia")

# Certifique-se de que cada 'tribo' tem um 'tipo_linha' correspondente
dados_longos$tipo_linha <- ifelse(dados_longos$tribo == "Ageroniini", "solid",
                                  ifelse(dados_longos$tribo == "Biblidini","dashed","twodash"))

# Crie o gráfico
gg_aberto <- ggplot(dados_longos, aes(x = ano, y = indice_abundancia, group = tribo, color = tribo, linetype = tribo)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.5,1.5))+
  scale_color_manual(values = cores_linhas) +
  scale_linetype_manual(values = tipos_linhas) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", size = 1) +
  theme_minimal() +
  labs(title = "",
       subtitle = "Borboletas de áreas abertas",
       x = "Ano",
       y = expression("Índice de Abundância (" * italic("i") * ")"),
       color = "Tribo",
       linetype = "Tribo") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.line = element_line(color = "black"),
        axis.line.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = tipos_linhas)))

# Exibir o gráfico
print(gg_aberto)

# Salvar a imagem usando ggsave()
ggsave(filename = "indice de abundancia_AM_Sudeste_aberto_linhas_.png",  # Escolha um nome adequado para o arquivo
       plot = gg_aberto,  # O objeto ggplot
       device = "png",  # Pode ser "jpeg", "tiff", etc.
       path = "/Users/isabelafreitasoliveira/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela",  # Especifique o diretório onde você deseja salvar o gráfico
       dpi = 300,  # Define a resolução (DPI)
       width = 12, height = 8, units = "in")  # Define o tamanho da imagem

######
#Gráficos juntos
library(gridExtra)

grid.arrange(gg_florestal, gg_aberto, ncol = 1,  heights = c(3,3))
ggsave("graficos_combinados_indice_AM_Sudeste_linhas.png", 
       grid.arrange(gg_florestal, gg_aberto,  ncol = 1,  heights = c(3,3)), 
       device = "png", 
       width = 12, height = 10, units = "in", 
       path = "/Users/isabelafreitasoliveira/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela")
