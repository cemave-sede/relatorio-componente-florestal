# Análise  de efeito da queda do tabocal nas tribos de borboletas frugívoras
# RESEX Cazumbá-Iracema
# Script: Isabela Freitas Oliveira

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

#setwd("~/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela")

dados_borboletas <- read.table("data/dados_analise_borboletas_Isabela_V2.txt", header = TRUE)
dados_borboletas$ano <- as.factor(dados_borboletas$ano)
dados_borboletas <- dados_borboletas[dados_borboletas$ano != "2023", ]
dados_borboletas <- dados_borboletas[!is.na(dados_borboletas$tribo), ]


dados_regiao_especifica <- dados_borboletas %>%
  filter(uc == 'Resex_do_Cazumba-Iracema')

# Calcular o esforço amostral para cada combinação de uc_ea, expedicao e ano
esforco_por_expedicao <- dados_regiao_especifica %>%
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

# Agregação para obter o esforço total por ano
esforco_por_ano <- esforco_total_por_uc_ano %>%
  group_by(ano,uc) %>%
  summarize(
    esforco = sum(esforco_total_ano),
    .groups = 'drop'
  )

# Agregação para obter o número total de indivíduos por ano
num_ind_por_ano <- dados_regiao_especifica %>%
  group_by(ano) %>%
  summarize(
    num_ind = sum(individuos, na.rm = TRUE),
    .groups = 'drop'
  )

#criando uma coluna de UA e selecionando somente as colunas interessadas pra analise
dados_regiao_especifica <- dados_regiao_especifica %>%
  mutate(ua = paste(uc_ea, transeccao, sep = "_"))

#combinando os dados por UA por ano = 12 (3 ea x 4 parcelas)
df_abu_ano <- dados_regiao_especifica %>%
  group_by(tribo, ua, uc_ea,uc, ano) %>%
  summarise(abundancia = sum(individuos, na.rm = TRUE)) %>%
  ungroup()

# Transforma o df para colocar os zeros
df_tabocal <- df_abu_ano %>%
  spread(key = tribo, value = abundancia, fill = 0)

# Retorna ao formato longo para as análises
df_long <- df_tabocal %>%
  pivot_longer(cols = Ageroniini:Satyrini, names_to = "tribo", values_to = "individuos")

# Agregação para obter a abundância total de cada tribo por ano
abundancia_por_ano <- df_long %>%
  group_by(tribo, ano) %>%
  summarize(
    abundancia_total = sum(individuos)
  )

# Juntar as tabelas pela coluna 'ano' e 'uc'
df_long_com_esforco <- left_join(df_long, esforco_por_ano, by = c("ano", "uc"))


# Calcular a abundância padronizada pelo esforço amostral
df_long <- df_long_com_esforco %>%
  mutate(abundancia_padronizada = (individuos / esforco) * 100)
#

library(tidyverse)

df_ageroniini<- df_long %>%
  filter(tribo == "Ageroniini")

# Converta a coluna 'ano' para um fator
df_ageroniini$ano <- as.factor(df_ageroniini$ano)

# Calcule a mediana anual para cada ano
medianas_anuais <- tapply(df_ageroniini$abundancia_padronizada, df_ageroniini$ano, median)

# Calcule a mediana geral das medianas anuais
mediana_geral_anual <- median(medianas_anuais, na.rm = TRUE)

# Exiba a mediana geral anual
mediana_geral_anual

# Remova os anos com NA das medianas anuais
medianas_anuais <- medianas_anuais[!is.na(medianas_anuais)]

# Crie um vetor para armazenar os resultados dos testes
resultados_teste_anual <- numeric(length(medianas_anuais))

# Faça um loop pelos anos e realize o teste de Wilcoxon para cada ano
for (ano_atual in names(medianas_anuais)) {
  # Realiza o teste de Wilcoxon comparando as medianas do ano atual com a mediana geral
  wilcox_test_resultado <- wilcox.test(df_ageroniini$abundancia_padronizada[df_ageroniini$ano == ano_atual], mu = mediana_geral_anual, exact = FALSE)
  
  # Armazena o valor p do teste no vetor de resultados
  resultados_teste_anual[ano_atual] <- wilcox_test_resultado$p.value
}

# Crie um data frame com as medianas anuais, a mediana geral anual e os valores-p
df_plot <- data.frame(Ano = names(medianas_anuais), MedianaAnual = medianas_anuais)
df_plot$MedianaGeral <- mediana_geral_anual
df_plot$ValorP <- resultados_teste_anual[match(df_plot$Ano, names(resultados_teste_anual))]

# Calcule os erros padrão para cada ano
erros_padrao <- sapply(names(medianas_anuais), function(ano) {
  dados_ano <- df_ageroniini$abundancia_padronizada[df_ageroniini$ano == ano]
  desvio_padrao_ano <- sd(dados_ano, na.rm = TRUE)
  erro_padrao_ano <- desvio_padrao_ano / sqrt(length(dados_ano))
  return(erro_padrao_ano)
})

# Adicione os intervalos de confiança ao data frame
df_plot$ErroPadrao <- erros_padrao
df_plot$IntervaloConfiancaInf <- pmax(df_plot$MedianaAnual - 1.96 * df_plot$ErroPadrao, 0)
df_plot$IntervaloConfiancaSup <- df_plot$MedianaAnual + 1.96 * df_plot$ErroPadrao

# Adiciona a linha do ano de 2020 ao dataframe
df_plot <- rbind(df_plot, data.frame(Ano = "2020", MedianaAnual = NA, MedianaGeral = NA, ValorP = NA, ErroPadrao = NA, IntervaloConfiancaInf = NA, IntervaloConfiancaSup = NA))

# Crie o gráfico ajustado com pontos e linhas
p_ageroniini <- ggplot(df_plot, aes(x = Ano, y = MedianaAnual)) +
  geom_point(color = "#bf812d", size = 3) +
  geom_line(size = 1, group = 1, color = "#bf812d") +  # Use group = 1 para conectar todos os pontos
  geom_errorbar(aes(ymin = IntervaloConfiancaInf, ymax = IntervaloConfiancaSup), width = 0.2, color = "#bf812d") +
  geom_hline(yintercept = mediana_geral_anual, linetype = "dashed", color = "red") +
  labs(title = "",
       subtitle = "",
       x = "", #legenda do eixo x
       y = "") + #legenda do eixo y
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.line = element_line(color = "black"),
        axis.line.x = element_blank(),
        legend.position = "none") +  # Remover a legenda
  ylim(0, 40) +  # Ajuste os limites conforme necessário
  geom_text(data = df_plot[df_plot$ValorP < 0.05, ], 
            aes(x = Ano, y = IntervaloConfiancaSup, label = "*"), 
            vjust = -0.5,  
            size = 10)

# Exiba o gráfico ajustado
print(p_ageroniini)

# Salvar a imagem usando ggsave()
ggsave(filename = "mediana_ageroniini_ajustado_pontos_linha_2020.png",  # Escolha um nome adequado para o arquivo
       plot = p_ageroniini,  # O objeto ggplot
       device = "png",  # Pode ser "jpeg", "tiff", etc.
       path = "/Users/isabelafreitasoliveira/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela/Destaque/Cazumbá-Iracema",  # Especifique o diretório onde você deseja salvar o gráfico
       dpi = 300,  # Define a resolução (DPI)
       width = 6, height = 5, units = "in")  # Define o tamanho da imagem
