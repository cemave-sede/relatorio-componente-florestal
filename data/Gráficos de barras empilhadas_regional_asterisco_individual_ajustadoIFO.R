# Gráficos de dados Borboletas - Protocolo básico - Regiões da Amazônia - individual
### Tribos com asterisco estão ausentes nos dados
## Script: Isabela Freitas Oliveira

# Carregar bibliotecas
library(ggplot2)
library(dplyr)
library(scales)
library(tools)

# Definir o diretório de trabalho
setwd("~/Documents/ICMBIO-COMOB/Dados_Monitora/Análise de dados/Monitora_Borboletas_Isabela")

# Função auxiliar para ajustar o título
ajustar_titulo <- function(titulo) {
  palavras <- strsplit(titulo, " ")[[1]]
  
  # Palavras que necessitam de correção de acentuação
  correcoes <- list(
    "leste" = "Região climática Leste Amazônica", "sudoeste" = "Região climática Sudoeste Amazônica", "central" = "Região climática Central Amazônica",
    "norte" = "Região climática Norte Amazônica", "sudeste" = "Região climática Sudeste Amazônica"
  )
  
  palavras <- sapply(palavras, function(palavra) {
    # Aplicar regras específicas para certas palavras
    if (tolower(palavra) %in% c("esec", "flona", "rebio", "resex", "parna")) {
      return(toupper(palavra))
    } else if (tolower(palavra) %in% c("de", "da", "do", "dos", "das")) {
      return(tolower(palavra))
    } else {
      # Corrigir acentuação, se necessário
      palavra_corrigida <- tolower(palavra)
      if (palavra_corrigida %in% names(correcoes)) {
        return(correcoes[[palavra_corrigida]])
      } else {
        return(tools::toTitleCase(palavra))
      }
    }
  })
  
  return(paste(palavras, collapse = " "))
}

# Função para gerar o gráfico para cada região
gerar_grafico_por_regiao <- function(dados, nome_regiao) {
  # Preparar o título
  titulo <- ajustar_titulo(nome_regiao)  # Utilizando a função de ajuste de título aqui
  
  # Filtrar os dados para a região atual
  dados_regiao <- subset(dados, regiao == nome_regiao & bioma == "Amazonia")
  dados_regiao <- dados_regiao[!is.na(dados_regiao$tribo), ] #tirar os dados 'NA' que estão na tribo
  
  # Calcular o total de indivíduos por ano para a região atual
  total_individuos_por_ano <- dados_regiao %>%
    group_by(ano) %>%
    summarise(total = sum(soma_individuos), .groups = 'drop')
  
  # Agregar dados por ano e tribo
  dados_agregados <- dados_regiao %>%
    group_by(ano, tribo) %>%
    summarize(frequencia = sum(soma_individuos), .groups = 'drop')
  
  # Calcular totais e proporções
  total_por_ano <- dados_agregados %>%
    group_by(ano) %>%
    summarize(total = sum(frequencia), .groups = 'drop')
  
  dados_agregados <- left_join(dados_agregados, total_por_ano, by = "ano") %>%
    mutate(proporcao = frequencia / total * 100)
  
  # Ajustar proporções
  dados_agregados <- dados_agregados %>%
    group_by(ano) %>%
    mutate(diferenca = 100 - sum(proporcao),
           proporcao_ajustada = ifelse(proporcao == max(proporcao), proporcao + diferenca, proporcao)) %>%
    ungroup() %>%
    dplyr::select(-diferenca, -proporcao) %>%
    rename(proporcao = proporcao_ajustada)
  
  # Ordem específica das tribos
  ordem_tribos <- c(
    "Brassolini", "Morphini", "Haeterini", 
    "Satyrini", "Coeini", "Epiphilini", 
    "Epicaliini", "Anaeini", "Melanitini", 
    "Preponini", "Callicorini", "Biblidini", 
    "Ageroniini"
  )
  
  # Cores correspondentes às tribos - BLINDSAFE colors (3 categorias de verde, 7 categorias de cinza e 3 categorias de marrom)
  cores_tribos <- c(
    "Brassolini" = "#35978f", "Morphini" = "#80cdc1", "Haeterini" = "#c7eae5", 
    "Satyrini" = "gray95", "Coeini" = "gray90", "Epiphilini" = "gray85", 
    "Epicaliini" = "gray80", "Anaeini" = "gray75", "Melanitini" = "gray70", 
    "Preponini" = "gray65", "Callicorini" = "#f6e8c3", "Biblidini" = "#dfc27d", 
    "Ageroniini" = "#bf812d"
  )
  
  # Ajustar a ordem e as cores das tribos com base nas tribos presentes
  cores_tribos <- cores_tribos[ordem_tribos]
  
  # Assegurar que a coluna 'tribo_clean' respeite a ordem definida
  dados_agregados$tribo <- factor(dados_agregados$tribo, levels = ordem_tribos)
  
  # Criar um data frame com a frequência agregada por tribo
  #frequencias_por_tribo_ano <- aggregate(frequencia ~ tribo + ano, dados_agregados, sum)
  
  # Criar um data frame com a frequência agregada por tribo
  frequencias_por_tribo_ano <- aggregate(frequencia ~ tribo, dados_agregados, sum)
  
  # Criar uma função para determinar se um asterisco deve ser adicionado ao nome da tribo
  adicionar_asterisco <- function(nome_tribo) {
    # Verificar se existe alguma frequência igual a zero para essa tribo em qualquer ano
    if (any(frequencias_por_tribo_ano$tribo == nome_tribo & frequencias_por_tribo_ano$frequencia == 0)) {
      return(paste0(nome_tribo, "*"))
    } else {
      return(nome_tribo)
    }
  }
  
  # Aplicar a função a cada tribo para criar o vetor de rótulos
  labels_tribos <- sapply(ordem_tribos, adicionar_asterisco)
  
  # Construir o gráfico
  gg <- ggplot(dados_agregados) +
    geom_bar(aes(x = ano, y = proporcao, fill = tribo), stat = "identity", position = position_stack(reverse = FALSE)) +
    geom_text(data = total_individuos_por_ano, aes(x = ano, y = 100, label = total), vjust = -0.5, size = 4, hjust = 0.5)+
    scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1), limits = c(0, 100))+
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = cores_tribos, 
                      labels = labels_tribos,
                      breaks = ordem_tribos) +
    labs(title = titulo,
         x = "Ano",
         y = "Proporção relativa de ocorrência (%)",
         fill = "Tribo") +
    theme_minimal() +
    theme(legend.position = "right", 
          legend.title = element_text(size = 18), 
          legend.text = element_text(size = 15), 
          plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 18),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          axis.line = element_line(colour = "gray40"),
          axis.line.x = element_blank()) +
    guides(fill = guide_legend(title = "Tribos", reverse = FALSE))
  
  
  # Exibir o gráfico
  print(gg)
}

# Carregar e preparar os dados
resultado_final <- read.csv("resultado_final.csv", header = TRUE)
resultado_final$ano <- factor(resultado_final$ano)
resultado_final$tribo <- gsub("\\*", "", resultado_final$tribo)
#resultado_final <- resultado_final[resultado_final$ano != "2023", ]

# Definir a região de interesse - colocar as regiões 'norte', 'sudoeste', 'sudeste', 'leste', 'central'
regiao_interesse <- "norte"

# Gerar o gráfico para a região de interesse
gerar_grafico_por_regiao(resultado_final, regiao_interesse)
