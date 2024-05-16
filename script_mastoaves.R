############################ Script Capítulo 5 - Mastoaves  ###################

# Autores: Arlindo Gomes Filho, Elildo Carvalho Jr e Marcos de Souza Fialho


##############################


# Carregando pacotes

library(tidyverse)
library(data.table)
library(descr)
library(RColorBrewer)
library(kableExtra)
library(knitr)
library(DT)
library(plotly)
library(ggthemes)
library(forcats)
library(leaflet)
library(sf)
library(rgdal)
library(treemapify)
library(reshape2)
library(randomcoloR)
library(gridExtra)
library(here)
library(htmlTable)



########## Carregando e limpando os dados ###########


# Carregando os dados no formato .csv

masto_aves_csv <- read_csv2("dados/mastoaves/base_masto_aves_esforco_corrigido.csv")

# No arquivo "base_masto_aves_eforco_corrigido.csv" a coluna de esforço 
# foi corrigida, visto que os valores estavam sem o ponto decimal, 
# indicando quantidades 10 vezes superiores ao valor real (ex.: 45 km e vez de 4.5 km)

# Convertendo os dados para o formato .rds

saveRDS(masto_aves_csv, file = "dados/mastoaves/masto_aves.rds")

masto_aves <- readRDS("dados/mastoaves/masto_aves.rds")

# Carregando a planilha com a UCs classificadas por grupo - proteção integral x
# uso sustentável

ucs_grupos <- read_csv2("dados/ucs/ucs_grupos.csv")


# Juntando a planilha ucs_grupos com a planilha masto_aves

masto_aves <- left_join(masto_aves, ucs_grupos, by = "cnuc")

#View(masto_aves)

# Corrigindo os nomes dos biomas

masto_aves <- masto_aves %>% 
  mutate(
    bioma = case_when(
      uc == "Esec de Pirapitinga" ~ "Cerrado",
      uc == "Esec Serra Geral do Tocantins" ~ "Cerrado",
      uc == "Parna da Chapada dos Veadeiros" ~ "Cerrado",
      uc == "Parna da Serra da Bodoquena" ~ "Cerrado",
      uc == "Parna da Serra do Cipó" ~ "Cerrado",
      uc == "Parna da Serra da Bocaina" ~ "Mata Atlântica",
      uc == "Parna da Serra dos Órgãos" ~ "Mata Atlântica",
      uc == "Parna do Iguaçu" ~ "Mata Atlântica",
      uc == "Parna do Superagui" ~ "Mata Atlântica",
      uc == "Rebio Guaribas" ~ "Mata Atlântica",
      uc == "Rebio de Una" ~ "Mata Atlântica",
      TRUE ~ "Amazônia"))

#View(masto_aves)

#unique(masto_aves$bioma)

# Contando o número de UCs por bioma

ucs_bioma <- masto_aves |> 
  group_by(bioma) |> 
  distinct(uc) |> 
  summarise(ucs = n())

# Corrigindo a lista de primatas - alterando registros na coluna "taxon_validado": 
# Mico spp para Mico sp e "Callicebus spp" para "Callicebus sp"

masto_aves <- masto_aves %>% 
  mutate(
    taxon_validado = case_when(
      taxon_validado == "Mico spp" ~ "Mico sp",
      taxon_validado == "Callicebus spp" ~ "Callicebus sp",
      taxon_validado == "Lagothrix cana" ~ "Lagothrix lagothricha cana",
      taxon_validado == "Lagothrix poeppigii" ~ "Lagothrix lagothricha poeppigii",
      TRUE ~ taxon_validado))



# Alterando o nome da espécie de veado na ESEC de Maracá de Ozotocerus para Mazama

masto_aves <- masto_aves %>% 
  mutate(
    taxon_validado = case_when(
      uc == "Esec de Maracá" & taxon_validado == "Ozotocerus bezoarticus" ~ "Mazama sp",
      TRUE ~ taxon_validado))



# Corrigindo validação de Ortalis motmot para algumas UCs -> Ortalis rucifeps

masto_aves <- masto_aves %>%
  mutate(
    taxon_validado = case_when(
      uc == "Esec da Terra do Meio" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      uc == "Parna da Serra do Pardo" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      uc == "Rebio do Tapirapé" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      uc == "Resex Renascer" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      uc == "Resex Riozinho do Anfrísio" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      uc == "Resex Verde para Sempre" & taxon_validado == "Ortalis motmot" ~ "Ortalis ruficeps",
      TRUE ~ taxon_validado))


# Checando o ajuste de Ortalis ruficeps

# aves1 <- masto_aves |>
#   filter(classe == "Aves")
# 
# unique(aves1$taxon_validado)



# Alterando o nome da ordem de Artiodactyla para Cetartiodactyla

masto_aves <- masto_aves %>%
  mutate(
    ordem = case_when(
      ordem == "Artiodactyla" ~  "Cetartiodactyla",
      TRUE ~ ordem)) 


unique(masto_aves$ordem)



# Ajustando nomes na coluna "especie" - corrigindo espécies com sp. para sp

masto_aves <- masto_aves %>%
  mutate(
    especie = case_when(
      especie == "Aotus sp." ~  "Aotus sp",
      especie == "Dasyprocta sp." ~  "Dasyprocta sp",
      especie == "Psophia sp." ~  "Psophia sp",
      especie == "Galea sp." ~  "Galea sp",
      especie == "Galictis sp." ~  "Galictis sp",
      TRUE ~ especie)) 


# Checando táxons de primatas

# list_primatas <- masto_aves |>
#   filter(ordem == "Primates")
# 
# unique(list_primatas$taxon_validado)

# Convertendo a classe de algumas variáveis para fator

masto_aves$uc <- as.factor(masto_aves$uc)
masto_aves$bioma <- as.factor(masto_aves$bioma)
masto_aves$uc_ca <- as.factor(masto_aves$uc_ca)
masto_aves$categoria <- as.factor(masto_aves$categoria)
masto_aves$grupo <- as.factor(masto_aves$grupo)

#glimpse(masto_aves)


                                   ### Figura 5.1 ###

# Scripts Gráfico 1 de esforço 
# Autores: Elildo Carvalho Jr & Marcos Fialho

library(here)
library(tidyverse)
library(RColorBrewer)


#----- ler dados

load(here("dados", "mastoaves", "monitora.RData"))

monitora <- monitora %>%
  mutate(
    uc = case_when(
      uc  == "Esec da Terra do Meio" ~ "ESEC da Terra do Meio",
      uc  == "Esec de Maracá" ~ "ESEC de Maracá",
      uc  == "Esec de Niquiá" ~ "ESEC de Niquiá",
      uc  == "Esec de Pirapitinga" ~ "ESEC de Pirapitinga",
      uc  == "Esec do Jari" ~ "ESEC do Jari",
      uc  == "Esec do Rio Acre" ~ "ESEC do Rio Acre",
      uc  == "Esec Serra Geral do Tocantins" ~ "ESEC Serra Geral do Tocantins",
      uc  == "Flona de Tapajós" ~ "FLONA de Tapajós",
      uc  == "Flona do Jamari" ~ "FLONA do Jamari",
      uc  == "Parna da Amazônia" ~ "PARNA da Amazônia",
      uc  == "Parna da Chapada dos Veadeiros" ~ "PARNA da Chapada dos Veadeiros",
      uc  == "Parna da Serra da Bocaina" ~ "PARNA da Serra da Bocaina",
      uc  == "Parna da Serra da Bodoquena" ~ "PARNA da Serra da Bodoquena",
      uc  == "Parna da Serra da Cutia" ~ "PARNA da Serra da Cutia",
      uc  == "Parna da Serra do Cipó" ~ "PARNA da Serra do Cipó",
      uc  == "Parna da Serra do Divisor" ~ "PARNA da Serra do Divisor",
      uc  == "Parna da Serra do Pardo" ~ "PARNA da Serra do Pardo",
      uc  == "Parna da Serra dos Órgãos" ~ "PARNA da Serra dos Órgãos",
      uc  == "Parna de Pacaás Novos" ~ "PARNA de Pacaás Novos",
      uc  == "Parna do Cabo Orange" ~ "PARNA do Cabo Orange",
      uc  == "Parna do Iguaçu" ~ "PARNA do Iguaçu",
      uc  == "Parna do Jaú" ~ "PARNA do Jaú",
      uc  == "Parna do Juruena" ~ "PARNA do Juruena",
      uc  == "Parna do Monte Roraima" ~ "PARNA do Monte Roraima",
      uc  == "Parna do Superagui" ~ "PARNA do Superagui",
      uc  == "Parna do Viruá" ~ "PARNA do Viruá",
      uc  == "Parna dos Campos Amazônicos" ~ "PARNA dos Campos Amazônicos",
      uc  == "Parna Mapinguari" ~ "PARNA Mapinguari",
      uc  == "Parna Montanhas do Tumucumaque" ~ "PARNA Montanhas do Tumucumaque",
      uc  == "Parna Nascentes do Lago Jari" ~ "PARNA Nascentes do Lago Jari",
      uc  == "Parna Serra da Mocidade" ~ "PARNA Serra da Mocidade",
      uc  == "Rebio de Una" ~ "REBIO de Una",
      uc  == "Rebio do Gurupi" ~ "REBIO do Gurupi",
      uc  == "Rebio do Jaru" ~ "REBIO do Jaru",
      uc  == "Rebio do Tapirapé" ~ "REBIO do Tapirapé",
      uc  == "Rebio do Uatumã" ~ "REBIO do Uatumã",
      uc  == "Rebio Guaribas" ~ "REBIO Guaribas",
      uc  == "Rebio Trombetas" ~ "REBIO Trombetas",
      uc  == "Resex Arapixi" ~ "RESEX Arapixi",
      uc  == "Resex Barreiro das Antas" ~ "RESEX Barreiro das Antas",
      uc  == "Resex Chico Mendes" ~ "RESEX Chico Mendes",
      uc  == "Resex do Alto Tarauacá" ~ "RESEX do Alto Tarauacá",
      uc  == "Resex do Cazumbá-Iracema" ~ "RESEX do Cazumbá-Iracema",
      uc  == "Resex do Lago do Capanã Grande" ~ "RESEX do Lago do Capanã Grande",
      uc  == "Resex do Rio do Cautário" ~ "RESEX do Rio do Cautário",
      uc  == "Resex Ipaú-Anilzinho" ~ "RESEX Ipaú-Anilzinho",
      uc  == "Resex Renascer" ~ "RESEX Renascer",
      uc  == "Resex Rio Ouro Preto" ~ "RESEX Rio Ouro Preto",
      uc  == "Resex Riozinho da Liberdade" ~ "RESEX Riozinho da Liberdade",
      uc  == "Resex Riozinho do Anfrísio" ~ "RESEX Riozinho do Anfrísio",
      uc  == "Resex Tapajós-Arapiuns" ~ "RESEX Tapajós-Arapiuns",
      uc  == "Resex Verde para Sempre" ~ "RESEX Verde para Sempre",
      TRUE ~ uc))


#unique(monitora$uc)


#----- gráfico de esforco anual e acumulado por bioma

monitora %>%
  filter(ano != 2023) %>%
  distinct(bioma, uc, ano, ea, data, esforco) %>%
  group_by(bioma, ano) %>%
  summarise(esforco_anual = sum(esforco),
            ucs = n_distinct(uc),
            taxa = n()/esforco_anual) %>%
  mutate(esf_acumulado = cumsum(esforco_anual)) %>%
  relocate(esf_acumulado, .after = esforco_anual) %>%
  ggplot(aes(x=ano)) +
  geom_line(aes(y = esf_acumulado), colour = "cyan4", linewidth = 1) +
  geom_line(aes(y = esforco_anual), colour = "#BF8120", linewidth = 1) +
  ylab("Esforço (km)") +
  xlab("") +
  theme_classic() +
  #scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  facet_wrap(~ bioma, scales = "free") +
  labs(x = "Ano")

# Salvando o gráfico

ggsave("imagens/cap05/ma_esforco_acumulado.jpg",
       #plot = last_plot(),
       device =  "jpeg",
       scale=1.5,
       width = 210,
       height = 70,
       units = "mm",
       dpi = 120)


                                ### Figura 5.2 ###


#----- grafico com numero de UCs ativas e numero acumulado de encontros por ano

monitora %>%
  filter(ano != 2023) %>%
  distinct(bioma, uc, ano, ea, data, esforco) %>%
  group_by(bioma, ano) %>%
  summarise(esforco_anual = sum(esforco),
            ucs = n_distinct(uc)) %>%
  left_join(monitora %>%
              arrange(ano) %>%
              distinct(taxon_validado, .keep_all = TRUE) %>%
              group_by(bioma, ano) %>%
              count() %>%
              ungroup() %>%
              rename(taxa_registrados = n),
            by = c("bioma", "ano")) %>%
  mutate(taxon_acumulado = cumsum(taxa_registrados)) %>%
  relocate(taxon_acumulado, .after = esforco_anual) %>%
  mutate(taxon_acumulado = case_when(is.na(taxon_acumulado) ~ max(taxon_acumulado, na.rm = TRUE),
                                     .default = taxon_acumulado)) %>%
  ggplot(aes(x=ano)) +
  geom_bar(aes(y = ucs*10), stat = "identity", 
           fill = "cyan4", colour = "cyan4") +
  geom_line(aes(y = taxon_acumulado), stat = "identity", colour = "#95D8D8", linewidth = 0.8) +
  scale_y_continuous(
    name = "Táxons validados (acumulado)",  # features of first axis
    sec.axis = sec_axis(~.*0.1, name="Número de UCs"), # add second axis and specify its features
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(2014, 2022, 2)) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12)) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  facet_wrap(~ bioma, scales = "free") +
  # theme(aspect.ratio = 3/4,  # Ajustar o painel para uma relação 3/4 laltura/largura
  #   plot.background = element_rect(fill = "white")) +
  labs(x = "Ano")


# Salvando o gráfico

ggsave("imagens/cap05/ma_ucs_ativas_taxa_acumulado.jpg",
       #plot = last_plot(),
       device =  "jpeg",
       scale=1.5,
       width = 210,
       height = 70,
       units = "mm",
       dpi = 120)



                              ### Figura 5.3 ###



#----- grafico de esforco total e anual por uc/bioma

pallete <- RColorBrewer::brewer.pal(length(unique(monitora$ano)), "Spectral")



monitora %>%
  filter(ano != 2023) %>%
  distinct(bioma, uc, ano, ea, data, esforco) %>%
  group_by(bioma, uc, ano) %>% 
  summarize(esforco = sum(esforco)) %>% 
  group_by(uc) %>%
  mutate(esf_total = sum(esforco)) %>%
  ungroup() %>%
  arrange(esf_total) %>%
  mutate(uc = factor(uc, levels = unique(uc)),
         ano = factor(ano)) %>% 
  ggplot(aes(x = uc, y = esforco, group = ano, fill = ano)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(desc(bioma)), scales = "free", space = "free_y")  +
  coord_flip() +
  
  theme_classic() +
  xlab("") +
  ylab("Esforço total (km)") +
  theme(
    #strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  scale_fill_manual(values = pallete, name = "Ano", 
                    guide = guide_legend(reverse = TRUE)) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(strip.text.y = element_blank()) +
  #theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 7, face = "bold")) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.line.x=element_blank()) +
  theme(panel.grid.major.x = element_line(color = "lightgray")) 


# Salvando o gráfico

ggsave("imagens/cap05/ma_esforco_total_uc_bioma.jpg",
       #plot = last_plot(),
       device =  "jpeg",
       scale=0.6,
       width = 240,
       height = 300,
       units = "mm",
       dpi = 120)


# -------------------------------------


# Criando paletas de cores

cor1 <- c(Primates = "#008B8B",
          Rodentia = "#4AB1B1",
          Tinamiformes = "#70C4C4",
          Galliformes = "#95D8D8",
          Artiodactyla = "#CDF5F5",
          Gruiformes = "#D9D9D9",
          Carnivora = "#CCCCCC",
          Pilosa = "#B2B2B2",
          Perissodactyla = "#999999",
          Cingulata = "#666666",
          Didelphimorphia = "#FEE6CE",
          Lagomorpha = "#FDAE6B",
          Cariamiformes = "#E6550D")


cor2 <- c("#008B8B",
          "#CDF5F5",
          "#D9D9D9",
          "#70C4C4",
          "#666666"
)

cor3 <-c(
  "#008B8B",
  "#129494",
  "#008B8B",
  "#38A8A8",
  "#4AB1B1",
  "#5DBBBB",
  "#70C4C4",
  "#82CECE",
  "#95D8D8",
  "#A8E2E2",
  "#BAEBEB",
  "#CDF5F5",
  "#E0FFFF"
)


cor4 <- c("#008B8B", "#BAEBEB", "#259E9E", "#E0FFFF", "#38A8A8", "#95D8D8")


# ---------------------------------


# Limpando dados da coluna taxon-validado para a contagem do número de 
# espécies por ordem (foram excluídos alguns regitros "gênero sp", para os casos
# em que todas as espécies do gênero já tinham sido registradas)


limpeza_registros_numero_especies_por_ordem <-  masto_aves %>%
  filter(!(masto_aves$taxon_validado  == "Aburria sp" |
             masto_aves$taxon_validado  == "Crax sp" |
             masto_aves$taxon_validado  == "Odonthophorus sp" |
             masto_aves$taxon_validado  == "Pauxi sp" |
             masto_aves$taxon_validado  == "Coendou sp" |
             masto_aves$taxon_validado  == "Myoprocta sp" |
             masto_aves$taxon_validado  == "Dasyprocta sp" |
             masto_aves$taxon_validado  == "Microsciurus sp" |
             masto_aves$taxon_validado  == "Sciurillus sp" |
             masto_aves$taxon_validado  == "Urosciurus sp" |
             masto_aves$taxon_validado  == "Guerlinguetus sp" |
             masto_aves$taxon_validado  == "Mazama sp" |
             masto_aves$taxon_validado  == "Leopardus sp" |
             masto_aves$taxon_validado  == "Cacajao sp" |
             masto_aves$taxon_validado  == "Callicebus sp" |
             masto_aves$taxon_validado  == "Callithrix sp" |
             masto_aves$taxon_validado  == "Cebus sp" |
             masto_aves$taxon_validado  == "Saimiri sp" 
  )) |> 
  filter(!is.na(ordem)) |>
  filter(!is.na(taxon_validado)) |> 
  group_by(taxon_validado, ordem) |> 
  summarise(count = n()) 

# Contagem do número de espécies por ordem

numero_especies_por_ordem <- limpeza_registros_numero_especies_por_ordem |> 
  group_by(ordem) |> 
  count() |> 
  arrange(desc(n))

# ------------------------------


# Limpeza de registros do número de espécies por ordem por bioma

limpeza_registros_numero_especies_por_ordem_bioma <-  masto_aves %>%
  filter(!(masto_aves$taxon_validado  == "Aburria sp" |
             masto_aves$taxon_validado  == "Crax sp" |
             masto_aves$taxon_validado  == "Odonthophorus sp" |
             masto_aves$taxon_validado  == "Pauxi sp" |
             masto_aves$taxon_validado  == "Coendou sp" |
             masto_aves$taxon_validado  == "Myoprocta sp" |
             masto_aves$taxon_validado  == "Dasyprocta sp" |
             masto_aves$taxon_validado  == "Microsciurus sp" |
             masto_aves$taxon_validado  == "Sciurillus sp" |
             masto_aves$taxon_validado  == "Urosciurus sp" |
             masto_aves$taxon_validado  == "Guerlinguetus sp" |
             masto_aves$taxon_validado  == "Mazama sp" |
             masto_aves$taxon_validado  == "Leopardus sp" |
             masto_aves$taxon_validado  == "Cacajao sp" |
             masto_aves$taxon_validado  == "Callicebus sp" |
             masto_aves$taxon_validado  == "Callithrix sp" |
             masto_aves$taxon_validado  == "Cebus sp" |
             masto_aves$taxon_validado  == "Saimiri sp" 
  )) |> 
  filter(!is.na(ordem)) %>%
  filter(!is.na(taxon_validado)) |> 
  group_by(taxon_validado, ordem, bioma) %>%
  summarise(count = n()) 


# Contagem do número de espécies por ordem por bioma

numero_especies_por_ordem_bioma <- limpeza_registros_numero_especies_por_ordem_bioma |> 
  group_by(bioma, ordem) |> 
  count() |> 
  arrange(desc(n))


# ---------------------------------


### Checando algumas informações paralelas


# Extraindo o número total de espécies de primatas

# n_especies_primatas <- numero_especies_por_ordem$n[1]

# Avaliação das espécies de primatas

# registros_primatas <- limpeza_registros_numero_especies_por_ordem_bioma %>%
#   select(ordem, taxon_validado) |> 
#   filter(!is.na(ordem)) |> 
#   filter(ordem == "Primates")
# 
# especies_primatas <- unique(registros_primatas$taxon_validado)
# especies_primatas

# Contagem de táxons - aves

# aves <- masto_aves |> 
#   filter(classe == "Aves")

# unique(aves$taxon_validado)

# Contagem de táxons - mamíferos

# mamiferos <- masto_aves |>
#   filter(classe == "Mammalia")

# unique(mamiferos$taxon_validado)


# ----------------------------------

                                   

                ### Figura 5.4 - Composição com 4 gráficos ###



# Gráfico do número de espécies e de registros por ordem - geral

registros_especies_geral <- masto_aves %>%
  filter(!is.na(ordem)) |> 
  group_by(ordem) %>%
  summarise(count = n()) |> 
  arrange(desc(count))

G_especies_registros_ordem_geral <- numero_especies_por_ordem  |> 
  ggplot() +
  geom_col(data = registros_especies_geral, aes(x = fct_reorder(ordem, count, .desc = F), y = count, fill = "darkgreen")) +
  geom_col(aes(x = fct_reorder(ordem, n, .desc = F), y = n, fill = "darkcyan")) +
  geom_text(aes(ordem, n, label = n, alpha = 0.5, hjust = -0.1), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c("darkcyan","#A8E2E2")) +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 6, face = "bold"),
    plot.title = element_text(size = 11),
    legend.position = "none",
    axis.title = element_text(size = 8, margin = margin(t = 10))
  ) +
  scale_y_log10(name = "", labels = scales::number_format(big.mark = ".")) +
  labs(title = "      Geral",
       y = "Número de espécies e de registros por ordem")

G_especies_registros_ordem_geral


# ----------------------------------


# Gráfico do número de espécies e de registros por ordem - bioma amazônia

registros_especies_amazonia <- masto_aves %>%
  filter(!is.na(ordem)) |>
  filter(bioma == "Amazônia") |> 
  group_by(ordem) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

G_especies_registros_ordem_bioma_amazonia <- numero_especies_por_ordem_bioma  |> 
  filter(bioma == "Amazônia") |> 
  ggplot() +
  geom_col(data = registros_especies_amazonia, aes(x = fct_reorder(ordem, count, .desc = F), y = count, fill = "darkgreen")) +
  geom_col(aes(x = fct_reorder(ordem, n, .desc = F), y = n, fill = "darkcyan")) +
  geom_text(aes(ordem, n, label = n, alpha = 0.5, hjust = -0.1), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c("darkcyan","#A8E2E2")) +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 6, face = "bold"),
    plot.title = element_text(size = 11),
    legend.position = "none",
    axis.title.x = element_text(size = 11, margin = margin(t = 10))
  ) +
  scale_y_log10(name = "", labels = scales::number_format(big.mark = ".")) +
  labs(title = "      Amazônia",
       y = "Número de espécies e de registros por ordem")

G_especies_registros_ordem_bioma_amazonia


# --------------------------------


# Gráfico do número de espécies e de registros por ordem - bioma mata atlântica

registros_especies_mata_atlantica <- masto_aves %>%
  filter(!is.na(ordem)) |>
  filter(bioma == "Mata Atlântica") |> 
  group_by(ordem) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

G_especies_registros_ordem_bioma_mata_atlantica <- numero_especies_por_ordem_bioma  |> 
  filter(bioma == "Mata Atlântica") |> 
  ggplot() +
  geom_col(data = registros_especies_mata_atlantica, aes(x = fct_reorder(ordem, count, .desc = F), y = count, fill = "darkgreen")) +
  geom_col(aes(x = fct_reorder(ordem, n, .desc = F), y = n, fill = "darkcyan")) +
  geom_text(aes(ordem, n, label = n, alpha = 0.5, hjust = -0.1), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c("darkcyan","#A8E2E2")) +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 6, face = "bold"),
    plot.title = element_text(size = 11),
    legend.position = "none",
    axis.title.x = element_text(size = 11, margin = margin(t = 10))
  ) +
  scale_y_log10(name = "", labels = scales::number_format(big.mark = ".")) +
  labs(title = "     Mata Atlântica",
       y = "Número de espécies e de registros por ordem")

G_especies_registros_ordem_bioma_mata_atlantica


# ---------------------------------


# Gráfico do número de espécies e de registros por ordem - bioma cerrado

registros_especies_cerrado <- masto_aves %>%
  filter(!is.na(ordem)) |>
  filter(bioma == "Cerrado") |> 
  group_by(ordem) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

G_especies_registros_ordem_bioma_cerrado <- numero_especies_por_ordem_bioma  |> 
  filter(bioma == "Cerrado") |> 
  ggplot() +
  geom_col(data = registros_especies_cerrado, aes(x = fct_reorder(ordem, count, .desc = F), y = count, fill = "darkgreen")) +
  geom_col(aes(x = fct_reorder(ordem, n, .desc = F), y = n, fill = "darkcyan")) +
  geom_text(aes(ordem, n, label = n, alpha = 0.5, hjust = -0.1), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c("darkcyan","#A8E2E2")) +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 6, face = "bold"),
    plot.title = element_text(size = 11),
    
    legend.position = "none",
    axis.title.x = element_text(size = 11, margin = margin(t = 10))
  ) +
  scale_y_log10(name = "", labels = scales::number_format(big.mark = ".")) +
  labs(title = "     Cerrado",
       y = "Número de espécies e de registros por ordem")

G_especies_registros_ordem_bioma_cerrado


# -----------------------------------------


# Juntando os 4 gráficos numa única figura

grafico_composto_ordens <- grid.arrange(G_especies_registros_ordem_geral, G_especies_registros_ordem_bioma_amazonia, G_especies_registros_ordem_bioma_mata_atlantica, G_especies_registros_ordem_bioma_cerrado,
                                        layout_matrix = matrix(c(1, 2, 3, 4), 
                                                               byrow = TRUE, nrow = 2, ncol = 2))

# Salvando o gráfico

ggsave("imagens/cap05/ma_grafico_composto_ordens.jpg", plot = grafico_composto_ordens, height = 4.5, width = 6, units = "in", dpi = 300)


# ---------------------------------------------


                          #### Cálculo das taxas de avistamento 

# Checando os nomes das colunas

#names(masto_aves) 

# Preenchendo as linhas vazias da coluna esforço

masto_aves <-
  masto_aves |> 
  arrange(uc, ano, ea) |>
  fill(esforco)

# View(masto_aves) 

# Alterando o nome das categorias para letras maiúsculas

masto_aves <- masto_aves %>%
  mutate(
    uc = case_when(
      uc  == "Esec da Terra do Meio" ~ "ESEC da Terra do Meio",
      uc  == "Esec de Maracá" ~ "ESEC de Maracá",
      uc  == "Esec de Niquiá" ~ "ESEC de Niquiá",
      uc  == "Esec de Pirapitinga" ~ "ESEC de Pirapitinga",
      uc  == "Esec do Jari" ~ "ESEC do Jari",
      uc  == "Esec do Rio Acre" ~ "ESEC do Rio Acre",
      uc  == "Esec Serra Geral do Tocantins" ~ "ESEC Serra Geral do Tocantins",
      uc  == "Flona de Tapajós" ~ "FLONA de Tapajós",
      uc  == "Flona do Jamari" ~ "FLONA do Jamari",
      uc  == "Parna da Amazônia" ~ "PARNA da Amazônia",
      uc  == "Parna da Chapada dos Veadeiros" ~ "PARNA da Chapada dos Veadeiros",
      uc  == "Parna da Serra da Bocaina" ~ "PARNA da Serra da Bocaina",
      uc  == "Parna da Serra da Bodoquena" ~ "PARNA da Serra da Bodoquena",
      uc  == "Parna da Serra da Cutia" ~ "PARNA da Serra da Cutia",
      uc  == "Parna da Serra do Cipó" ~ "PARNA da Serra do Cipó",
      uc  == "Parna da Serra do Divisor" ~ "PARNA da Serra do Divisor",
      uc  == "Parna da Serra do Pardo" ~ "PARNA da Serra do Pardo",
      uc  == "Parna da Serra dos Órgãos" ~ "PARNA da Serra dos Órgãos",
      uc  == "Parna de Pacaás Novos" ~ "PARNA de Pacaás Novos",
      uc  == "Parna do Cabo Orange" ~ "PARNA do Cabo Orange",
      uc  == "Parna do Iguaçu" ~ "PARNA do Iguaçu",
      uc  == "Parna do Jaú" ~ "PARNA do Jaú",
      uc  == "Parna do Juruena" ~ "PARNA do Juruena",
      uc  == "Parna do Monte Roraima" ~ "PARNA do Monte Roraima",
      uc  == "Parna do Superagui" ~ "PARNA do Superagui",
      uc  == "Parna do Viruá" ~ "PARNA do Viruá",
      uc  == "Parna dos Campos Amazônicos" ~ "PARNA dos Campos Amazônicos",
      uc  == "Parna Mapinguari" ~ "PARNA Mapinguari",
      uc  == "Parna Montanhas do Tumucumaque" ~ "PARNA Montanhas do Tumucumaque",
      uc  == "Parna Nascentes do Lago Jari" ~ "PARNA Nascentes do Lago Jari",
      uc  == "Parna Serra da Mocidade" ~ "PARNA Serra da Mocidade",
      uc  == "Rebio de Una" ~ "REBIO de Una",
      uc  == "Rebio do Gurupi" ~ "REBIO do Gurupi",
      uc  == "Rebio do Jaru" ~ "REBIO do Jaru",
      uc  == "Rebio do Tapirapé" ~ "REBIO do Tapirapé",
      uc  == "Rebio do Uatumã" ~ "REBIO do Uatumã",
      uc  == "Rebio Guaribas" ~ "REBIO Guaribas",
      uc  == "Rebio Trombetas" ~ "REBIO Trombetas",
      uc  == "Resex Arapixi" ~ "RESEX Arapixi",
      uc  == "Resex Barreiro das Antas" ~ "RESEX Barreiro das Antas",
      uc  == "Resex Chico Mendes" ~ "RESEX Chico Mendes",
      uc  == "Resex do Alto Tarauacá" ~ "RESEX do Alto Tarauacá",
      uc  == "Resex do Cazumbá-Iracema" ~ "RESEX do Cazumbá-Iracema",
      uc  == "Resex do Lago do Capanã Grande" ~ "RESEX do Lago do Capanã Grande",
      uc  == "Resex do Rio do Cautário" ~ "RESEX do Rio do Cautário",
      uc  == "Resex Ipaú-Anilzinho" ~ "RESEX Ipaú-Anilzinho",
      uc  == "Resex Renascer" ~ "RESEX Renascer",
      uc  == "Resex Rio Ouro Preto" ~ "RESEX Rio Ouro Preto",
      uc  == "Resex Riozinho da Liberdade" ~ "RESEX Riozinho da Liberdade",
      uc  == "Resex Riozinho do Anfrísio" ~ "RESEX Riozinho do Anfrísio",
      uc  == "Resex Tapajós-Arapiuns" ~ "RESEX Tapajós-Arapiuns",
      uc  == "Resex Verde para Sempre" ~ "RESEX Verde para Sempre",
      TRUE ~ uc))

# Checando nomes alterados

#unique(masto_aves$uc)


# Limpando a planilha - selecionando apenas colunas relevantes

masto_aves <- masto_aves |>
  select(
    cnuc,
    uc,
    bioma,
    ano,
    ea,
    everything(),
    -c(
      h_fim,
      cond_clim,
      populacao,
      tempo_censo,
      observadores,
      problema_amostragem,
      nome_ea,
      h_inicio,
      velocidade,
      n_guia,
      h_avistamento,
      observacoes
    )
  )

#glimpse(masto_aves)


#### Ajustando os códigos CNUC para o formato padrão --------------

masto_aves$cnuc <- as.character(masto_aves$cnuc)

masto_aves <- masto_aves %>% 
  mutate(
    Cnuc = case_when(
      cnuc == "284" ~ "0000.00.0284",
      cnuc == "1626" ~ "0000.00.1626",
      cnuc == "179" ~ "0000.00.0179",
      cnuc == "189" ~ "0000.00.0189",
      cnuc == "207" ~ "0000.00.0207",
      cnuc == "232" ~ "0000.00.0232",
      cnuc == "238" ~ "0000.00.0238",
      cnuc == "241" ~ "0000.00.0241",
      cnuc == "242" ~ "0000.00.0242",
      cnuc == "257" ~ "0000.00.0257",
      cnuc == "285" ~ "0000.00.0285",
      cnuc == "169" ~ "0000.00.0169",
      cnuc == "174" ~ "0000.00.0174",
      cnuc == "210" ~ "0000.00.0210",
      cnuc == "258" ~ "0000.00.0258",
      cnuc == "260" ~ "0000.00.0260",
      cnuc == "163" ~ "0000.00.0163",
      cnuc ==  "67" ~ "0000.00.0067",
      cnuc == "118" ~ "0000.00.0118",
      cnuc == "136" ~ "0000.00.0136",
      cnuc ==  "47" ~ "0000.00.0047",
      cnuc ==  "57" ~ "0000.00.0057",
      cnuc ==  "60" ~ "0000.00.0060",
      cnuc ==  "61" ~ "0000.00.0061",
      cnuc ==  "68" ~ "0000.00.0068",
      cnuc == "123" ~ "0000.00.0123",
      cnuc ==  "76" ~ "0000.00.0076",
      cnuc == "139" ~ "0000.00.0139",
      cnuc == "142" ~ "0000.00.0142",
      cnuc == "143" ~ "0000.00.0143",
      cnuc == "148" ~ "0000.00.0148",
      cnuc == "149" ~ "0000.00.0149",
      cnuc == "151" ~ "0000.00.0151",
      cnuc == "152" ~ "0000.00.0152",
      cnuc == "172" ~ "0000.00.0172",
      cnuc == "173" ~ "0000.00.0173",
      cnuc == "178" ~ "0000.00.0178",
      cnuc == "187" ~ "0000.00.0187",
      cnuc == "188" ~ "0000.00.0188",
      cnuc == "196" ~ "0000.00.0196",
      cnuc == "202" ~ "0000.00.0202",
      cnuc == "208" ~ "0000.00.0208",
      cnuc == "211" ~ "0000.00.0211",
      cnuc == "213" ~ "0000.00.0213",
      cnuc == "221" ~ "0000.00.0221",
      cnuc == "222" ~ "0000.00.0222",
      cnuc == "256" ~ "0000.00.0256",
      cnuc == "259" ~ "0000.00.0259",
      cnuc == "274" ~ "0000.00.0274",
      cnuc == "281" ~ "0000.00.0281",
      cnuc == "1633" ~ "0000.00.1633",
      cnuc == "1810" ~ "0000.00.1810"))

#unique(masto_aves$Cnuc)
#View(masto_aves)

# cnuc <- masto_aves |>
#   distinct(uc, Cnuc) |>
#   arrange()
# 
# cnuc

# Contagem de táxons - aves

aves <- masto_aves |>
  filter(classe == "Aves")

unique(aves$taxon_validado)

# Contagem de táxons - mamíferos

mamiferos <- masto_aves |>
  filter(classe == "Mammalia")

unique(mamiferos$taxon_validado)


# --------------------------------------


#### Calculando o esforço por trilha (ea)


# Visualizando o esforço por trilha por dia em cada ano e UC

#unique(masto_aves$esforco) # Valores únicos de esforço

esforco_dia_trilha <- masto_aves  |> 
  distinct(uc, bioma, grupo, ano, ea, data, esforco) 

#View(esforco_dia_trilha)


esforco_dia_trilha_rep <- masto_aves  |> # número amostragens por trilha
  distinct(uc, bioma, grupo, ano, ea, data, esforco) |> 
  count(uc, bioma, grupo, ano, ea, esforco)

#View(esforco_dia_trilha_rep)

## Estimando o esforço total naquela trilha por ano por UC

esforco_total_trilha <- esforco_dia_trilha |> # esforço total por trilha
  group_by(uc, bioma, grupo, ano, ea) |>
  summarise(extensao = sum(esforco))

#View(esforco_total_trilha)

# Checando trilhas com esforço > 50 km

trilhas_esforco_total_maior_50km <- esforco_total_trilha |>
  filter(extensao > 50)

# print(trilhas_esforco_total_maior_50km, n = 70)
# View(trilhas_esforco_total_maior_50km)


# ---------------------------------



#### Calculando o número de registros total

registros_total_trilha <- masto_aves |>
  group_by(uc, bioma, grupo, ano, ea)  |>
  count() |>
  drop_na()

#View(registros_total)

# Juntanto as tabelas esforco_total_trilha + registros_total_trilha

taxa_avistamento_trilha <-
  left_join(registros_total_trilha, esforco_total_trilha, c("uc", "bioma", "grupo", "ano", "ea"))

#View(taxa_avistamento_trilha)

# Obs.: A taxa_avistamento_trilha considera todos os registros (aves e mamíferos conjuntamente)

taxa_avistamento_trilha <- taxa_avistamento_trilha |>
  mutate(taxa_avistamento = (n/extensao) * 10)

#View(taxa_avistamento_trilha_total)


# Taxa de avistamento média por UC por ano

taxa_avistamento_trilha_media_uc_ano <- taxa_avistamento_trilha |>
  group_by(uc, bioma, grupo, ano) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n))
  )

#View(taxa_avistamento_trilha_media_uc_ano)


# ----------------------------



## Calculando o número de registros total

registros_total_uc_ano <- masto_aves |>
  group_by(uc, Cnuc, bioma, grupo, ano)  |>
  count() |>
  drop_na()

#View(registros_total_uc_ano)

esforco_total_uc_ano <- esforco_total_trilha |> 
  group_by(uc, bioma, grupo, ano) |>
  summarise(extensao_total = sum(extensao))

#View(esforco_total_uc_ano)


# Juntanto tabelas esforco_total_uc_ano + registros_total_uc_ano

taxa_avistamento_geral_uc_ano <-
  left_join(registros_total_uc_ano, esforco_total_uc_ano, c("uc", "bioma", "grupo", "ano"))

#View(taxa_avistamento_geral_uc_ano)

taxa_avistamento_geral_uc_ano <- taxa_avistamento_geral_uc_ano |>
  mutate(taxa_avistamento = (n / extensao_total) * 10)

#View(taxa_avistamento_geral_uc_ano)


# Calculando a taxa de avistamento geral média para cada UC - média das taxas anuais

taxa_avistamento_geral_uc <- taxa_avistamento_geral_uc_ano |>
  group_by(uc, Cnuc, bioma, grupo) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975)*ep,
  )


#View(taxa_avistamento_geral_uc)

write.csv(taxa_avistamento_geral_uc, "dados/mastoaves/taxa_avistamento_geral_uc.csv", row.names = FALSE)


# -------------------------------



#### Processamento dos dados para elaboração do mapa da taxa geral de avistamento por UC

# Lendo arquivos .shp e tabelas

brasil_regioes <- readOGR("shapes/regioes_2010.shp", verbose = FALSE)

ucs_comp_florestal <-  readOGR("shapes/ucs_comp_florestal.shp", verbose = FALSE)

taxa_avistamento_geral_mapa <- read_csv("dados/mastoaves/taxa_avistamento_geral_uc.csv")

#View(taxa_avistamento_geral_mapa)

ucs_comp_florestal_unido <- sp::merge(ucs_comp_florestal, taxa_avistamento_geral_mapa, by = "Cnuc")

#ucs_comp_florestal_unido

ucs_comp_florestal_unido$taxa_avist_media <- round(ucs_comp_florestal_unido$taxa_avist_media, 2)

#ucs_comp_florestal_unido


# -----------------------------------


## Calculando a taxa de avistamento por classe - mamíferos e aves - 2014 a 2022


## Calculando o número de registros por classe por ano

registros_total_classe_ano <- masto_aves |>
  group_by(uc, bioma, grupo, ano, classe)  |>
  count() |>
  drop_na()

#View(registros_total_classe_ano)

esforco_total_uc_ano <- esforco_total_trilha |> 
  group_by(uc, bioma, grupo, ano) |>
  summarise(extensao_total = sum(extensao))

#View(esforco_total_uc_ano)


# Juntanto as tabelas esforco_total_uc_ano + registros_total_classe_ano

taxa_avistamento_total_classe_ano <-
  left_join(registros_total_classe_ano, esforco_total_uc_ano, c("uc", "bioma", "grupo", "ano"))

#View(taxa_avistamento_total_classe_ano)

taxa_avistamento_total_classe_ano <- taxa_avistamento_total_classe_ano |>
  mutate(taxa_avistamento = (n / extensao_total) * 10)

#View(taxa_avistamento_total_classe_ano)

# Calculando a taxa de avistamento por classe por uc

taxa_avistamento_total_classe_uc <- taxa_avistamento_total_classe_ano |>
  group_by(uc, bioma, grupo, classe) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975)*ep 
  )


#View(taxa_avistamento_total_classe_uc)


# -------------------------------------


## Calculando o número de registros por classe, por trilha, por ano e por UC

registros_classe <- masto_aves |>
  group_by(uc, ano, bioma, ea, classe)  |>
  count() |>
  drop_na(classe)

#View(registros_classe)


# Juntanto as tabelas esforco_total_trilha + registros_classe

taxa_avistamento_trilha_classe <-
  left_join(registros_classe, esforco_total_trilha, c("uc", "bioma", "ano", "ea"))

#View(taxa_avistamento_trilha_classe)

taxa_avistamento_trilha_classe <- taxa_avistamento_trilha_classe |>
  mutate(taxa_avistamento = (n / extensao) * 10)

#View(taxa_avistamento_trilha_classe)



# Cálculo da taxa de avistamento por classe por ano

taxa_avistamento_classe_ano <- taxa_avistamento_trilha_classe |>
  group_by(uc, bioma, ano, classe) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975) * ep)

#View(taxa_avistamento_classe_ano)


# -------------------------------------


                                   

                                    ### Figura 5.5 ###



# Taxa de encontro geral mamíferos e aves por bioma - emparelhado

G_taxa_avistamento_geral_bioma_emparelhado_media_IC <- taxa_avistamento_total_classe_uc |> 
  group_by(bioma, classe) |> 
  summarise(
    n = n(),
    taxa_avist_media_bioma = mean(taxa_avist_media),
    dp = sd(taxa_avist_media, na.rm = TRUE),
    ep = dp / sqrt(n),
    IC_95 = qnorm(.975) * ep,
    minimo = min(taxa_avist_media, na.rm = TRUE),
    maximo = max(taxa_avist_media, na.rm = TRUE)) |> 
  
  
  ggplot(aes(
    x = fct_reorder(bioma, taxa_avist_media_bioma, .desc = T),
    y = taxa_avist_media_bioma), color = classe) +
  geom_pointrange(
    aes(color = classe, ymin = taxa_avist_media_bioma - IC_95, ymax =             taxa_avist_media_bioma + IC_95), 
    position = position_dodge2(width = 0.5),
    width = 0.5,
    #size = 0.5,
    linewidth = 0.7
  ) +
  geom_point(aes(colour = classe), size = 3, position = position_dodge2(width = 0.5)) +
  scale_color_manual(values = c("#359780", "#BF8120")) +
  theme_minimal() +
  theme(axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8),
        axis.text=element_text(size=13),
        axis.title.y = element_text(size = 13, margin = margin(r = 20))) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(0, 15)) +
  labs(title = " ", x = " ", y = "Avistamentos/10 km")


G_taxa_avistamento_geral_bioma_emparelhado_media_IC

# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_avistamento_media_bioma.jpg",
       height = 4.5, width = 6, units = "in", dpi = 300)



# --------------------------------------


                        ### Figura 5.6 (composisão com 4 gráficos) ###


### Taxa de encontro de mamíferos e aves ao longo do tempo - geral e por bioma - 2014 a 2022
  

# Taxa de avistamento geral mamíferos e aves emparelhado - 2014-2022

G_taxa_avistamento_geral_periodo_emparelhado_media_IC <- taxa_avistamento_total_classe_ano |> 
  filter(ano != 2023) |> 
  group_by(ano, classe) |> 
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento, na.rm = TRUE),
    ep = dp / sqrt(n),
    IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media), color = classe) +
  geom_pointrange(
    aes(color = classe, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95), 
    position = position_dodge2(width = 0.5),
    width = 0.5,
    #size = 1.5,
    linewidth = 0.7) +
  scale_color_manual(values = c("#359780", "#BF8120")) +
  geom_point(aes(color = classe), width = 0.5, size = 1.5, position = position_dodge2(width = 0.5)) +
  
  geom_hline(aes(yintercept = 8.27), colour = "#BF8120", show.legend = FALSE, linewidth = 0.5) +
  geom_abline(aes(slope = 0, intercept = 4.82), colour = "#359780", show.legend = FALSE, linewidth = 0.5) +
  
  scale_color_manual(values = c("#359780", "#BF8120", "#359780", "#BF8120")) +
  
  theme_minimal() +
  theme(axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        #legend.title = element_blank(),
        #legend.title = element_text(size = 9),
        #legend.position = c(0.87, 0.80),
        legend.position = "none",
        plot.title = element_text(size = 10),
        axis.text=element_text(size=8),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 9, margin = margin(r = 10))) +
  #scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "Geral", colour = "Geral", x = NULL, y = "Avistamentos/10 km")


print(G_taxa_avistamento_geral_periodo_emparelhado_media_IC)

# ggsave("imagens/cap05/G_taxa_avistamento_geral_periodo_emparelhado_media_IC.png", height = 4.5, width = 6, units = "in", dpi = 300)


# --------------------------


# Taxa de avistamento geral mamíferos e aves emparelhado - Amazônia - 2014-2022

G_taxa_avistamento_amazonia_periodo_emparelhado_media_IC <- taxa_avistamento_total_classe_ano |> 
  filter(bioma == "Amazônia" & ano != 2023) |> 
  group_by(ano, classe) |> 
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento, na.rm = TRUE),
    ep = dp / sqrt(n),
    IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media)) +
  geom_pointrange(
    aes(color = classe, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95), 
    #position = "dodge2",
    position = position_dodge2(width = 0.5),
    width = 0.5,
    #size = 0.6,
    linewidth = 0.7) +
  #scale_color_manual(values = c("#359780", "#BF8120", "red", "blue")) +
  geom_point(aes(colour = classe), size = 1.5, position = position_dodge2(width = 0.5)) +
  geom_hline(aes(yintercept = 9.34), colour = "#BF8120", show.legend = FALSE, linewidth = 0.5) +
  geom_abline(aes(slope = 0, intercept = 5.27), colour = "#359780", show.legend = FALSE, linewidth = 0.5) +
  
  scale_color_manual(values = c("#359780", "#BF8120", "#359780", "#BF8120")) +
  
  theme_minimal() +
  theme(axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        #legend.title = element_text(size = 9),
        legend.position = c(0.75, 0.87),
        legend.text = element_text(size = 9),
        #legend.position = "none",
        plot.title = element_text(size = 10),
        axis.text=element_text(size=8),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 9, margin = margin(r = 10))) +
  #scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "Amazônia", colour = "Amazônia", x = NULL, y = NULL)


print(G_taxa_avistamento_amazonia_periodo_emparelhado_media_IC)

# ggsave("imagens/cap05/G_taxa_avistamento_amazonia_periodo_emparelhado_media_IC.png", height = 4.5, width = 6, units = "in", dpi = 300)


# -----------------------------



# Taxa de avistamento geral mamíferos e aves emparelhado - Mata Atlântica - 2014-2022

G_taxa_avistamento_mata_atlantica_periodo_emparelhado_media_IC <- taxa_avistamento_total_classe_ano |> 
  filter(bioma == "Mata Atlântica" & ano != 2023) |> 
  group_by(ano, classe) |> 
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento, na.rm = TRUE),
    ep = dp / sqrt(n),
    IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media)) +
  geom_pointrange(
    aes(color = classe, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95), 
    #position = "dodge2",
    position = position_dodge2(width = 0.5),
    width = 0.5,
    #size = 0.6,
    linewidth = 0.7) +
  scale_color_manual(values = c("#359780", "#BF8120")) +
  geom_point(aes(colour = classe), size = 1.5, position = position_dodge2(width = 0.5)) +
  
  geom_hline(aes(yintercept = 3.17), colour = "#BF8120", show.legend = FALSE, linewidth = 0.5) +
  geom_abline(aes(slope = 0, intercept = 1.84), colour = "#359780", show.legend = FALSE, linewidth = 0.5) +
  
  scale_color_manual(values = c("#359780", "#BF8120", "#359780", "#BF8120")) +
  
  theme_minimal() +
  theme(axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        #legend.title = element_blank(),
        #legend.title = element_text(size = 9),
        #legend.position = c(0.77, 0.80),
        legend.position = "none",
        plot.title = element_text(size = 10),
        axis.text=element_text(size=8),
        axis.title.y = element_text(size = 9, margin = margin(r = 10))) +
  #scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_x_continuous(breaks = taxa_avistamento_trilha_classe$ano) +
  labs(title = "Mata Atlântica", colour = "Mata Atlântica", x = "Ano", y = "Avistamentos/10 km")

print(G_taxa_avistamento_mata_atlantica_periodo_emparelhado_media_IC)

# ggsave("imagens/cap05/G_taxa_avistamento_mata_atlantica_periodo_emparelhado_media_IC.png", height = 4.5, width = 6, units = "in", dpi = 300)


# ------------------------------------------


# Taxa de encontro geral mamíferos e aves emparelhado - Cerrado - 2014-2022

G_taxa_avistamento_cerrado_periodo_emparelhado_media_IC <- taxa_avistamento_total_classe_ano |>
  filter(bioma == "Cerrado" & ano != 2023) |>
  group_by(ano, classe) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento, na.rm = TRUE),
    ep = dp / sqrt(n),
    IC_95 = qnorm(.975) * ep) |>
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media)) +
  geom_pointrange(
    aes(color = classe, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95), 
    #position = "dodge2",
    position = position_dodge2(width = 0.5),
    width = 0.5,
    #size = 0.6,
    linewidth = 0.7) +
  scale_color_manual(values = c("#359780", "#BF8120")) +
  geom_point(aes(colour = classe), size = 1.5, position = position_dodge2(width = 0.5)) +
  
  geom_hline(aes(yintercept = 4.07), colour = "#BF8120", show.legend = FALSE, linewidth = 0.5) +
  geom_abline(aes(slope = 0, intercept = 3.26), colour = "#359780", show.legend = FALSE, linewidth = 0.5) +
  
  scale_color_manual(values = c("#359780", "#BF8120", "#359780", "#BF8120")) +
  
  
  theme_minimal() +
  theme(axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        #legend.title = element_blank(),
        #legend.title = element_text(size = 9),
        #legend.position = c(0.75, 0.84),
        legend.position = "none",
        plot.title = element_text(size = 10),
        axis.text=element_text(size=8),
        axis.title.y = element_text(size = 9, margin = margin(r = 10))) +
  #scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_x_continuous(breaks = taxa_avistamento_trilha_classe$ano) +
  
  labs(title = "Cerrado", colour = "Cerrado", x = "Ano", y = NULL)

print(G_taxa_avistamento_cerrado_periodo_emparelhado_media_IC)

# ggsave("imagens/cap05/G_taxa_avistamento_cerrado_periodo_emparelhado_media_IC.png", height = 4.5, width = 6, units = "in", dpi = 300)

# --------------------------------------



## Juntando os 4 gráficos numa única figura

grafico_taxas_biomas_tempo <- grid.arrange(G_taxa_avistamento_geral_periodo_emparelhado_media_IC, G_taxa_avistamento_amazonia_periodo_emparelhado_media_IC, G_taxa_avistamento_mata_atlantica_periodo_emparelhado_media_IC, G_taxa_avistamento_cerrado_periodo_emparelhado_media_IC,
                                           layout_matrix = matrix(c(1, 2, 3, 4), 
                                                                  byrow = TRUE, nrow = 2, ncol = 2))


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxas_biomas_tempo.jpg", plot = grafico_taxas_biomas_tempo, height = 4.5, width = 6, units = "in", dpi = 300)


# ---------------------------------------



                              ### Figura 5.7 ###


### Abundância de mamíferos nas unidades de conservação
  
  
# Gráfico da taxa de avistamento total de mamíferos - média e IC

cores_biomas <- c("cyan4", "#BF8120", "#95D8D8")

G_taxa_avistamento_geral_mamiferos_media_IC <- taxa_avistamento_total_classe_uc |>
  filter(classe == "Mammalia") |> 
  
  ggplot(aes(
    x = fct_reorder(uc, taxa_avist_media, .desc = F),
    y = taxa_avist_media)) +
  geom_point(size = 1) +
  geom_pointrange(
    aes(color = bioma, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95),
    size = 0.3,
    linewidth = 0.4) +
  #colour = "#BF8120") +
  
  coord_flip() +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = "Avistamentos/10 km") +
  scale_color_manual(values = cores_biomas) +
  theme_minimal() +
  theme(#panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #axis.text=element_text(size=10),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 10, margin = unit(c(3, 0, 0, 0), "mm"))) +
  
  labs(title = "Mamíferos", col = NULL)

G_taxa_avistamento_geral_mamiferos_media_IC


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_avistamento_mamiferos.jpg", plot =  G_taxa_avistamento_geral_mamiferos_media_IC, height = 4.5, width = 6, units = "in", dpi = 300)


# ------------------------------


                                  ### Figura 5.8 ###



# Gráfico da taxa de avistamento total de aves - média e IC


G_taxa_avistamento_geral_aves_media_IC <- taxa_avistamento_total_classe_uc |>
  filter(classe == "Aves") |>
  
  ggplot(aes(
    x = fct_reorder(uc, taxa_avist_media, .desc = F),
    y = taxa_avist_media
  )) +
  geom_point(size = 1) +
  geom_pointrange(
    aes(color = bioma, ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95),
    size = 0.3,
    linewidth = 0.4) +
  
  coord_flip() +
  scale_x_discrete(name = " ") +
  scale_y_continuous(limits = c(0, 15), name = "Avistamentos/10 km") +
  scale_color_manual(values = cores_biomas) +
  theme_minimal() +
  theme(#panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #axis.text=element_text(size=10),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 10, margin = unit(c(3, 0, 0, 0), "mm"))) +
  
  labs(title = "Aves", col = NULL)

G_taxa_avistamento_geral_aves_media_IC


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_avistamento_aves.jpg", plot = G_taxa_avistamento_geral_aves_media_IC, height = 4.5, width = 6, units = "in", dpi = 300)


# ---------------------------------


                                   ### Figura 5.9 ###
  

## Variação espacial na taxa de avistamento média - mamíferos e aves conjuntamente
  

backg <- htmltools::tags$style(".leaflet-container { background: white; }" )

colours_red <- colorNumeric(palette = "Reds", domain = NULL)

mapa <- leaflet(ucs_comp_florestal_unido) |>
  
  #Base groups
  setView(lng = -58, lat = -14,
          zoom = 4) %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  # addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  # addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorldMap") %>%
  
  
  # addPolygons(data = brasil, weight = 0.3,
  #             color = "black",
  #             fillColor = "white") |> 
  
  addPolygons(data = brasil_regioes, weight = 0.3,
              color = "black",
              fillColor = "white") |>
  
  addPolygons(data = ucs_comp_florestal_unido,
              fillColor = ~ colours_red(ucs_comp_florestal_unido$taxa_avist_media),
              fillOpacity = 1,
              weight = 0.3,
              color = "black",
              popup = paste("Nome: ", ucs_comp_florestal_unido$uc, "<br>",
                            "Bioma: ", ucs_comp_florestal_unido$BiomaCRL, "<br>",
                            "Taxa de avistamento: ", ucs_comp_florestal_unido$taxa_avist_media, "<br>")) |>
  
  htmlwidgets::prependContent(backg) |> 
  
  addLegend(
    pal = colours_red,
    values = ~ ucs_comp_florestal_unido$taxa_avist_media,
    title = "Taxa de avistamento",
    position = "bottomleft"
  ) |>
  
  # Layers Control
  
  addLayersControl(
    baseGroups = c(#"StamenTonerLite",
      "Imágenes de ESRI",
      "Open Street Map"),
    
    position = "topright")

mapa


# --------------------------------------


## Calculando o número de registros por espécie, por trilha, por ano e por UC

registros_taxon_validado <- masto_aves |>
  group_by(uc, ano, ea, classe, taxon_validado) |>
  count() |>
  drop_na(taxon_validado)

# View(registros_taxon_validado)


# Juntanto as tabelas esforco_total_trilha + registros_taxon_validado

taxa_avistamento_trilha_taxon_validado <-
  left_join(registros_taxon_validado,
            esforco_total_trilha,
            c("uc", "ano", "ea"))

#View(taxa_avistamento_trilha_taxon_validado)



# Calculando a taxa de avistamento

taxa_avistamento_trilha_taxon_validado <-
  taxa_avistamento_trilha_taxon_validado |>
  mutate(taxa_avistamento = (n / extensao) * 10)

#View(taxa_avistamento_trilha_taxon_validado)


# Taxa de avistamento de espécies por UC e por ano

taxa_avistamento_trilha_taxon_validado_ano <-
  taxa_avistamento_trilha_taxon_validado |>
  group_by(uc, ano, classe, taxon_validado) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975) * ep)


#View(taxa_avistamento_trilha_taxon_validado_ano)


# Taxa de avistamento de espécies de mamíferos por UC

taxa_avistamento_trilha_taxon_validado_mamiferos <-
  taxa_avistamento_trilha_taxon_validado |>
  filter(classe == "Mammalia") |>
  group_by(uc, taxon_validado) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975) * ep)

#View(taxa_avistamento_trilha_taxon_validado_mamiferos)



# Taxa de encontro de espécies de aves por UC

taxa_avistamento_trilha_taxon_validado_aves <-
  taxa_avistamento_trilha_taxon_validado |>
  filter(classe == "Aves") |>
  group_by(uc, taxon_validado) |>
  summarise(
    n = n(),
    taxa_avist_media = mean(taxa_avistamento),
    dp = sd(taxa_avistamento),
    ep = dp / (sqrt(n)),
    IC_95 = qnorm(.975) * ep)

#View(taxa_avistamento_trilha_taxon_validado_aves)


# ------------------------------------


### Preparando os dados de espécies ameaçadas


lista_spp_ameacadas_salve <- read_csv2("dados/spp_ameacadas/lista_spp_ameacadas_salve_ajustada_cemave_cpb_13.12.2023.csv")

#View(lista_spp_ameacadas_salve)


# Alterando do nome da coluna nome_cientifico para taxon_validado

lista_spp_ameacadas_salve <- lista_spp_ameacadas_salve |>
  rename(taxon_validado = nome_cientifico)

# Juntando as tabelas taxa_avistamento_trilha_taxon_validado + lista_spp_ameacadas_salve e gerando a planilha de taxa de avistamento das espécies ameaçadas

taxa_avistamento_spp_ameacadas <- left_join(taxa_avistamento_trilha_taxon_validado, lista_spp_ameacadas_salve, by = "taxon_validado")

#View(taxa_avistamento_spp_ameacadas)

# Limpando a planilha

taxa_avistamento_spp_ameacadas <- taxa_avistamento_spp_ameacadas |>
  select(-classe.y) |> 
  rename(classe = classe.x) |> 
  filter(!is.na(categoria))


#View(taxa_avistamento_spp_ameacadas)

# Obtendo o total de registros de espécies ameaçadas por uc

taxa_avistamento_spp_ameacadas_uc <- taxa_avistamento_spp_ameacadas |>
  group_by(uc, taxon_validado) |> 
  count()

#View(taxa_avistamento_spp_ameacadas_uc)

# ----------------------------



#### Abundância para algumas espécies ameaçadas de mamíferos e aves
  
                                  

                                ### Figura 5.10 ###
  

# Variação no tempo das taxas de espécies ameaçadas - primatas Atelídeos


cor_ameacadas2 <- c("#008B8B", "#666666", "#BF8120", "brown")


G_especie_ameacada_ucs_ano_media_primatas_atelideos <- taxa_avistamento_spp_ameacadas |>
  group_by(ano, classe, ordem, taxon_validado, categoria) |> 
  filter(taxon_validado %in% c("Ateles chamek", "Ateles marginatus")) |>
  summarise(n = n(),
            taxa_avist_media = mean(taxa_avistamento),
            sd = sd(taxa_avistamento),
            ep = sd / sqrt(n),
            IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media, color = taxon_validado, shape = categoria)) +
  # geom_errorbar(
  #   aes(ymin = taxa_avist_media - IC_95, ymax = taxa_avist_media + IC_95),
  #   size = 0.8,
  #   linewidth = 0.5,
  #   width = 0.1,
  #   colour = "cyan4") +
  geom_line(size = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text=element_text(size=10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    #axis.line.y = element_line(),
    #axis.ticks.y = element_line(),
    title = element_text(size = 10),
    legend.text=element_text(size=10, face = "italic"),
    legend.position = "right",
    legend.title = element_blank()) +
  scale_color_manual(values = cor_ameacadas2) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_x_continuous(breaks = taxa_avistamento_spp_ameacadas$ano) +
  labs(title = "Primatas - Atelídeos",
       x = "Ano",
       y = "Avistamentos/10 km",
       col = NULL)

G_especie_ameacada_ucs_ano_media_primatas_atelideos

# Salvando o gráfico

ggsave("imagens/cap05/ma_taxas_spp_ameacadas_atelideos.jpg", plot = G_especie_ameacada_ucs_ano_media_primatas_atelideos, height = 4.5, width = 6, units = "in", dpi = 300)


# ----------------------------------



                                ### Figura 5.11 ###


# Variação no tempo das taxas de espécies ameaçadas - outros primatas




cor_ameacadas1 <- c("#008B8B", "#70C4C4", "#666666", "#BF8120", "brown")


G_especie_ameacada_ucs_ano_media_outros_primatas <- taxa_avistamento_spp_ameacadas |>
  group_by(ano, classe, ordem, taxon_validado, categoria) |> 
  filter(taxon_validado %in% c("Mico rondoni", "Chiropotes utahickae")) |>
  summarise(n = n(),
            taxa_avist_media = mean(taxa_avistamento),
            sd = sd(taxa_avistamento),
            ep = sd / sqrt(n),
            IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media, color = taxon_validado, shape = categoria)) +
  # geom_errorbar(
  #   aes(ymin = taxa_enc_media - IC_95, ymax = taxa_enc_media + IC_95),
  #   size = 0.8,
  #   linewidth = 0.5,
  #   width = 0.1,
  #   colour = "cyan4") +
  geom_line(size = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text=element_text(size=10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    #axis.line.y = element_line(),
    #axis.ticks.y = element_line(),
    title = element_text(size = 10),
    legend.text=element_text(size=10, face = "italic"),
    legend.position = "right",
    legend.title = element_blank()) +
  scale_color_manual(values = cor_ameacadas1) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_x_continuous(breaks = taxa_avistamento_spp_ameacadas$ano) +
  labs(title = "Outros Primatas",
       x = "Ano",
       y = "Avistamentos/10 km",
       col = NULL)

G_especie_ameacada_ucs_ano_media_outros_primatas


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_spp_ameacadas_outros_primatas.jpg", plot =  G_especie_ameacada_ucs_ano_media_outros_primatas,  height = 4.5, width = 6, units = "in", dpi = 300)

# ------------------------------------

                             
                             ### Figura 5.12 ###


# Variação no tempo das taxas de espécies ameaçadas - tamanduá


cor_ameacadas3 <- c("cyan4")

G_especie_ameacada_ucs_ano_media_tamandua <- taxa_avistamento_spp_ameacadas |>
  group_by(ano, classe, ordem, taxon_validado, categoria) |> 
  filter(taxon_validado %in% c("Myrmecophaga tridactyla")) |>
  summarise(n = n(),
            taxa_avist_media = mean(taxa_avistamento),
            sd = sd(taxa_avistamento),
            ep = sd / sqrt(n),
            IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media, color = taxon_validado, shape = categoria)) +
  # geom_errorbar(
  #   aes(ymin = taxa_enc_media - IC_95, ymax = taxa_enc_media + IC_95),
  #   size = 0.8,
  #   linewidth = 0.5,
  #   width = 0.1,
  #   colour = "cyan4") +
  geom_line(size = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text=element_text(size=10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    #axis.line.y = element_line(),
    #axis.ticks.y = element_line(),
    title = element_text(size = 10),
    legend.text=element_text(size=10, face = "italic"),
    legend.position = "right",
    legend.title = element_blank()) +
  scale_color_manual(values = cor_ameacadas3) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(breaks = taxa_avistamento_spp_ameacadas$ano) +
  labs(title = "Outros mamíferos",
       x = "Ano",
       y = "Avistamentos/10 km",
       col = NULL)

G_especie_ameacada_ucs_ano_media_tamandua


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_app_ameacadas_tamandua.jpg", plot = G_especie_ameacada_ucs_ano_media_tamandua, height = 4.5, width = 6, units = "in", dpi = 300)


# --------------------------------------


                              ### Figura 5.13 ###


# Variação no tempo das taxas de espécies ameaçadas - aves


cor_ameacadas1 <- c("#008B8B", "#70C4C4", "#666666", "#BF8120", "brown")



G_especie_ameacada_ucs_ano_media_aves_selecionadas <- taxa_avistamento_spp_ameacadas |>
  group_by(ano, classe, ordem, taxon_validado, categoria) |> 
  filter(taxon_validado %in% c("Aburria cujubi", "Psophia obscura", "Psophia dextralis", "Psophia interjecta", "Psophia viridis")) |>
  summarise(n = n(),
            taxa_avist_media = mean(taxa_avistamento),
            sd = sd(taxa_avistamento),
            ep = sd / sqrt(n),
            IC_95 = qnorm(.975) * ep) |> 
  
  ggplot(aes(
    x = ano,
    y = taxa_avist_media, color = taxon_validado, shape = categoria)) +
  # geom_errorbar(
  #   aes(ymin = taxa_enc_media - IC_95, ymax = taxa_enc_media + IC_95),
  #   size = 0.8,
  #   linewidth = 0.5,
  #   width = 0.1,
  #   colour = "cyan4") +
  geom_line(size = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text=element_text(size=10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    #axis.line.y = element_line(),
    #axis.ticks.y = element_line(),
    title = element_text(size = 10),
    legend.text=element_text(size=10, face = "italic"),
    legend.position = "right",
    legend.title = element_blank()) +
  scale_color_manual(values = cor_ameacadas1) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_x_continuous(breaks = taxa_avistamento_spp_ameacadas$ano) +
  labs(title = "Aves ameaçadas",
       x = "Ano",
       y = "Avistamentos/10 km",
       col = NULL)

G_especie_ameacada_ucs_ano_media_aves_selecionadas


# Salvando o gráfico

ggsave("imagens/cap05/ma_taxa_spp_ameacadas_aves.jpg", plot = G_especie_ameacada_ucs_ano_media_aves_selecionadas,  height = 4.5, width = 6, units = "in", dpi = 300)


# ---------------------------------

                          
                                ### Figura 5.14 ###


### Média Geométrica das populações (Living Planet Index -- LPI)
  

#----- ler dados
load(here("dados", "mastoaves", "tabela_Gmean.RData"))

# plotar serie temporal para G
tabela_Gmean %>%
  group_by(ano) %>%
  mutate(G = G/first(G)) %>% # escala 2014=1
  summarize(media = mean(G),
            lower10 = quantile(G, prob= 0.1),
            lower25 =  quantile(G, prob= 0.25),
            upper90 =  quantile(G, prob= 0.9),
            upper975 =  quantile(G, prob= 0.975)) %>%
  ggplot(aes(ano, media)) +
  geom_line(color = "#00AFBB", size = 1, alpha=1) + 
  geom_point(shape=21, fill = "white", stroke=1, size = 2, color="cyan4", alpha = 1) +
  #stat_summary(geom = "line", fun.y = mean) +
  #stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  geom_ribbon(aes(ymin = lower10, ymax = upper90), fill="cyan4", alpha=0.2) +
  geom_ribbon(aes(ymin = lower25, ymax = upper975), fill="cyan4", alpha=0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha=.5) +
  scale_x_continuous(breaks = tabela_Gmean$ano) +
  xlab("") +
  ylab("Média geométrica") +
  theme_classic() + 
  theme(axis.title.y = element_text(size = 12))  +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) + 
  theme(legend.position="none") +
  #theme(axis.line.x=element_blank()) +
  coord_cartesian(ylim = c(0.75, 1.25)) 


# Salvando o gráfico

ggsave("imagens/cap05/ma_media_geometrica.jpg",
       #plot = last_plot(),
       device =  "jpeg",
       scale=1.5,
       width = 210,
       height = 70,
       units = "mm",
       dpi = 300)


# knitr::include_graphics("imagens/cap05/ma_media_geometrica.JPG")


# -------------------------------


                                ### Tabela 5.1 ###


tabela_mutuns <- read.csv2("dados/tabelas/mutuns.csv", encoding="UTF-8")

tab_mutuns <- kbl(tabela_mutuns,
                  booktabs = TRUE,
                  longtable = FALSE,
                  caption = "Frequência relativa de mutuns no total de registros de aves das unidades de conservação do Programa Monitora na Amazônia.",
                  col.names = c("Unidade de Conservação", "Mutuns (%)"), align = c("l", "c")) %>%
  column_spec(2, italic = F) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = c("repeat_header"), repeat_header_text = "\\textit{(continuação)}")

tab_mutuns

#-----------------------

### Tabela alternativa

# tab_mutuns <- datatable(tabela_mutuns,
#                            #caption = 'Frequência relativa de mutuns (Crax spp. e Pauxi spp.) no total de registros de aves das unidades de conservação do Programa Monitora na Amazônia.',
# 
#                            colnames = c("Unidade de Conservação", "% de Mutuns"),
#                           
#                            #extensions = 'Buttons',
#                            elementId = NULL,
#                            # extensions= "Scroller",
#                            style="bootstrap",
#                            class="stripe hover",
#                            width="100%",
#                            rownames = F,
#                            autoHideNavigation = T,
#                            escape =FALSE,
#                            #filter = "top",
#                            options=list(deferRender=TRUE,
#                                    pageLength = 10,
#                                    scrollY=300,
#                                    dom = 'Bfrtip')) |>  
#                          #buttons = c('copy', 'csv', 'excel', 'pdf'))) %>% 
#     
# #formatStyle('unidade_conservacao',  fontWeight = 'italic', fontStyle = 'italic', `text-align` = 'left') %>% 
#     
# formatStyle(columns = c("unidade_conservacao"), `text-align` = 'left') %>%
# formatStyle(columns = c("mutuns."), `text-align` = 'center') 
# 
# 
# tab_mutuns

# ---------------------------
