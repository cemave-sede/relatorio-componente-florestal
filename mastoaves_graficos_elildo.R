#
# scripts para produzir 3 graficos para o relatorio florestal
# Elildo Carvalho Jr & Marcos Fialho

# Obs 1: outros graficos produzidos anteriormente (incluindo taxas de crescimento de populacoes,
# media geometrica das tendencias etc ja foram entregues, Rmarkdown para gera-los esta em outro arquivo 

library(here)
library(tidyverse)
library(RColorBrewer)

#-----------3 graficos para o relatorio florestal-------------------

#----- ler dados
load(here("data", "monitora.RData"))

#----- grafico de esforco anual e acumulado por bioma
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
  geom_line(aes(y = esf_acumulado), colour = "darkgreen", linewidth = 1) +
  geom_line(aes(y = esforco_anual), colour = "lightgreen", linewidth = 1) +
  ylab("Esforço (km)") +
  xlab("") +
  theme_classic() +
  #scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  theme(strip.text.x = element_text(size = 16)) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  facet_wrap(~ bioma, scales = "free")
  
# salvar
ggsave("/media/elildojr/elildojr/esforco_acumulado.jpg",
       plot = last_plot(),
       device =  "jpeg",
       scale=1.5,
       width = 210,
       height = 70,
       units = "mm",
       dpi = 120)

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
           fill = "lightgreen", colour = "lightgreen") +
  geom_line(aes(y = taxon_acumulado), stat = "identity", colour = "darkgreen", linewidth = 0.8) +
  scale_y_continuous(
    name = "Taxa validados (acumulado)",  # features of first axis
    sec.axis = sec_axis(~.*0.1, name="Número de UCs"), # add second axis and specify its features
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(2014, 2022, 2)) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 16)) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  facet_wrap(~ bioma, scales = "free")

ggsave("/media/elildojr/elildojr/ucs_ativas_taxa_acumulado.jpg",
       plot = last_plot(),
       device =  "jpeg",
       scale=1.5,
       width = 210,
       height = 70,
       units = "mm",
       dpi = 120)


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
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  scale_fill_manual(values = pallete, name = "Ano", 
                    guide = guide_legend(reverse = TRUE)) +
  theme(strip.text.x = element_text(size = 16)) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 8))

ggsave("/media/elildojr/elildojr/esforco_total_uc_bioma.jpg",
       plot = last_plot(),
       device =  "jpeg",
       scale=0.6,
       width = 240,
       height = 280,
       units = "mm",
       dpi = 120)
