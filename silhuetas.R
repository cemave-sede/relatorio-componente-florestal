######### Figuras 5.10, 5.11, 5.12 e 5.13 com as silhuetas (colorido) das espécies #########


#Pacotes
library(grid)
library(png)


##### Script que deve substituir a partir da linha 1797 do script atual, seção
#"Abundância para algumas espécies ameaçadas de mamíferos e aves" até a 2060
#(ggsave("imagens/cap05/ma_taxa_spp_ameacadas_aves.jpg", plot = ...

                            
                        ### IMPORTANTE ! ! ! ###

#Os caminhos para as funções readPNG e ggsave precisam ser alterados conforme os novos caminhos
#Não houve modificações nos ggplot originais


                  ### Script alterado a partir daqui ###
                  #↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓#


#### Abundância para algumas espécies ameaçadas de mamíferos e aves

font_size_nomes_silhuetas <- 6 #Tamanho da fonte dos nomes abaixo das silhuetas

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

#Imagens das silhuetas

tamanho_imagem_atelideos_ameacados <- 15 #Tamanho em mm

##Carregando a imagem e convertendo-a em grob
#A. chamek
chamek <- readPNG("chamek.png")
chamek_grob <- rasterGrob(chamek, interpolate = TRUE, width = unit(tamanho_imagem_atelideos_ameacados, "mm"), height = unit(tamanho_imagem_atelideos_ameacados, "mm"))
#P. marginatus
marginatus <- readPNG("marginatus.png")
marginatus_grob <- rasterGrob(marginatus, interpolate = TRUE, width = unit(tamanho_imagem_atelideos_ameacados, "mm"), height = unit(tamanho_imagem_atelideos_ameacados, "mm"))

#Nomes científicos abaixo das silhuetas
chamek_text_grob <- textGrob("A. chamek", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
marginatus_text_grob <- textGrob("A. marginatus", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))

#Posição das silhuetas
distancia_silhueta_atelideos_ameacados <- 0.75     #Distância entre as silhuetas
largura_silhueta_atelideos_ameacados <- 1
x_inicial_silhueta_atelideos_ameacados <- 2014     #Posição x inicial da 1ª silhueta
y_min_silhueta_atelideos_ameacados <- 1.1         #y min da 1ª silhueta
y_max_silhueta_atelideos_ameacados <- 1.6         #y max da 1ª silhueta

#Posição dos nomes das silhuetas
y_max_nome_silhueta_atelideos_ameacados <- y_max_silhueta_atelideos_ameacados - 0.22       #Em relação a silhueta
y_min_nome_silhueta_atelideos_ameacados <- y_max_nome_silhueta_atelideos_ameacados - 0.17 #Em relação a silhueta

#Posição das 5 silhuetas geradas automaticamente conforme as entradas anteriores. Não requer alteração
xmin_silhueta_chamek <- x_inicial_silhueta_atelideos_ameacados
xmax_silhueta_chamek <- xmin_silhueta_chamek + largura_silhueta_atelideos_ameacados

xmin_silhueta_marginatus <- x_inicial_silhueta_atelideos_ameacados + largura_silhueta_atelideos_ameacados + distancia_silhueta_atelideos_ameacados
xmax_silhueta_marginatus <- xmin_silhueta_marginatus + largura_silhueta_atelideos_ameacados

#Plot G_especie_ameacada_ucs_ano_media_primatas_atelideos com as silhuetas e os nomes. Não requer alteração
G_especie_ameacada_ucs_ano_media_primatas_atelideos_silhuetas <- G_especie_ameacada_ucs_ano_media_primatas_atelideos +
  #A. chamek
  annotation_custom(grob = chamek_grob, #silhueta
                    xmin = xmin_silhueta_chamek, xmax = xmax_silhueta_chamek,
                    ymin = y_min_silhueta_atelideos_ameacados, ymax = y_max_silhueta_atelideos_ameacados) +
  annotation_custom(grob = chamek_text_grob, #texto
                    xmin = xmin_silhueta_chamek, xmax = xmax_silhueta_chamek,
                    ymin = y_min_nome_silhueta_atelideos_ameacados, ymax = y_max_nome_silhueta_atelideos_ameacados) +
  #P. marginatus
  annotation_custom(grob = marginatus_grob, #silhueta
                    xmin = xmin_silhueta_marginatus, xmax = xmax_silhueta_marginatus,
                    ymin = y_min_silhueta_atelideos_ameacados, ymax = y_max_silhueta_atelideos_ameacados) +
  annotation_custom(grob = marginatus_text_grob, #texto
                    xmin = xmin_silhueta_marginatus, xmax = xmax_silhueta_marginatus,
                    ymin = y_min_nome_silhueta_atelideos_ameacados, ymax = y_max_nome_silhueta_atelideos_ameacados)

G_especie_ameacada_ucs_ano_media_primatas_atelideos_silhuetas

# Salvando o gráfico

ggsave("ma_taxas_spp_ameacadas_atelideos_silhuetas.jpg", plot = G_especie_ameacada_ucs_ano_media_primatas_atelideos_silhuetas, height = 4.5, width = 6, units = "in", dpi = 300)


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

#Imagens das silhuetas

tamanho_imagem_outros_primatas <- 15 #Tamanho em mm

##Carregando a imagem e convertendo-a em grob
#C. utahickae
utahickae <- readPNG("utahickae.png")
utahickae_grob <- rasterGrob(utahickae, interpolate = TRUE, width = unit(tamanho_imagem_outros_primatas, "mm"), height = unit(tamanho_imagem_outros_primatas, "mm"))
#M. rondoni
rondoni <- readPNG("rondoni.png")
rondoni_grob <- rasterGrob(rondoni, interpolate = TRUE, width = unit(9, "mm"), height = unit(22, "mm"))

#Nomes científicos abaixo das silhuetas
utahickae_text_grob <- textGrob("C. utahickae", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
rondoni_text_grob <- textGrob("M. rondoni", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))

#Posição das silhuetas
distancia_silhueta_outros_primatas <- 0.55    #Distância entre as silhuetas
largura_silhueta_outros_primatas <- 1
x_inicial_silhueta_outros_primatas <- 2015    #Posição x inicial da 1ª silhueta
y_min_silhueta_outros_primatas <- 1.1         #y min da 1ª silhueta
y_max_silhueta_outros_primatas <- 1.6         #y max da 1ª silhueta

#Posição dos nomes das silhuetas
y_max_nome_silhueta_outros_primatas <- y_max_silhueta_outros_primatas - 0.22      #Em relação a silhueta
y_min_nome_silhueta_outros_primatas <- y_max_nome_silhueta_outros_primatas - 0.17 #Em relação a silhueta

#Posição das 2 silhuetas geradas automaticamente conforme as entradas anteriores. Não requer alteração
xmin_silhueta_utahickae <- x_inicial_silhueta_outros_primatas
xmax_silhueta_utahickae <- xmin_silhueta_utahickae + largura_silhueta_outros_primatas

xmin_silhueta_rondoni <- x_inicial_silhueta_outros_primatas + largura_silhueta_outros_primatas + distancia_silhueta_outros_primatas
xmax_silhueta_rondoni <- xmin_silhueta_rondoni + largura_silhueta_outros_primatas

#Plot G_especie_ameacada_ucs_ano_media_outros_primatas com as silhuetas e os nomes. Não requer alteração
G_especie_ameacada_ucs_ano_media_outros_primatas_silhuetas <- G_especie_ameacada_ucs_ano_media_outros_primatas +
  #C. utahickae
  annotation_custom(grob = utahickae_grob, #silhueta
                    xmin = xmin_silhueta_utahickae, xmax = xmax_silhueta_utahickae,
                    ymin = y_min_silhueta_outros_primatas, ymax = y_max_silhueta_outros_primatas) +
  annotation_custom(grob = utahickae_text_grob, #texto
                    xmin = xmin_silhueta_utahickae, xmax = xmax_silhueta_utahickae,
                    ymin = y_min_nome_silhueta_outros_primatas, ymax = y_max_nome_silhueta_outros_primatas) +
  #M. rondoni
  annotation_custom(grob = rondoni_grob, #silhueta
                    xmin = xmin_silhueta_rondoni, xmax = xmax_silhueta_rondoni,
                    ymin = y_min_silhueta_outros_primatas - 0.49, ymax = y_max_silhueta_outros_primatas + 0.1) +
  annotation_custom(grob = rondoni_text_grob, #texto
                    xmin = xmin_silhueta_rondoni + 0.20, xmax = xmax_silhueta_rondoni + 0.4,
                    ymin = y_min_nome_silhueta_outros_primatas, ymax = y_max_nome_silhueta_outros_primatas)

G_especie_ameacada_ucs_ano_media_outros_primatas_silhuetas

# Salvando o gráfico

ggsave("ma_taxa_spp_ameacadas_outros_primatas_silhuetas.jpg", plot =  G_especie_ameacada_ucs_ano_media_outros_primatas_silhuetas,  height = 4.5, width = 6, units = "in", dpi = 300)


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

#Imagem da silhueta

##Carregando a imagem e convertendo-a em grob
#M. tridactyla
tridactyla <- readPNG("tridactyla.png")
tridactyla_grob <- rasterGrob(tridactyla, interpolate = TRUE, width = unit(30, "mm"), height = unit(12, "mm"))

#Nome científico abaixo da silhueta
tridactyla_text_grob <- textGrob("M. tridactyla", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))

#Plot G_especie_ameacada_ucs_ano_media_tamandua com a silhueta e os nome. Não requer alteração
G_especie_ameacada_ucs_ano_media_tamandua_silhuetas <- G_especie_ameacada_ucs_ano_media_tamandua +
  #M. tridactyla
  annotation_custom(grob = tridactyla_grob, #silhueta
                    xmin = 2014.5, xmax = 2016,
                    ymin = 0.31, ymax = 0.61) +
  annotation_custom(grob = tridactyla_text_grob, #texto
                    xmin = 2014.5, xmax = 2016,
                    ymin = 0.43, ymax = 0.53)

G_especie_ameacada_ucs_ano_media_tamandua_silhuetas

# Salvando o gráfico

ggsave("ma_taxa_app_ameacadas_tamandua_silhuetas.jpg", plot = G_especie_ameacada_ucs_ano_media_tamandua_silhuetas, height = 4.5, width = 6, units = "in", dpi = 300)


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

#Imagens das silhuetas

tamanho_imagem_aves_ameacadas <- 10 #Tamanho em mm

##Carregando a imagem e convertendo-a em grob
#A. cujubi
cujubi <- readPNG("cujubi.png")
cujubi_grob <- rasterGrob(cujubi, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas + 9, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. dextralis
dextralis <- readPNG("dextralis.png")
dextralis_grob <- rasterGrob(dextralis, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. interjecta
interjecta <- readPNG("interjecta.png")
interjecta_grob <- rasterGrob(interjecta, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas - 2, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. obscura
obscura <- readPNG("obscura.png")
obscura_grob <- rasterGrob(obscura, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. virridis
virridis <- readPNG("virridis.png")
virridis_grob <- rasterGrob(virridis, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas - 3, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))

#Nomes científicos abaixo das silhuetas
cujubi_text_grob <- textGrob("A. cujubi", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
dextralis_text_grob <- textGrob("P. dextralis", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
interjecta_text_grob <- textGrob("P. interjecta", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
obscura_text_grob <- textGrob("P. obscura", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))
virridis_text_grob <- textGrob("P. virridis", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = font_size_nomes_silhuetas))

#Posição das silhuetas
distancia_silhueta_aves_ameacadas <- 0.65     #Distância entre as silhuetas
largura_silhueta_aves_ameacadas <- 1
x_inicial_silhueta_aves_ameacadas <- 2014.2     #Posição x inicial da 1ª silhueta
y_min_silhueta_aves_ameacadas <- 1.15         #y min da 1ª silhueta
y_max_silhueta_aves_ameacadas <- 1.65         #y max da 1ª silhueta

#Posição dos nomes das silhuetas
y_max_nome_silhueta_aves_ameacadas <- y_max_silhueta_aves_ameacadas - 0.2       #Em relação a silhueta
y_min_nome_silhueta_aves_ameacadas <- y_max_nome_silhueta_aves_ameacadas - 0.15 #Em relação a silhueta

#Posição das 5 silhuetas geradas automaticamente conforme as entradas anteriores. Não requer alteração
xmin_silhueta_cujubi <- x_inicial_silhueta_aves_ameacadas
xmax_silhueta_cujubi <- xmin_silhueta_cujubi + largura_silhueta_aves_ameacadas

xmin_silhueta_dextralis <- x_inicial_silhueta_aves_ameacadas + largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas + 0.2
xmax_silhueta_dextralis <- xmin_silhueta_dextralis + largura_silhueta_aves_ameacadas

xmin_silhueta_interjecta <- x_inicial_silhueta_aves_ameacadas + 2*(largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas) + 0.2
xmax_silhueta_interjecta <- xmin_silhueta_interjecta + largura_silhueta_aves_ameacadas

xmin_silhueta_obscura <- x_inicial_silhueta_aves_ameacadas + 3*(largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas) + 0.2
xmax_silhueta_obscura <- xmin_silhueta_obscura + largura_silhueta_aves_ameacadas

xmin_silhueta_virridis <- x_inicial_silhueta_aves_ameacadas + 4*(largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas)
xmax_silhueta_virridis <- xmin_silhueta_virridis + largura_silhueta_aves_ameacadas

#Plot G_especie_ameacada_ucs_ano_media_aves_selecionadas com as silhuetas e os nomes. Não requer alteração
G_especie_ameacada_ucs_ano_media_aves_selecionadas_silhuetas <- G_especie_ameacada_ucs_ano_media_aves_selecionadas +
  #A. cujubi
  annotation_custom(grob = cujubi_grob, #silhueta
                    xmin = xmin_silhueta_cujubi, xmax = xmax_silhueta_cujubi,
                    ymin = y_min_silhueta_aves_ameacadas, ymax = y_max_silhueta_aves_ameacadas) +
  annotation_custom(grob = cujubi_text_grob, #texto
                    xmin = xmin_silhueta_cujubi, xmax = xmax_silhueta_cujubi,
                    ymin = y_min_nome_silhueta_aves_ameacadas, ymax = y_max_nome_silhueta_aves_ameacadas) +
  #P. dextralis
  annotation_custom(grob = dextralis_grob, #silhueta
                    xmin = xmin_silhueta_dextralis, xmax = xmax_silhueta_dextralis,
                    ymin = y_min_silhueta_aves_ameacadas, ymax = y_max_silhueta_aves_ameacadas) +
  annotation_custom(grob = dextralis_text_grob, #texto
                    xmin = xmin_silhueta_dextralis, xmax = xmax_silhueta_dextralis,
                    ymin = y_min_nome_silhueta_aves_ameacadas, ymax = y_max_nome_silhueta_aves_ameacadas) +
  #P. interjecta
  annotation_custom(grob = interjecta_grob, #silhueta
                    xmin = xmin_silhueta_interjecta, xmax = xmax_silhueta_interjecta,
                    ymin = y_min_silhueta_aves_ameacadas, ymax = y_max_silhueta_aves_ameacadas) +
  annotation_custom(grob = interjecta_text_grob, #texto
                    xmin = xmin_silhueta_interjecta, xmax = xmax_silhueta_interjecta,
                    ymin = y_min_nome_silhueta_aves_ameacadas, ymax = y_max_nome_silhueta_aves_ameacadas) +
  #P. obscura
  annotation_custom(grob = obscura_grob, #silhueta
                    xmin = xmin_silhueta_obscura, xmax = xmax_silhueta_obscura,
                    ymin = y_min_silhueta_aves_ameacadas, ymax = y_max_silhueta_aves_ameacadas) +
  annotation_custom(grob = obscura_text_grob, #texto
                    xmin = xmin_silhueta_obscura, xmax = xmax_silhueta_obscura,
                    ymin = y_min_nome_silhueta_aves_ameacadas, ymax = y_max_nome_silhueta_aves_ameacadas) +
  #P. virridis
  annotation_custom(grob = virridis_grob, #silhueta
                    xmin = xmin_silhueta_virridis, xmax = xmax_silhueta_virridis,
                    ymin = y_min_silhueta_aves_ameacadas, ymax = y_max_silhueta_aves_ameacadas) +
  annotation_custom(grob = virridis_text_grob, #texto
                    xmin = xmin_silhueta_virridis, xmax = xmax_silhueta_virridis,
                    ymin = y_min_nome_silhueta_aves_ameacadas, ymax = y_max_nome_silhueta_aves_ameacadas)

G_especie_ameacada_ucs_ano_media_aves_selecionadas_silhuetas

ggsave("ma_taxa_app_ameacadas_aves_selecionadas_silhuetas.jpg", plot = G_especie_ameacada_ucs_ano_media_aves_selecionadas_silhuetas, height = 4.5, width = 6, units = "in", dpi = 300)
