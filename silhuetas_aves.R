#Imagens das silhuetas
library(grid)
library(png)

tamanho_imagem_aves_ameacadas <- 15 #Tamanho em mm

##Carregando a imagem e convertendo-a em grob
#A. cujubi
cujubi <- readPNG("cujubi.png")
cujubi_grob <- rasterGrob(cujubi, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. dextralis
dextralis <- readPNG("dextralis.png")
dextralis_grob <- rasterGrob(dextralis, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. interjecta
interjecta <- readPNG("interjecta.png")
interjecta_grob <- rasterGrob(interjecta, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. obscura
obscura <- readPNG("obscura.png")
obscura_grob <- rasterGrob(obscura, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))
#P. virridis
virridis <- readPNG("virridis.png")
virridis_grob <- rasterGrob(virridis, interpolate = TRUE, width = unit(tamanho_imagem_aves_ameacadas, "mm"), height = unit(tamanho_imagem_aves_ameacadas, "mm"))

#Nomes científicos abaixo das silhuetas
tamanho_fonte_nome_cientifico_aves_ameacadas <- 8
cujubi_text_grob <- textGrob("A. cujubi", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = tamanho_fonte_nome_cientifico_aves_ameacadas))
dextralis_text_grob <- textGrob("P. dextralis", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = tamanho_fonte_nome_cientifico_aves_ameacadas))
interjecta_text_grob <- textGrob("P. interjecta", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = tamanho_fonte_nome_cientifico_aves_ameacadas))
obscura_text_grob <- textGrob("P. obscura", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = tamanho_fonte_nome_cientifico_aves_ameacadas))
virridis_text_grob <- textGrob("P. virridis", x = unit(0.5, "npc"), y = unit(-0.2, "npc"), gp = gpar(fontface = "italic", fontsize = tamanho_fonte_nome_cientifico_aves_ameacadas))

#Posição das silhuetas
distancia_silhueta_aves_ameacadas <- 0.75     #Distância entre as silhuetas
largura_silhueta_aves_ameacadas <- 1
altura_silhueta_aves_ameacadas <- 1
x_inicial_silhueta_aves_ameacadas <- 2014     #Posição x inicial da 1ª silhueta
y_min_silhueta_aves_ameacadas <- 1.15         #y min da 1ª silhueta
y_max_silhueta_aves_ameacadas <- 1.65         #y max da 1ª silhueta

#Posição dos nomes das silhuetas
y_max_nome_silhueta_aves_ameacadas <- y_max_silhueta_aves_ameacadas - 0.2       #Em relação a silhueta
y_min_nome_silhueta_aves_ameacadas <- y_max_nome_silhueta_aves_ameacadas - 0.15 #Em relação a silhueta

#Posição das 5 silhuetas geradas automaticamente conforme as entradas anteriores. Não requer alteração
xmin_silhueta_cujubi <- x_inicial_silhueta_aves_ameacadas
xmax_silhueta_cujubi <- xmin_silhueta_cujubi + largura_silhueta_aves_ameacadas

xmin_silhueta_dextralis <- x_inicial_silhueta_aves_ameacadas + largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas
xmax_silhueta_dextralis <- xmin_silhueta_dextralis + largura_silhueta_aves_ameacadas

xmin_silhueta_interjecta <- x_inicial_silhueta_aves_ameacadas + 2*(largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas)
xmax_silhueta_interjecta <- xmin_silhueta_interjecta + largura_silhueta_aves_ameacadas

xmin_silhueta_obscura <- x_inicial_silhueta_aves_ameacadas + 3*(largura_silhueta_aves_ameacadas + distancia_silhueta_aves_ameacadas)
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