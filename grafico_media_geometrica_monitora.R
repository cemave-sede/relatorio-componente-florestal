
# grafico serie temporal media geometrica (G) programa Monitora
library(here)
library(tidyverse)

#----- ler dados
load(here("data", "tabela_Gmean.RData"))

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
  geom_point(shape=21, fill = "white", stroke=1, size = 2, color="steelblue", alpha = 1) +
  #stat_summary(geom = "line", fun.y = mean) +
  #stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  geom_ribbon(aes(ymin = lower10, ymax = upper90), fill="steelblue", alpha=0.2) +
  geom_ribbon(aes(ymin = lower25, ymax = upper975), fill="steelblue", alpha=0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha=.5) +
  xlab("") +
  ylab("Média geométrica") +
  theme_classic() + 
  theme(axis.title.y = element_text(size = 12))  +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) + 
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0.75, 1.25)) 
