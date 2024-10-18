# capitulo capano (set/2024)
# analise quantitiva do banco de dados de revisão de literatura
# autoras: Andrea Steiner, Elia Cia, Georgia Ribeiro e Nathaly Lohane


# carregar pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(tokenizers)
library(ggplot2)
library(RColorBrewer) # paleta de cores display.brewer.all() / display.brewer.all(colorblindFriendly = TRUE)
library(viridis)   # paleta de cores - grafico multicategorias
library(forcats)   # fct_reorder function to order graphs
library(scales)    # label_wrap
library(maditr)

library(tidyverse) # analise de texto
library(tidytext)  # analise de texto


# paleta de cor (Set2): display.brewer.pal(7,"Set2")

# importar bases
df_base = read.csv("data/database_litrev.csv")
df_class_llm = read.csv("data/df_class_final_artigos.csv")
df_rev_abstracts = read.csv("data/revised_abstracts.csv")

# ================== preenchimento das variaveis ==================
## Substituindo valores vazios por nulo
df = df_base
for (col in names(df)) {
  df[[col]][df[[col]] == ""] <- NA
} 

## Substituindo coluna "abstract" por versao revisada (sem textos em portugues)
df = left_join(select(df,-c(abstract)), df_rev_abstracts, by="id")

## Função para calcular o percentual de preenchimento por coluna
perc_preenchimento <- function(df) {
  sapply(df, function(col) {
    sum(!is.na(col)) / length(col) * 100
  })
}

## Aplicando
percentual_df <- data.frame(coluna = names(df), percentual_preenchido = perc_preenchimento(df))
print(percentual_df)

# ================== comparacao entre classificações tematicas ==================
## selecionar colunas de classificações tematicas 
df_slct <- select(df, c(id, tema))
df_class_llm_slct <- select(df_class_llm, c(id, classificacao_llm_final))

## unir classificações tematicas em uma base unica e igualar labels
df_all_class = left_join(df_slct, df_class_llm_slct, by="id")  %>%
  mutate(tema_esp = case_when(tema == 1 ~ "Poluição Atmosférica",
                              tema == 2 ~ "Conservação da Biodiversidade",
                              tema == 3 ~ "Mudanças Climáticas",
                              tema == 4 ~ "Transição Energética",
                              tema == 5 ~ "Florestas e Uso da terra",
                              tema == 6 ~ "Governança Marinha/Oceanos",
                              tema == 7 ~ "Mineração",
                              tema == 8 ~ "Governança de Água Doce",
                              tema == 9 ~ "Múlti-temático"),
         tema_llm = case_when(classificacao_llm_final == "Air Pollution" ~ "Poluição Atmosférica",
                              classificacao_llm_final == "Biodiversity Conservation" ~ "Conservação da Biodiversidade",
                              classificacao_llm_final == "Climate Change" ~ "Mudanças Climáticas",
                              classificacao_llm_final == "Energy Transition" ~ "Transição Energética",
                              classificacao_llm_final == "Forests and Land Use" ~ "Florestas e Uso da terra",
                              classificacao_llm_final == "Freshwater Governance" ~ "Governança Marinha/Oceanos",
                              classificacao_llm_final == "Mining" ~ "Mineração",
                              classificacao_llm_final == "Marine/Ocean Governance" ~ "Governança de Água Doce",
                              classificacao_llm_final == "Multi-Theme" ~ "Múlti-temático"))

write.csv(df_all_class, "data/df_class_tematicas.csv", fileEncoding="UTF-16LE") # encoding para manter acentos

## criar tabela de frequencia cruzada (crosstab)
df_cross_tema = as.data.frame(table(df_all_class$tema_esp, df_all_class$tema_llm))
df_wide <- dcast(df_cross_tema, Var1 ~ Var2, value.var = "Freq") # resgatando formato "cross" da tabela
colnames(df_wide)[1] <- "tema_esp / tema_llm" # renomear a coluna para identificar as colunas do cruzamento
write.csv(df_wide, "data/df_comparar_class_tematica.csv", fileEncoding="UTF-16LE") # encoding para manter acentos


# plotar matriz de comparação
## add categoria nao utilizada no tema_esp com freq = 0
df_add_govmar = df_cross_tema %>%
  rename(tema_esp = Var1, tema_llm = Var2) %>%
  filter(tema_esp == "Conservação da Biodiversidade") %>%
  mutate(tema_esp = "Governança Marinha/Oceanos", Freq = 0)

## preparar df
df_cross_tema2 = df_cross_tema %>%
  rename(tema_esp = Var1, tema_llm = Var2) 
df_cross_tema2 = union(df_cross_tema2, df_add_govmar)

df_cross_tema2 = df_cross_tema2%>%
  mutate(tema_esp = as.character(tema_esp), tema_llm = as.character(tema_llm),
         diag = if_else(tema_esp == tema_llm, TRUE, FALSE)) %>%
  mutate(tema_esp = factor(tema_esp), tema_llm = factor(tema_llm))
  

## ajustar ordem das categorias p/ grafico
levels(df_cross_tema2$tema_esp)
df_cross_tema2$tema_esp <- fct_relevel(df_cross_tema2$tema_esp, rev)
df_cross_tema2$tema_esp <- fct_relevel(df_cross_tema2$tema_esp, "Múlti-temático")

levels(df_cross_tema2$tema_llm)
df_cross_tema2$tema_llm <- fct_relevel(df_cross_tema2$tema_llm, "Múlti-temático", after=8)


## plotar matriz
df_cross_tema2 %>%
  ggplot(aes(tema_llm, tema_esp, fill= Freq)) +
  geom_tile(color = NA) +  # Sem borda para todas as células
  geom_tile(data = df_cross_tema2[df_cross_tema2$diag == TRUE, ], 
            aes(tema_llm, tema_esp), color = "gray40", size = 0.5) +  # Borda apenas nas diagonais
  geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="turquoise4") +
  scale_x_discrete(position = "top", labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(15))+
  labs(x = "LLM",y = "Especialista", fill="Qtd. de\n artigos") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
ggsave("images/matriz_temas2.png", width = 12, height = 6, dpi = 300)

## analise (% de concordancia)
aggregate(df_cross_tema2$Freq, by=list(Concordancia=df_cross_tema2$diag), FUN=sum)

# ================== classificação tematica final ==================
df_all_class_abstract = left_join(df, df_all_class, by="id") %>%
  select(id, abstract, tema_esp, tema_llm, classificacao_llm_final)

write.csv(df_all_class_abstract, "data/df_all_class_abstract.csv", fileEncoding="UTF-16LE")


## opcao A - observar palavras chaves (entrave: idiomas diferentes)
df_refine_tema_opA = df %>% select(id, authors.keywords, keywords.plus) %>%
  mutate(keywords = tolower(if_else(is.na(authors.keywords), keywords.plus, authors.keywords)))  %>%
  select(id, keywords)

## opcao B - frequencia de palavras agrupadas pra cada classificação 
df_refine_tema_opB = df %>% select(id, abstract)
df_freq_words <- df_refine_tema_opB %>%
  mutate(abstract = str_to_lower(abstract)) %>%      # Transforma em lowercase e remover pontuações
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%             # Remove stopwords e palavras não alfabéticas
  filter(str_detect(word, "^[a-z]+$")) %>%
  group_by(id, word) %>%                             # Conta a frequência das palavras por artigo (id)
  summarise(frequencia = n(), .groups = 'drop') %>%  # Ordena por frequência decrescente
  arrange(id, desc(frequencia))

### comparar distribuição por classificações
df_words_class = left_join(df_freq_words, select(df_all_class,-c(tema, classificacao_llm_final)), by="id") %>%
  mutate(concordancia = if_else(tema_esp == tema_llm, 1, 0)) %>%
  filter(concordancia == 0)

df_words_by_tema_esp = df_words_class %>%
  group_by(tema_esp, word) %>% 
  summarise(across(frequencia, sum))

df_words_by_tema_esp = df_words_class %>%
  group_by(tema_esp, word) %>%
  summarise(frequencia_total = sum(frequencia), .groups = 'drop') %>%
  group_by(tema_esp) %>%                                                # total de palavras por tema_esp
  mutate(total_palavras = sum(frequencia_total)) %>% 
  mutate(percentual = round((frequencia_total / total_palavras) * 100, 2)) %>%    # % da palavra em relação ao total do tema_esp
  arrange(tema_esp, desc(frequencia_total))

# transformar df em matriz para plot (Document-Term Matrix)
# df_short_words_tema_esp = df_words_by_tema_esp %>% select(tema_esp,word,frequencia_total)
dtm_tema_esp = df_words_by_tema_esp %>%
  select(tema_esp,word,frequencia_total) %>%
  cast_dtm(tema_esp,word,frequencia_total)

matriz <- df_words_by_tema_esp %>%
  select(tema_esp, word, frequencia_total) %>%
  tidyr::pivot_wider(
    names_from = tema_esp, 
    values_from = frequencia_total,
    values_fill = 0  # Para preencher os valores ausentes com 0
  )
  
df_freq = df_words_by_tema_esp %>%
  select(tema_esp, word, frequencia_total)
dft = data.matrix(df_freq[1:2])

comparison.cloud(df_matriz_tema_esp, max.words=500, random.order=FALSE,c(4,0.4), title.size=1.4)
# ================== frequencia de artigos por variaveis de interesse ==================
## ---- Grafico de barras ----
int_vars = c("empirico", "pp.ambiental.policy.process", "theoretical.framework", "pergunta.de.pesquisa",
                "inferencia.causal", "desenho.pesquisa", "met", "ciclo", "tema", "idioma", "local_afiliacao", "local_pesquisado")

df_intvars = df[int_vars]
freq_tables = purrr::map(df_intvars, ~ count(tibble(x = .x), x) %>% 
             mutate(percentual = (n / sum(n) * 100)))


## "theoretical.framework"
df_freq_frmwk = freq_tables$theoretical.framework

df_freq_frmwk %>% 
  mutate(x = fct_reorder(x, n)) %>% 
  ggplot(aes(n, x)) +
  geom_bar(fill = "royalblue1", width = 0.6, stat="identity") +
  geom_text(aes(label=n), size = 3.5, hjust = -0.5) +
  scale_x_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)) +
  labs(title = "Artigos por abordagem teórica", x = "Quantidade", y = "Abordagem") + 
  theme_bw() + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
ggsave("images/qtd_byframework.png", width = 6, height = 4, dpi = 300)


## "tema"
### classificação especialista
df_freq_tema = freq_tables$tema
df_freq_tema = df_freq_tema %>%
  mutate(tema = case_when(x == 1 ~ "Poluição Atmosférica",
                          x == 2 ~ "Conservação da Biodiversidade",
                          x == 3 ~ "Mudanças Climáticas",
                          x == 4 ~ "Transição Energética",
                          x == 5 ~ "Florestas e Uso da terra",
                          x == 6 ~ "Governança Marinha/Oceanos",
                          x == 7 ~ "Mineração",
                          x == 8 ~ "Governança de Água Doce",
                          x == 9 ~ "Múlti-temático"))

### classificação llm
df_freq_tema_llm <- df_class_llm %>%
  group_by(classificacao_llm_final) %>%
  summarise(n_llm = n()) %>%
  mutate(percentual_llm = (n_llm / sum(n_llm) * 100)) %>%
  mutate(tema = case_when(classificacao_llm_final == "Air Pollution" ~ "Poluição Atmosférica",
                          classificacao_llm_final == "Biodiversity Conservation" ~ "Conservação da Biodiversidade",
                          classificacao_llm_final == "Climate Change" ~ "Mudanças Climáticas",
                          classificacao_llm_final == "Energy Transition" ~ "Transição Energética",
                          classificacao_llm_final == "Forests and Land Use" ~ "Florestas e Uso da terra",
                          classificacao_llm_final == "Freshwater Governance" ~ "Governança Marinha/Oceanos",
                          classificacao_llm_final == "Mining" ~ "Mineração",
                          classificacao_llm_final == "Marine/Ocean Governance" ~ "Governança de Água Doce",
                          classificacao_llm_final == "Multi-Theme" ~ "Múlti-temático"))

### unir as 2 classificações
df_temas <- full_join(df_freq_tema, df_freq_tema_llm, by="tema")
df_temas <- select(
  df_temas, -c(x, classificacao_llm_final))[, c("tema", "n", "percentual", "n_llm", "percentual_llm")] %>% 
  rename("n_esp" = "n", "percentual_esp" = "percentual") %>%
  replace_na(list(n_esp = 0, percentual_esp = 0))

### plotar
df_temas %>%
  pivot_longer(cols = c(n_esp, n_llm), 
               names_to = "Método", 
               values_to = "n") %>%
  mutate(tema = fct_reorder(tema, percentual_esp), Método = factor(Método, levels = c("n_llm","n_esp")),
         Método = case_when(Método == "n_llm" ~ "LLM",
                          Método == "n_esp" ~ "Especialista")) %>% 
  ggplot(aes(x = tema, y = n, fill = Método)) + 
  geom_bar(width=.8, stat='identity', position=position_dodge(.9)) + coord_flip() +
  geom_text(aes(label=n), size = 3.5, position = position_dodge(width = 0.9), hjust = -0.2) +
  scale_y_continuous(limits = c(0, 40),breaks = seq(0, 40, by = 40)) +
  scale_fill_manual(values = c("yellowgreen", "turquoise4")) + 
  labs(title = "Classificação temática dos artigos",
       subtitle = "Comparação entre métodos", x = "", y = "") + 
  theme_bw() + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
ggsave("images/comparar_class_tematica.png", width = 6, height = 4, dpi = 300)


## framework por local de afiliação (pais)
df_intvars %>%
  group_by(local_afiliacao, theoretical.framework) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill=theoretical.framework, y=n, x=local_afiliacao)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = n), size=2.5, position = position_stack(vjust = .5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Abordagem teórica por país de afiliação do autor",
       subtitle = "Primeira perspectiva",  x = "", y = "Quantidade",
       fill = "Abordagem") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     plot.title = element_text(size=12),
                     legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 7),
                     axis.title = element_text(size=10),
                     axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))
ggsave("images/qtd_byframework_pais.png", width = 6, height = 4, dpi = 300)


## pais por framework
# - paleta de cores - #
# Criar uma paleta personalizada com 15 cores, inspirada na Set3
paleta15_newSet3 <- c(
  "#8DD3C7", "#FFFFB3", "#80B1D3", "#FC8B7E", "#BEBADA", "#FDB462",  "#B3DE69", "#FCCDE5", 
  "#D9D9D9",   "#B786b8", "#CCEBC5",   "#FFED6F", "#FFD1B3",   "#DEB7F7",   "#8BCEF4" 
)
# verde-água claro, amarelo claro, roxo claro, vermelho claro, azul claro, laranja, 
# verde claro, cinza claro, roxo, verde-menta, amarelo vibrante, laranja claro, lilas, azul ceu


df_abordagem_pais = df_intvars %>%
  group_by(theoretical.framework, local_afiliacao) %>%
  summarise(n = n())

df_abordagem_pais %>%
  ggplot(aes(x = theoretical.framework, y = n, fill = local_afiliacao)) +
  geom_bar(position = "stack", stat = "identity", color = "white") +  # Adicionar bordas
  geom_text(aes(label=local_afiliacao), position = position_stack(vjust = 0.5), color="grey10", size=3) + # Add label
  scale_fill_manual(values = paleta15_newSet3) +
  theme_bw() +  # Estilo de gráfico minimalista
  labs(title = "Abordagem teórica por país de afiliação do autor",
       subtitle = "Segunda perspectiva", 
       x = "", y = "Quantidade",
       fill = "País") +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 15),
    plot.subtitle = element_text(hjust = 0, size = 12),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(hjust = 0.5, size = 10)
  )
ggsave("images/qtd_byframework_pais_2opcao.png", width = 9, height = 5, dpi = 300)



total_local_afiliacao = n_distinct(df_intvars$local_afiliacao)
df_uso_abordagens = df_intvars %>% group_by(theoretical.framework) %>% 
  summarize(n_distinct = n_distinct(local_afiliacao)) %>%
  mutate(perc_uso = (n_distinct / total_local_afiliacao) * 100)


### MOSAICO: qtd de artigos por país de afiliação
df_abord_pais = df_intvars %>%
  select(theoretical.framework, local_afiliacao) 

df_abord_pais
mosaicplot(local_afiliacao~theoretical.framework,data=df_abord_pais, col=c("Blue","Red"))



## === cruzamento abordagens e outras caracteristicas da pesquisa === #
# quebra de linha nos rotulos
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# grafico 1- ciclo de pp

df_freq_by = df_intvars %>%
  mutate(ciclo_label = case_when(ciclo == 0 ~ "Nenhuma",
                                 ciclo == 1 ~ "Agenda",
                                 ciclo == 2 ~ "Formulação",
                                 ciclo == 3 ~ "Implementação",
                                 ciclo == 4 ~ "Monitoramento/Avaliação",
                                 ciclo == 5 ~ "Duas ou mais etapas")) %>%
  group_by(theoretical.framework, ciclo_label) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n) * 100)) 

unique(df_freq_by$ciclo_label)
df_freq_by$ciclo_label <- ordered(df_freq_by$ciclo_label,
                 levels = c("Agenda", "Formulação", "Implementação", "Monitoramento/Avaliação",
                            "Duas ou mais etapas", "Nenhuma"))

# opcao 1 (categorias na vertical)

df_freq_by %>%
  ggplot(aes(x=ciclo_label, y=n, fill=ciclo_label))+
  geom_bar(stat='identity')+
  geom_text(aes(label=n), position = position_dodge(width=1), vjust=-.5, color="grey10", size=3) + # Add label
  facet_wrap(~theoretical.framework, ncol=5)+ scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(breaks = seq(0,12, by = 3), limit=c(0,12)) +
  labs(title = "Caracterização das abordagens teóricas",
       subtitle = "Etapas do ciclo de políticas públicas", x = "", y = "", fill="") + 
  theme_bw()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 15),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    strip.background = element_rect(color="gray"),
    panel.border = element_rect(fill = "transparent",color = "gray")
  )
ggsave("images/abordagemXciclo.png", width = 8, height = 3, dpi = 500)



# opcao 2 (categorias na horizontal)
df_freq_by %>%
  ggplot(aes(x=n, y=fct_rev(ciclo_label)))+
  geom_bar(stat='identity', fill = 'royalblue1')+
  facet_wrap(~theoretical.framework, ncol=5)+ 
  scale_x_continuous(breaks = seq(0,12, by = 3), limit=c(0,12)) +
  labs(title = "Ciclo de PP por abordagem teórica", x = "", y = "") + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 15),
    axis.text.y = element_text(hjust = 1, size = 10),
    strip.background = element_rect(color="gray"),
    panel.border = element_rect(fill = "transparent",color = "gray")
  )
ggsave("images/abordagemXciclo_op2.png", width = 9, height = 4, dpi = 300)

# grafico 2- desenho de pesquisa

df_freq_by2 = df_intvars %>%
  mutate(des_pesquisa_label = case_when(desenho.pesquisa == 1 ~ "Estudo de caso",
                                        desenho.pesquisa == 2 ~ "Small-n",
                                        desenho.pesquisa == 3 ~ "Large-n")) %>%
  group_by(theoretical.framework, des_pesquisa_label) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n) * 100)) 
  
  
df_freq_by2$des_pesquisa_label <- ordered(df_freq_by2$des_pesquisa_label,
                                            levels = c("Estudo de caso", "Small-n", "Large-n"))

df_freq_by2 %>%
  ggplot(aes(x=des_pesquisa_label, y =n, fill = des_pesquisa_label))+
  geom_bar(stat='identity')+
  geom_text(aes(label=n), position = position_dodge(width=1), vjust=-.5, color="grey10", size=3) + # Add label
  facet_wrap(~theoretical.framework, ncol=5)+ scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(breaks = seq(0,17, by = 1), limit=c(0,17)) +
  labs(title = "", subtitle = "Desenho de pesquisa", x = "", y = "", fill = "") + 
  theme_bw()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 15),
   # legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    strip.background = element_rect(color="gray"),
    panel.border = element_rect(fill = "transparent",color = "gray")
  )
ggsave("images/abordagemXdesenhoPesquisa.png", width = 8, height = 3, dpi = 500)


# grafico 3- metodologia

df_freq_by3 = df_intvars %>%
  mutate(met_label = case_when(met == 1 ~ "Quantitativa",
                               met == 2 ~ "Qualitativa",
                               met == 3 ~ "Multimétodos")) %>%
  group_by(theoretical.framework, met_label) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n) * 100)) 


df_freq_by3$met_label <- ordered(df_freq_by3$met_label,
                                          levels = c("Quantitativa", "Qualitativa", "Multimétodos"))

df_freq_by3 %>%
  ggplot(aes(x=met_label, y =n, fill = met_label))+
  geom_bar(stat='identity')+
  geom_text(aes(label=n), position = position_dodge(width=1), vjust=-.5, color="grey10", size=3) + # Add label
  facet_wrap(~theoretical.framework, ncol=5)+ scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(breaks = seq(0,17, by = 1), limit=c(0,17)) +
  labs(title = "", subtitle = "Metodologia", x = "", y = "", fill = "") + 
  theme_bw()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 15),
   # legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    strip.background = element_rect(color="gray"),
    panel.border = element_rect(fill = "transparent",color = "gray")
  )
ggsave("images/abordagemXmetodologia.png", width = 8, height = 3, dpi = 500)


### analise base geral
df_intvars %>%
  mutate(met_label = case_when(met == 1 ~ "Quantitativa",
                               met == 2 ~ "Qualitativa",
                               met == 3 ~ "Multimétodos")) %>%
  group_by(met_label) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n) * 100))

## === outros graficos === ##

### MOSAICO: metodologia X desenho de pesquisa (linkar com algum estudo c essa hipotese)
df_pesquisa_met = df_intvars %>%
  select(desenho.pesquisa,	met) %>% 
  mutate(des_pesquisa_label = case_when(desenho.pesquisa == 1 ~ "Estudo de caso",
                                        desenho.pesquisa == 2 ~ "Small-n",
                                        desenho.pesquisa == 3 ~ "Large-n"),
         met_label = case_when(met == 1 ~ "Quanti",
                               met == 2 ~ "Quali",
                               met == 3 ~ "Multimétodos"))

par(mar = rep(2, 4))
png("images/mosaic_metdesenho.png", width = 12, height = 6, unit = "in", res = 300)
mosaicplot(des_pesquisa_label ~ met_label,data = df_pesquisa_met,
           main = "Metodologia X Desenho de pesquisa",
           xlab = "", ylab = "") # nao gostei
dev.off()

### Opcao 2- cross tab
## criar tabela de frequencia cruzada (crosstab)
df_cross_metdes = as.data.frame(table(df_pesquisa_met$met_label, df_pesquisa_met$des_pesquisa_label))
df_cross_metdes = df_cross_metdes %>% rename(met = Var1, desenho = Var2) 

df_cross_metdes$desenho <- ordered(df_cross_metdes$desenho,
                                  levels = c("Large-n", "Small-n", "Estudo de caso"))

## plotar matriz
df_cross_metdes %>%
  ggplot(aes(desenho, met, fill= Freq)) +
  geom_tile(color = NA) +  # Sem borda para todas as células
  geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="turquoise4") +
  scale_x_discrete(position = "top", labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(15))+
  labs(x = "",y = "", fill="Qtd. de\n artigos") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
ggsave("images/matriz_metdesenho.png", width = 12, height = 6, dpi = 300)


## framework e ciclo de pp - stacked bar 
df_intvars %>%
  mutate(ciclo_label = case_when(ciclo == 0 ~ "Nenhuma",
                                 ciclo == 1 ~ "Agenda",
                                 ciclo == 2 ~ "Formulação",
                                 ciclo == 3 ~ "Implementação",
                                 ciclo == 4 ~ "Monitoramento/Avaliação",
                                 ciclo == 5 ~ "Duas ou mais etapas")) %>%
  group_by(theoretical.framework, ciclo_label) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n) * 100)) %>%
  ggplot(aes(fill=ciclo_label, y=n, x=theoretical.framework)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = n), size=2.5, position = position_stack(vjust = .5)) +
  scale_fill_brewer(palette = "Set2") +
  #labs(title = "Abordagem teórica por país", x = "", y = "Quantidade de artigos",
  #     fill = "Abordagem") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.title = element_text(size = 9), 
                     legend.text = element_text(size = 7),
                     axis.text.y = element_text(size = 9))
ggsave("images/qtd_byciclo_framework.png", width = 10, height = 4, dpi = 300)

## ---- Graficos de linha ----
# Qtd de artigos por ano
count_by_year <- df %>%
  mutate(year = as.integer(str_pad(gsub('\\.', '', Publication.Year), width = 4, side = "right", pad = "0"))) %>%
  group_by(year) %>%
  summarise(frequencia = n())

## plot
ggplot(count_by_year, aes(x = year, y = frequencia)) +
  geom_line(color = "royalblue1", size = 0.4) + 
  geom_point(color = "black", size = 1) +
  geom_text(aes(label=frequencia), size = 3, vjust= -0.5, hjust= -0.4) + 
  scale_x_continuous(breaks = seq(2003, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0,15, by = 1), limit=c(0,15)) +
  labs(title = "Artigos por ano", x = "Ano", y = "Quantidade") +
  theme_bw()
ggsave("images/qtd_by_year.png", width = 6, height = 4, dpi = 300)


# Qtd de citacoes por ano e outros agrupamentos
df_citation = df %>%
  select(id, Publication.Year, tema, theoretical.framework, cited.reference.count) %>%
  mutate(
    year = as.integer(str_pad(gsub('\\.', '', Publication.Year), width = 4, side = "right", pad = "0")),
    tema_label = case_when(tema == 1 ~ "Poluição Atmosférica",
                                tema == 2 ~ "Conservação da Biodiversidade",
                                tema == 3 ~ "Mudanças Climáticas",
                                tema == 4 ~ "Transição Energética",
                                tema == 5 ~ "Florestas e Uso da terra",
                                tema == 6 ~ "Governança Marinha/Oceanos",
                                tema == 7 ~ "Mineração",
                                tema == 8 ~ "Governança de Água Doce",
                                tema == 9 ~ "Múlti-temático")
    ) %>%
  select(!c(Publication.Year, tema))
  
summary(df_citation$cited.reference.count)

#plot - opcao 1
df_citation %>%
  ggplot(aes(x=year, y=cited.reference.count, col=factor(tema_label), label = theoretical.framework)) + 
  geom_point(aes(shape=factor(theoretical.framework)), size = 2) + 
  geom_text(aes(label=ifelse((year < 2020) | (year > 2022) | (cited.reference.count > 80),
                             as.character(theoretical.framework),'')),
            hjust=0.5, vjust=1.7, size = 2.5, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2003, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 160, by = 20)) +
  scale_color_brewer(palette="Dark2") +
  labs(colour = "Tema", shape = "Abordagem",
       title = "Citações por ano e caracteristicas do artigo",
       x = "Ano", y = "Quantidade de citações") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 9, margin = margin(b = 1, unit = "pt")), 
        legend.text = element_text(size = 7, margin = margin(l = 1, unit = "pt")),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.height = unit(0.4, "cm"))
ggsave("images/qtd_citation_by_year.png", width = 8, height = 4, dpi = 300)

  
#plot - opcao 2
df_citation %>%
ggplot(aes(x=year, y=cited.reference.count, col=factor(tema_label), label = theoretical.framework)) + 
geom_point(aes(shape=factor(theoretical.framework)), size = 2) + 
geom_text(hjust = 0.5, vjust =2, size=2.5, show.legend = FALSE) +
theme_classic() +
scale_x_continuous(breaks = seq(2003, 2023, by = 2)) +
scale_color_brewer(palette="Dark2") +
labs(colour = "Tema")

#plot - opcao 3
df_citation %>%
  ggplot(aes(x=year, y=cited.reference.count, col=factor(tema_label), label=theoretical.framework)) + 
  geom_point() +
  geom_label_repel(#aes(label = Name),
                   #box.padding   = 0.35, 
                   point.padding = 0.3,
                   segment.color = 'grey50',
                   size=3) +
  theme_classic() +
  scale_x_continuous(breaks = seq(2003, 2023, by = 2)) +
  scale_color_brewer(palette="Dark2") +
  labs(colour = "Tema")

#plot - opcao 4
df_citation %>%
  ggplot(aes(x=year, y=cited.reference.count, col=factor(tema_label))) + 
  geom_point(aes(shape=factor(theoretical.framework))) + 
  scale_x_continuous(breaks = seq(2003, 2023, by = 2)) +
  labs(shape = "Abordagem", colour = "Tema")

# ================== nuvem de palavras  ==================
