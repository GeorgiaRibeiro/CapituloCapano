# carregar pacotes
library(dplyr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggwordcloud)
#library(tm)

# ==== FUNCTIONS ==== 
# quebra de linha nos rotulos
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# ==== opção final (abstracts concat por tema) ==== 
# Importar bases
df = read.csv("data/df_all_class_abstract.csv", fileEncoding="UTF-16LE")
df = df %>% select(id, abstract, tema_esp) 

df_concat_bytema = df %>% 
  group_by(tema_esp) %>% 
  mutate(all_abstracts_tema = paste0(abstract, collapse =  "-- ")) %>%
  group_by(tema_esp, all_abstracts_tema) %>% 
  summarize(id2 = min(id)) %>%
  ungroup() %>%
  select(id2, tema_esp, all_abstracts_tema) %>%
  unique()

# Criar o corpus
docs2 <- corpus(df_concat_bytema, docid_field = "id2", text_field = "all_abstracts_tema")

par(mar = rep(0, 4))  # Ajusta as margens da plotagem
#par(mar = c(0,0,0,0))
#par(mar = c(3,3,1, 1))
docs2 |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  #dfm_group(groups = tema_esp) |>
  dfm_group(groups = addline_format(tema_esp)) |>
  dfm_trim(min_termfreq = 2, verbose = TRUE) |>
  textplot_wordcloud(comparison = TRUE,
                     max_size = 4,
                     min_size = 1,
                     labelsize = 0.7,
                     # max_words = 150,
                     #min_count = 2,
                     #scale=c(4, .5)
  )


# ==== opção considerando CLASSIFICAÇÃO TEMÁTICA DO MODELO - validar "justificativas" ==== 
# Importar bases
df = read.csv("data/df_all_class_abstract.csv", fileEncoding="UTF-16LE")
df_llm = df %>% select(id, abstract, tema_llm) 

df_concat_bytema_llm = df_llm %>% 
  group_by(tema_llm) %>% 
  mutate(all_abstracts_tema = paste0(abstract, collapse =  "-- ")) %>%
  group_by(tema_llm, all_abstracts_tema) %>% 
  summarize(id2 = min(id)) %>%
  ungroup() %>%
  select(id2, tema_llm, all_abstracts_tema) %>%
  unique()

table(df_llm$tema_llm)

### excluir categorias menos frequente (nuvem palavra so compara ate 8 temas)
df_concat_bytema_llm = df_concat_bytema_llm %>% 
  filter(! tema_llm %in% c("Conservação da Biodiversidade", "Poluição Atmosférica", 'overnança de Água Doce'))

# Criar o corpus
docs2_llm <- corpus(df_concat_bytema_llm, docid_field = "id2", text_field = "all_abstracts_tema")

par(mar = rep(0, 4))  # Ajusta as margens da plotagem
#par(mar = c(0,0,0,0))
#par(mar = c(3,3,1, 1))
docs2_llm |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  #dfm_group(groups = tema_esp) |>
  dfm_group(groups = addline_format(tema_llm)) |>
  dfm_trim(min_termfreq = 2, verbose = TRUE) |>
  textplot_wordcloud(comparison = TRUE,
                     max_size = 4,
                     min_size = 1,
                     labelsize = 0.7,
                     # max_words = 150,
                     #min_count = 2,
                     scale=c(4, 1)
  )


# ===== OUTROS TESTES ===== #
# ==== opção A. melhor saída (conferir pq n ta plotando) ==== 
# Importar bases
df = read.csv("data/df_all_class_abstract.csv", fileEncoding="UTF-16LE")
df = df %>% select(id, abstract, tema_esp) 

# Criar o corpus
docs <- corpus(df, docid_field = "id", text_field = "abstract")

par(mar = rep(0, 4))  # Ajusta as margens da plotagem
#par(mar = c(0,0,0,0))
#par(mar = c(3,3,1, 1))
dfmat_docs <- docs |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  #dfm_group(groups = tema_esp) |>
  dfm_group(groups = addline_format(tema_esp)) |>
  dfm_trim(min_termfreq = 2, verbose = FALSE) |>
  textplot_wordcloud(comparison = TRUE,
                     # max_words = 150,
                     max_size = 5,
                     min_size = 1,
                     labelsize = .5,
                     #min_count = 2,
                     scale=c(4, .5)
  )

# ==== opção B. ==== 

#install.packages("ggwordcloud")
library(ggwordcloud)
# Criar dataframe de palavras
corpus <- corpus(df, text_field = "abstract", docid_field = "id")
docvars(corpus, "tema_esp") <- df$tema_esp  # Associar 'tema_esp' ao corpus

# Criar a matriz documento-feição (dfm), agrupando por 'tema_esp'
dfmat <- corpus |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  dfm_group(groups = tema_esp)

# Extrair as frequências das palavras
word_freq <- textstat_frequency(dfmat, groups = "tema_esp")

# Criar a nuvem de palavras com ggwordcloud, agrupada por 'tema_esp'
ggplot(word_freq, aes(label = feature, size = frequency, color = group)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +  # Ajustar o tamanho máximo das palavras
  facet_wrap(~ group, scales = "free_y") +  # Separar por 'tema_esp'
  theme_minimal()
