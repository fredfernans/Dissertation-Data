install.packages("stringi")
install.packages("gt")
install.packages("tidyr")


library(readr)


library(stringi)
library(stringr)

library(readxl)
library(dplyr)
library(quanteda.textplots)
library(ggwordcloud)
library(gt)
library(tidyr)
library(quanteda.textstats)


write_csv(meu_df, "df_2000_2010.csv")



# CRIANDO A BASE DE DADOS

base <- "/Users/fredericofernandes/Downloads/dissertação/Fred's Dissertation/un_brazil_coding.xlsx"
abas <- excel_sheets(base)

ler_aba <- function(aba) {
  read_excel(base, sheet = aba) %>%
    mutate(aba_origem = aba) # Adiciona a coluna com o nome da aba
}

base_completa <- bind_rows(lapply(abas, ler_aba))
head(base_completa)

# CRIANDO BASE PARA ANÁLISES DE FREQUÊNCIA

# criando um corpus

corp <- corpus(base_completa, text_field = "unidade_registro")

corp_obstaculos <- corpus_subset(corp, aba_origem %in% c('categoria_obstaculos') & ano %in% c(2019:2022))

corp_parceiros <- corpus_subset(corp, aba_origem %in% c('categoria_parceiros') & ano %in% c(2019:2022))


corp_proposito <- corpus_subset(corp, aba_origem %in% c('categoria_proposito_politico', 
                                                        'categoria_proposito_economico'
                                                        )
                                      & ano %in% c(1985:1994)  
                                )

##& ano %in% c(2019:2022))


c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010)

c(1995,1996,1997,1998,1999,2000,2001,2002)

corp_responsabilidade <- corpus_subset(corp, aba_origem %in% c('categoria_responsabilidade') & ano %in% c(2019:2022
                                                                                                          ))


summary(corp_obstaculos,5)


# criando uma DFM

toks_obstaculos <- tokens(corp_obstaculos, remove_punct = T, remove_numbers = T)
toks <- tokens_select(toks_obstaculos, pattern = stopwords('pt'), selection = 'remove')
toks <- tokens_remove(toks, pattern = c('sr', 'senhor', 'senhores', 'presidente', 'esclarecimentos', 'quero', 'ser', 'srs', 'é', 'aqui'))

dfmat <- dfm(toks)

topfeatures(dfmat, 10)  # features mais frequentes

obstaculos_dfm <- base_completa %>%
  mutate(text = stri_trans_general(unidade_registro, "Latin-ASCII")) %>%
  mutate(text = str_remove_all(unidade_registro, "[[:digit:]]")) %>%
  corpus(docid_field = "doc_id", text_field = "unidade_registro") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), min_nchar = 2) %>%
  tokens_wordstem(language = "pt") %>%
  dfm() %>%
  dfm_select(pattern = c("sr", "total", "senhor", "deput", "vot", "president", "bet", 
                         "mansur", "palm", "secretário-geral", "fern", "cardos", "henriqu", "milhõ"),  selection = "remove") %>%
  dfm_subset(subset = aba_origem %in% c('categoria_responsabilidade') &
                                                          ano %in% c(2016:2022))



dfm_proposito <- base_completa %>%
  mutate(text = stri_trans_general(unidade_registro, "Latin-ASCII")) %>%
  mutate(text = str_remove_all(unidade_registro, "[[:digit:]]")) %>%
  corpus(docid_field = "doc_id", text_field = "unidade_registro") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), min_nchar = 2) %>%
  tokens_wordstem(language = "pt") %>%
  dfm() %>%
  dfm_select(pattern = c("sr", "total", "senhor", "deput", "vot", "president", "bet", 
                         "mansur", "palm","brasil", "secretário-geral", "fern", "cardos", "henriqu", "milhõ"),  selection = "remove") %>%
  dfm_subset(subset = aba_origem %in% c('categoria_proposito_politico', 'categoria_proposito_economico') &
               ano %in% c(2016:2022))


## Adicionando variável presidente

docvars(obstaculos_dfm, "presidente") <- ifelse(docvars(obstaculos_dfm, "ano") %in% c(2016:2018), 
                                                "Temer", 
                                                "Bolsonaro")


## Adicionando variável mandato

docvars(obstaculos_dfm, "mandato") <- ifelse(docvars(obstaculos_dfm, "ano") %in% c(1995,1996,1997,1998,1999,2000,2001,2002), 
                                             1, 
                                             2)


class(docvars(obstaculos_dfm)$ano)

class(docvars(dfm_proposito)$ano)


topfeatures(dfm_proposito, 10)  # features mais frequentes

word_freq <- textstat_frequency(obstaculos_dfm, n = 35)

# Criar a nuvem de palavras com fundo transparente
ggplot(word_freq, aes(label = feature, size = frequency, color = frequency)) +
  geom_text_wordcloud(eccentricity = 1, shape = "circle") +
  scale_size_area(max_size = 15) + # Ajuste o tamanho das palavras
  scale_color_gradient(low = "gray40", high = "black") + # Cores personalizadas
  theme_minimal() + 
  theme(panel.background = element_rect(fill = "transparent", color = NA), # Fundo transparente
        plot.background = element_rect(fill = "transparent", color = NA))


## NUVEM DE PALAVRAS COMPARATIVAS


dfmat_corp_language <- dfm_group(obstaculos_dfm, groups = presidente)

library(quanteda.textplots)

set.seed(132)
textplot_wordcloud(
  dfmat_corp_language, 
  comparison = TRUE, 
  max_words = 35, 
  color = c("black", "gray40")  # Cores em tons de preto/cinza
 ## comparison_layout = "vertical"  # Deixa os nomes dos grupos na horizontal
)



## CRIAR TABELA -- Divisão por anos

top_terms_per_year <- function(dfm_proposito, n_terms = 10) {
  # Obtendo os anos do docvars
  anos <- docvars(dfm_proposito, "ano")
  
  # Criando lista com os top termos para cada ano
  lista_termos <- lapply(unique(anos), function(ano) {
    termos <- names(topfeatures(dfm_proposito[anos == ano, ], n = n_terms))
    data.frame(Rank = 1:n_terms, Termos = termos, Ano = ano)
  })
  
  # Unindo os dados e pivotando
  tabela <- bind_rows(lista_termos) %>%
    pivot_wider(names_from = Ano, values_from = Termos)
  
  return(tabela)
}

# Gerando a tabela corrigida
tabela_termos <- top_terms_per_year(dfm_proposito, n_terms = 10)

# Formatando a tabela com gt
library(gt)

gt(tabela_termos) %>%
  tab_header(
    title = "Qualificadores mais recorrentes relacionados à dimensão política da identidade internacional do Brasil (1985-1989)",
 ##   subtitle = "Palavras mais recorrentes extraídas dos documentos"
  )

tabela_termos %>%
  select(
    Rank,
    all_of(
      as.character(sort(as.numeric(setdiff(names(.), "Rank"))))
    )
  ) %>%
  gt()

----------------------
  ## COOCORRÊNCIA
----------------------  



### Co-ocorrência

library(quanteda.textplots)

fcmat <- fcm(obstaculos_dfm)
print(class(fcmat))
print(fcmat)

## Alternativa ao topfeatures

topfeatures.fcm <- function(x,
                            n = 100,
                            decreasing = TRUE,
                            scheme = c("count", "docfreq"),
                            ...) {
  topfeatures(as.dfm(Matrix::forceSymmetric(x)), n = n, decreasing = decreasing, ...)
}

topfeatures.fcm(fcmat, n = 35)


feat <- names(topfeatures.fcm(fcmat, n = 35))

fcmat_select <- fcm_select(fcmat, pattern = feat, selection = "keep")

print(dim(fcmat_select))



size <- log(colSums(dfm_select(obstaculos_dfm, feat, selection = "keep")))
set.seed(144)
textplot_network(fcmat_select, min_freq = 0.90,
                 vertex_size = size / max(size) * 3)


# REDE N-GRAMS

install.packages("igraph")
install.packages("ggraph")

library(igraph)
library(ggraph)
library(tidytext)


## Análise Dimensão Obstáculos

# Converter o corpus para um data frame
df_obstaculos <- data.frame(text = as.character(corp_obstaculos), stringsAsFactors = FALSE)


# Gerar bigramas
onu_obs_bigrams <- df_obstaculos %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

obs_bigrams_separated <- onu_obs_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

stop_w <- tibble(word = stopwords(source = "stopwords-iso", language = "pt"))

stop_w <- stop_w %>%
  add_row(word = "senhor") %>%
  add_row(word = "presidente") %>%
  add_row(word = "henrique") %>%
  add_row(word = "fernandes") %>%
  add_row(word = "cardoso") %>%
  add_row(word = 'delegados') %>%
  add_row(word = 'senhores')

obs_bigrams_filtered <- obs_bigrams_separated %>%
  filter(!word1 %in% stop_w$word) %>%
  filter(!word2 %in% stop_w$word)

obs_bigram_counts <- obs_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)

obs_bigram_graph <- obs_bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()


set.seed(2017)

ggraph(obs_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +  # Remove fundos e eixos 
  coord_equal(clip = "off")


## Análise Dimensão Parceiros

# Converter o corpus para um data frame
df_parceiros <- data.frame(text = as.character(corp_parceiros), stringsAsFactors = FALSE)

# Gerar bigramas
onu_partners_bigrams <- df_parceiros %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

partners_bigrams_separated <- onu_partners_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

stop_w <- tibble(word = stopwords(source = "stopwords-iso", language = "pt"))

stop_w <- stop_w %>%
  add_row(word = "senhor") %>%
  add_row(word = "presidente") %>%
  add_row(word = "henrique") %>%
  add_row(word = "fernandes") %>%
  add_row(word = "cardoso") %>%
  add_row(word = 'delegados') %>%
  add_row(word = 'árabes') %>%
  add_row(word = 'pretendemos') %>%
  add_row(word = 'seguir') %>%
  add_row(word = 'senhores')

partners_bigrams_filtered <- partners_bigrams_separated %>%
  filter(!word1 %in% stop_w$word) %>%
  filter(!word2 %in% stop_w$word)

partners_bigrams_count <- partners_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)

bigram_partners_graph <- partners_bigrams_count %>%
  filter(n >= 2) %>%
  graph_from_data_frame()


set.seed(2017)

ggraph(bigram_partners_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void() +
  coord_equal(clip = "off")  # Permite que o texto ultrapasse os limites do painel



## Análise Dimensão Responsabilidade

# Converter o corpus para um data frame
df_responsabilidade <- data.frame(text = as.character(corp_responsabilidade), stringsAsFactors = FALSE)

# Gerar bigramas
onu_roles_bigrams <- df_responsabilidade %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

roles_bigrams_separated <- onu_roles_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

roles_bigrams_filtered <- roles_bigrams_separated %>%
  filter(!word1 %in% stop_w$word) %>%
  filter(!word2 %in% stop_w$word)

roles_bigrams_count <- roles_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)

bigram_roles_graph <- roles_bigrams_count %>%
  filter(n >= 2) %>%
  graph_from_data_frame()


set.seed(2017)

ggraph(bigram_roles_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()  # Remove fundos e eixos


### GRÁFICO DE BARRAS



library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(tidyr)

library(quanteda)
library(dplyr)
library(ggplot2)
library(quanteda.textstats)

library(quanteda)
library(dplyr)
library(ggplot2)
library(quanteda.textstats)

# Criar a variável 'presidente' corretamente ANTES de criar o corpus
base_completa <- base_completa %>%
  filter(ano >= 1985 & ano <= 2002) %>%
  mutate(presidente = case_when(
    ano >= 1985 & ano <= 1989 ~ "José Sarney",
    ano >= 1990 & ano <= 1992 ~ "Fernando Collor",
    ano >= 1992 & ano <= 1994 ~ "Itamar Franco",
    ano >= 1995 & ano <= 2002 ~ "Fernando Henrique Cardoso",
    TRUE ~ NA_character_  # Para evitar erros com valores inesperados
  ))

head(base_completa)

# Criar corpus corretamente
corpus_base <- corpus(base_completa, text_field = "unidade_registro")


# Atribuir ID único ao nome de cada documento (para evitar 'text1', 'text2'...)

base_completa <- base_completa %>%
  mutate(doc_id = row_number())  # Criar doc_id numérico único se não existir


docnames(corpus_base) <- base_completa$doc_id

# Atribuir metadados do presidente ao corpus
docvars(corpus_base, "presidente") <- base_completa$presidente

print(docvars(obstaculos_dfm))  # Deve mostrar uma coluna "presidente"

# Verificar se todos os documentos têm um presidente associado
print(table(docvars(corpus_base, "presidente"), useNA = "always"))

# Remover possíveis documentos sem presidente (caso haja NA)
corpus_base <- corpus_subset(corpus_base, !is.na(presidente))

# Criar dfm
obstaculos_dfm <- corpus_base %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), min_nchar = 2) %>%
  tokens_wordstem(language = "pt") %>%
  dfm() %>%
  dfm_select(pattern = c("sr", "total", "país","brasil", "brasileir", "senhor", "deput", "vot", "president", "bet", 
                         "mansur", "palm", "secretário-geral", "fern", "cardos", "henriqu", "milhõ"),  selection = "remove") %>%
  dfm_subset(subset = aba_origem %in% c('categoria_responsabilidade') &
               ano %in% c(1985,1986, 1987, 1988,1989, 1990, 1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002))

head(obstaculos_dfm)

# Agora agrupar por presidente corretamente
## dfm_presidente <- dfm_group(obstaculos_dfm, groups = "presidente")

dfm_presidente <- dfm_group(obstaculos_dfm, groups = docvars(obstaculos_dfm, "presidente"))

head(dfm_presidente)


# Verificar se a estrutura do `dfm_presidente` está correta
print(ndoc(dfm_presidente))  # Deve ter 4 documentos, um por presidente
print(docvars(dfm_presidente))

df_freq <- convert(dfm_presidente, to = "data.frame")

# Remover colunas de metadados que não são numéricas
df_freq <- df_freq %>%
  select(where(is.numeric))  # Mantém apenas colunas numéricas

# Adicionar de volta os nomes dos presidentes como uma coluna separada
df_freq$presidente <- rownames(dfm_presidente)

# Ajustar para formato tidy (long format)
df_freq <- df_freq %>%
  pivot_longer(-presidente, names_to = "feature", values_to = "frequency") %>%
  filter(frequency > 0) %>%  # Remover termos com frequência zero
  group_by(presidente) %>%
  arrange(desc(frequency), .by_group = TRUE) %>%
  slice_head(n = 5) %>%  # Pegar os 5 termos mais frequentes por presidente
  ungroup()


## Calcular o percentual

df_freq <- df_freq %>%
  group_by(presidente) %>%
  mutate(percentual = (frequency / sum(frequency)) * 100) %>%
  ungroup()

# Ajustar a ordem dos presidentes manualmente
df_freq$presidente <- factor(df_freq$presidente, 
                             levels = c(
                                        "Fernando Henrique Cardoso"
                                        , "Itamar Franco"
                                        , "Fernando Collor"
                                        ,"José Sarney"))

df_freq$presidente <- factor(df_freq$presidente, 
                             levels = c(
                               "José Sarney"
                               , "Fernando Collor"
                               , "Itamar Franco"
                               , 
                               "Fernando Henrique Cardoso"
                               ))


# Criar o gráfico ajustado
ggplot(df_freq, aes(x = percentual, y = presidente, fill = feature)) +
  geom_col(position = "dodge") +  # Remove a borda preta das barras
  scale_fill_grey(start = 0.3, end = 0.9) +  # Tons de cinza ao preto
  geom_text(aes(label = feature), position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 2.5) +  # Adiciona rótulos próximos às barras
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +  # Define eixo X de 0 a 100
  labs(x = "Percentual (%)",
       y = "Presidente") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove a legenda



### Código resumido

processar_texto_por_presidente <- function(base) {
  
  # 1. Filtrar dados entre 1985 e 2002
  base <- base %>%
    filter(ano >= 2003 & ano <= 2022) %>%
    mutate(presidente = case_when(
      ano >= 2003 & ano <= 2010 ~ "Lula",
      ano >= 2011 & ano <= 2015 ~ "Dilma",
      ano >= 2016 & ano <= 2018 ~ "Temer",
      ano >= 2019 & ano <= 2022 ~ "Bolsonaro",
      TRUE ~ NA_character_
    )) 
  
  # 2. Criar corpus corretamente
  corpus_base <- corpus(base, text_field = "unidade_registro")
  
  # 3. Atribuir ID único ao nome de cada documento
  base <- base %>%
    mutate(doc_id = row_number())  # Criar doc_id numérico único
  
  docnames(corpus_base) <- base$doc_id
  docvars(corpus_base, "presidente") <- base$presidente
  
  # 4. Remover documentos sem presidente
  corpus_base <- corpus_subset(corpus_base, !is.na(presidente))
  
  # 5. Criar dfm (document-feature matrix)
  obstaculos_dfm <- corpus_base %>%
    tokens(remove_punct = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), min_nchar = 2) %>%
    tokens_wordstem(language = "pt") %>%
    dfm() %>%
    dfm_select(pattern = c("sr", "total", "país","brasil", "brasileir", "senhor", "deput", "vot", "president", "bet", 
                           "mansur", "palm", "secretário-geral", "fern", "cardos", "henriqu", "milhõ"), selection = "remove") %>%
    dfm_subset(subset = aba_origem %in% c('categoria_responsabilidade', 'categoria_parceiros') & 
                 ano %in% c(2003:2022)) 
  
  # 6. Agrupar por presidente
  dfm_presidente <- dfm_group(obstaculos_dfm, groups = docvars(obstaculos_dfm, "presidente"))
  
  # 7. Converter para data.frame e ajustar formato
  df_freq <- convert(dfm_presidente, to = "data.frame") %>%
    select(where(is.numeric))  # Mantém apenas colunas numéricas
  
  # Adicionar nome dos presidentes
  df_freq$presidente <- rownames(dfm_presidente)
  
  # 8. Transformar em formato longo (tidy)
  df_freq <- df_freq %>%
    pivot_longer(-presidente, names_to = "feature", values_to = "frequency") %>%
    filter(frequency > 0) %>%
    group_by(presidente) %>%
    mutate(percentual = (frequency / sum(frequency)) * 100) %>% # Calcula percentual antes da filtragem
    arrange(desc(frequency), .by_group = TRUE) %>%
    slice_head(n = 5) %>%  # Agora pegamos os 10 mais frequentes com o percentual correto
    ungroup()
  
  # 10. Ajustar ordem dos presidentes
  df_freq$presidente <- factor(df_freq$presidente, 
                               levels = c("Lula", "Dilma", "Temer", "Bolsonaro"))
  
  return(df_freq)
}


df_resultado <- processar_texto_por_presidente(base_completa)
head(df_resultado)


# filtrar Collor e Itamar

df_collor_itamar <- df_resultado %>%
  filter(presidente %in% c("Fernando Collor", "Itamar Franco"))


ggplot(df_resultado, aes(x = percentual, y = feature, fill = feature)) +
  geom_col(position = "dodge", show.legend = FALSE) +  # Remove legenda e borda
  scale_fill_grey(start = 0.3, end = 0.9) +  # Tons de cinza ao preto
  geom_text(aes(label = feature), hjust = -0.1, size = 3, angle = 90) +  # Girar texto
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +  # Eixo X de 0 a 100
  labs(x = "Percentual (%)",
       y = NULL) +  # Remove o nome do eixo Y
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  # Remove os rótulos do eixo X
  facet_wrap(~presidente, scales = "free_y") +  # Separar gráficos por presidente
  coord_flip()  # **Inverte os eixos**


## Gráfico empilhado verticalmente

ggplot(df_collor_itamar, aes(x = percentual, y = feature, fill = feature)) +
  geom_col(position = "dodge", show.legend = FALSE) +  # Remove legenda e borda
  scale_fill_grey(start = 0.3, end = 0.9) +  # Tons de cinza ao preto
  geom_text(aes(label = feature), hjust = -0.1, size = 3, angle = 90) +  # Girar texto
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +  # Eixo X de 0 a 100
  labs(x = "Percentual (%)",
       y = NULL) +  # Remove o nome do eixo Y
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  # Remove os rótulos do eixo X
  facet_grid(presidente ~ ., scales = "free_y") +  # Empilha os gráficos
  coord_flip()  # **Inverte os eixos**


ggplot(df_freq, aes(x = reorder(feature, frequency), y = percentual, fill = presidente)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ presidente, scales = "free_y") +
  coord_flip() +  # Inverte as barras para horizontal
  labs(x = NULL, y = "Percentual (%)") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))


## KEY WORDS IN CONTEXT



toks_proposito <- tokens(corp_parceiros, remove_punct = TRUE) #tokenizando e removendo a pontuação
kw <- kwic(toks_proposito, pattern =  c('desenvolv*'))

kw_tibble <- as_tibble(kw)[, c("pre", "keyword", "post")]


## Gráfico Frequência stems

# 1. Tokenizar, remover stopwords e pontuação, aplicar stemming
toks_stem <- corp_proposito |>
  tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE
  ) |>
  tokens_remove(pattern = stopwords("portuguese")) |>
  tokens_wordstem(language = "portuguese")

# 2. Criar dfm
dfm_stem <- dfm(toks_stem)

# 3. Converter para data.frame com ano
df_stem <- convert(dfm_stem, to = "data.frame")
df_stem$ano <- docvars(dfm_stem, "ano")
df_stem$doc_id <- rownames(df_stem)

# 4. Reorganizar em formato longo
df_long <- df_stem |>
  pivot_longer(
    cols = -c(doc_id, ano),
    names_to = "stem",
    values_to = "freq"
  )

# 5. Calcular total de tokens (sem stopwords) por ano
totais_ano <- df_long |>
  group_by(ano) |>
  summarise(total_tokens = sum(freq), .groups = "drop")

# 6. Filtrar os stems desejados e calcular a porcentagem
stems_desejados <- c("liberdad", "democrac", "democrát", "desenvolv", "social")

library(ggplot2)
library(dplyr)
install.packages("ggrepel")

library(ggplot2)
library(dplyr)
library(ggrepel)


library(ggplot2)
library(dplyr)

# Garantir que o ano é inteiro
df_freq$ano <- as.integer(df_freq$ano)

# Dados dos governos
governos <- data.frame(
  presidente = c("Sarney", "Collor", " /Itamar", "FHC", "Lula", "Dilma", "Temer", "Bolsonaro"),
  inicio = c(1985, 1990, 1992, 1995, 2003, 2011, 2016, 2019),
  fim = c(1989, 1992, 1994, 2002, 2010, 2016, 2018, 2022)
)

# Gráfico
ggplot(df_freq, aes(x = ano, y = porcentagem, color = stem, linetype = stem)) +
  # Faixas dos governos
  geom_rect(data = governos, aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "gray95", alpha = 0.5) +
  
  # Curvas suavizadas
  geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 1) +
  
  # Nome dos governos
  annotate("text", x = (governos$inicio + governos$fim) / 2,
           y = max(df_freq$porcentagem, na.rm = TRUE) + 0.1,
           label = governos$presidente,
           size = 2.5, fontface = "bold", color = "gray40") +
  
  # Estilização das cores e tipos de linha
  scale_color_manual(values = c(
    "democrac" = "gray20",
    "democrát" = "gray35",
    "desenvolv" = "gray50",
    "liberdad" = "gray65",
    "social"   = "gray80"
  )) +
  scale_linetype_manual(values = c(
    "democrac" = "solid",
    "democrát" = "dashed",
    "desenvolv" = "dotdash",
    "liberdad" = "twodash",
    "social"   = "longdash"
  )) +
  
  labs(
    title = "Porcentagem dos stems por ano",
    x = "Ano",
    y = "Porcentagem (%)",
    color = "Stem",
    linetype = "Stem"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  xlim(min(df_freq$ano), max(df_freq$ano) + 3)
