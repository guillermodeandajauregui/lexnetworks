library(tidyverse)
library(tm)
library(tidytext)
library(readtext)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)
library(ggwordcloud)

# leer leyes y armar corpus ----

text_uri <- list.files(path = "NOM canceladas/", full.names = T)
textoi <- readtext::readtext(file = text_uri)
#my_corpus <- tm::VCorpus(VectorSource(textoi[["text"]]))
my_corpus <- tm::Corpus(VectorSource(textoi[["text"]]))

#my_corpus.2 <- VCorpus(my_corpus)

# limpieza: remove stopwords and shit ----

mis_stops <- tm::stopwords(kind = "es")

# muy despues tal vez necesite stops en ingles

mis_stops.en <- tm::stopwords(kind = "en")

# my_corpus <- tm_map(x = my_corpus, FUN = removeWords, c(mis_stops))

corpolist <- 
  lapply(seq_along(my_corpus), FUN = function(i){
    #head(my_corpus[[i]][["content"]])
    txt <- my_corpus[[i]][["content"]] 
    txt <- tolower(txt)
    txt <- removeWords(txt, mis_stops)
    txt <- stripWhitespace(txt)
    
  })


names_corpus <- text_uri %>% basename() 

#names(my_corpus) <- names_corpus
names(corpolist) <- names_corpus


# armar tidytext object ----
my_tidy <- corpolist %>% lapply(as_tibble) %>% bind_rows(.id = "lex")



my_tidy.monogram <- my_tidy %>% unnest_tokens(word, value) %>% select(lex, word) %>% group_by(lex) %>% mutate(num_gram = row_number())
my_tidy.bigram <- my_tidy %>% unnest_tokens(word, value, token = "ngrams", n=2) %>% select(lex, word) %>% group_by(lex) %>% mutate(num_gram = row_number())
my_tidy.bigram.sep <- my_tidy.bigram %>% separate(col = word, into = c("p1", "p2"), sep = " ")

my_tidy.pentagram <- my_tidy %>% unnest_tokens(word, value, token = "ngrams", n=5) %>% select(lex, word) %>% group_by(lex) %>% mutate(num_gram = row_number())
my_tidy.pentagram.sep <- my_tidy.pentagram %>% separate(col = word, into = c("p1", "p2", "p3", "p4", "p5"), sep = " ")


my_tidy.pentagram.sep <- 
my_tidy.pentagram.sep %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) %>% 
  select(nom, everything()) %>% 
  ungroup() %>% 
  select(everything(), lex)

my_tidy.pentagram.sep %>% 
  group_by(nom) %>% 
  select(-lex) %>% 
  filter(p1 == "reglamento") %>% 
  group_by(nom, p1, p2, p3, p4, p5) %>% 
  tally(sort = T) 






library(tabulizer)
my_meds <- vroom::vroom("data/drug_names.tsv")
path_to_cofepris <- "data/LMR_2023-01_actualizaci_n_10_febrero_2023.pdf"

xxx <- pdftools::pdf_text(pdf = path_to_cofepris)
yyy <- tabulizer::extract_tables(file = path_to_cofepris)


farmacos_mx <- lapply(yyy, FUN = function(i){i[,2]}) %>% unlist() %>% unique() %>% sort()

farmacos_mx %>% head() # tibble() #%>%  vroom::vroom_write("farmacos_mx.txt")

farmacos_mx.light <- 
  farmacos_mx %>% str_extract("\\w+")

farmacos_mx.light <- farmacos_mx.light[grepl("^[A-Z]+$", farmacos_mx.light)]
farmacos_mx.light <- farmacos_mx.light[str_length(farmacos_mx.light)>3]
farmacos_mx.light <- farmacos_mx.light %>% unique() %>% sort()
farmacos_mx.light.min <- farmacos_mx.light %>% str_to_lower()

my_tidy.pentagram.sep %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) %>% 
  group_by(nom) %>% 
  select(-lex) %>% 
  filter(p1%in%farmacos_mx.light.min) %>% 
  ungroup() %>% 
  slice(120:130)


my_tidy.pentagram.sep %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) %>% 
  group_by(nom) %>% 
  select(-lex) %>% 
  filter(p1%in%"tamoxifen") 





################################################################################

leyes_limpias <- readxl::read_xlsx("leyes_target.xlsx", sheet = 2) %>% janitor::clean_names()
  
leyes_limpias <-
  leyes_limpias %>% 
  mutate(lex_clean = semi_uniformada_formato %>% str_squish())

lex_global_clean <- leyes_limpias$lex_clean


lex_global_clean %>% str_split(pattern = " ") %>% sapply(length)


my_tidy.decimoctavograma <- my_tidy %>% unnest_tokens(word, value, token = "ngrams", n=18) %>% select(lex, word) %>% group_by(lex) %>% mutate(num_gram = row_number())



################################################################################
buscar_ley <- 
  my_tidy.decimoctavograma %>% 
  filter(grepl(paste0("^", lex_global_clean, collapse = "|"), word))


buscar_ley %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) 


reglamentos <- vroom::vroom("reglamentos_target.tsv.csv", delim = "\t", col_names = F)


reglamentos.clean <- reglamentos %>% mutate(reglamentos = X1 %>% removeWords(mis_stops) %>% str_remove("[[:punct:]]") %>%  str_squish()) %>% pull(reglamentos)

lex_global_clean.reg <- c(lex_global_clean, reglamentos.clean)

buscar_ley <- 
  my_tidy.decimoctavograma %>% 
  filter(grepl(paste0("^", lex_global_clean.reg, collapse = "|"), word))


names(lex_global_clean.reg) <- lex_global_clean.reg



buscar_ley <- 
  lapply(lex_global_clean.reg, function(i){
  pattern = paste0("^", i)
  
  my_tidy.decimoctavograma %>% 
    ungroup() %>% 
    filter(grepl(pattern=pattern, x = word))
    
})


buscar_ley %>% 
  bind_rows(.id = "ley_citada")


library(igraph)
library(tidygraph)
library(ggraph)

g <- 
  buscar_ley %>% 
  bind_rows(.id = "ley_citada") %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) %>% 
  group_by(nom, ley_citada) %>% 
  tally() %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph()


g <- 
  g %>% 
  mutate(nom = grepl(pattern = "^NOM", name))

{set.seed(123)
g %>% 
  ggraph() + 
  geom_node_point(aes(fill = nom, color = nom)) + 
  geom_edge_link(arrow = grid::arrow())
  }

buscar_ley.tbl <- 
  buscar_ley %>% 
  bind_rows(.id = "ley_citada") 


buscar_ley.tbl %>% 
   vroom::vroom_write(file = "red_noms_leyes.txt")


buscar_ley.tbl %>% 
  group_by(ley_citada) %>% 
  tally(sort = T) %>% 
  print(n = 22)








################################################################################

# buscar meds ----

meds_clean <- readxl::read_xlsx("data/LimpiaMedicamentos_VF.xlsx")
meds_clean <- meds_clean$`Cambio de nombre y apellido` %>% str_to_lower()
meds_clean <- meds_clean %>% str_squish() %>% unique()
names(meds_clean) <- meds_clean


buscar_meds <- 
  lapply(meds_clean, function(i){
    pattern = paste0("^", i)
    
    my_tidy.decimoctavograma %>% 
      ungroup() %>% 
      filter(grepl(pattern=pattern, x = word))
    
  })


buscar_meds.tbl <- 
  buscar_meds %>% 
  bind_rows(.id = "farmaco")

buscar_meds.tbl %>% 
  group_by(farmaco, lex) %>% 
  tally(sort = T)

buscar_meds.tbl %>% 
  vroom::vroom_write("nom_farmacos.txt")


buscar_meds.tbl %>% 
  group_by(farmaco) %>% 
  tally(sort = T)

################################################################################

# buscar procesos ----

mis_procesos <- c("proceso", "diagnóstico", "tratamiento")
names(mis_procesos) <- mis_procesos

buscar_procesos <- 
  lapply(mis_procesos, function(i){
    pattern = paste0("^", i)
    
    my_tidy.decimoctavograma %>% 
      ungroup() %>% 
      filter(grepl(pattern=pattern, x = word))
    
  })

buscar_procesos.tbl <- 
  buscar_procesos %>% 
  bind_rows(.id = "keywords")

buscar_procesos.tbl %>% 
  filter(keywords=="diagnóstico")


buscar_procesos.tbl %>% vroom::vroom_write("red_nom_procesos.txt")





################################################################################

# conteos de palabras ----

# global

conteos_global <- 
  my_tidy.monogram %>% 
  group_by(word) %>% 
  tally(sort = T)


conteos_global %>% 
  head(20)

conteos_por_norma <- 
  my_tidy.monogram %>% 
  group_by(lex, word) %>% 
  tally(sort = T)

conteos_por_norma %>% 
  arrange(lex, desc(n))


conteos_por_norma %>% 
  arrange(lex, desc(n)) %>% 
  mutate(n_row = row_number()) %>% 
  filter(n_row < 21) %>% 
  ggplot() + 
  aes(n, word, fill = lex) + 
  geom_bar(stat = "identity") + 
  facet_wrap(facets = vars(lex), scales = "free_y") + 
  theme_minimal() + 
  theme(legend.position = "none")
  

################################################################################

# analisis de sentimientos ----

dict_sent <- vroom::vroom("~/DATA/lexico_afinn.en.es.csv")

inner_join(x = my_tidy.monogram, y = dict_sent, by=c("word" = "Palabra")) %>% 
  group_by(lex) %>% 
  summarise(puntaje = sum(Puntuacion))

my_tidy.monogram %>% 
  ungroup() %>% 
  slice(925:927)

dict_sent %>% 
 # slice(926)
  filter(Palabra=="nom")
  
  
# una redecita --- 
# redes de palabras con sus colores de sentimientos asi aca bien aca

my_tidy.bigram.sep


g <- 
  my_tidy.bigram.sep %>% 
  select(p1, p2, lex) %>% 
  graph_from_data_frame(directed = T) %>% 
  as_tbl_graph()


dict_sent.light <- 
  dict_sent %>% 
  select(-Word) %>% 
  group_by(Palabra) %>% 
  filter(Puntuacion == max(Puntuacion)) %>% 
  unique() 

g.sent <- 
  left_join(x = g, y = dict_sent.light, by=c("name" = "Palabra")) %>% 
  filter(!is.na(Puntuacion))
  
g.sent %>% 
  mutate(componente = group_components()) %>% 
  filter(componente == 1) %>% 
  ggraph() + 
  geom_node_text(mapping = aes(color = Puntuacion, label=name)) + 
  geom_edge_link(alpha = 0.1) + 
  scale_color_gradient2(mid="black") #+ 

################################################################################

# Ya las figuras: ---- 

# F1 las palabras claves top 20 globales y 


# c("a", "b", "c")%in%c("b", "d", "e", "a")

p1 <- 
  conteos_global %>% 
  mutate(es_num = as.numeric(word)) %>%  #quito numeros
  filter(is.na(es_num)) %>% 
  filter(!(word%in%mis_stops.en)) %>%  #quito stops en inglés 
  head(20) %>% 
  arrange((n)) %>% 
  mutate(word = as_factor(word)) %>% 
  ggplot() + 
  aes(x = n, y = word) + 
  geom_bar(stat = "identity", fill = "navy") + 
  ylab("Palabra") + 
  xlab("Frecuencia") + 
  ggtitle("Palabras más frecuentes en las 34 NOMs canceladas", 
          subtitle = "20 palabras más frecuentes") + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 12))


setwd("figuras/")
p1
ggsave(p1, path = "p1.pdf", width = 15, height = 10, units = "in")


p1_plat <- 
  conteos_global %>% 
  mutate(es_num = as.numeric(word)) %>%  #quito numeros
  filter(is.na(es_num)) %>% 
  filter(!(word%in%mis_stops.en)) %>%  #quito stops en inglés 
  filter(!(word%in%c("salud", "norma", "oficial", "ssa2", "nom"))) %>% 
  head(20) %>% 
  arrange((n)) %>% 
  mutate(word = as_factor(word)) %>% 
  ggplot() + 
  aes(x = n, y = word) + 
  geom_bar(stat = "identity", fill = "navy") + 
  ylab("Palabra") + 
  xlab("Frecuencia") + 
  ggtitle("Palabras más frecuentes en las 34 NOMs canceladas", 
          subtitle = "20 palabras más frecuentes") + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 12))


ggsave("figuras/p1_sans.pdf", plot = p1_plat, width = 20, height = 15, units = "in")

{
  set.seed(725)
  
  p1a <- 
  conteos_global %>% 
  mutate(es_num = as.numeric(word)) %>%  #quito numeros
  filter(is.na(es_num)) %>% 
  filter(!(word%in%mis_stops.en)) %>%  #quito stops en inglés 
  filter(n > 100) %>% 
  filter(!grepl(pattern = "^www", x = word)) %>%
  filter(!grepl(pattern = "php", x = word))  %>%
  filter(!grepl(pattern = "^http", x = word))  %>% 
  wordcloud2::wordcloud2()
  }


p1a
# y vemos las de las leyes individuales ---- 

## sacar las normas ----

conteos_por_norma <- 
  conteos_por_norma %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*"))  
  
mis_noms <- 
  conteos_por_norma %>% 
    pull(nom) %>% 
    unique()

names(mis_noms) <- mis_noms 



top_20_por_norma <- 
  map(.x = mis_noms, .f = function(i){
    
    conteos_por_norma %>% 
      filter(nom == i) %>% 
      ungroup() %>% 
      select(nom, word, n) %>% 
      mutate(es_num = as.numeric(word)) %>%  #quito numeros
      filter(is.na(es_num)) %>% 
      filter(!(word%in%mis_stops.en)) %>%  #quito stops en inglés
      filter(str_length(word)>2) %>%  #quito palabras de largo menor a 3
      filter(!grepl(pattern = "^www", x = word)) %>%
      filter(!grepl(pattern = "php", x = word))  %>%
      filter(!grepl(pattern = "^http", x = word))  %>% 
      slice(1:15) %>%
      arrange(n) %>% 
      mutate(word = as_factor(word)) 
    
  })

names(top_20_por_norma) <- mis_noms

mis_histogramas_de_tops <- 
  map(.x = mis_noms, .f = function(i){
    
    #saco los conteos   
    top_20_por_norma[[i]] %>%
      #los paso al ggplot
      ggplot() +
      aes(x = n, y = word) +
      geom_bar(stat = "identity", fill = "navy") +
      ylab("Palabra") +
      xlab("Frecuencia") +
      ggtitle(i,
              ) +
      theme_minimal() +
      theme(axis.text = element_text(size = 8))

      
    })




p2 <- patchwork::wrap_plots(mis_histogramas_de_tops)

ggsave("figuras/p2_rv.pdf", p2,  width = 20, height = 15, units = "in")

# F2 sentimiento ---- 


data_sentimiento <- 
  inner_join(x = my_tidy.monogram, 
             y = dict_sent, 
             by=c("word" = "Palabra")) #%>% 


data_sentimiento %>% 
  group_by(lex) %>% 
  summarise(puntaje = sum(Puntuacion))


p3 <- 
  data_sentimiento %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*"))  %>% 
  group_by(nom) %>% 
  summarise(puntaje = sum(Puntuacion), 
            palabras = n(), 
            sentimiento_promedio = puntaje / palabras
            ) %>% 
  mutate(color = ifelse(sentimiento_promedio < 0, "red", "dodgerblue")) %>% 
  ggplot() + 
  aes(y = nom, x = sentimiento_promedio, fill = color) + 
  geom_bar(stat="identity") + 
  scale_fill_identity() + 
  xlab("Sentimiento promedio") + 
  ylab("Norma Oficial Mexicana") + 
  ggtitle("Análisis de sentimiento del texto") + 
  theme_minimal()

p3


# P4 redes de palabras

p4 <- 
  g.sent %>% 
  simplify() %>% 
  as_tbl_graph() %>% 
  mutate(componente = group_components()) %>% 
  filter(componente == 1) %>% 
  ggraph() + 
  geom_node_text(mapping = aes(color = Puntuacion, label=name)) + 
  #geom_edge_link(alpha = 0.1, arrow = grid::arrow(length = units())) + 
  geom_edge_link(arrow = grid::arrow(length = unit(0.125, "inches")), alpha = 0.25, color = "black", position = "jitter") + 
  theme_graph() + 
  theme(legend.position = "none") + 
  scale_color_gradient2(mid="black") #+ 

p4

g.sent %>% 
  simplify() %>% 
  as_tbl_graph() %>% 
  mutate(componente = group_components()) %>% 
  filter(componente == 1) %>% 
  mutate(grado = centrality_degree(mode = "all")) %>% 
  arrange(desc(grado)) %>% 
  as_tibble() %>% 
  print(n=10)
  

#p5 por nom 

my_tidy.bigram.sep.light <- 
  my_tidy.bigram.sep %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*"))  %>% 
  ungroup() %>% 
  select(nom, p1, p2) 


my_tidy.bigram.sep.light %>% 
  filter(nom == "NOM-006-SSA2-2013") %>% 
  select(-nom) %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  left_join(x = ., y = dict_sent.light, by=c("name" = "Palabra")) %>% 
  filter(!is.na(Puntuacion))

lista_redes_texto <- 
  lapply(mis_noms, function(i){
    
    ggg <- 
      my_tidy.bigram.sep.light %>% 
      filter(nom == i) %>% 
      select(-nom) %>% 
      graph_from_data_frame() %>% 
      as_tbl_graph() %>% 
      mutate(es_num = as.numeric(name)) %>%  #quito numeros
      filter(is.na(es_num)) %>% 
      filter(!(name%in%mis_stops.en)) %>%  #quito stops en inglés 
      filter(!grepl(pattern = "^www", x = name)) %>%
      filter(!grepl(pattern = "php", x = name))  %>%
      filter(!grepl(pattern = "^http", x = name))  %>% 
      left_join(x = ., y = dict_sent.light, by=c("name" = "Palabra")) %>% 
      filter(!is.na(Puntuacion))
    
    ggg %>% 
      simplify() %>% 
      as_tbl_graph() %>% 
      mutate(componente = group_components()) %>% 
      filter(componente == 1) %>% 
      ggraph() + 
      #geom_node_text(mapping = aes(color = Puntuacion, label=name)) +
      geom_node_text(mapping = aes(label=name)) + 
      geom_edge_link(arrow = grid::arrow(length = unit(0.125, "inches")), 
                     alpha = 0.25, 
                     color = "black", 
                     position = "jitter") + 
      theme_graph() + 
      theme(legend.position = "none") + 
      scale_color_gradient2(mid="black") + 
      ggtitle(label = i)
    
  })

lista_redes_texto %>% patchwork::wrap_plots()

# yo creo que este no aporta ... 

# p5 red de leyes ---- 

g_leyes <- 
  buscar_ley %>% 
  bind_rows(.id = "ley_citada") %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*")) %>% 
  group_by(nom, ley_citada) %>% 
  tally() %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  mutate(is_nom = name%in%mis_noms)

## esta va para cytoscape --- 


g_leyes %>% write_graph(file = "red_leyes.graphml", format = "graphml")

# p6 

buscar_meds.tbl %>% 
  mutate(nom = str_extract(lex, pattern = "^[^.]*"))  %>% 
  ungroup() %>% 
  select(nom, farmaco) %>% 
  group_by(nom, farmaco) %>% 
  tally(sort = T) %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  mutate(is_nom = name%in%mis_noms) %>% 
  write_graph(file = "red_farmacos.graphml", format = "graphml")
  ggraph() + 
  geom_node_label(aes(label = name)) + 
  geom_edge_arc()
