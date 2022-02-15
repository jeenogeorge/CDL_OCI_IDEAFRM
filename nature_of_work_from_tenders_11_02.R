library(utils)
library(stringr)
library(tidyverse)
library(purrr)
library(RecordLinkage)
library(qdapDictionaries)
library(qdap)
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)
library(igraph)
library(ggraph)
library(qqplot)
#read the file with tenders
assam_filtrd_tender <- read.csv('E:/cdl/tables/assam_tender_locations_filtered_01_02.csv')
nrow(assam_filtrd_tender)
#deleting punctuations from tender title\
assam_filtrd_tender$tender.title <- gsub("[[:punct:][:blank:]]+", " ", assam_filtrd_tender$tender.title)
# deleting trailing space
assam_filtrd_tender$tender.title<-gsub("\\n"," ", assam_filtrd_tender$tender.title)
# normalising the case
assam_filtrd_tender$tender.title <- tolower(assam_filtrd_tender$tender.title)
#extracting words from tender-title
for (i in 1:length(assam_filtrd_tender$tender.title)){    
  #split the text by space
  #i <- 294#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_filtrd_tender$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  #split <- split[nchar(split)> 3]
  split <- split[tolower(split) %in% GradyAugmented]
  assam_filtrd_tender$words[i] <- paste(split,  collapse = " ")
  }
assam_filtrd_tender <- assam_filtrd_tender %>% dplyr::select(ocid, words)

#Create a vector containing only the text
text <- assam_filtrd_tender$words
#text <- read.delim("E:/trail.txt")
d <- tibble(txt = text)
clean <- d %>%
  unnest_tokens(bigram, txt, token = "ngrams", n = 2)
series <- clean
series %>%
  count(bigram, sort = TRUE)
series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)
A <- series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)
write.csv(A, file = 'E:/cdl/tables/word_associatn.csv')

(bigram_graph <- series %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    filter(n > 20) %>%
    graph_from_data_frame()
)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#clean <-  d %>% tidytext::unnest_tokens(bigram, d, token = "ngrams", n = 2) %>% 
  #dplyr::select(everything())

#***************for the new cloud
# Create a corpus  
docs <- Corpus(VectorSource(text))
class(docs)
#clean the text data
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
#create document-term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
view(df)
write.csv(df, file = 'E:/cdl/tables/word_cloud.csv')
#generate the word count
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=500, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

#***************************************************************
#3 WORD ASSOCIATION - NO USE

clean <- d %>%
  unnest_tokens(bigram, txt, token = "ngrams", n = 3)
series <- clean
series %>%
  count(bigram, sort = TRUE)
series %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
B <- series %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
write.csv(B, file = 'E:/cdl/tables/three_word_association.csv')
(bigram_graph <- series %>%
    separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2,word3, sort = TRUE) %>%
    unite("bigram", c(word1, word2, word3), sep = " ") %>%
    filter(n > 20) %>%
    graph_from_data_frame()
)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
