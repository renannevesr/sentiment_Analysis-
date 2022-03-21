library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
#organização 1 palavra por linha
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#utilizando o nrc
nrc_joy <- get_sentiments("nrc") %>% 
  #filtra os sentimentos de alegria
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  #fazendo a seleção das palavras
  inner_join(nrc_joy) %>%
  #contagem
  count(word, sort = TRUE)


#n
library(tidyr)
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#plotagem
library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
