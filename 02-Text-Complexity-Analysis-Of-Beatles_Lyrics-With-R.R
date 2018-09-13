# IMPORT LIBRARIES
library(geniusR)
library(tidyverse)
library(tidytext)
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(stringr)
library(syllable)
library(ggplot2)
library(scales)
library(gridExtra)
library(lsa)
library(rlist)
library(data.table)


# DOWNLOAD LYRICS

albums <-  tibble(
  artist = 
    rep("The Beatles", 13),
  album = c(
    "Please Please Me", "With The Beatles", "A Hard Day s Night",
    "Beatles For Sale", "Help", "Rubber Soul",
    "Revolver", "Sgt Pepper s Lonely Hearts Club Band", "Magical Mystery Tour",
    "The Beatles The White Album", "Yellow Submarine", "Abbey Road",
    "Let It Be"
  )
)

album_lyrics <- albums %>% 
  mutate(tracks = map2(artist, album, genius_album))

beatles_lyrics <- album_lyrics %>% 
  unnest(tracks) 

beatles_albums <- beatles_lyrics %>%
  distinct(album)


# PLOT WORD FREQUENCIES OF SELECTED ALBUMS

tidy_beatles <- beatles_lyrics %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word)>2)%>%
  anti_join(stop_words) %>%
  group_by(album) %>%
  count(word, sort = TRUE) %>%
  ungroup()

total_words <- tidy_beatles %>% 
  group_by(album) %>% 
  summarize(total = sum(n))

tidy_beatles <- left_join(tidy_beatles, total_words)

tidy_beatles <- tidy_beatles %>%
  mutate(freq = n / total)

ppm <- tidy_beatles %>%
  filter(str_detect(album, "Please"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Please Please Me") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

help <- tidy_beatles %>%
  filter(str_detect(album, "Help"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Help") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

mys <- tidy_beatles %>%
  filter(str_detect(album, "Mystery"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Magical Myster Tour") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

lib <- tidy_beatles %>%
  filter(str_detect(album, "Let"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Let It Be") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

grid.arrange(ppm, help, mys, lib, nrow = 2)


# MEASURING COSINE SIMILARITY OF ALBUMS

tidy_beatles <- beatles_lyrics %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word)>2)%>%
  anti_join(stop_words) %>%
  group_by(album) %>%
  count(word, sort = TRUE) %>%
  ungroup()

total_words <- tidy_beatles %>% 
  group_by(album) %>% 
  summarize(total = sum(n))

tidy_beatles <- left_join(tidy_beatles, total_words)

tidy_beatles <- tidy_beatles %>%
  mutate(freq = n / total)

ppm <- tidy_beatles %>%
  filter(str_detect(album, "Please"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Please Please Me") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

help <- tidy_beatles %>%
  filter(str_detect(album, "Help"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Help") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

mys <- tidy_beatles %>%
  filter(str_detect(album, "Mystery"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Magical Myster Tour") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

lib <- tidy_beatles %>%
  filter(str_detect(album, "Let"))%>%
  arrange(desc(freq)) %>%
  top_n(10)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, freq, fill=I("steelblue3"), col=I("black"))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency") +
  coord_flip() + 
  theme_bw() + 
  labs(title = "Word frequency in Let It Be") + 
  theme(plot.title = element_text(size = rel(1))) +
  scale_y_continuous(labels = percent)

grid.arrange(ppm, help, mys, lib, nrow = 2)


# MEASURING AND PLOTTING COSINE SIMILARITY ACROSS ALBUMS

cos <- tidy_beatles %>%
  select(album, word, freq)

cos_w <- spread(cos, key = album, value = freq) # This is a matrix where entries are frequencies of words in the the various albums. NAs are replaced by zeros in the next command. 

cos_w[is.na(cos_w)] <- 0

cos_w <- cos_w %>%
  select(-word)

title <- beatles_albums[[1]][1]
a <- cos_w %>% select(as.character(title))


cosines <- data.table(album = character(),
                      cosines = numeric())

for(i in 2:nrow(beatles_albums)){
  title1 <- beatles_albums[[1]][i]
  l <- list(title1)
  b <- cos_w %>% select(as.character(title1))
  l <- list.append(l, round(sum(a*b)/sqrt(sum(a^2)*sum(b^2)),3))
  cosines <- rbind(cosines, l)
}

cosines <- data.frame(cosines)

cosines <- cosines%>%
  arrange(desc(cosines))

cosines$album <- factor(cosines$album, levels = cosines$album[order(cosines$cosines)])

ggplot(cosines) + 
  geom_col(aes(album, cosines, fill=I("steelblue3"), col=I("black")),show.legend = F) +
  theme_bw() + coord_flip() + 
  labs(title = "Cosine similarities with Please Please Me", y = "cosine similarity") + 
  theme(plot.title = element_text(size = rel(1.25))) + 
  ylim(0,1)



## MEASURING AND PLOTTING TEXT COMPLEXITY

text_complexity <- data.table(album = character(),
                              mGunningFog = numeric(),
                              mARI = numeric())

for(i in 1:nrow(beatles_albums)){
  name <- beatles_albums[[1]][i]
  l <-  list(name)
  temp_lyr <- beatles_lyrics %>%
    filter(album == as.character(name)) %>%
    distinct(text)
  rwstat <- readability_word_stats(temp_lyr[,1])
  l <-  list.append(l, 0.4*(rwstat$n.words/nrow(temp_lyr)) +
                      100*(rwstat$n.complexes/rwstat$n.words))
  l <- list.append(l, 4.71*(rwstat$n.chars/rwstat$n.words) +
                     0.5*(rwstat$n.words/nrow(temp_lyr)) - 21.43)
  text_complexity <- rbind(text_complexity, l)
}

ggplot(data = text_complexity, aes(mARI, mGunningFog)) + 
  geom_point(color = "darkblue") +
  geom_text(aes(x = mARI, y = mGunningFog, label = album), hjust=1, vjust=-0.5) + 
  theme_bw() + labs(title = "Text complexity of Beatles albums") + 
  theme(plot.title = element_text(size = rel(1.25)))  +
  ylim(4,8) + xlim(-3,1)