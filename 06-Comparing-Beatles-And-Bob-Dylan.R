library(geniusR)
library(tidyverse)
library(tidytext)
library(tidyr)
library(installr)
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
library(pander)


# DOWNLOADING LYRICS FROM GENIUS.COM

albums <-  tibble(
  artist = c(
    rep("The Beatles", 13),
    rep("Bob Dylan", 36),
    rep("The Alan Parsons Project", 10)
  )
  ,
  album = c(
    "Please Please Me", "With The Beatles", "A Hard Day s Night",
    "Beatles For Sale", "Help", "Rubber Soul",
    "Revolver", "Sgt Pepper s Lonely Hearts Club Band", "Magical Mystery Tour",
    "The Beatles The White Album", "Yellow Submarine", "Abbey Road",
    "Let It Be",
    "Bob dylan", "The freewheelin bob dylan", "Another side of bob dylan",
    "The times they are a changin", "Bringing it all back home", "Highway 61 revisited",
    "Blonde on blonde", "John wesley harding", "Nashville skyline",
    "New morning", "Self portrait", "Pat garrett billy the kid",
    "Triplicate", "Blood on the tracks", "The basement tapes",
    "Desire", "Street legal", "Slow train coming",
    "Saved", "Shot of love", "Infidels",
    "Empire burlesque", "Knocked out loaded", "Down in the groove",
    "Oh mercy", "Under the red sky", "Good as i been to you",
    "World gone wrong", "Time out of mind", "Love and theft",
    "Modern times", "Together through life", "Christmas in the heart",
    "Tempest", "Shadows in the night", "Fallen angels",
    "Tales of mystery and imagination edgar allan poe", "I robot", "Pyramid",
    "Eve", "The turn of a friendly card", "Eye in the sky", 
    "Ammonia avenue", "Vulture culture", "Stereotomy",
    "Gaudi"
  )
)

all_lyrics <- album_lyrics <- albums %>% 
  mutate(tracks = map2(artist, album, genius_album))

full_lyrics <- all_lyrics %>% 
  unnest(tracks) %>%
  arrange(desc(artist))


# COSINE SIMILARITIES OF BEATLES, THE ALAN PARSONS PROJECT AND BOB DYLAN DISCOGRAPHIES

tidy_lyrics <- full_lyrics %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word)>2)%>%
  anti_join(stop_words) %>%
  group_by(artist) %>%
  count(word, sort = TRUE) %>%
  ungroup()

total_full_words <- tidy_lyrics %>% 
  group_by(artist) %>% 
  summarize(total = sum(n))

tidy_lyrics <- left_join(tidy_lyrics, total_full_words)

tidy_lyrics <- tidy_lyrics %>%
  mutate(freq = n / total)

full_cos <- tidy_lyrics %>%
  select(artist, word, freq)

full_cos_w <- spread(full_cos, key = artist, value = freq)

full_cos_w[is.na(full_cos_w)] <- 0

full_cos_w_matrix <- data.matrix(full_cos_w, rownames.force = NA)

full_cos_w_matrix <- full_cos_w_matrix[, -1]

cosine_matrix <- cosine(full_cos_w_matrix)

cm <- data.frame(cosine_matrix)

pander(cm, caption = "Cosine similarity matrix")


# CALCULATING AND PLOTTING TEXT COMPLEXITIES

artists <- full_lyrics %>%
  distinct(artist)

full_text_complexity <- data.table(artist = character(),
                                   mGunningFog = numeric(),
                                   mARI = numeric())

for(i in 1:nrow(artists)){
  artista <- artists[[1]][i]
  l <-  list(artista)
  temp_lyr <- full_lyrics %>%
    filter(artist == artista) %>%
    distinct(text)
  rwstat <- readability_word_stats(temp_lyr[,1])
  l <-  list.append(l, 0.4*(rwstat$n.words/nrow(temp_lyr)) +
                      100*(rwstat$n.complexes/rwstat$n.words))
  l <- list.append(l, 4.71*(rwstat$n.chars/rwstat$n.words) +
                     0.5*(rwstat$n.words/nrow(temp_lyr)) - 21.43)
  full_text_complexity <- rbind(full_text_complexity, l)
}

ggplot(data = full_text_complexity, aes(mARI, mGunningFog)) + 
  geom_point(color = "darkblue") +
  geom_text(aes(x = mARI, y = mGunningFog, label = artist), hjust=1, vjust=-0.5) + 
  theme_bw() + labs(title = "Text complexity comparison") + 
  theme(plot.title = element_text(size = rel(1.25)))  +
  xlim(-1, 1) + ylim(5,7)



