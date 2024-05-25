### FINAL PROJECT
## Digital archives and methods 2024
# Nicoline, Rebecca, and Jacob

# Loading packages
library(tidyverse)
library(tidytext)
library(urltools)
library(dplyr)
library(ggplot2)

# Installing SENTIDA
remotes::install_github("Guscode/Sentida", force = T)
library(Sentida)

# Loading link to Mediestream search and decoding
link <- "https://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=%28j%C3%B8d%2A%20OR%20juda%2A%20OR%20hebr%2A%20OR%20semit%2A%29%20AND%20py%3A%5B1701%20TO%201800%5D&fields=timestamp&fields=fulltext_org&fields=familyId&fields=lplace&max=-1&structure=header&structure=content&format=CSV"
url_decode(link)
jews_1701_to_1800 <- read_csv(link)

# Decoded link:
"https://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=(jød* OR juda* OR hebr* OR semit*) AND py:[1701 TO 1800]&fields=timestamp&fields=fulltext_org&fields=familyId&fields=lplace&max=-1&structure=header&structure=content&format=CSV"

# Extracting grams containing jews with Tidytext package

# Creating 11 word grams
jews_grams <- jews_1701_to_1800 %>%
  unnest_tokens(grams, fulltext_org, token = "ngrams", n = 11) %>%
  filter(!is.na(grams))

# Creating separate columns
jewgrams_separated <- jews_grams %>%
  separate(grams, c("word1", "word2", "word3", "word4", "word5", "word6", "word7", "word8", "word9", "word10", "word11"), sep = " ")

# Separating 11 word sentences into rows containing one word; 6th row will contain a word stemming from Mediestream search string. Filter function contains the regular expression "\\b"search string word"[a-zæø]*"
jewgrams_filtered <- jewgrams_separated %>%
  filter(str_detect(word6, "\\bjød[a-zæø]*|\\bjuda[a-zæø]*|\\bhebr[a-zæø]*\\bsemit[a-zæø]*"))

# Re-uniting rows containing one word into a single row containing 11 words; 6th word will be a word stemming from Mediestream search string
jewgrams_united <- jewgrams_filtered %>%
  unite(grams, word1, word2, word3, word4, word5, word6, word7, word8, word9, word10, word11, sep = " ")

# Applying SENTIDA score
jewgrams_sentida <- jewgrams_united %>%
  rowwise()%>%
  mutate(sentida_score = sentida(grams, output = "mean"))

# Arranging by descending SENTIDA score
jewgrams_negative <- jewgrams_sentida %>%
  arrange(+sentida_score)

# Arranging byascending SENTIDA score
jewgrams_positive <- jewgrams_sentida %>%
  arrange(-sentida_score)

# Viewing most common SENTIDA scores; most are fairly neutral
jewgrams_sentida %>%
  count(sentida_score, sort = TRUE)

# Viewing most common cities in which newspapers in data set are published
jews_1701_to_1800 %>%
  count(lplace, sort = TRUE)

# Viewing most common newspapers in data set
jews_1701_to_1800 %>%
  count(familyId, sort = TRUE)

# Creating new data frame containing only newspapers from København (Copenhagen) (Sjælland/Zealand)
jewgrams_sentida_onlycph <- jewgrams_negative %>%
  filter(lplace == "København") %>%
  arrange(+sentida_score, sort = TRUE)

# Creating new data frame containing only newspapers from Ribe (Jylland/Jutland)
jewgrams_sentida_onlyribe <- jewgrams_negative %>%
  filter(lplace == "Ribe") %>%
  arrange(+sentida_score, sort = TRUE)

# Creating new data frame containing only newspapers from Odense (Fyn/Funen)
jewgrams_sentida_onlyodense <- jewgrams_negative %>%
  filter(lplace == "Odense") %>%
  arrange(+sentida_score, sort = TRUE)


### Using ggplot2 to create visualizations

# 1: All newspapers

# Blank space in which plot will be placed
jewgrams_sentida %>%
  ggplot()

# Adding x and y values and units
jewgrams_sentida %>%
  ggplot(aes(x=familyId,y=sentida_score))

# PLacing data on the plot
jewgrams_sentida %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot()

# Making sure overlapping values clearly visualized; before they were all same shade of black on the same point of plot
jewgrams_sentida %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot(alpha=0.3)

# Adding colors and titles for more clarity
jewgrams_sentida %>%
  ggplot(aes(x=familyId,y=sentida_score, color=familyId)) +
  geom_boxplot(alpha=0.3) +
  ggtitle("Visualisation of Sentida scores") +
  labs(y= "Sentida score", x = "Newspaper") +
  labs(color="List of newspapers")

# 2: Copenhagen newspapers; same process as with all newspapers
jewgrams_sentida_onlycph %>%
  ggplot()

jewgrams_sentida_onlycph %>%
  ggplot(aes(x=familyId,y=sentida_score))

jewgrams_sentida_onlycph %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot()

jewgrams_sentida_onlycph %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot(alpha=0.3)

jewgrams_sentida_onlycph %>%
  ggplot(aes(x=familyId,y=sentida_score, color=familyId)) +
  geom_boxplot(alpha=0.3) +
  ggtitle("Visualisation of Sentida scores") +
  labs(y= "Sentida score", x = "Newspaper") +
  labs(color="List of newspapers")

# 3: Odense newspapers; same process again
jewgrams_sentida_onlyodense %>%
  ggplot()

jewgrams_sentida_onlyodense %>%
  ggplot(aes(x=familyId,y=sentida_score))

jewgrams_sentida_onlyodense %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot()

jewgrams_sentida_onlyodense %>%
  ggplot(aes(x=familyId,y=sentida_score)) +
  geom_boxplot(alpha=0.3)

jewgrams_sentida_onlyodense %>%
  ggplot(aes(x=familyId, y=sentida_score, color=familyId)) +
  geom_boxplot(alpha=0.3) +
  ggtitle("Visualisation of Sentida scores") +
  labs(y= "Sentida score", x = "Newspaper") +
  labs(color="List of newspapers")

# Negative, positive and neutral. Note: arrange function does not work properly with sentida scores. Reason is unknown.

# Negative; 112 observations
jewgrams_onlynegative <- jewgrams_sentida %>%
  filter(sentida_score <0)

# Positive; 285 observations
jewgrams_onlypositive <- jewgrams_sentida %>%
  filter(sentida_score >0)

# Neutral (scores between -1 and 1); 399 observations; this appears to not be working properly; e.g., extremely negative scores up to -9.5 are somehow included
jewgrams_neutral <- jewgrams_sentida %>%
  filter(sentida_score >-1) %>%
  filter(sentida_score <1) %>%

# Failed attempt at creating data frame containing completely neutral scores. This does not work
jewgrams_neutral0 <- jewgrams_sentida %>%
  filter(sentida_score 0)

