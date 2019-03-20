library(tidytext)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


ripper <- read.csv("ripper_data_set.csv") 

ripper_tall <- ripper %>%
  gather(word = 3:525, value = freq)

Jripper <- ripper_tall %>%
  filter(author == "Jack the Ripper")

Jripper <- Jripper[,-2]

Jripper <- aggregate(. ~ author + key, data = Jripper, sum)

Jripper <- Jripper %>%
  filter(freq != 0)

Jripper$freq <- Jripper$freq * 100

set.seed(1234)
wordcloud(words = Jripper$key, freq = Jripper$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "RdGy"))

Uripper <- ripper_tall %>%
  filter(author != "Jack the Ripper")

Uripper <- Uripper[,-1:-2]

Uripper <- aggregate(. ~ key, data = Uripper, sum)

Uripper <- Uripper %>%
  filter(freq != 0)

Uripper$freq <- Uripper$freq * 100

set.seed(1234)
wordcloud(words = Uripper$key, freq = Uripper$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "RdGy"))


