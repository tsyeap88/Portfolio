##Necessary Libraries
library(tidyverse)
#install.packages("readtext")
library(readtext)
#install.packages("tidytext")
library(tidytext)

##import letter data
##The Known letters of Jack the Ripper
Ripper_1 <- readtext("Ripper_1.txt")
Ripper_2 <- readtext("Ripper_2.txt")
Ripper_3 <- readtext("Ripper_3.txt")

###Suspects
##Prince Albert: letters
Albert_1 <- readtext("Albert_1.txt")
Albert_2 <- readtext("Albert_2.txt")

##Joe Barnett: police statement
Barnett_1 <- readtext("Barnett_1.txt")

#Carl Feigenbaum: quote
Feigenbaum <- readtext("Feigenbaum_1.txt")

#Lewis Carroll: letters
Lewis_Carroll_1 <- readtext("Lewis_carroll_1.txt")
Lewis_Carroll_2 <- readtext("Lewis_carroll_2.txt")
Lewis_Carroll_3 <- readtext("Lewis_carroll_3.txt")

##Mary Pearcey: letters
Pearcey_1 <- readtext("Pearcey_1.txt")
Pearcey_2 <- readtext("Pearcey_2.txt")

##Walter Richard Sickert
Sickert_1 <- readtext("Sickert_1.txt")

###Creating Word Counts
ripper_1_count <- Ripper_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
ripper_1_count <- ripper_1_count %>%
count(word, sort = TRUE)

ripper_2_count <- Ripper_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
ripper_2_count <- ripper_2_count %>%
  count(word, sort = TRUE)

ripper_3_count <- Ripper_3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
ripper_3_count <- ripper_3_count %>%
  count(word, sort = TRUE)

albert_1_count <- Albert_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
albert_1_count <- albert_1_count %>%
  count(word, sort = TRUE)

albert_2_count <- Albert_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
albert_2_count <- albert_2_count %>%
  count(word, sort = TRUE)

Barnett_1_count <- Barnett_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
Barnett_1_count <- Barnett_1_count %>%
  count(word, sort = TRUE)

feigenbaum_1_count <- Feigenbaum %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
feigenbaum_1_count <- feigenbaum_1_count %>%
  count(word, sort = TRUE)

carrol_1_count <- Lewis_Carroll_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
carrol_1_count <- carrol_1_count %>%
  count(word, sort = TRUE)

carrol_2_count <- Lewis_Carroll_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
carrol_2_count <- carrol_2_count %>%
  count(word, sort = TRUE)

carrol_3_count <- Lewis_Carroll_3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
carrol_3_count <- carrol_3_count %>%
  count(word, sort = TRUE)

pearcey_1_count <- Pearcey_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
pearcey_1_count <- pearcey_1_count %>%
  count(word, sort = TRUE)

pearcey_2_count <- Pearcey_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
pearcey_2_count <- pearcey_2_count %>%
  count(word, sort = TRUE)

sickert_1_count <- Sickert_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
sickert_1_count <- sickert_1_count %>%
  count(word, sort = TRUE)
############################

###Building the dataframe
frequency <- bind_rows(mutate(ripper_1_count, author = "Jack the Ripper", text_source = "RipperLetter1"),
                       mutate(ripper_2_count, author = "Jack the Ripper", text_source = "RipperLetter2"),
                       mutate(ripper_3_count, author = "Jack the Ripper", text_source = "RipperLetter3"),
                       mutate(albert_1_count, author = "Prince Albert", text_source = "AlbertLetter1"),
                       mutate(albert_2_count, author = "Prince Albert", text_source = "AlbertLetter2"),
                       mutate(carrol_1_count, author = "Lewis Carroll", text_source = "LewisCarrollLetter1"),
                       mutate(carrol_2_count, author = "Lewis Carroll", text_source = "LewisCarrollLetter2"),
                       mutate(carrol_3_count, author = "Lewis Carroll", text_source = "LewisCarrollLetter3"),
                       mutate(Barnett_1_count, author = "Joe Barnett", text_source = "BarnettTestimony"),
                       mutate(feigenbaum_1_count, author = "Carl Feigenbaum", text_source = "FeigenbaumQuote"),
                       mutate(pearcey_1_count, author = "Mary Pearcey", text_source = "PearceyLetter1"),
                       mutate(pearcey_2_count, author = "Mary Pearcey", text_source = "PearceyLetter2"),
                       mutate(sickert_1_count, author = "Walter Richard Sickert", text_source = "SickertLetter1")) 

##using dplyr to spread the word into the column names
frequency_1 <- frequency %>% spread(word, n)
frequency_1 <- frequency_1 %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

##Removing columns that contain numbers
frequency_1 <- frequency_1[, -3:-19]

#Min/max normalization function
normfunc <- function(x) {
  return ((x-min(x))/(max(x) - min(x)))
}

##applying normalization function row wise
norm <- as.data.frame(t(apply(frequency_1[,3:525], 1, normfunc)))

##columns 
titles <- as.data.frame(frequency_1[,1:2])

##append columns
final_ripper_set <- cbind(titles, norm)

##create csv file
write.csv(final_ripper_set, "ripper_data_set.csv")

