library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
library(data.table)
library(stringr)
#setting working directory to R folder so I dont have to do it manually every time
setwd("~/R")
#setting encoding system to correct format. Not exactly sure what this is doing, but it fixed a bug so I need it.
Sys.setlocale('LC_ALL','C')

#creating words to be filtered out of bag of words
validation_words <- c("revolution", "science", "scientific", "industrial", "middle ages", "modern")
religion_words <- c("religion", "sacred", "worship", "faith", "divine", "churches", "god", "christian", "bible", "heaven", "hell")
progress_words <- c("progress", "improvement", "hope", "happiness", "universal", "virtue", "vice", "truth", "false", "work", "marriage", "heroic", "evil", "jesus", "christ", "rome", "greece", "heaven", "hell")

# ^^ PREFATORY INFORMATION ABOVE, RUN WHEN WORKING WITH BOTH SCRUBBED AND UNSCRUBBED VERSION ^^



#make a list of all txt files in the folder containing both male and female authors, use path to get to the file location
both_corpus <- list.files(path = "./scrubbed/1880s-all-female-male-scrubbed", full.names = TRUE)
female_corpus <- list.files(path = "./scrubbed/1880sfemalecorpustextfiles-scrubbed", full.names = TRUE)
male_corpus <- list.files(path = "./scrubbed/1880smalecorpustextfiles-scrubbed", full.names = TRUE)

#create empty dataset for each corpus, this will be added to in the for loop
both_dataset <- data.frame()
female_dataset <- data.frame()
male_dataset <- data.frame()

#iterate through the entire length of the list of files, this will apply fread on every document in the corresponding folder
#change the range of the for loop if the code if having issues being run. The data set is very large and can crash if "i in 1:length(list of files)" is left on all the for loops
#you have to be careful when running the entire loop, and it works much better when RStudio is being run locally rather than through the jupyter hub
for (i in 1:length(both_corpus)) {
  both_temp_data <- fread(both_corpus[i], sep = '\n', col.names = "text") #read in files using the fread function from the data.table package
  both_dataset <- rbindlist(list(both_dataset, both_temp_data)) #for each iteration, bind the new data to the building dataset
}
for (i in 1:length(female_corpus)) {
  female_temp_data <- fread(female_corpus[i], sep = '\n', col.names = "text")
  female_dataset <- rbindlist(list(female_dataset, female_temp_data))
}
for (i in 1:length(male_corpus)){
  male_temp_data <- fread(male_corpus[i], sep = '\n', col.names = "text")
  male_dataset <- rbindlist(list(male_dataset, male_temp_data))
}

#unnest each corpus by words
both_words <- both_dataset %>% unnest_tokens(word,text)
female_words <- female_dataset %>% unnest_tokens(word,text)
male_words <- male_dataset %>% unnest_tokens(word,text)

#top 20 words of each corpus
both_wc_20 <- both_words %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n))
female_wc_20 <- female_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n))
male_wc_20 <- male_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n))


#WORKING ON MALE/FEMALE CORPUS
#gathering count of words from each category Cohen created
both_validation_filter <- both_words %>% filter(word %in% validation_words)
both_validation_count <- both_validation_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
both_religion_filter <- both_words %>% filter(word %in% religion_words)
both_religion_count <- both_religion_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
both_progress_filter <- both_words %>% filter(word %in% progress_words)
both_progress_count <- both_progress_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))


#WORKING ON FEMALE CORPUS
#gathering count of words from each category Cohen created
female_validation_filter <- female_words %>% filter(word %in% validation_words)
female_validation_count <- female_validation_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
female_religion_filter <- female_words %>% filter(word %in% religion_words)
female_religion_count <-female_religion_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
female_progress_filter <- female_words %>% filter(word %in% progress_words)
female_progress_count <- female_progress_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))


#WORKING ON MALE CORPUS
#gathering count of words from each category Cohen created
male_validation_filter <- male_words %>% filter(word %in% validation_words)
male_validation_count <- male_validation_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
male_religion_filter <- male_words %>% filter(word %in% religion_words)
male_religion_count <- male_religion_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))
male_progress_filter <- male_words %>% filter(word %in% progress_words)
male_progress_count <- male_progress_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))



#GRAPHS

#top 20 words for each corpus
ggplot(both_wc_20, aes(word,n))+
  geom_col(color = "green4", fill = "green1")+
  ggtitle("Top Words in the Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_wc_20, aes(word,n))+
  geom_col(color = "violetred4", fill = "lightpink")+
  ggtitle("Top Words in the Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_wc_20, aes(word,n))+
  geom_col(color = "skyblue4", fill="cadetblue1")+
  ggtitle("Top Words in the Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words both corpus
ggplot(both_progress_count, aes(word,n))+
  geom_col(color = "green4", fill = "green1")+
  ggtitle("Progress Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(both_religion_count, aes(word,n))+
  geom_col(color = "green4", fill = "green1")+
  ggtitle("Religion Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(both_validation_count, aes(word,n))+
  geom_col(color = "green4", fill = "green1")+
  ggtitle("Validation Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words female corpus
ggplot(female_progress_count, aes(word,n))+
  geom_col(color = "violetred4", fill = "lightpink")+
  ggtitle("Progress Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_religion_count, aes(word,n))+
  geom_col(color = "violetred4", fill = "lightpink")+
  ggtitle("Religion Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_validation_count, aes(word,n))+
  geom_col(color = "violetred4", fill = "lightpink")+
  ggtitle("Validation Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words male corpus
ggplot(male_progress_count, aes(word,n))+
  geom_col(color = "skyblue4", fill="cadetblue1")+
  ggtitle("Progress Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_religion_count, aes(word,n))+
  geom_col(color = "skyblue4", fill="cadetblue1")+
  ggtitle("Religion Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_validation_count, aes(word,n))+
  geom_col(color = "skyblue4", fill="cadetblue1")+
  ggtitle("Validation Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()






#WE ARE NOW WORKING WITH THE UN-SCRUBBED VERSION OF THE TEXT

#additional words to add to bigram filter
dustin_words <- c("money","poor","lord","boy")
loni_words <- c("religion", "restraint", "gentility","marriage", "pollution", "disloyalty", "disorder", "nature", "hands", "head", "heart", "house", "effluvia")
#using words from dan cohens project along with dustin and loni's words
bag_o_words <- c(religion_words,progress_words,validation_words,dustin_words,loni_words)

#This is where we decide which words are included in our smart lexicon filter
SMART <- stop_words %>% 
  filter(lexicon == "SMART") %>%
  filter(!word %in% bag_o_words)

#creating a list of all unscrubbed txt files
both_corpus_full <- list.files(path = "./unscrubbed/1880sbothcorpus", full.names = TRUE)
female_corpus_full <- list.files(path = "./unscrubbed/1880sfemalecorpus", full.names = TRUE)
male_corpus_full <- list.files(path = "./unscrubbed/1880smalecorpus", full.names = TRUE)

#create empty dataset for each corpus with the full txt, this we be added to in the for loop
both_dataset_full <- data.frame()
female_dataset_full <- data.frame()
male_dataset_full <- data.frame()

#same as above for loops, but for the unscrubbed versions of the text
for (i in 1:length(both_corpus_full)) {
  both_temp_data_full <- fread(both_corpus_full[i], sep = '\n', col.names = "text")
  both_dataset_full <- rbindlist(list(both_dataset_full, both_temp_data_full))
}
for (i in 1:length(female_corpus_full)) {
  female_temp_data_full <- fread(female_corpus_full[i], sep = '\n', col.names = "text")
  female_dataset_full <- rbindlist(list(female_dataset_full, female_temp_data_full))
}
for (i in 1:length(male_corpus_full)) {
  male_temp_data_full <- fread(male_corpus_full[i], sep = '\n', col.names = "text")
  male_dataset_full <- rbindlist(list(male_dataset_full, male_temp_data_full))
}

#creating bigrams for each corpus and then filtering by our modified SMART word filter
both_bigram <- both_dataset_full %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word) %>%
  unite(bigram, word1, word2, sep = " ")

female_bigram <- female_dataset_full %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word) %>%
  unite(bigram, word1, word2, sep = " ")

male_bigram <- male_dataset_full %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word) %>%
  unite(bigram, word1, word2, sep = " ")


#filtering
both_bigram_filtered <- both_bigram %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% bag_o_words) %>%             ## this lets us see how the words we use are used in relation to smart words
  count(word1, word2, wt = n, sort = TRUE)

female_bigram_filtered <- female_bigram %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% bag_o_words) %>%             ## this lets us see how the words we use are used in relation to smart words
  count(word1, word2, wt = n, sort = TRUE)

male_bigram_filtered <- male_bigram %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% bag_o_words) %>%             ## this lets us see how the words we use are used in relation to smart words
  count(word1, word2, wt = n, sort = TRUE)



#TEMPLATE FOR GRAPHING BIGRAMS 

#use this for male corpora
both_bigram_filtered %>%
  filter(word1 == "lord") %>% #replace "lord" with whatever word you would like to examine. It has to be included in our bag_o_words
  top_n(20) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot() +
  aes(word2, n) +
  geom_col(color = "green4", fill = "green1")+
  coord_flip()

#use this for female corpora
female_bigram_filtered %>%
  filter(word1 == "lord") %>%
  top_n(20) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot() +
  aes(word2, n) +
  geom_col(color = "violetred4", fill = "lightpink")+
  coord_flip()

#use this for male corpora
male_bigram_filtered %>%
  filter(word1 == "lord") %>%
  top_n(20) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot() +
  aes(word2, n) +
  geom_col(color="skyblue4", fill="cadetblue1") +
  coord_flip()






