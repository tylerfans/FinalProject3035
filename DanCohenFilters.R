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
for (i in 1:10) {
  both_temp_data <- fread(both_corpus[i], sep = '\n', col.names = "text") #read in files using the fread function from the data.table package
  both_dataset <- rbindlist(list(both_dataset, both_temp_data)) #for each iteration, bind the new data to the building dataset
}
for (i in 1:10) {
  female_temp_data <- fread(female_corpus[i], sep = '\n', col.names = "text")
  female_dataset <- rbindlist(list(female_dataset, female_temp_data))
}
for (i in 1:10){
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
  geom_col()+
  ggtitle("Top Words in the Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_wc_20, aes(word,n))+
  geom_col()+
  ggtitle("Top Words in the Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_wc_20, aes(word,n))+
  geom_col()+
  ggtitle("Top Words in the Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words both corpus
ggplot(both_progress_count, aes(word,n))+
  geom_col()+
  ggtitle("Progress Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(both_religion_count, aes(word,n))+
  geom_col()+
  ggtitle("Religion Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(both_validation_count, aes(word,n))+
  geom_col()+
  ggtitle("Validation Words in Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words female corpus
ggplot(female_progress_count, aes(word,n))+
  geom_col()+
  ggtitle("Progress Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_religion_count, aes(word,n))+
  geom_col()+
  ggtitle("Relion Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(female_validation_count, aes(word,n))+
  geom_col()+
  ggtitle("Validation Words in Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

#cohen words male corpus
ggplot(male_progress_count, aes(word,n))+
  geom_col()+
  ggtitle("Progress Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_religion_count, aes(word,n))+
  geom_col()+
  ggtitle("Religion Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()
ggplot(male_validation_count, aes(word,n))+
  geom_col()+
  ggtitle("Validation Words in Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()

















