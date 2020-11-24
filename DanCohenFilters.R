library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
library(data.table)
library(stringr)
setwd("~/R")
Sys.setlocale('LC_ALL','C')


#corpus for male and female scrubbed
both_corpus <- list.files(path = "./scrubbed/1880s-all-female-male-scrubbed", full.names = TRUE) #make a list of all txt files in the folder containing both male and female authors, use path to get to the file location
both_dataset <- data.frame() #create a new data frame that can be added to in the for-loop
for (i in 1:10){ #iterate through the entire length of the list, this will read in all of the files during the loop
  both_temp_data <- fread(both_corpus[i], sep = '\n', col.names = "text") #read in files using the fread function from the data.table package
  both_dataset <- rbindlist(list(both_dataset, both_temp_data)) #for each iteration, bind the new data to the building dataset
}
both_words <- both_dataset %>% unnest_tokens(word,text) #unnest all of the words in the data frame

validation_words <- c("revolution", "science", "scientific", "industrial", "middle ages", "modern")
religion_words <- c("religion", "sacred", "worship", "faith", "divine", "churches", "god", "christian", "bible", "heaven", "hell")
progress_words <- c("progress", "improvement", "hope", "happiness", "universal", "virtue", "vice", "truth", "false", "work", "marriage", "heroic", "evil", "jesus", "christ", "rome", "greece", "heaven", "hell")

validation_filter <- both_words %>% filter(word %in% validation_words)
validation_count <- validation_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))

religion_filter <- both_words %>% filter(word %in% religion_words)
religion_count <- religion_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))

progress_filter <- both_words %>% filter(word %in% progress_words)
progress_count <- progress_filter %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word,n))

# ^^ Can cause crashes, have to be careful ^^ overfills the environment I thik 

#get top 20 words for male/female corpus
both_wc_20 <- both_words %>% 
  count(word, sort = TRUE) %>%  #counting by word, makes sure to sort
  top_n(20) %>%  #only using the first 20 values
  mutate(word = reorder(word, n)) #reorders the colums order. new order is word,n

#create a graph of the top 20 words
ggplot(both_wc_20, aes(word,n))+
  geom_col()+
  ggtitle("Top Words in the Male/Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()




#I am skipping comments on the next two blocks of code becasue it is an identical explanation of the previous block of code, the only thing that is changing is the folder location.

#corpus for scrubbed female
female_corpus <- list.files(path = "./scrubbed/1880sfemalecorpustextfiles-scrubbed", full.names = TRUE)
female_dataset <- data.frame()
for (i in 1:length(female_corpus)) {
  female_temp_data <- fread(female_corpus[i], sep = '\n', col.names = "text") #read in files using the fread function from the data.table package
  female_dataset <- rbindlist(list(female_dataset, female_temp_data)) #for each iteration, bind the new data to the building dataset
}

female_words <- female_dataset %>% unnest_tokens(word,text)

female_wc_20 <- female_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n))

ggplot(female_wc_20, aes(word,n))+
  geom_col()+
  ggtitle("Top Words in the Female Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()





#corpus for scrubbed male
male_corpus <- list.files(path = "./scrubbed/1880smalecorpustextfiles-scrubbed", full.names = TRUE)
male_dataset <- data.frame()
for (i in 1:length(male_corpus)){
  male_temp_data <- fread(male_corpus[i], sep = '\n', col.names = "text") #read in files using the fread function from the data.table package
  male_dataset <- rbindlist(list(male_dataset, male_temp_data)) #for each iteration, bind the new data to the building dataset
}
male_words <- male_dataset %>% unnest_tokens(word,text)

male_wc_20 <- male_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n))

ggplot(male_wc_20, aes(word,n))+
  geom_col()+
  ggtitle("Top Words in the Male Corpus")+
  xlab("words")+
  ylab(NULL)+
  coord_flip()















