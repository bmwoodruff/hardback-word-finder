#Enter your letters, required and optional.  
#This program will find all word combinations. Each word MUST contain the required letters. 
library(tidyverse)
library(stringr)

letters_required <- "abd,."
letters_optional <- "d"

letters_required <- letters_required %>% str_remove_all("[^[:alpha:]]")

#I need a function that will return a list of valid words, along with an updated match score, and length. 
#The function should take as input a dictionary. 
#It should also take as input how many wild cards you can use, so that you know how much to filter. I may not use two dictionaries, but oh well. 



words <- read_lines("2of12inf.txt") %>% 
  str_remove_all("!") %>% 
  str_remove_all("%")
my_dat <- tibble(word = words) %>% 
  mutate(matches = c(0)) %>% 
  mutate(length = str_length(word))

#c("hi!","What%","then!%") %>% str_remove_all("!") %>% str_remove_all("%")

#my_letters <- letters_required
wanted_letters_min <- str_length(letters_required)
wanted_letters_max <- str_length(letters_required)+str_length(letters_optional)

my_let_table <- 
  tibble(letter = letters_required %>% str_split("") %>% unlist()) %>% 
  distinct()%>%
  mutate(freq = str_count(letters_required,letter))
unique_letters <- my_let_table %>% pull(letter)
letters_freq <- my_let_table %>% pull(freq)

req_dict <- my_dat
i <- 0
while(i < length(unique_letters)){
  i <- i+1
  req_dict <- req_dict %>% 
    mutate(matches = matches + pmin(str_count(word,unique_letters[i]),letters_freq[i]))
}

req_dict <- req_dict %>% 
  filter(matches >= wanted_letters_min) %>%  
  filter(length>= wanted_letters_min) %>% 
  filter(length<= wanted_letters_max) %>% 
  arrange(-matches) 
#req_dict %>% View()

#Now I need to adapt this to include "required" letters, and "optional" letters, and maybe timeless classics too. I think I've got the correct dictionary in use now, so there should be a perfect match. 

#Idea, start with the required dictionary above.  
#Then add match values from the optional letters. 
#Tricky part will be to deal with the repeats from both lists.
#If something is on both lists, then I have to modify it (removing the required parts first)
#So, how to do this? If a letter shows up on both lists, no worries.  What I need to do is just count the number of times it appears, and then subtract the times it appears from the required list.  Not too hard, but will require some work. 

req_dict

req_dict_updated <- req_dict

my_let_table_opt <- 
  tibble(letter = letters_optional %>% str_split("") %>% unlist()) %>% 
  distinct()%>%
  mutate(freq_opt = str_count(letters_optional,letter)) %>% 
  left_join(my_let_table,by = "letter") %>% 
  mutate(freq=replace_na(freq,0))
unique_letters_opt <- my_let_table_opt %>% pull(letter)
letters_freq_opt <- my_let_table_opt %>% pull(freq_opt)
letters_freq_used <- my_let_table_opt %>% pull(freq)

i <- 0
while(i<length(unique_letters_opt)){
  i <- i+1
  req_dict_updated <- req_dict_updated %>% 
    mutate(matches = matches + pmin(str_count(word,unique_letters_opt[i])-letters_freq_used[i],letters_freq_opt[i]))
}

req_dict_updated %>% 
  arrange(-matches) %>% 
  View()
