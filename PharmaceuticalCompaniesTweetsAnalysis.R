library(tidytext)
library(stringr)
library(RColorBrewer) 
name <- ""
tweetsGilead <- read_csv('GileadSciencessearch.csv')
tweetsGilead$name <- "Gilead" 

 
tweetsMerck <- read_csv('Merck.csv')
tweetsMerck$name <- "Merck"

  
tweets <- rbind(tweetsGilead,tweetsMerck)
# Keep only the complete rows

remove_reg <- "&amp;|&lt;|&gt;|#"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
library(tidyr)

frequency <- tidy_tweets %>% 
  group_by(location) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(location) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(location, word, freq) %>% 
  spread(location, freq)

#%>% arrange(Julia, David)

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Gilead / Merck)) %>%
  arrange(desc(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("log odds ratio (Gilead/Merck)") +
  scale_fill_discrete(name = "", labels = c("Gilead", "Merck"))

totals <- tidy_tweets %>% 
  group_by(name, X1) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(name) %>% 
  summarise(total_rts = sum(rts))

word_by_rts <- tidy_tweets %>% 
  group_by(X1, word, name) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(name, word) %>% 
  summarise(retweets = median(rts), uses = n()) %>%
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()

word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(name) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets,fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ name, scales = "fixed", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")

totalsFav <- tidy_tweets %>% 
  group_by(name, X1) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(name) %>% 
  summarise(total_favs = sum(favs))

words_by_Fav <- tidy_tweets %>% group_by(X1, word, name) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(name, word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites != 0) %>%
  ungroup()

words_by_Fav %>%
  filter(uses >= 5) %>%
  group_by(name) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ name, scales = "fixed", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")




