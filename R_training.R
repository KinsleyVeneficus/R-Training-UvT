library(tidyverse) 
library(tidytext) 
library(topicmodels) 
library(tm) 
library(SnowballC) 

a <- read_csv("deceptive-opinion.csv")

# Filter on hotel
# Function to take out every i

top_terms_by_topic_tfidf <- function(df, text, group, plot = T){
  
  group <- enquo(group)
  text <- enquo(text)
  words <- df %>%
    unnest_tokens(word, !!text) %>%
    count(!!group, word) %>% 
    ungroup()
  total_words <- words %>% 
    group_by(!!group) %>% 
    summarize(total = sum(n))
  words <- left_join(words, total_words)
  tf_idf <- words %>%
    bind_tf_idf(word, !!group, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  if(plot == T){
    group_name <- quo_name(group)
    tf_idf %>% 
      group_by(!!group) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    return(tf_idf)
  }
}


t = top_terms_by_topic_tfidf(df = a, 
                             text = text, 
                             group = polarity, 
                             plot = T)

# ----- Excercise 2 ------  Create a re-usable function


month_a = as.POSIXlt("1978/06/30")
month_b = as.POSIXlt("1979/06/30")

age_1 = month_b$year - month_a$year