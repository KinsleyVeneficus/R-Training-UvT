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

# ----- Exercise 2 ------  Create a re-usable function

# Function to calculate the age in years from two dates. 
# It's not required to put the dates in order.
#
# start_date is the first date (format: Y/m/d)
# end_date is the second date (format: Y/m/d)
# decimals you want the output in
# returns difference in years

days_in_year <- 365

age_from_dates <- function(start_date, end_date, decimals=0){
  
  age_in_days <- abs(as.numeric(as.Date(start_date) - as.Date(end_date)))
  age_in_years <- round(age_in_days/days_in_year, decimals)
  
  return(age_in_years)
}

age_from_dates("1978/01/30", "1979/06/30")
