library(tidyverse)
library(lme4Test)
library(brms)
library(ggridges)
theme_set(theme_minimal())

setwd("Dropbox/Data/Projects/stonks-nlp/")

# read the data containing the posts, scores, and tickers
posts_df <- read_csv("Analysis/scored_named_posts.csv", 
                     col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# munge the data to long format
posts_df <- posts_df %>% 
  separate_rows("all_found_companies", sep = " ") %>% 
  rename(ticker = all_found_companies,
         n_comments = comms_num) %>% 
  select(id, date, ticker, sentiment_score, n_comments, url)


# EDA ---------------------------------------------------------------------

# sentiment scores over time
posts_df %>% 
  ggplot(aes(x = date, y = sentiment_score)) +
  geom_point()

tmp <- posts_df %>% 
  count(ticker) %>% 
  slice_max(n, prop = 0.05) %>%
  left_join(posts_df, by = 'ticker') %>%
  group_by(ticker) %>% 
  mutate(mean_score = mean(sentiment_score))
tmp %>% 
  mutate(ticker = factor(ticker, levels = unique(tmp$ticker[order(tmp$mean_score)]))) %>% 
  ggplot(aes(x = sentiment_score, y = ticker, fill = ticker)) +
  geom_density_ridges(alpha = 0.9) +
  labs(title = "Distribution of sentiment scores of top 5% mentioned tickers in r/wallstreetbets",
       caption = "2020-01-01 to 2020-04-10",
       x = "Sentiment score (VADER)",
       y = NULL) +
  theme(legend.position = 'none')
rm(tmp)
ggsave("Plots/scores_by_top_mentions.png",
       width = 20,
       height = 16,
       units = 'cm')

# counts of tickers
posts_df %>% 
  count(ticker) %>% 
  slice_max(n, prop = 0.1) %>% 
  ggplot(aes(x = n, y = reorder(ticker, n))) +
  geom_col() +
  labs(title = "Top 10% mentioned tickers in r/wallstreetbets posts",
       caption = "2020-01-01 to 2020-04-10",
       x = "n mentions",
       y = NULL)
ggsave("Plots/top_mentions.png",
       width = 20,
       height = 16,
       units = 'cm')

# n tickers per post
posts_df %>% 
  count(id) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(color = 'white') +
  labs(title = "Companies mentioned per post",
       caption = "2020-01-01 to 2020-04-10",
       x = "n companies per post",
       y = 'n')

# n comments per post
posts_df %>% 
  ggplot(aes(x = n_comments)) +
  geom_histogram(color = 'white') +
  labs(title = "Comments per post",
       caption = "2020-01-01 to 2020-04-10",
       x = "n comments per post",
       y = 'n')

# look at by sector?


# frequentist -------------------------------------------------------------



# bayesian ----------------------------------------------------------------


