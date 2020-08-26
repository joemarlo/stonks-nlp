library(tidyverse)
library(lme4)
library(rstanarm)
library(ggridges)
options(mc.cores = parallel::detectCores())
theme_set(theme_minimal())

setwd("~/Dropbox/Data/Projects/stonks-nlp/")

# read the data containing the posts, scores, and tickers
posts_df <- read_csv("Analysis/scored_named_posts.csv", 
                     col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# munge the data to long format
posts_df <- posts_df %>% 
  separate_rows("all_found_companies", sep = " ") %>% 
  select(post_id = id, date, ticker = all_found_companies, 
         sentiment_score, n_comments = comms_num, url) %>% 
  mutate(date = as.Date(date))


# EDA ---------------------------------------------------------------------

# sentiment scores over time
posts_df %>% 
  select(-ticker) %>% 
  distinct() %>% 
  ggplot(aes(x = date, y = sentiment_score)) +
  geom_point(alpha = 0.6) +
  labs(title = "Sentiment scores over time",
       caption = "2020-01-01 to 2020-04-10",
       x = "Date",
       y = 'Sentiment score (VADER)')

# sentiment scores by number of comments
posts_df %>% 
  select(-ticker) %>% 
  distinct() %>% 
  ggplot(aes(x = n_comments, y = sentiment_score)) +
  geom_point(alpha = 0.6) +
  labs(title = "Comments per post",
       caption = "2020-01-01 to 2020-04-10",
       x = "n comments per post",
       y = 'Sentiment score (VADER)')

# plot the distributions of the top tickers 
tmp <- posts_df %>% 
  count(ticker) %>% 
  slice_max(n, prop = 0.05) %>%
  left_join(posts_df, by = 'ticker') %>%
  group_by(ticker) %>% 
  mutate(mean_score = mean(sentiment_score))
tmp %>% 
  mutate(ticker = factor(ticker, levels = unique(tmp$ticker[order(tmp$mean_score)]))) %>% 
  ggplot(aes(x = sentiment_score, y = ticker, fill = ticker)) +
  geom_density_ridges(alpha = 0.9, color = 'grey40') +
  labs(title = "Distribution of sentiment scores of top 5% mentioned securities in r/wallstreetbets",
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
  labs(title = "Top 10% mentioned securities in r/wallstreetbets",
       caption = "2020-01-01 to 2020-04-10",
       x = "n posts",
       y = NULL)
ggsave("Plots/top_mentions.png",
       width = 20,
       height = 16,
       units = 'cm')

# n tickers per post
posts_df %>% 
  count(post_id) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(color = 'white') +
  labs(title = "Securities mentioned per post",
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


# read in the robinhood usage data ----------------------------------------

# get the names of the csvs that match the tickers in posts_df 
files_to_read <- list.files("Data/Robinhood_usage") %>%
  enframe() %>% 
  mutate(ticker = str_remove(value, ".csv")) %>% 
  right_join(posts_df[, 'ticker']) %>% 
  pull(value) %>% 
  unique()

# read in the data into one dataframe
RH_usage <- map_dfr(files_to_read, function(filename){
  df <- read_csv(paste0("Data/Robinhood_usage/", filename))
  df$ticker <- str_remove(filename, ".csv")
  return(df)
})

# capture only the usage at the end of the day
RH_usage <- RH_usage %>% 
  mutate(date = as.Date(timestamp)) %>% 
  group_by(ticker, date) %>% 
  filter(timestamp == max(timestamp)) %>% 
  ungroup() %>% 
  select(-timestamp)

# overall RH usage
RH_usage %>% 
  group_by(date) %>% 
  summarize(n = sum(users_holding)) %>% 
  ggplot(aes(x = date, y = n)) + 
  geom_line() +
  geom_vline(xintercept = as.Date('2020-02-19')) +
  annotate(geom = 'text', x = as.Date('2020-02-15'), y = 1.0e+7,
           label = "Market peak: 2/19", hjust = 1) +
  geom_vline(xintercept = as.Date('2020-03-20')) +
  annotate(geom = 'text', x = as.Date('2020-03-26'), y = 5.0e+6,
           label = "NY stay-at-home\norder: 3/20", hjust = 0) +
  geom_vline(xintercept = as.Date('2020-03-23')) +
  annotate(geom = 'text', x = as.Date('2020-03-28'), y = 1.0e+7,
           label = "Market\nbottom: 3/23", hjust = 0) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Total unique securities owned by Robinhood users",
       x = NULL,
       y = "n users that hold the security")
ggsave("Plots/RH_usage.png",
       width = 20,
       height = 12,
       units = 'cm')

# RH usage over time by ticker
tmp <- RH_usage %>% 
  group_by(ticker) %>% 
  summarize(mean_user = mean(users_holding), .groups = 'drop') %>% 
  slice_max(mean_user, prop = 0.05) %>%
  mutate(name = paste0(ticker, ": ", substr(as.character(mean_user), 1, 3), "k"))
tmp %>% 
  select(ticker) %>% 
  semi_join(RH_usage, .) %>%
  left_join(tmp %>% select(ticker, name), by = 'ticker') %>% 
  mutate(name = factor(name, levels = tmp$name)) %>% 
  ggplot(aes(x = date, y = users_holding, color = name)) +
  geom_line(alpha = 0.5) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Top 5% most frequently held securities",
       caption = paste0(range(RH_usage$date), collapse = " to "),
       x = NULL,
       y = "n users that hold the security",
       color = 'Ticker: mean users')
rm(tmp)
ggsave("Plots/RH_usage_top_tickers.png",
       width = 20,
       height = 12,
       units = 'cm')

get_date_of_interest <- function(ticker, date, method = c("lead", "lag")) {
  # function returns a lead/lag usage number after accounting for weekends
  
  if (class(date) != 'Date') return(NA)
  weekday <- weekdays(date)
    
  # lead min(two days, 2 business days)
  if (method == 'lead') {
      new_date <- case_when(
        weekday == "Sunday" ~ date + 2,
        weekday == "Monday" ~ date + 2,
        weekday == "Tuesday" ~ date + 2,
        weekday == "Wednesday" ~ date + 2,
        weekday == "Thursday" ~ date + 4,
        weekday == "Friday" ~ date + 3,
        weekday == "Saturday" ~ date + 2
      )
    }
    
  # lag one business days
  if (method == 'lag') {
      new_date <- case_when(
        weekday == "Sunday" ~ date - 2,
        weekday == "Monday" ~ date - 3,
        weekday == "Tuesday" ~ date - 1,
        weekday == "Wednesday" ~ date - 1,
        weekday == "Thursday" ~ date - 1,
        weekday == "Friday" ~ date - 1,
        weekday == "Saturday" ~ date - 1
      )
    }
    
    # get the users on that date
    users_holding <- RH_usage$users_holding[RH_usage$ticker == ticker & RH_usage$date == new_date]
    if (length(users_holding) != 1) users_holding <- NA
    
    return(users_holding)
  }

# merge back with posts_df
final_df <- posts_df %>% 
  left_join(RH_usage, by = c("date", "ticker")) %>% 
  rowwise() %>% 
  mutate(users_holding_lead = get_date_of_interest(ticker, date, "lead"),
         users_holding_lag = get_date_of_interest(ticker, date, "lag")) %>% 
  ungroup()

# throw out rows we don't have all user data on
final_df <- na.omit(final_df)

# calculate % change in users
final_df$percent_change <- final_df$users_holding_lead / final_df$users_holding_lag - 1 

# add boolean identifying before / after market peak
final_df$post_peak <- final_df$date > as.Date('2020-02-19')


# frequentist -------------------------------------------------------------

# correlation between outcome and predictor
cor(final_df$sentiment_score, final_df$percent_change)

# plot percent_change vs. sentiment_score
final_df %>% 
  ggplot(aes(x = sentiment_score, y = percent_change, color = post_peak)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  scale_y_log10()

# fit a lm
broom::tidy(lm(percent_change ~ sentiment_score + n_comments + post_peak, data = final_df))

# fit mlm with post_peak as a random effect
mlm_freq_model <- lme4::lmer(percent_change ~ sentiment_score + n_comments + (1 | post_peak),
                             REML = T, data = final_df)
summary(mlm_freq_model)

# test if mlm model grouping on post_peak is necessary
lmerTest::rand(mlm_freq_model) # results indicate it is not necessary

# fit a second mlm with post-peak as random effect and fixed effect
mlm_freq_model_2 <- lme4::lmer(percent_change ~ sentiment_score + n_comments + post_peak + (1 | post_peak),
                               REML = T, data = final_df)

# fit a third mlm with post-peak as random effect and fixed effect and ticker as random effect
# this means intercepts vary across the ticker:post_peak groups but not the slopes
mlm_freq_model_3 <- lme4::lmer(percent_change ~ sentiment_score + n_comments + post_peak + (1 | ticker / post_peak),
                               REML = T, data = final_df)

# fit a fourth mlm same as three but w/ post_peak as fixed effect
mlm_freq_model_4 <- lme4::lmer(percent_change ~ sentiment_score + n_comments + (1 | ticker / post_peak),
                               REML = T, data = final_df)

# fit a fifth mlm where the effect of sentiment score and n_comments varies differently based on post_peak 
# we're allowing sentiment_score and n_comments to vary between the two post_peak groups
  # then there's global "average" coefficients for sentiment_score and n_comments
mlm_freq_model_5 <- lme4::lmer(percent_change ~ sentiment_score + n_comments + (1 + sentiment_score + n_comments | post_peak),
                               REML = T, data = final_df)

# compare the two models and retain the one the explains most variability
anova(mlm_freq_model, mlm_freq_model_2)
anova(mlm_freq_model_2, mlm_freq_model_3)
anova(mlm_freq_model_3, mlm_freq_model_4)
anova(mlm_freq_model_3, mlm_freq_model_5)
mlm_freq_model <- mlm_freq_model_3
rm(mlm_freq_model_2, mlm_freq_model_3, mlm_freq_model_4, mlm_freq_model_5)

# look at the residuals
plot(mlm_freq_model)
DescTools::RMSE(predict(mlm_freq_model), final_df$percent_change)

# plot the 95% confidence range of the fixed effects
confint(mlm_freq_model) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  .[4:7,] %>% 
  mutate(estimate = fixef(mlm_freq_model)) %>% 
  ggplot(aes(x = rowname, y = estimate, ymin = X2.5.., ymax = X97.5..)) +
  geom_point() +
  geom_linerange() + 
  coord_flip() +
  labs(title = "95% confidence interval of MLM fixed-effects",
       subtitle = "Frequentist model with company and pre/post peak as random effect",
       x = NULL,
       y = "Estimate (% change in users)")


# bayesian ----------------------------------------------------------------


