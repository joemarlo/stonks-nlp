library(tidyverse)
library(rstanarm)
library(lubridate)
library(mclust)

# use multi-core for model fit
options(mc.cores = parallel::detectCores())

# load data
df <- read_csv("~/stonks-nlp/Analysis/cleaned_data.csv")
df$sentiment_score[is.na(df$sentiment_score)] <- -999
df <- df[complete.cases(df), ]

# collapse dates to week
df <- df %>% mutate(week = as.factor(isoweek(date)))


# Use model based clustering to identify strata
blocks <- df %>% group_by(ticker) %>% 
  summarise(max_hold = max(users_holding))

model_clust <- Mclust(blocks$max_hold)
summary(model_clust)
plot(model_clust, what = "BIC")
blocks$volume <- model_clust$classification



# Plot mclust results
ggplot(blocks, aes(max_hold, fill = as.factor(volume))) + 
  geom_density(alpha = .5)

ggplot(blocks, aes(log(max_hold), fill = as.factor(volume))) + 
  geom_density(alpha = .5)

# apply stratified random sample based on max holding per ticker
set.seed(2)
blocks <- blocks %>% 
  group_by(volume) %>% 
  sample_n(10) %>% 
  print(n=Inf) %>% 
  select(ticker, volume)

df <- left_join(blocks, df)

# save final analysis data 
write_csv(df, "Analysis/wsb_analysis.csv")


# fit baseline models for week, ticker and wek + ticker
week.model <- stan_glmer(users_holding ~ (1|week), 
                   family = neg_binomial_2(), data = df, 
                   seed = 2, chains = 4)

ticker.model <- stan_glmer(users_holding ~ (1|volume), 
                    family = neg_binomial_2(), data = df, 
                    seed = 2, chains = 4, adapt_delta = .99, 
                    iter = 4000)

base <- stan_glmer(users_holding ~ (1|ticker) +  (1|week), 
                   family = neg_binomial_2(), data = df, 
                   seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

# fit wsb candidate models 

m1 <- stan_glmer(users_holding ~ wsb + (1|ticker) +  (1|week), 
                     family = neg_binomial_2(), data = df, 
                     seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

m2 <- stan_glmer(users_holding ~ wsb + (1|ticker) +  (wsb|week), 
                 family = neg_binomial_2(), data = df, 
                 seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

m3 <- stan_glmer(users_holding ~ wsb + (wsb|ticker) +  (1|week), 
                 family = neg_binomial_2(), data = df, 
                 seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

m4 <- stan_glmer(users_holding ~ wsb + (wsb|ticker) +  (wsb|week), 
                 family = neg_binomial_2(), data = df, 
                 seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

m5 <- stan_glmer(users_holding ~ wsb*week + (wsb|ticker), 
                 family = neg_binomial_2(), data = df, 
                 seed = 2, chains = 4, adapt_delta = .99, iter = 4000)

# model selection
base <- loo(base)
loo_1 <- loo(m1)
loo_2 <- loo(m2)
loo_3 <- loo(m3)
loo_4 <- loo(m4)
loo_5 <- loo(m5)

loo_compare(base, loo_1, loo_2, loo_3, loo_4, loo_5)


### Fit models for sentiment score 
# flter out non-wsb data
sentiment <- df %>% filter(sentiment_score != -999)


# fit basline model
base.sentiment <- stan_glmer(users_holding ~ (1|ticker) +  (1|week), 
                             family = neg_binomial_2(), data = sentiment, 
                             seed = 2, chains = 4, adapt_delta = .99)

# fit candidate models
sentiment.1 <- stan_gamm4(users_holding ~ s(sentiment_score), 
                          random = ~ (1|ticker) +  (1|week), 
                             family = neg_binomial_2(), data = sentiment, 
                             seed = 2, chains = 4, adapt_delta = .99)

sentiment.2 <- stan_glmer(users_holding ~ sentiment_score +
                            (sentiment_score|ticker) +  (1|week), 
                          family = neg_binomial_2(), data = sentiment, 
                          seed = 2, chains = 4, adapt_delta = .99)

sentiment.3 <- stan_glmer(users_holding ~ sentiment_score +
                            (sentiment_score|ticker) +  (sentiment_score|week), 
                          family = neg_binomial_2(), data = sentiment, 
                          seed = 2, chains = 4, adapt_delta = .99)

sentiment.4 <- stan_glmer(users_holding ~ sentiment_score +
                            (1|ticker) +  (sentiment_score|week), 
                          family = neg_binomial_2(), data = sentiment, 
                          seed = 2, chains = 4, adapt_delta = .99)

# model selection
s_loo_base <- loo(base.sentiment)
s_loo_1 <- loo(sentiment.1)
s_loo_2 <- loo(sentiment.2)
s_loo_3 <- loo(sentiment.3)
s_loo_4 <- loo(sentiment.4)
loo_compare(s_loo_base, s_loo_1, s_loo_2, s_loo_3, s_loo_4)

