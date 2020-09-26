library(tidyverse)
library(egg)

# WSB Prediction ----------------------------------------------------------
# pull out posterior
post_m4 <- as.data.frame(m4)

# Plot Fixed Effect of WSB with 80% CI
q10 <- quantile(post_m4$wsb, .1) 
q90 <- quantile(post_m4$wsb, .9)
dens <- density(post_m4$wsb)
dd <- with(dens, data.frame(x, y))

p1 <- qplot(x, y, data = dd, geom="line") +
  geom_ribbon(data = subset(dd, x > q10 & x < q90),
              aes(ymax = y),
              ymin = 0, fill = "steel blue", colour = NA, alpha = 0.6) + 
  geom_vline(xintercept = mean(post_m4$wsb)) + 
  theme_bw() + 
  labs(y = element_blank(), x = expression(~beta* "WSB"), 
       title = "WSB Population Average")

ggsave("Plots/wsb.png")

# Plot Random Slopes and Intercepts 

# Population Level intercept
mu_a_sims <- as.matrix(m4, 
                       pars = "(Intercept)")

# Ticker Specifric Intercepts
tick_sims <- as.matrix(m4, 
                       regex_pars = "b\\[\\(Intercept\\) ticker\\:")

# Week SPecific Intercepts
week_sims <- as.matrix(m4, 
                       regex_pars = "b\\[\\(Intercept\\) week\\:")

# WSB fixed effect
b_wsb <- as.matrix(m4, 
                   regex_pars = "wsb")
b_wsb <- b_wsb[,1]

# WSB ticker random slope
b_ticker_wsb <- as.matrix(m4, 
                          regex_pars = "b\\[\\wsb\ ticker\\:")
# WSB week  random slope
b_week_wsb <- as.matrix(m4, 
                        regex_pars = "b\\[\\wsb\ week\\:")

## Preds with and without WSB
# Ticker
a_sims <- as.numeric(mu_a_sims) + tick_sims      
b_sims <- as.numeric(mu_a_sims) + tick_sims + b_ticker_wsb + b_wsb
# Week 
w.a_sims <- as.numeric(mu_a_sims) + week_sims      
w.b_sims <- as.numeric(mu_a_sims) + week_sims + b_week_wsb+ b_wsb

# Obtain sigma_y and sigma_alpha^2
# draws for sigma_y
s_y_sims <- as.matrix(m4, 
                      pars = "reciprocal_dispersion")
# draws for sigma_alpha^2
s__alpha_sims <- as.matrix(m4, 
                           pars = "Sigma[ticker:(Intercept),(Intercept)]")


# Compute mean, SD, median, and 95% credible interval of varying intercepts and slopes

# Posterior mean and SD of each alpha and beta combination

a_mean <- apply(X = a_sims,     
                MARGIN = 2,
                FUN = mean)

b_mean <- apply(X = b_sims,     
                MARGIN = 2,
                FUN = mean)

w.a_mean <- apply(X = w.a_sims,     
                  MARGIN = 2,
                  FUN = mean)
w.b_mean <- apply(X = w.b_sims,    
                  MARGIN = 2,
                  FUN = mean)


a_sd <- apply(X = a_sims,       
              MARGIN = 2,
              FUN = sd)

b_sd <- apply(X = b_sims,       
              MARGIN = 2,
              FUN = sd)

w.a_sd <- apply(X = w.a_sims,       
                MARGIN = 2,
                FUN = sd)

w.b_sd <- apply(X = w.b_sims,       
                MARGIN = 2,
                FUN = sd)

# Posterior median and 95% credible interval
a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")


b_quant <- apply(X = b_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
b_quant <- data.frame(t(b_quant))
names(b_quant) <- c("beta_Q2.5", "beta_Q50", "beta_Q97.5")



w.a_quant <- apply(X = w.a_sims, 
                   MARGIN = 2, 
                   FUN = quantile, 
                   probs = c(0.025, 0.50, 0.975))
w.a_quant <- data.frame(t(w.a_quant))
names(w.a_quant) <- c("Q2.5", "Q50", "Q97.5")


w.b_quant <- apply(X = w.b_sims, 
                   MARGIN = 2, 
                   FUN = quantile, 
                   probs = c(0.025, 0.50, 0.975))
w.b_quant <- data.frame(t(w.b_quant))
names(w.b_quant) <- c("beta_Q2.5", "beta_Q50", "beta_Q97.5")

# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)
a_df <- a_df %>% mutate(par = "alpha", 
                        ticker = get_variables(a_sims), 
                        order = order(a_df$a_mean))
b_df <- data.frame(b_mean, b_sd, b_quant)
b_df <- b_df %>% mutate(par = "alpha + beta", 
                        ticker = get_variables(b_sims), 
                        order = order(a_df$a_mean))

w.a_df <- data.frame(w.a_mean, w.a_sd, w.a_quant)
w.a_df <- w.a_df %>% mutate(par = "alpha", 
                            ticker = get_variables(w.a_sims), 
                            order = order(w.a_df$w.a_mean))
w.b_df <- data.frame(w.b_mean, w.b_sd, w.b_quant)
w.b_df <- w.b_df %>% mutate(par = "alpha + beta", 
                            ticker = get_variables(w.b_sims), 
                            order = order(w.a_df$w.a_mean))



a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  # a vector of school rank 
a_df <- a_df[order(a_df$order), ]
b_df <- b_df[order(b_df$order), ]
b_df$a_rank <- a_df$a_rank 

w.a_df <- w.a_df[order(w.a_df$w.a_mean), ]
w.a_df$a_rank <- c(1 : dim(w.a_df)[1])  # a vector of school rank 
w.a_df <- w.a_df[order(w.a_df$order), ]
w.b_df <- w.b_df[order(w.b_df$order), ]
w.b_df$a_rank <- w.a_df$a_rank 



names(b_df) <- names(a_df)
eff_df <- rbind(a_df, b_df)

names(w.b_df) <- names(w.a_df)
w.eff_df <- rbind(w.a_df, w.b_df)

# Plot week and ticker random effects on one plot 

names(w.eff_df) <- names(eff_df)
w.eff_df <- w.eff_df %>% mutate(random = "week")
eff_df <- eff_df %>% mutate(random = "ticker")

rands <- rbind(w.eff_df, eff_df)

p2 <- ggplot(data = rands, 
             aes(x = a_rank, 
                 y = a_mean, 
                 col = par)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0), alpha = .7) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  scale_x_continuous("Rank", 
                     breaks = seq(from = 0, 
                                  to = 50, 
                                  by = 5)) + 
  scale_y_continuous("Varying Intercpets and Slopes") + 
  theme_bw() + 
  facet_grid(random~., scales = "free") + 
  theme(legend.title = element_blank()) + 
  labs(title = "Ticker and Week Specific Effects")


# combine plots 
comb <- ggarrange(p1, p2)
ggsave("Plots/wsb_combined.png",comb,     
       width = 20,
       height = 12,
       units = 'cm')



# Sentiment Score ---------------------------------------------------------
# Plot sentiment fixed effect
sent_3 <- as.data.frame(sentiment.3)
q10 <- quantile(sent_3$sentiment_score, .1)
q90 <- quantile(sent_3$sentiment_score, .9)
dens <- density(sent_3$sentiment_score)
dd <- with(dens, data.frame(x, y))

p3 <- qplot(x, y, data = dd, geom="line") +
  geom_ribbon(data = subset(dd, x > q10 & x < q90),
              aes(ymax = y),
              ymin = 0, fill = "steel blue", colour = NA, alpha = 0.6) + 
  geom_vline(xintercept = mean(sent_3$sentiment_score)) + 
  theme_bw() + 
  labs(y = element_blank(), x = expression(~beta* "Sentiment Score"), 
       title = "Sentiment Score Population Average")

# Plot sentiment Random effects 


mu_a_sims <- as.matrix(sentiment.3, 
                       pars = "(Intercept)")

tick_sims <- as.matrix(sentiment.3, 
                       regex_pars = "b\\[\\(Intercept\\) ticker\\:")

week_sims <- as.matrix(sentiment.3, 
                       regex_pars = "b\\[\\(Intercept\\) week\\:")

b_ticker_wsb <- as.matrix(sentiment.3, 
                          regex_pars = "sentiment_score\ ticker\\:")

b_week_wsb <- as.matrix(sentiment.3, 
                        regex_pars = "sentiment_score\ week\\:")

b_wsb <- as.matrix(sentiment.3, 
                   regex_pars = "sentiment_score")
b_wsb <- b_wsb[,1]

a_sims <- as.numeric(mu_a_sims) + tick_sims      
b_sims <- as.numeric(mu_a_sims) + tick_sims + b_ticker_wsb + b_wsb 

w.a_sims <- as.numeric(mu_a_sims) + week_sims      
w.b_sims <- as.numeric(mu_a_sims) + week_sims + b_week_wsb +  b_wsb


# Obtain sigma_y and sigma_alpha^2
# draws for sigma_y
s_y_sims <- as.matrix(sentiment.3, 
                      pars = "reciprocal_dispersion")
# draws for sigma_alpha^2
s__alpha_sims <- as.matrix(sentiment.3, 
                           pars = "Sigma[ticker:(Intercept),(Intercept)]")

# Compute mean, SD, median, and 95% credible interval of varying intercepts

# Posterior mean and SD of each alpha
a_mean <- apply(X = a_sims,     # posterior mean
                MARGIN = 2,
                FUN = mean)
b_mean <- apply(X = b_sims,     # posterior mean
                MARGIN = 2,
                FUN = mean)

w.a_mean <- apply(X = w.a_sims,     # posterior mean
                  MARGIN = 2,
                  FUN = mean)
w.b_mean <- apply(X = w.b_sims,     # posterior mean
                  MARGIN = 2,
                  FUN = mean)


a_sd <- apply(X = a_sims,       # posterior SD
              MARGIN = 2,
              FUN = sd)

b_sd <- apply(X = b_sims,       # posterior SD
              MARGIN = 2,
              FUN = sd)

w.a_sd <- apply(X = w.a_sims,       # posterior SD
                MARGIN = 2,
                FUN = sd)

w.b_sd <- apply(X = w.b_sims,       # posterior SD
                MARGIN = 2,
                FUN = sd)

# Posterior median and 95% credible interval
a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")


b_quant <- apply(X = b_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
b_quant <- data.frame(t(b_quant))
names(b_quant) <- c("beta_Q2.5", "beta_Q50", "beta_Q97.5")



w.a_quant <- apply(X = w.a_sims, 
                   MARGIN = 2, 
                   FUN = quantile, 
                   probs = c(0.025, 0.50, 0.975))
w.a_quant <- data.frame(t(w.a_quant))
names(w.a_quant) <- c("Q2.5", "Q50", "Q97.5")


w.b_quant <- apply(X = w.b_sims, 
                   MARGIN = 2, 
                   FUN = quantile, 
                   probs = c(0.025, 0.50, 0.975))
w.b_quant <- data.frame(t(w.b_quant))
names(w.b_quant) <- c("beta_Q2.5", "beta_Q50", "beta_Q97.5")

# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)
a_df <- a_df %>% mutate(par = "alpha", 
                        ticker = get_variables(a_sims), 
                        order = order(a_df$a_mean))
b_df <- data.frame(b_mean, b_sd, b_quant)
b_df <- b_df %>% mutate(par = "alpha + beta", 
                        ticker = get_variables(b_sims), 
                        order = order(a_df$a_mean))

w.a_df <- data.frame(w.a_mean, w.a_sd, w.a_quant)
w.a_df <- w.a_df %>% mutate(par = "alpha", 
                            ticker = get_variables(w.a_sims), 
                            order = order(w.a_df$w.a_mean))
w.b_df <- data.frame(w.b_mean, w.b_sd, w.b_quant)
w.b_df <- w.b_df %>% mutate(par = "alpha + beta", 
                            ticker = get_variables(w.b_sims), 
                            order = order(w.a_df$w.a_mean))



a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  # a vector of school rank 
a_df <- a_df[order(a_df$order), ]
b_df <- b_df[order(b_df$order), ]
b_df$a_rank <- a_df$a_rank 

w.a_df <- w.a_df[order(w.a_df$w.a_mean), ]
w.a_df$a_rank <- c(1 : dim(w.a_df)[1])  # a vector of school rank 
w.a_df <- w.a_df[order(w.a_df$order), ]
w.b_df <- w.b_df[order(w.b_df$order), ]
w.b_df$a_rank <- w.a_df$a_rank 



names(b_df) <- names(a_df)
eff_df <- rbind(a_df, b_df)

names(w.b_df) <- names(w.a_df)
w.eff_df <- rbind(w.a_df, w.b_df)


p4 <- ggplot(data = rands, 
             aes(x = a_rank, 
                 y = a_mean, 
                 col = par)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0), alpha = .7) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  scale_x_continuous("Rank", 
                     breaks = seq(from = 0, 
                                  to = 50, 
                                  by = 5)) + 
  scale_y_continuous("Varying Intercpets and Slopes") + 
  theme_bw() + 
  facet_grid(random~., scales = "free") + 
  theme(legend.title = element_blank()) + 
  labs(title = "Ticker and Week Specific Effects")


sent <- ggarrange(p3, p4)
sent
ggsave("Plots/sentiment_combined.png",sent,     
       width = 20,
       height = 12,
       units = 'cm')

