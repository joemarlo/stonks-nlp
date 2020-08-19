import pandas as pd
import datetime as dt
import os
import numpy as np
import nltk
from nltk.corpus import stopwords
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import tidytext
import matplotlib.pyplot as plt
import seaborn as sns
import praw

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")
#os.chdir("/Users/joemarlo/Dropbox/Data/Projects/stonks-nlp")

# read in the scraped posts
posts_df = pd.read_csv("Scraping_WSB/scraped_posts.csv")

# remove NaNs
posts_df = posts_df.dropna(subset=['title', 'body']).reset_index()

# remove words
#vader.lexicon.pop('no')

# find the most common words
# first tokenize the words
tokens_df = tidytext.unnest_tokens(posts_df, "word", "body")
tokens_df = tokens_df.reset_index()

# remove stop_words
stop_words = set(stopwords.words('english'))
is_stopword = ~tokens_df[['word']].isin(stop_words)
tokens_df = tokens_df[np.array(is_stopword)]

# get the top 200 most frequent words
pd.set_option("display.max_rows", 200)
tokens_df[["word"]].value_counts().head(n=200)

# instantiate the SentimentIntensityAnalyzer
vader = SentimentIntensityAnalyzer()

# add custom words words
WSB_lingo = {
"tendies": 100,
"bear": -100,
"bull": 100,
"buy": 100,
"dip": -50,
"falls": -100,
"retard": 0,
"retards": 0,
"autistic": 10,
"moon": 100,
"mars": 100,
"musk": 10,
"elon": 10,
"gay bears": -50,
"put": -100,
"puts": -100
}

# add custom words
vader.lexicon.update(WSB_lingo)

# add words from McDonald financial corpus
positive_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Negative.csv"
negative_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Positive.csv"
neutral_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Uncertainty.csv"
positive_df = pd.read_csv(positive_url, header=None)
negative_df = pd.read_csv(negative_url, header=None)
neutral_df = pd.read_csv(neutral_url, header=None)

# add sentiment scores
positive_df.loc[:,1] = 10
negative_df.loc[:,1] = -10
neutral_df.loc[:,1] = 0

# convert to dictionary
positive_dict = positive_df.set_index(0).to_dict()
negative_dict = negative_df.set_index(0).to_dict()
neutral_dict = neutral_df.set_index(0).to_dict()

# add the words
vader.lexicon.update(positive_dict)
vader.lexicon.update(negative_dict)
vader.lexicon.update(neutral_dict)

# run the analyzer on the original post body
scores = [vader.polarity_scores(body) for body in posts_df.body]

# pull out the compound scores
compound_scores = []
for score in range(0, len(scores)):
    compound_scores.append(scores[score]["compound"])

# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(compound_scores).set_title('Distribution of sentiment scores of r/WallStreetBets posts')
plt.show()

# add to dataframe
posts_df[["body_score"]] = compound_scores

# run the analyer on the post title and add to dataframe
posts_df[["title_score"]] = [vader.polarity_scores(title)["compound"] for title in posts_df.title]

# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(posts_df.title_score).set_title('Distribution of sentiment scores of r/WallStreetBets post titles')
plt.show()

# insert Reddit credentials here
reddit = praw.Reddit(...)

# make sure we're in read-only mode
reddit.read_only = True

# get the mean sentiment score for all of the top-level comments per post
post_mean_scores = []
for i in range(len(posts_df.id)):
    submission = reddit.submission(id=posts_df.id[i])
    submission.comments.replace_more(limit=0)
    comment_scores = []
    for top_level_comment in submission.comments:
        comment_scores.append(vader.polarity_scores(top_level_comment.body)["compound"])
    if len(comment_scores) > 0:
        post_mean_scores.append(np.mean(comment_scores))
    else:
        post_mean_scores.append(np.nan)

# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(post_mean_scores).set_title('Distribution of sentiment scores of r/WallStreetBets comments (mean per post)')
plt.show()

# add scores to dataframe
posts_df[["mean_top_level_comment_score"]] = post_mean_scores

# write out dataframe
posts_df.to_csv("Analysis/scored_posts.csv", index=False)
