import pandas as pd
import datetime as dt
import os
import numpy as np
import nltk
from nltk.corpus import stopwords
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import tidytext

# set working directory
#os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")
os.chdir("/Users/joemarlo/Dropbox/Data/Projects/stonks-nlp")

# read in the scraped posts
posts_df = pd.read_csv("Scraping_WSB/scraped_posts.csv")

# remove NaNs
posts_df = posts_df.dropna(subset=['title', 'body'])

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
"gay bears": -50
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

# run the analyzer
scores = [vader.polarity_scores(body) for body in posts_df.body]
