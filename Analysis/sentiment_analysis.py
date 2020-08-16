import pandas as pd
import datetime as dt
import os
import numpy as np
import nltk
from nltk.corpus import stopwords
from nltk.sentiment.vader import SentimentIntensityAnalyzer
#import tidytext

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")

# read in the scraped posts
posts_df = pd.read_csv("Scraping_WSB/scraped_posts.csv")

# remove NaNs
posts_df = posts_df.dropna(subset=['title', 'body'])

# tokenize the words
#tokens_df = tidytext.unnest_tokens(posts_df, "word", "body")

# remove stop_words
#nltk.download('stopwords')

# instantiate the SentimentIntensityAnalyzer
vader = SentimentIntensityAnalyzer()

# remove words
#vader.lexicon.pop('no')

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
"elon": 10
}

# add words
vader.lexicon.update(WSB_lingo)

# run the analyzer
scores = [vader.polarity_scores(body) for body in posts_df.body]
