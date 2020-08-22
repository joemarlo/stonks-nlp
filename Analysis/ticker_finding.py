import pandas as pd
import datetime as dt
import os
import numpy as np
#import tidytext
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
import matplotlib.pyplot as plt
import seaborn as sns
from fuzzywuzzy import process

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")
#os.chdir("/Users/joemarlo/Dropbox/Data/Projects/stonks-nlp")

# read in the scored posts and make lower case
posts_df = pd.read_csv("Analysis/scored_posts.csv")
posts_df.body = posts_df.body.str.lower()

# first tokenize the words
tokens = [word_tokenize(body) for body in posts_df.body]

# remove stopwords and puncation
stop_words = set(stopwords.words('english'))

# remove the stop words and non-alpha from the tokens
clean_tokens = []
for sentence in tokens:
    filtered_sentence = []
    for w in sentence:
        if w not in stop_words and w.isalpha():
            filtered_sentence.append(w)
    clean_tokens.append(filtered_sentence)

# read in the tickers df and make lower case
tickers_df = pd.read_csv("Data/tickers.csv")
tickers_df.ticker = tickers_df.ticker.str.lower()
tickers_df.name = tickers_df.name.str.lower()

# need to parse out LLC etc; first figure out which are most frequent
# tokenize and count most frequent tokens in company names
names_tokens = [word_tokenize(name) for name in tickers_df.name]
flat_names_tokens = [item for sublist in names_tokens for item in sublist]
pd.Series(flat_names_tokens).value_counts()[0:50]
del flat_names_tokens

words_to_remove = [
'corp',
'inc',
'.',
'ltd',
'holdings',
'group',
'co',
'trust',
'financial',
'lp',
'plc',
'international',
'pharmaceuticals',
'partners',
'technologies',
'bancorp',
'capital',
'therapeutics',
'the',
'energy',
'tech'
]

# remove the words
clean_names = []
for sentence in names_tokens:
    clean_word = [word for word in sentence if word not in words_to_remove]
    clean_names.append(' '.join(clean_word))

# run the leven distance and return the top match but only if that top match
    # has a score greater than 95
# this takes a while to run (30min+)
matches = []
for sentence in clean_tokens:
    sentence_matches = []
    for word in sentence:
        best_match = process.extractOne(word, clean_names)
        if best_match[1] > 95:
            sentence_matches.append(best_match[0])
    matches.append(sentence_matches)


# find all the matching tickers per post
# TODO remove tickers that are english words
found_tickers = []
for post in tokens:
    post_tickers = []
    for word in post:
        is_ticker = word.lower() in list(tickers_df.ticker)
        if is_ticker:
            post_tickers.append(word)
    found_tickers.append(post_tickers)



name_boolean = []
for post in tokens:
    for word in post:
        name_boolean.append(word.lower() in list(tickers_df.name))
