import pandas as pd
import datetime as dt
import os
import re
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

# read in the tickers df and make lower case
tickers_df = pd.read_csv("Data/tickers.csv")
tickers_df.ticker = tickers_df.ticker.str.lower()
tickers_df.name = tickers_df.name.str.lower()

# filter tickers_df to just the top ones
top_tickers = pd.read_csv("Data/ticker_names.csv")
top_tickers.ticker = top_tickers.ticker.str.lower()
tickers_df = pd.merge(tickers_df, top_tickers, on='ticker')

# first pull out all strings that start with $
found_dollars = []
for post in posts_df.body:
    captured_text = list(set(re.compile('(\$[a-z]+)').findall(post)))
    # make sure text in is ticker list
    matches = []
    for ticker in captured_text:
        clean_ticker = ticker.replace("$", "")
        if clean_ticker in list(tickers_df.ticker):
            matches.append(clean_ticker)
    #found_dollars.append(matches)
    found_dollars.append(', '.join(set(matches)))

# add list to main dataframe
posts_df["dollar_tickers"] = found_dollars
del found_dollars, matches, captured_text, clean_ticker

# now we need to search for tickers and company names without dollar signs
# first tokenize the words (fyi this seperates out the $)
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
# this takes a while to run (10min)
matches = []
for post in clean_tokens:
    post_matches = []
    for word in post:
        best_match = process.extractOne(word, clean_names)
        if best_match[1] > 95:
            post_matches.append(best_match[0])
    matches.append(post_matches)
del sentence, post_matches, word, best_match

# match the extraction to the ticker
ticker_matches = []
for match in matches:
    post_tickers = []
    for word in match:
        index = np.argwhere(np.array(clean_names) == word)[0]
        post_tickers.append(tickers_df.ticker[int(index)])
    ticker_matches.append(', '.join(set(post_tickers)))

# add list to main dataframe
posts_df["found_names"] = ticker_matches

# find all the matching tickers per post
# TODO remove tickers that are english words
# we need to retain the case here b/c they often put the ticker in uppercase
    # otherwise there are many incorrect matches
reg_case_posts_df = pd.read_csv("Analysis/scored_posts.csv")
reg_case_tokens = [word_tokenize(body) for body in reg_case_posts_df.body]
reg_case_clean_tokens = []
for post in reg_case_tokens:
    filtered_post = []
    for w in post:
        if w not in stop_words and w.isalpha():
            filtered_post.append(w)
    reg_case_clean_tokens.append(filtered_post)

found_tickers = []
for post in reg_case_clean_tokens:
    post_tickers = []
    for word in post:
        is_ticker = word in list(tickers_df.ticker.str.upper())
        if is_ticker:
            post_tickers.append(word)
    found_tickers.append(', '.join(set(post_tickers)))

del reg_case_posts_df

# add to main dataframe
posts_df["found_tickers"] = found_tickers

# add new columns with all found companies
posts_df["all_found_companies"] = posts_df.dollar_tickers + ", " + posts_df.found_names + ", " + posts_df.found_tickers
posts_df['all_found_companies'] = posts_df['all_found_companies'].apply(lambda x: re.sub(", ", " ", x).strip().upper())

# write out dataframe
posts_df[["url", "comms_num", "date", "sentiment_score", "all_found_companies"]].to_csv("Analysis/scored_named_posts.csv", index=False)
