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
import time

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

# add custom words words
WSB_lingo = {
"tendies": 100,
"weaklies": -100,
"bear": -100,
"bull": 100,
"buy": 100,
"dip": -50,
"falls": -100,
"retard": 0,
"retards": 0,
"autistic": 50,
"autist": 50,
"autism": 50,
"moon": 100,
"mars": 100,
"musk": 10,
"elon": 10,
"gay bears": -50,
"put": -100,
"puts": -100,
"degen": 0,
"YOLO": 100,
"moon": 50,
"drill": -50,
"printer": 50
}

# add custom words
vader.lexicon.update(WSB_lingo)

# run the analyzer on the original post body
scores = [vader.polarity_scores(body) for body in posts_df.body]

# pull out the compound scores
compound_scores = []
for score in range(0, len(scores)):
    compound_scores.append(scores[score]["compound"])

# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(compound_scores).set_title("Distribution of sentiment scores of r/WallStreetBets posts' body text")
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
# TODO: need to fix try except because it will return a multidimenional list
post_scores = []
for i in range(len(posts_df.id)):
    submission = reddit.submission(id=posts_df.id[i])
    try:
        submission.comments.replace_more(limit=0)
    except:
        try:
            time.sleep(30)
            submission.comments.replace_more(limit=0)
        except:
            post_scores.append(np.nan)
            continue
    comment_scores = []
    comment_upvotes = []
    for top_level_comment in submission.comments:
        comment_scores.append(vader.polarity_scores(top_level_comment.body)["compound"])
        comment_upvotes.append(top_level_comment.score)
    if len(comment_scores) > 0:
        # first scale comments b/t 0:1 because upvotes can be negative
        comment_upvotes = np.array(comment_upvotes)
        if np.all(comment_upvotes == 0):
            # if there are no upvotes then just replace with 1s; otherwise it'll divide by 0 in calculating weighted_score
            comment_upvotes_scaled = np.repeat(1, len(comment_upvotes))
        else:
            # make sure denominator is not 0
            comment_range = max(comment_upvotes) - min(comment_upvotes)
            if comment_range != 0:
                comment_upvotes_scaled = (comment_upvotes - min(comment_upvotes)) / comment_range
            else:
                comment_upvotes_scaled = np.repeat(1, len(comment_upvotes))
        # create a weighted sentiment score based on the number of upvotes
        weighted_score = (np.array(comment_scores) * comment_upvotes_scaled).sum() / comment_upvotes_scaled.sum()
        post_scores.append(weighted_score)
    else:
        post_scores.append(np.nan)


# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(post_scores).set_title('Distribution of sentiment scores of r/WallStreetBets comments (weighted by comment points per post)')
plt.show()

# add scores to dataframe
posts_df[["comment_score"]] = post_scores

# create one score out of the three
posts_df["sentiment_score"] = posts_df[["body_score", "title_score", "comment_score"]].mean(axis='columns')

# histogram of scores
plt.figure(figsize=(9, 5))
sns.distplot(posts_df["sentiment_score"]).set_title('Distribution of sentiment scores of r/WallStreetBets (mean of title, body, and comments scores)')
plt.show()

# write out dataframe
posts_df.to_csv("Analysis/scored_posts.csv", index=False)
