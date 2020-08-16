import praw
import pandas as pd
import datetime
import os

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")

reddit = praw.Reddit(
client_id = "V9-uqOgDp7Mx6w",
client_secret = "qwr2uQldSuR1jXB9RGfpLfhbuAk",
username = "tall_george_",password = "Brewer5!",
user_agent =  "Get_Stonks by /u/tall_george_")

# make sure we're in read-only mode
reddit.read_only = True

def get_date(submission):
	time = submission.created
	return datetime.datetime.fromtimestamp(time)

"""
wsb = reddit.subreddit("wallstreetbets")
day_titles = []
day_text = []
for submission in wsb.search("daily discussion",time_filter='day'):
	day_titles.append(submission.title)
	day_text.append(submission.selftext)


week_titles = []
week_text = []
week_comments = []
post_dates = []
num_coms = []
num_score = []
for submission in wsb.search("daily discussion", limit=5000):
	week_titles.append(submission.title)
	week_text.append(submission.selftext)
	post_dates.append(get_date(submission))
	num_coms.append(submission.num_comments)
	num_score.append(submission.score)

	#week_comments.append(submission.comments)
	#week_comments[0][0].body
	# first index is comment tree of post, secound index is comment within
"""


# 20200816 method
# https://www.storybench.org/how-to-scrape-reddit-with-python/

# define the subreddit we're interested in
wsb = reddit.subreddit("wallstreetbets")

# define dict of the items we want to pull
items_dict = { "flair":[],
                "title":[],
                "score":[],
                "id":[], "url":[],
                "comms_num": [],
                "created": [],
                "body":[]}

# pull the data
for submission in wsb.new(limit=None):
    items_dict["flair"].append(submission.link_flair_text)
    items_dict["title"].append(submission.title)
    items_dict["score"].append(submission.score)
    items_dict["id"].append(submission.id)
    items_dict["url"].append(submission.url)
    items_dict["comms_num"].append(submission.num_comments)
    items_dict["created"].append(submission.created)
    items_dict["body"].append(submission.selftext)

# convert dict to dataframe
items_df = pd.DataFrame(items_dict)

# clean up date
items_df['date'] = items_df["created"].apply(get_date)

# filter to just include daily discussion
dd_df = items_df[items_df['flair'] == 'DD']

def get_date(created):
    return datetime.datetime.fromtimestamp(created)




# write out dataframe
items_df.to_csv("inital_pull.csv", index=False)
