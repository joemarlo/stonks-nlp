from psaw import PushshiftAPI
import praw
import pandas as pd
import datetime as dt
import os
import numpy as np

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")
#os.chdir("/Users/joemarlo/Dropbox/Data/Projects/stonks-nlp")

# insert Reddit credentials here
reddit = praw.Reddit(...)

# make sure we're in read-only mode
reddit.read_only = True

# use PRAW credentials; then PSAW returns the IDs that we can use in PRAW
api = PushshiftAPI(reddit)

# set range of dates to scrape
start_day = dt.datetime(2020, 3, 1)
date_list = [start_day + dt.timedelta(days=x) for x in range(92)]

# create empty list to hold submission ids
all_ids = list()

# iterate through the dates and pull the posts
for day in date_list:
    # set starting day for this loop
    start_epoch=int(day.timestamp())
    # add one day to start_epoch
    end_epoch=start_epoch+(24*60*60)

    # get the submission ids for a given day
    results = list(api.search_submissions(after=start_epoch,
                            before=end_epoch,
                            subreddit='wallstreetbets'
                            ))

    # add ids to master list
    all_ids.append(results)


# flatten list
all_ids = [item for sublist in all_ids for item in sublist]

# get flairs associated with the results id
flairs = list()
for submission in all_ids:
    flairs.append(submission.link_flair_text)

# get submission ids that match the discussion flairs
DD_ids = list(np.array(all_ids)[np.isin(np.array(flairs), ["DD", "Daily Discussion", "Discussion"])])

# define dict of the items we want to pull
items_dict = { "flair":[],
                "title":[],
                "score":[],
                "id":[], "url":[],
                "comms_num":[],
                "created":[],
                "body":[],
                "date":[]}

# pull the data
for submission in DD_ids:
    items_dict["flair"].append(submission.link_flair_text)
    items_dict["title"].append(submission.title)
    items_dict["score"].append(submission.score)
    items_dict["id"].append(submission.id)
    items_dict["url"].append(submission.url)
    items_dict["comms_num"].append(submission.num_comments)
    items_dict["created"].append(submission.created)
    items_dict["body"].append(submission.selftext)
    items_dict["date"].append(submission.created_utc)

# convert dict to dataframe
items_df = pd.DataFrame(items_dict)

# define function to get the date form the timestamp
def get_date(created):
    return dt.datetime.fromtimestamp(created)

# clean up date column
items_df['date'] = items_df["created"].apply(get_date)

# remove rows that contained removed posts, deleted
items_df = items_df[items_df['body'] != '[removed]']
items_df = items_df[items_df['body'] != '[deleted]']

# write out dataframe
items_df.to_csv("Scraping_WSB/scraped_posts.csv", index=False)
