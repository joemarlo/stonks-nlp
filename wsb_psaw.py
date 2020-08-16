from psaw import PushshiftAPI
import praw
import pandas as pd
import datetime as dt
import os
import numpy as np

# set working directory
os.chdir("/home/joemarlo/Dropbox/Data/Projects/stonks-nlp")

reddit = praw.Reddit(
client_id = "V9-uqOgDp7Mx6w",
client_secret = "qwr2uQldSuR1jXB9RGfpLfhbuAk",
username = "tall_george_",password = "Brewer5!",
user_agent =  "Get_Stonks by /u/tall_george_")

# make sure we're in read-only mode
reddit.read_only = True

# use PRAW credentials; then PSAW returns the IDs that you can use in PRAW
api = PushshiftAPI(reddit)


# set range of dates to scrape
start_day = dt.datetime(2020, 1, 1)
date_list = [start_day + dt.timedelta(days=x) for x in range(10)]

# create empty list to hold submission ids
DD_ids = list()

for day in date_list:
    # set starting day for this loop
    start_epoch=int(day.timestamp())
    # add one day to start_epoch
    end_epoch=start_epoch+(24*60*60)

    # get the submission ids for a given day
    results = list(api.search_submissions(after=start_epoch,
                            before=end_epoch,
                            subreddit='wallstreetbets',
                            #link_flair_text='DD',
#                            filter=['url','author', 'title', 'subreddit'],
                            limit=1000
                            ))

    # get flairs associated the results id
    flairs = list()
    for submission in results:
        flairs.append(submission.link_flair_text)

    # get submission ids that match DD
    todays_ids = list(np.array(results)[np.array(flairs) == "DD"])

    # add ids to master list
    DD_ids.append(todays_ids)



# define dict of the items we want to pull
items_dict = { "flair":[],
                "title":[],
                "score":[],
                "id":[], "url":[],
                "comms_num": [],
                "created": [],
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

def get_date(created):
    return dt.datetime.fromtimestamp(created)

# clean up date
items_df['date'] = items_df["created"].apply(get_date)
