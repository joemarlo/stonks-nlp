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

#api = PushshiftAPI()

start_epoch=int(dt.datetime(2020, 1, 1).timestamp())
end_epoch=int(dt.datetime(2020, 1, 2).timestamp())

results = list(api.search_submissions(after=start_epoch,
                            before=end_epoch,
                            subreddit='wallstreetbets',
                            filter=['url','author', 'title', 'subreddit'],
                            limit=100
                            ))

# search using praw based on the ids obtained via psaw
reddit.submission(id='eiota0d').title

flairs = list()
for submission in results:
    flairs.append(submission.link_flair_text)

flairs = np.array(flairs)
results[np.where(flairs == "DD")[0]]

np.where(flairs == "DD")[0]

def get_DD_submissions(flairs):
    if flairs == 'DD':
        submission[i]
        print(submission)

for i in flairs:


url = 'http://www.politico.com/story/2017/02/mike-flynn-russia-ties-investigation-235272'
url_results = list(api.search_submissions(url=url, limit=500))

len(url_results), any(r.url == url for r in url_results)
