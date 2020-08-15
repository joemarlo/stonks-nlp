import praw    
import pandas as pd	
import datetime
reddit = praw.Reddit(
client_id = "V9-uqOgDp7Mx6w", 
client_secret = "qwr2uQldSuR1jXB9RGfpLfhbuAk", 
username = "tall_george_",password = "Brewer5!", 
user_agent =  "Get_Stonks by /u/tall_george_")


def get_date(submission):
	time = submission.created
	return datetime.datetime.fromtimestamp(time)


wsb = reddit.subreddit("wallstreetbets")
day_titles = []
day_text = []
for submission in wsb.search("flair:DD",time_filter='day'): 
	day_titles.append(submission.title)
	day_text.append(submission.selftext)


week_titles = []
week_text = []
week_comments = []
post_dates = []
num_coms = []
num_score = []
for submission in wsb.search("flair:DD",time_filter='all', limit=1000): 
	week_titles.append(submission.title)
	week_text.append(submission.selftext)
	post_dates.append(get_date(submission))
	num_coms.append(submission.num_comments)
	num_score.append(submission.score)

	#week_comments.append(submission.comments)
	#week_comments[0][0].body 
	# firsr index is comment tree of post, secound index is comment within

