#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 14 14:02:51 2020

@author: patrickschulze
"""
import math
import numpy as np
import tweepy
import pandas as pd

#Add your credentials here
twitter_keys = {
        'consumer_key':        'MpdDoacsyDgW6Bu4CeCVl13Cc',
        'consumer_secret':     'hCUwwMGaS83zPs6pkihWqS3FPek2uVDDxrBWYg9ooJ670pmC8V',
        'access_token_key':    '1243953821220589568-OHPXrSYgIDX5vyvosUKmLEZCxvLgzR',
        'access_token_secret': 'nP93r3AbO05HvzLELGaJjcrmj85gXLN2MmgdkwFMhpOtk'
    }

#Setup access to API
auth = tweepy.OAuthHandler(twitter_keys['consumer_key'], twitter_keys['consumer_secret'])
auth.set_access_token(twitter_keys['access_token_key'], twitter_keys['access_token_secret'])
api = tweepy.API(auth, wait_on_rate_limit=True)

# helper function to check whether tweet is retweet
def is_rewteet(x):
    try:
        res=not(math.isnan(x))
    except:
        res=True
    return(res)
# helper function to retrieve hashtags
def get_hashtags(x):
    hashtags_dict = x['hashtags']
    hashtags_text = [x['text'] for x in hashtags_dict]
    return(hashtags_text)
    
# function to download tweets for a specific user with Tweepy
def download_tweets_tweepy(username):
    #initialize a list to hold all the tweepy Tweets
    alltweets = []
    #specify relevant columns
    colnames = ['created_at', 'full_text', 'in_reply_to_user_id_str','is_quote_status', \
                'retweet_count', 'favorite_count', 'favorited', 'retweeted_status', \
                'is_retweet', 'retweet_full_text', 'followers_count', 'location', 'hashtags']
    try:	
        #make initial request for most recent tweets (200 is the maximum allowed count)
        new_tweets = api.user_timeline(screen_name = username,count=200,tweet_mode="extended")	
        #save most recent tweets
        alltweets.extend(new_tweets)	
        #save the id of the oldest tweet less one
        oldest = alltweets[-1].id - 1	
        #keep grabbing tweets until there are no tweets left to grab
        while len(new_tweets) > 0:		
            #all subsequent requests use the max_id param to prevent duplicates
            new_tweets = api.user_timeline(screen_name = username,count=200,max_id=oldest,tweet_mode="extended")		
            #save most recent tweets
            alltweets.extend(new_tweets)		
            #update the id of the oldest tweet less one
            oldest = alltweets[-1].id - 1
        #convert output to pandas DataFrame	
        outtweets = pd.DataFrame([tweet.__dict__ for tweet in alltweets])
        #check whether tweet is retweet
        outtweets['is_retweet'] = outtweets['retweeted_status'].apply(is_rewteet)
        #retrieve full_text if tweet is retweet
        outtweets['retweet_full_text'] = [outtweets['retweeted_status'][i].full_text \
                                          if outtweets['is_retweet'][i] \
                                              else '' for i in range(outtweets.shape[0])]
        #retrieve number of followers of user
        outtweets['followers_count'] = [x.followers_count for x in outtweets['author']]
        #retrieve location
        outtweets['location'] = [x.location for x in outtweets['author']]
        #retrieve hashtags as list
        outtweets['hashtags'] = outtweets['entities'].apply(get_hashtags)
        #store only relevant columns
        outtweets = outtweets[colnames]
        #add boolean column for availability (True)
        outtweets.insert(0, 'available', True)
    except:
        print('data for user %s cannot be downloaded' %username)
        #initialize DataFrame with row full of NaNs
        outtweets = pd.DataFrame(np.nan, index = [0], columns = colnames)
        #add boolean column for availability (False)
        outtweets.insert(0, 'available', False)
    #add column with username
    outtweets.insert(0, 'username', username)
    return(outtweets)
