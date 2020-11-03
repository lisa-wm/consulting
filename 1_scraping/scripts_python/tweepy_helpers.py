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

# Add your credentials here

# Asmik
# my_keys = {
#     'consumer_key': '',
#     'consumer_secret': '',
#     'access_token_key': '',
#     'access_token_secret': ''
# }

# Lisa
my_keys = {
    'consumer_key': 'o0g3JVWSKzRYv9dQp2SEPdjXp',
    'consumer_secret': 'AyvUIFzB82w3ZetyTXf1PbHiSxK7CgdcJo0D5jfKAoFlUuP0iH',
    'access_token_key': '1302924762914660354-7ydX1jUVSnscL60hhl83biPGNVeQoH',
    'access_token_secret': '9NqtnWj2q8uLuQkLMWdamJyIEb56hlGJOVgrydzoakorT'
}

twitter_keys = my_keys

# Set up access to API

auth = tweepy.OAuthHandler(twitter_keys['consumer_key'], twitter_keys['consumer_secret'])
auth.set_access_token(twitter_keys['access_token_key'], twitter_keys['access_token_secret'])
api = tweepy.API(auth, wait_on_rate_limit = True)

# Helper function to check whether tweet is retweet
def is_rewteet(x):
    try:
        res = not(math.isnan(x))
    except:
        res = True
    return(res)

# Helper function to retrieve hashtags
def get_hashtags(x):
    hashtags_dict = x['hashtags']
    hashtags_text = [x['text'] for x in hashtags_dict]
    return(hashtags_text)

# Helper function to retrieve user mentions
def get_mentions(x):
    mentions_dict = x['user_mentions']
    mentions_text = [x['screen_name'] for x in mentions_dict]
    return(mentions_text)
    
# Function to download tweets for a specific user with Tweepy

def download_tweets_tweepy(username):
    
    # Initialize a list to hold all the tweepy Tweets
    alltweets = []
    # Specify relevant columns
    colnames = [
        'created_at', 
        'full_text', 
        'quoted_status',
        'retweet_count', 
        'favorite_count', 
        'is_retweet',
        'retweet_full_text',
        'followers_count', 
        'location', 
        'hashtags',
        'mentions'
    ]
    
    try:
        
        # Make initial request for most recent tweets (200 is the maximum allowed count)
        new_tweets = api.user_timeline(screen_name = username,count = 200, tweet_mode = "extended")	
        # Save most recent tweets
        alltweets.extend(new_tweets)
        # Save the id of the oldest tweet less one
        oldest = alltweets[-1].id - 1
        
        # Keep grabbing tweets until there are no tweets left to grab
        while len(new_tweets) > 0:
            
            # All subsequent requests use the max_id param to prevent duplicates
            new_tweets = api.user_timeline(screen_name = username,
                                           count = 200,
                                           max_id = oldest,
                                           tweet_mode = "extended")
            # Save most recent tweets
            alltweets.extend(new_tweets)
            oldest = alltweets[-1].id - 1
            
        # Convert output to pandas DataFrame
        outtweets = pd.DataFrame([tweet.__dict__ for tweet in alltweets])
        
        # Check whether tweet is retweet
        outtweets['is_retweet'] = outtweets['retweeted_status'].apply(is_rewteet)
        # retrieve full_text if tweet is retweet
        outtweets['retweet_full_text'] = [outtweets['retweeted_status'][i].full_text \
                                          if outtweets['is_retweet'][i] \
                                              else '' for i in range(outtweets.shape[0])]
        
        # Retrieve other metrics
        outtweets['followers_count'] = [x.followers_count for x in outtweets['author']]
        outtweets['location'] = [x.location for x in outtweets['author']]
        outtweets['hashtags'] = outtweets['entities'].apply(get_hashtags)
        outtweets['mentions'] = outtweets['entities'].apply(get_mentions)
        outtweets = outtweets[colnames]
        
        # Add boolean column for availability (True)
        outtweets.insert(0, 'available', True)
        
    except:

        print('Data for user %s cannot be downloaded' %username)
        outtweets = pd.DataFrame(np.nan, index = [0], columns = colnames)
        outtweets.insert(0, 'available', False)
        
    # Add column with username
    outtweets.insert(0, 'username', username)
    return(outtweets)
