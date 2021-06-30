# Load dataset with information about: tweet, topic, from, to
import nltk
import os
import json
import copy
import pandas as pd
import numpy as np
import random


### helper function

LABEL_SET = {    
        "BIO": {
            "label_map": {
                "O": "O", 
                "B-positive": "B", 
                "B-negative": "B", 
                "B-neutral": "B",
                "B-conflict": "B",
                "I-positive": "I",
                "I-negative": "I",
                "I-neutral": "I",
                "I-conflict": "I",
            },
            "label_list": ["O", "B", "I"]
        },
        "AO": {
            "label_map": {
                "O": "O",
                "B-positive": "A", 
                "B-negative": "A", 
                "B-neutral": "A",
                "B-conflict": "A",
                "I-positive": "A",
                "I-negative": "A",
                "I-neutral": "A",
                "I-conflict": "A",
            },
            "label_list": ["O", "A"]
        },
        "PNNO": {
            "label_map": {
                "O": "O",
                "B-positive": "positive", 
                "B-negative": "negative", 
                "B-neutral": "neutral",
                "B-conflict": "neutral",
                "I-positive": "positive",
                "I-negative": "negative",
                "I-neutral": "neutral",
                "I-conflict": "neutral",
            },
            "label_list": ["O", "positive", "negative", "neutral"]
        }
    }


def _tokenize_text(raw_text, from_index, to_index, label_map, sentiment):
    # warning this code this borrowed from the project : https://github.com/howardhsu/DE-CNN
    # the code needs cleaner re-writing.
    text = []
    for ix, c in enumerate(raw_text):
        # assuming that the tokenizer always yields fine-grained tokens for aspects 
        # so tokenizer won't affect the performance of AE.
        if (c=='/' or c=='*' or c=='-' or c=='=') and len(text)>0 and text[-1]!=' ':
            text.append(' ')
        if ix==int(from_index ) and len(text)>0 and text[-1]!=' ':
            text.append(' ')
        elif ix==int(to_index ) and len(text)>0 and text[-1]!=' ' and c!=' ': 
            text.append(' ')
        text.append(c)
        if (c=='/' or c=='*' or c=='-' or c=='=') and text[-1]!=' ':
            text.append(' ')

    text="".join(text)
    tokens=nltk.word_tokenize(text, language='german')
    lb = [label_map["O"]]*len(tokens)
    
    token_idx, pt, tag_on=0, 0, False
    for ix, c in enumerate(raw_text):
        if pt>=len(tokens[token_idx] ):
            pt=0
            token_idx+=1
            if token_idx >= len(tokens):
                break
        if ix==from_index: #from
            assert pt == 0 and c != ' '
            lb[token_idx] = label_map["B-" + sentiment]
            tag_on = True
        elif ix==to_index: #to
            assert pt == 0
            tag_on = False
        elif tag_on and pt==0 and c!=' ':
            lb[token_idx] = label_map["I-" + sentiment]
        if c==' ' or ord(c)==160: # skip spaces.
            pass
        elif tokens[token_idx][pt:pt+2]=='``' or tokens[token_idx][pt:pt+2]=="''":
            pt+=2
        else:
            pt+=1
    return tokens, lb

def ae_task(data):
    corpus = []
    for index, tweet in data.iterrows():
        tokens, lb = _tokenize_text(raw_text = tweet['twitter_full_text_topic'], 
                                from_index = tweet['from'], 
                                to_index = tweet['to'],
                                label_map = LABEL_SET["BIO"]["label_map"],
                                sentiment = tweet['label'])
        corpus.append({"id": tweet['doc_id'], 
                       "tokens": tokens, 
                       "labels": lb}) 
        
    return corpus

def asc_task(data):
    corpus = []
    for index, tweet in data.iterrows():
        corpus.append({"id": tweet['doc_id'], 
                       "sentence": tweet['twitter_full_text_topic'], 
                       "term": tweet['topic'], 
                       "polarity": tweet['label']})     
        
    return corpus


               
def write_json(filename, corpus, meta=['O', 'B', 'I'], target_dir = 'D:/UNI/01_Master/3. Semester/Consulting/00_BERT/'):
    path = os.path.join(target_dir, filename)
    with open(path, "w") as fw:
        json.dump({"data": {rec["id"]: rec for rec in corpus}, "meta": meta}, fw)
            
##########################################            
### Apply AE TASK on the entire dataset
##########################################

# Load the dataset into a pandas dataframe.
df = pd.read_csv("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/data_labeled_processed_bert.csv")

df["doc_id"] = df["doc_id"].str.extract("(\d*\.?\d+)", expand=True)

df['label_binary'] = [1 if label == 'positive' else 0 for label in df.label]

df = df.dropna(subset=['label'])

# Get the lists of tweets and their labels.
tweets = df.twitter_full_text_topic.values
labels = df.label_binary.values


# Split into train / validation / test - sets

from sklearn.model_selection import train_test_split

train_tweets, test_tweets = train_test_split(df, test_size = 0.2, random_state=2020, stratify = df.label_binary)

# test_tweets.groupby('label_binary').count()
         
corpus = ae_task(data = train_tweets)

# validation_size = round(len(train_tweets) * 0.2)
train_corpus = corpus[:-194]
dev_corpus = corpus[-194:]

test_corpus = ae_task(data = test_tweets)

# Save preprocessed datasets for ae task 

write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/train_ae_task.json", corpus = train_corpus)        
write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/dev_ae_task.json", corpus = dev_corpus)
write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/test_ae_task.json", corpus = test_corpus)


##########################################            
### Apply ASC TASK on the entire dataset
##########################################

corpus_asc = asc_task(data = train_tweets)

# validation_size = round(len(train_tweets) * 0.2)
train_corpus_asc = corpus_asc[:-194]
dev_corpus_asc = corpus_asc[-194:]

test_corpus_asc = asc_task(data = test_tweets)

# Save preprocessed datasets for ae task 

write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/train_asc_task.json", corpus = train_corpus_asc)        
write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/dev_asc_task.json", corpus = dev_corpus_asc)
write_json("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/test_asc_task.json", corpus = test_corpus_asc)

####

test_tweets.to_csv("D:/UNI/01_Master/3. Semester/Consulting/00_BERT/Daten/test_set_final.csv")
