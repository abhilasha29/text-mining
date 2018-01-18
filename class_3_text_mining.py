#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 19 19:50:57 2017

@author: abhilashakumari
"""

import os
os.chdir("/Users/abhilashakumari/downloads/textmining")
from nltk.corpus import reuters
from nltk import FreqDist
import string
from nltk.corpus import stopwords
from nltk import pos_tag
dictionary1 = {'ADJ':'a', 'ADJ_SAT':'s','ADV':'r','NOUN':'n','VERB':'v'}
# We'll use the reuters corpus in NLTK.
# The same steps of preprocessing can be done on documents read in from external files.

# How many files are there in the corpus?
# What are their categories? Single or multiple categories for one file?
len(reuters.fileids()) #
cats = [ reuters.categories(f) for f in reuters.fileids() ] # for every file in retuers file reuter list  show categories for each of the articles and keep in cats
cat_num = [ len(c) for c in cats ]
fd_num = FreqDist(cat_num)
fd_num.plot()

# How many documents are there in each category?
# FreqDist() can be used to find the answer, but we need to flatten the list of categories first.
cats_flat = [ c for l in cats for c in l ] # cats contains list of lists ,so flattens each list inside the list
fd_cat = FreqDist(cats_flat)
fd_cat
fd_cat.most_common(20)

# Let's pick two categories and visualize the articles in each category using word cloud
grain = reuters.fileids('grain') 
trade = reuters.fileids('trade')
#tokenised document
grain_tok = [ reuters.words(f) for f in grain ]  # retrieving all file id for grain category, grain category articles as tokens we have 
trade_tok = [ reuters.words(f) for f in trade ] # retrieving all file id for grain category

#grain_tok has grain tokens 

             
#Let's define a function preprocess() to perform the preprocessing steps given a file (token list):
#   punctuation removal, case lowering, stopword removal, 
#   stemming/lemmatization, further cleaning
stop = stopwords.words('english')
snowball = nltk.SnowballStemmer('english')
#wnl = nltk.WordNetLemmatizer()
def pos_tagging(grain_tok):
    print("type of grain token is",type(grain_tok))
    tokens = [ ' '.join(f) for f in grain_tok ]
    print("@@@@@",type(tokens))
    tokens = pos_tag(word_tokenize(tokens), tagset='universal')
    tokens =[t.lower() for t in tokens if t not in string.punctuation]
    tokens =[t for t in tokens if t not in stop ]
    return tokens

def preprocess(toks):
    toks = [ t.lower() for t in toks if t not in string.punctuation ]
    toks = [t for t in toks if t not in stop ]
    toks = [ snowball.stem(t) for t in toks ]
    
#    toks = [ wnl.lemmatize(t) for t in toks ]
    toks_clean = [ t for t in toks if len(t) >= 3 ]
    return toks_clean

# Preprocess each file in each category
grain_clean = [ preprocess(f) for f in grain_tok ] #for every article in grain category preprocess it
trade_clean = [ preprocess(f) for f in trade_tok ]

# Flatten the list of lists for FreqDist
grain_flat = [ c for l in grain_clean for c in l ]
trade_flat = [ c for l in trade_clean for c in l ]

fd_grain = FreqDist(grain_flat)
fd_trade = FreqDist(trade_flat)

# Generate word clouds for the two categories.
from wordcloud import WordCloud
import matplotlib.pyplot as plt

wc_grain = WordCloud(background_color="white").generate_from_frequencies(fd_grain)
plt.imshow(wc_grain, interpolation='bilinear')
plt.axis("off")
plt.show()
#for trade category we will get different words in wordcloud , when we are looking at documents appearing from another category categories may overlap , 
wc_trade = WordCloud(background_color="white").generate_from_frequencies(fd_trade)
plt.imshow(wc_trade, interpolation='bilinear')
plt.axis("off")
plt.show()

# Finally, how to generate TDM
#vectorizer: feature extraction in text , countvextorizer can do binary vectorizing as well as 
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer #using countvectorizer for binary categorization #tfidf indexing, 

# sklearn requires the input to be text string
grain_text = [ ' '.join(f) for f in grain_clean ]
type(grain_text)

# Create a matrix using term frequency first using CountVectorizer
# The result is in sparse matrix format
vec_tf = CountVectorizer()
grain_tf = vec_tf.fit_transform(grain_text)
grain_tf # this will only show metadata , will not show actual 
#its a sparse matrix , have over 36000 over

# Where are the columns and rows then?
vec_tf.get_feature_names() #columns names are not stored , so we call get_featurenames to get columns
grain_tf_m = grain_tf.toarray()#call toarray() , to get back the actual array.

vec_tf_2 = CountVectorizer(min_df = 2)
grain_tf_2 = vec_tf_2.fit_transform(grain_text)
grain_tf_2

#To have binary indexing, set "binary=True" ,gives all 0 and 1 values

vec_bin = CountVectorizer(binary=True)# while initializing count vectoriser if we pass binary , then binary vectorization happens
grain_bin = vec_bin.fit_transform(grain_text)
grain_bin.toarray()[:10]

# And tfidf indexing
vec_tfidf = TfidfVectorizer(min_df = 2)
grain_tfidf = vec_tfidf.fit_transform(grain_text)
grain_tfidf
grain_tfidf.toarray()[:10]

# To save the vectorized results for future use
#pickle saves the entire object , so inorder to store in the form of object and not as string put option b, rb(read object)
import pickle #dump result into a file and then next time just use it 
pickle.dump(grain_tfidf, open("tfidf.pkl", "wb"))
pickle.dump(vec_tfidf.vocabulary_, open("feature.pkl","wb")) # take variable vocabulary from vectorizer to know thw column of the data,vec_tfidf.vocabulary_
#load the content
loaded_vec = TfidfVectorizer(decode_error="replace",vocabulary=pickle.load(open("feature.pkl", "rb")))
tfidf = pickle.load(open("tfidf.pkl", "rb" ) )
tfidf

#####################################
import nltk
from nltk import word_tokenize, pos_tag
pos_tagg = pos_tagging(grain_tok)

# ===== POS Tagging using NLTK =====

#sent = '''Professor Tan Eng Chye, NUS Deputy President and Provost, and Professor 
#Menahem Ben-Sasson, President of HUJ signed the joint degree agreement at NUS, 
#in the presence of Ambassador of Israel to Singapore Her Excellency Amira Arnon 
#and about 30 invited guests, on July 03, 2013.
#'''

# The input for POS tagger needs to be tokenized first.
#type(grain_flat)

sent = str1 = ''.join(grain_text)
sent_pos = pos_tag(word_tokenize(sent))
sent_pos

# A more simplified tagset - universal
sent_pos2 = pos_tag(word_tokenize(sent), tagset='universal')
sent_pos2

# The wordnet lemmatizer works properly with the pos given
wnl = nltk.WordNetLemmatizer()
wnl.lemmatize('signed', pos = 'v')

#------------------------------------------------------------------------
# Exercise: remember the wordcloud we created last week? Now try creating 
# a wordcloud with only nouns, verbs, adjectives, and adverbs, with nouns 
# and verbs lemmatized.
#-------------------------------------------------------------------------