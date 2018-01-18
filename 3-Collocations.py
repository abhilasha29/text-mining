# -*- coding: utf-8 -*-
"""
Created on Tue Sep 12 14:46:28 2017

@author: issfz
"""
import string
from nltk.corpus import reuters
from nltk.collocations import BigramCollocationFinder, TrigramCollocationFinder
from nltk.metrics import BigramAssocMeasures, TrigramAssocMeasures #bigram associations 
from nltk.corpus import stopwords

grain_tok = [ reuters.words(f) for f in reuters.fileids('grain') ] #return values are already tokenised
trade_tok = [ reuters.words(f) for f in reuters.fileids('trade') ] 

words = [ w.lower() for f in grain_tok for w in f ] #lower case to prevent case sensitivity
bcf = BigramCollocationFinder.from_words(words) # will give words but not matrix
top100 = bcf.nbest(BigramAssocMeasures.likelihood_ratio, 100) #will give top n no of best candidates based on certain criteria(likelihood ration we can use to start with)
top = [ (t1, t2) for (t1, t2) in top100 if t1 not in string.punctuation and t2 not in string.punctuation ]

stopset = set(stopwords.words('english'))
filter_stops = lambda w: len(w) < 3 or w in stopset # filter stop words , more filtering required, prepare filter pattern first , we use lambda function
bcf.apply_word_filter(filter_stops) 
bcf.nbest(BigramAssocMeasures.likelihood_ratio, 10)
bcf.nbest(BigramAssocMeasures.chi_sq, 10)
bcf.nbest(BigramAssocMeasures.jaccard, 10)
bcf.nbest(BigramAssocMeasures.mi_like, 10)
bcf.nbest(BigramAssocMeasures.pmi, 10)
bcf.nbest(BigramAssocMeasures.raw_freq, 10)
bcf.score_ngram(BigramAssocMeasures.likelihood_ratio, "run", "after") # new bigram to check collocation


words2 = [ w.lower() for w in webtext.words('singles.txt')] #trigram collocation finder
tcf = TrigramCollocationFinder.from_words(words2)
tcf.apply_word_filter(filter_stops)
tcf.apply_freq_filter(1)
tcf.nbest(TrigramAssocMeasures.likelihood_ratio, 10)

bcf2 = BigramCollocationFinder.from_words(words, window_size = 10)
bcf2.apply_word_filter(filter_stops)
bcf2.apply_freq_filter(2)
bcf2.nbest(BigramAssocMeasures.likelihood_ratio, 50)

from nltk.corpus import reuters
from nltk import FreqDist

grain = reuters.fileids('grain')
trade = reuters.fileids('trade')

grain_tok = [ reuters.words(f) for f in grain ] 
trade_tok = [ reuters.words(f) for f in trade ] 

grain_words = [ t.lower() for t in ts for ts in grain_tok ]
