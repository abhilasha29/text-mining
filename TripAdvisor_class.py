#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep  3 21:36:07 2017

@author: abhilashakumari
"""
import pandas as pd
import urllib
import urllib.request

from bs4 import BeautifulSoup

theurl = "https://www.tripadvisor.com.sg/Hotel_Review-g294265-d301583-Reviews-Raffles_Hotel_Singapore-Singapore.html"

def make_soup(url):
    thepage = urllib.request.urlopen(url)
    soupdata = BeautifulSoup(thepage, "html.parser")
    return soupdata
def add_five(x):
    y = x+5
    return y
add_five(5)

soup = make_soup(theurl)
print(soup.prettify)

j=1
for link in soup.find_all("div",{"class":"rating reviewItemInline"}):
    print(j)
    #print(link)
    #print(link.find_all('span')[0])
    print(link.find_all('span')[0].attrs['class'][1])
    #print(link.find_all('span')[1])
    print(link.find_all('span')[1].attrs['title'])
    #print(link.find_all('span')[1].text)
    j= j+1
    
j=1
set1=[]
for link in soup.find_all("div",{"class":"review-container"}):
    #print(j)
    #print(link.find('p').text)
    set1.append(link.find('p').text)
    j=j+1
    print(type(set1),len(set1))
 set1   
set2=[]
set3=[]
j=1
for link in soup.find_all("div",{"class":"rating reviewItemInline"}):
    #print(j)
    #print(link.find_all('span')[0].attrs['class'][1])
    set2.append(link.find_all('span')[0].attrs['class'][1])
    set3.append(link.find_all('span')[1].attrs['title'])
    j= j+1
    
    print(type(set2),len(set2))
    print(type(set3),len(set3))
df = pd.DataFrame({'c1':set1,
                   'c2':set2,
                   'c3':set3})
df.head()
df.columns=['Review','Rate','Date']
df.head()