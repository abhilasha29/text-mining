#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 20 17:48:52 2017

@author: abhilashakumari
"""

service.data().ga().get(
  ids='ga:'+ profile_id*,
  srart_date = '2017-01-01',end_date = '2017-05-01', 
  metrics = 'ga:customsimension7', dimensions = 'ga:customdimesion1',
  max_results = '500').execute()
  
)