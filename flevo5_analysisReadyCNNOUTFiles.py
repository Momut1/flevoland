#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 24 07:31:15 2021

@author: momut1
"""

import pandas as pd
import os
import re
import numpy as np

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

resultDir = '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/'

files = []
# r=root, d=directories, f = files
for r, d, f in os.walk(resultDir):
    for file in f:
        if '_check' in file:
            files.append(os.path.join(r, file))
            
#sort the files list
files.sort()

#looking for indices of unprocessed .cnn_check.csv
indices = [i for i, x in enumerate(files) if "75" in x]

files[104]#49
files[133]#75

for i in range(0, len(files)):
    print('currently on run number ' + files[i])
    cnn_out_data_check_Int = pd.read_csv(files[i],converters={"cnn_labels": lambda x: x.strip("[]").split(", "), "cnn_values": lambda x: x.strip("[]").split(", ")})

    #rename colnames to free the original names as that is what the script uses
    cnn_out_data_check_Int.columns = ['cnn_labels__RAW', 'cnn_values__RAW', 'basename', 'code_bbch_surveyed']
    
    #create empty columns
    cnn_out_data_check_Int['cnn_labels'] = ""
    cnn_out_data_check_Int['cnn_values'] = np.nan
    
    for j in range(0, cnn_out_data_check_Int.shape[0]):
        #print(j)
        
        ##cnn_labels
        cnn_out_data_check_Int.cnn_labels__RAW[j] = [re.sub(r'\W', '', i) for i in cnn_out_data_check_Int.cnn_labels__RAW[j]]
        cnn_out_data_check_Int['cnn_labels'][j] = cnn_out_data_check_Int.cnn_labels__RAW[j][0]
         
        ##cnn_values
        cnn_out_data_check_Int.cnn_values[j] = cnn_out_data_check_Int.cnn_values__RAW[j][0]
        
        #basename
        cnn_out_data_check_Int.basename[j] = re.sub("/", "", cnn_out_data_check_Int.basename[j])
         
        ##code_bbch_surveyed
        #cnn_out_data_check_Int.code_bbch_surveyed[j] = cnn_out_data_check_Int['cnn_labels'][j].upper()
        
    
    cnn_out_data_check_Int.to_csv(files[i], header = True, index = False)
