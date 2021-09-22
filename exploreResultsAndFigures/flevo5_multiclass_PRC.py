#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  4 13:02:36 2021

@author: momut1
"""
#https://stackoverflow.com/questions/56090541/how-to-plot-precision-and-recall-of-multiclass-classifier

import pandas as pd
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)
import sys
import numpy as np
np.set_printoptions(threshold=sys.maxsize)
from ast import literal_eval

from sklearn.metrics import precision_recall_curve, roc_curve
from sklearn.preprocessing import label_binarize

import matplotlib.pyplot as plt
#%matplotlib inline


#import own data BBCH PICTURE
dfbbch_prc_input_raw = pd.read_csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB/44/df.bbch_prcintput.csv')
dfbbch_prc_input_raw = dfbbch_prc_input_raw.sort_values('V1')
dfbbch_prc_input_raw = dfbbch_prc_input_raw.reset_index()
dfbbch_prc_input_raw = dfbbch_prc_input_raw.drop(columns=['index'])
dfbbch_target = np.array(dfbbch_prc_input_raw['V1'])

n_classes = len(set(dfbbch_target))
classes4binerize = sorted(list(set(list(dfbbch_prc_input_raw['V1']))))
colors4binerize = ['#ee731a','#ee731a','#727620','#1aa72b','#f5e825','#f5e825','#ad2cb1','#ad2cb1','#d48310','#d48310','#d48310','#d48310','#e71029','#e71029','#becf50','#60ef27','#e0e75d','#e0e75d','#e0e75d']

#binerize
Y = label_binarize(dfbbch_target, classes=classes4binerize)
print('rows:',  Y.sum(axis=1))

#convert every string with probabilities to numeric array
y_score = np.zeros(shape=(dfbbch_prc_input_raw.shape[0],n_classes))
for i in range(0,dfbbch_prc_input_raw.shape[0]):
    y_score[i] = list(map(float, literal_eval(dfbbch_prc_input_raw['V3'][i])))

#check
len(Y) == len(y_score)
len(Y[1]) == len(y_score[1])
len(classes4binerize) == len(Y[1])
len(colors4binerize ) == len(classes4binerize)


# precision recall curve
precision = dict()
recall = dict()
fig = plt.figure(figsize=(20,10))
for ii in range(n_classes):
    #print(ii)
    precision[ii], recall[ii], threshh = precision_recall_curve(Y[:, ii],
                                                        y_score[:, ii])
    plt.plot(recall[ii], precision[ii], lw=2, label='{}'.format(classes4binerize[ii]), color='{}'.format(colors4binerize[ii]))
    
plt.xlabel("recall", fontsize = 30)
plt.ylabel("precision", fontsize = 30)
plt.legend(loc='best', fontsize = 12)
plt.xticks(fontsize=25, rotation=0, ha='center', va='top')
plt.yticks(fontsize=25, rotation=0, ha='center', va='top')
plt.show()


#import own data CROP PICTURE
dfcrop_prc_input_raw = pd.read_csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB/44/df.crop_prcintput.csv')
dfcrop_prc_input_raw = dfcrop_prc_input_raw.sort_values('V1')
dfcrop_prc_input_raw = dfcrop_prc_input_raw.reset_index()
dfcrop_prc_input_raw = dfcrop_prc_input_raw.drop(columns=['index'])
dfcrop_target = np.array(dfcrop_prc_input_raw['V1'])

n_classes = len(set(dfcrop_target))
classes4binerize = sorted(list(set(list(dfcrop_prc_input_raw['V1']))))
colors4binerize = ['#ee731a','#727620','#1aa72b','#f5e825','#ad2cb1','#d48310','#e71029','#becf50','#60ef27','#e0e75d']

#binerize
Y = label_binarize(dfcrop_target, classes=classes4binerize)
print('rows:',  Y.sum(axis=1))

#convert every string with probabilities to numeric array
y_score = np.zeros(shape=(dfcrop_prc_input_raw.shape[0],n_classes))
for i in range(0,dfcrop_prc_input_raw.shape[0]):
    y_score[i] = list(map(float, literal_eval(dfcrop_prc_input_raw['V3'][i])))

#check
len(Y) == len(y_score)
len(Y[1]) == len(y_score[1])
len(classes4binerize) == len(Y[1])
len(colors4binerize ) == len(classes4binerize)


# precision recall curve
precision = dict()
recall = dict()
fig = plt.figure(figsize=(20,10))
for ii in range(n_classes):
    #print(ii)
    precision[ii], recall[ii], threshh = precision_recall_curve(Y[:, ii],
                                                        y_score[:, ii])
    plt.plot(recall[ii], precision[ii], lw=2, label='{}'.format(classes4binerize[ii]), color='{}'.format(colors4binerize[ii]))
    
plt.xlabel("recall", fontsize = 30)
plt.ylabel("precision", fontsize = 30)
plt.legend(loc='best', fontsize = 12)
plt.xticks(fontsize=25, rotation=0, ha='center', va='top')
plt.yticks(fontsize=25, rotation=0, ha='center', va='top')
plt.show()


#import own data BBCH PARCEL
dfbbch_prc_input_raw = pd.read_csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB/44/df.bbch_par_prcintput.csv')
dfbbch_prc_input_raw = dfbbch_prc_input_raw.sort_values('code_bbch_surveyed')
dfbbch_prc_input_raw = dfbbch_prc_input_raw.reset_index()
dfbbch_prc_input_raw = dfbbch_prc_input_raw.drop(columns=['index'])
dfbbch_target = np.array(dfbbch_prc_input_raw['code_bbch_surveyed'])

n_classes = len(set(dfbbch_target))
classes4binerize = sorted(list(set(list(dfbbch_prc_input_raw['code_bbch_surveyed']))))
colors4binerize = ['#ee731a','#ee731a','#727620','#1aa72b','#f5e825','#f5e825','#ad2cb1','#ad2cb1','#d48310','#d48310','#d48310','#d48310','#e71029','#e71029','#becf50','#60ef27','#e0e75d','#e0e75d','#e0e75d']

#binerize
Y = label_binarize(dfbbch_target, classes=classes4binerize)
print('rows:',  Y.sum(axis=1))

#convert every string with probabilities to numeric array
y_score = np.zeros(shape=(dfbbch_prc_input_raw.shape[0],n_classes))
for i in range(0,dfbbch_prc_input_raw.shape[0]):
    y_score[i] = list(map(float, literal_eval(dfbbch_prc_input_raw['prob_arranged'][i])))

#check
len(Y) == len(y_score)
len(Y[1]) == len(y_score[1])
len(classes4binerize) == len(Y[1])
len(colors4binerize ) == len(classes4binerize)


# precision recall curve
precision = dict()
recall = dict()
fig = plt.figure(figsize=(20,10))
for ii in range(n_classes):
    #print(ii)
    precision[ii], recall[ii], threshh = precision_recall_curve(Y[:, ii],
                                                        y_score[:, ii])
    plt.plot(recall[ii], precision[ii], lw=2, label='{}'.format(classes4binerize[ii]), color='{}'.format(colors4binerize[ii]))
    
plt.xlabel("recall", fontsize = 30)
plt.ylabel("precision", fontsize = 30)
plt.legend(loc='best', fontsize = 12)
plt.xticks(fontsize=25, rotation=0, ha='center', va='top')
plt.yticks(fontsize=25, rotation=0, ha='center', va='top')
plt.show()



#import own data BBCH PARCEL
dfcrop_prc_input_raw = pd.read_csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB/44/df.crop_par_prcintput.csv')
dfcrop_prc_input_raw = dfcrop_prc_input_raw.sort_values('code_surveyed')
dfcrop_prc_input_raw = dfcrop_prc_input_raw.reset_index()
dfcrop_prc_input_raw = dfcrop_prc_input_raw.drop(columns=['index'])
dfcrop_target = np.array(dfcrop_prc_input_raw['code_surveyed'])

n_classes = len(set(dfcrop_target))
classes4binerize = sorted(list(set(list(dfcrop_prc_input_raw['code_surveyed']))))
colors4binerize = ['#ee731a','#727620','#1aa72b','#f5e825','#ad2cb1','#d48310','#e71029','#becf50','#60ef27','#e0e75d']

#binerize
Y = label_binarize(dfcrop_target, classes=classes4binerize)
print('rows:',  Y.sum(axis=1))

#convert every string with probabilities to numeric array
y_score = np.zeros(shape=(dfcrop_prc_input_raw.shape[0],n_classes))
for i in range(0,dfcrop_prc_input_raw.shape[0]):
    y_score[i] = list(map(float, literal_eval(dfcrop_prc_input_raw['prob_arranged'][i])))

#check
len(Y) == len(y_score)
len(Y[1]) == len(y_score[1])
len(classes4binerize) == len(Y[1])
len(colors4binerize ) == len(classes4binerize)


# precision recall curve
precision = dict()
recall = dict()
fig = plt.figure(figsize=(20,10))
for ii in range(n_classes):
    #print(ii)
    precision[ii], recall[ii], threshh = precision_recall_curve(Y[:, ii],
                                                        y_score[:, ii])
    plt.plot(recall[ii], precision[ii], lw=2, label='{}'.format(classes4binerize[ii]), color='{}'.format(colors4binerize[ii]))
    
plt.xlabel("recall", fontsize = 30)
plt.ylabel("precision", fontsize = 30)
plt.legend(loc='best', fontsize = 12)
plt.xticks(fontsize=25, rotation=0, ha='center', va='top')
plt.yticks(fontsize=25, rotation=0, ha='center', va='top')
plt.show()









################################ ROC CURVE #############################
#roc curve
noskill = [0.0, 1.0]
fpr = dict()
tpr = dict()
fig = plt.figure(figsize=(30,15))

for iii in range(n_classes):
    fpr[iii], tpr[iii], _ = roc_curve(Y[:, iii],
                                  y_score[:, iii])
    plt.plot(fpr[iii], tpr[iii], lw=2, label='{}'.format(classes4binerize[iii]), color='{}'.format(colors4binerize[iii]))

plt.xlabel("false positive rate", fontsize = 30)
plt.ylabel("true positive rate", fontsize = 30)
plt.legend(loc='best', fontsize = 12)
plt.xticks(fontsize=25, rotation=0, ha='center', va='top')
plt.yticks(fontsize=25, rotation=0, ha='center', va='top')
plt.plot(noskill,noskill, 'k--', linewidth=3)
plt.show()