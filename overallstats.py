from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
import pandas as pd
from sklearn.metrics import accuracy_score
import argparse
import numpy as np
import csv


def readcsv_panda(path):
    return pd.read_csv(path)

def extract_metrics(run, tranval, gt):
    count = run[0].split("/")[4]
    
    print(count)
    
    root_path = run[0].split("Results")[0]
    # take the val missclassifications
    aux= tranval[tranval['run'] == int(count)]

    #print(aux)
    val_acc = aux[aux['set'] =='validation'].Acc.values[0]
    train_acc = aux[aux['set'] == 'train'].Acc.values[0]

    #handle the test results;: "string"
    
    ####### CHANGE 1 - cnn_values cannot be np.float64 - change to str?
    
    test_results = pd.read_csv(run[0], dtype={"cnn_labels": str, "cnn_values": float, "basename": str, "code_bbch_surveyed": str}, low_memory = False)
    #test_results['basename'] = test_results["files"].apply(lambda x: x[x.find('/NL'):].strip('\']'))
    # drop the information that is not needed
    test_results.drop(test_results.columns.difference(['cnn_labels', 'cnn_values', 'basename']), 1, inplace=True)
    
    ####### CHANGE 2 - extract cnn_labels and cnn_values at index 0
    #test_results['cnn_labels'] = ""
    #test_results['cnn_values'] = ""

    #for roww in range(0,test_results.shape[0]):
     #   test_results['cnn_labels'][roww] = test_results["cnn_labels"].apply(eval)[roww][0]
      #  test_results['cnn_values'][roww] = test_results["cnn_values"].apply(eval)[roww][0]
    
    if 'BBCH' in run[0]:
        #if BBCH do not erase BSO
        trash_count = len(test_results[test_results.cnn_labels == 'tsh0'])
        bso_count = len(test_results[test_results.cnn_labels == 'bso0'])
        test_results = test_results[test_results.cnn_labels != 'tsh0']
        test_results['cnn_labels'] = test_results['cnn_labels'].str.upper()
        # join them by basename of results
        df_join = pd.merge(test_results, gt, how='left', on="basename")
        inx = df_join.loc[pd.isna(df_join["code_bbch_surveyed"]), :].index
        df_join.drop(df_join.index[inx], inplace=True)

        #conver cnn_labels to uppercase
        df_join.cnn_labels = df_join.cnn_labels.str.upper()
        
        #clean the mess of this specific test set
        df_join = df_join[df_join.code_bbch_surveyed != 'BSO1']
        df_join = df_join[df_join.code_bbch_surveyed != 'BSO2']

        #accuracies without bare soil
        df_noBSO = df_join[df_join.code_bbch_surveyed != 'BSO0']
        df_noBSO =  df_noBSO[df_noBSO.cnn_labels != 'BSO0']
        gt_noBSO = df_noBSO['code_bbch_surveyed'].to_numpy()
        p_noBSO = df_noBSO['cnn_labels'].to_numpy()
        test_bbch_pict_noBSO = accuracy_score(gt_noBSO, p_noBSO)

        #accuracies with bare soil class
        gt_aux = df_join['code_bbch_surveyed'].to_numpy()
        p = df_join['cnn_labels'].to_numpy()
        test_bbch_pict_BSO = accuracy_score(gt_aux, p)
        #do the metrics for the aggregation values
        test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO = Aggregation_stats(df_join, BBCH=True)

        return trash_count, bso_count, val_acc, train_acc, test_bbch_pict_noBSO, test_bbch_pict_BSO, \
               test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO
    else:
        trash_count = len(test_results[test_results.cnn_labels == 'tsh'])
        bso_count = len(test_results[test_results.cnn_labels == 'bso'])
        test_results = test_results[test_results.cnn_labels != 'tsh']
        test_results['cnn_labels'] = test_results['cnn_labels'].str.upper()
        # join them by basename of results
        df_join = pd.merge(test_results, gt, how='left', on="basename")

        #conver cnn_labels to uppercase
        df_join.cnn_labels = df_join.cnn_labels.str.upper()

        #accuracies without bare soil
        df_noBSO = df_join[df_join.code_bbch_surveyed != 'BSO']
        df_noBSO = df_noBSO[df_noBSO.cnn_labels != 'BSO']
        gt_noBSO = df_noBSO['code_bbch_surveyed'].to_numpy()
        p_noBSO = df_noBSO['cnn_labels'].to_numpy()
        test_acc_noBSO = accuracy_score(gt_noBSO, p_noBSO)

        #accuracies with bare soil class
        gt_aux = df_join['code_bbch_surveyed'].to_numpy()
        p = df_join['cnn_labels'].to_numpy()
        test_acc_BSO = accuracy_score(gt_aux, p)

        #do the metrics for the aggregation values
        #Aggregation_stats(df_join, BBCH=False)
        return val_acc, train_acc, test_acc_BSO, test_acc_noBSO, trash_count, bso_count

def extract_hyperparames(run):
    count = run[0].split("/")[4]
    root_path = run[0].split("Results")[0]
    # take the params
    params = open(root_path + "commands/" + count + "_infocommand.txt", 'r')
    params = params.read()

    # Augmentation flag
    if (params.find("flip") > 0 or params.find("brightness") > 0):
        augment = "Augmetations"
    else:
        augment = "No augmetations"

    LR = params.split('--learning_rate ')[1].split(' ')[0]
    BS = params.split('--train_batch_size ')[1].split(' ')[0]

    # optimizer flag
    momentum = params.split('--momentum ')[1].split(' ')[0]
    if momentum == '0.000000':
        optimizer = "Gradient Descent"
    else:
        optimizer = "Adam"

    if "mobilenet" in params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_'):
        model = "mobilenet"
        n_layers = params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_')[2]
        input_s = params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_')[3].split("/")[0]
    else:
        model = "Inception"
        input_s = 299
        n_layers = 0

    return augment, LR, BS, momentum, optimizer

def overall_stats(results_path, resultstrainval_path, output_full_path, gt):
    runs = csv.reader(open(results_path))
    tranval =  readcsv_panda(resultstrainval_path)
    col_names = ['count', 'augment', 'LR', 'BS', 'momentum', 'optimizer','trash_count', 'bso_count',  \
                 'val_acc', 'train_acc', 'test_bbch_pict_noBSO', 'test_bbch_pict_BSO','test_bbch_parcel_BSO', 'test_bbch_parcel_noBSO',\
                 'test_crop_parcel_BSO', 'test_crop_parcel_noBSO', 'test_crop_pict_BSO', 'test_crop_pict_noBSO']
    my_df = pd.DataFrame(columns=col_names)
    for run in runs:
        count = run[0].split("/")[4]
        #return [augment, LR. BS, momentum, optimizer]
        augment, LR, BS, momentum, optimizer = extract_hyperparames(run)

        #return val_acc, train_acc, test_acc, trash_count, bso_count
        trash_count, bso_count, val_acc, train_acc, test_bbch_pict_noBSO, test_bbch_pict_BSO, \
               test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO = extract_metrics(run, tranval, gt)
        add_row_stats(my_df,count, augment, LR, BS, momentum, optimizer, trash_count, bso_count, val_acc, train_acc, test_bbch_pict_noBSO, test_bbch_pict_BSO,test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO )

    my_df.to_csv(output_full_path)

def add_row_stats(my_df,count,augment, LR, BS, momentum, optimizer, trash_count, bso_count, val_acc, train_acc, test_bbch_pict_noBSO, test_bbch_pict_BSO,
               test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO ):
    new_row = [count, augment, LR, BS, momentum, optimizer, trash_count, bso_count, val_acc, train_acc, test_bbch_pict_noBSO, test_bbch_pict_BSO,
               test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO]
    my_df.loc[len(my_df)] = new_row

def Aggregation_stats(df, BBCH):
    if BBCH:
        #create the df with CROP only
        df_crop = df.copy()

        #print('--------aggstats df_crop.nrow--------------')
        #print(df_crop.shape[0])


        df_crop['code_bbch_surveyed'] = df_crop['code_bbch_surveyed'].str.replace('\d+', '')

        #print('--------aggstats df_crop.code_bbch_surveyed.vcounts--------------')
        #print(df_crop['code_bbch_surveyed'].value_counts())

        df_crop['cnn_labels'] = df_crop['cnn_labels'].str.replace('\d+', '')

        #print('--------aggstats df_crop.cnn_labels.vcounts--------------')
        #print(df_crop['cnn_labels'].value_counts())

        #df_crop.drop(df_crop.columns.difference(['code_bbch_surveyed', 'cnn_labels', 'objectid_survey']), 1, inplace=True)

        ###########calculate the accuracy per picture
        #accuracies without bare soil
        df_noBSO = df_crop[df_crop.code_bbch_surveyed != 'BSO']

        df_noBSO =  df_noBSO[df_noBSO.cnn_labels != 'BSO']

        #print('--------df_noBSO.nrow------------')
        #print(df_noBSO.shape[0])


        #print('--------df_noBSO.cnn_labels.value_counts()------------')
        #print(df_noBSO.code_bbch_surveyed.value_counts())


        #print('--------df_noBSO.cnn_labels.value_counts()------------')
        #print(df_noBSO.cnn_labels.value_counts())


        gt_noBSO = df_noBSO['code_bbch_surveyed'].to_numpy()
        p_noBSO = df_noBSO['cnn_labels'].to_numpy()
        test_crop_pict_noBSO = accuracy_score(gt_noBSO, p_noBSO)

        #print('--------test_crop_pict_noBSO------------')
        #print(test_crop_pict_noBSO)

        #accuracies with bare soil class
        gt_aux = df_crop['code_bbch_surveyed'].to_numpy()
        p = df_crop['cnn_labels'].to_numpy()
        test_crop_pict_BSO = accuracy_score(gt_aux, p)

        #############Accuracies per parcel
        ####CROP
        par = Parcel_Aggregation(df_crop)
        #accuracies with bare soil class
        gt_aux = par['code_bbch_surveyed'].to_numpy()
        p = par['result'].to_numpy()
        test_crop_parcel_BSO = accuracy_score(gt_aux, p)

        #accuracies without bare soil
        #print('-------------------- HERE -----------------')
        par = Parcel_Aggregation(df_noBSO)
        gt_noBSO = par['code_bbch_surveyed'].to_numpy()
        p_noBSO = par['result'].to_numpy()
        test_crop_parcel_noBSO = accuracy_score(gt_noBSO, p_noBSO)
        #print('-------------------- STOP  -----------------')

        #print('--------test_crop_parcel_noBSO------------')
        #print(test_crop_parcel_noBSO)

        ####BBCH
        # accuracies without bare soil
        df_noBSO = df[df.code_bbch_surveyed != 'BSO0']
        df_noBSO = df_noBSO[df_noBSO.cnn_labels != 'BSO0']
        par = Parcel_Aggregation(df_noBSO)
        gt_noBSO = par['code_bbch_surveyed'].to_numpy()
        p_noBSO = par['result'].to_numpy()
        test_bbch_parcel_noBSO = accuracy_score(gt_noBSO, p_noBSO)

        # accuracies with bare soil class
        par = Parcel_Aggregation(df)
        gt_noBSO = par['code_bbch_surveyed'].to_numpy()
        p_noBSO = par['result'].to_numpy()
        test_bbch_parcel_BSO = accuracy_score(gt_aux, p)

        return test_bbch_parcel_BSO, test_bbch_parcel_noBSO, test_crop_parcel_BSO, test_crop_parcel_noBSO, test_crop_pict_BSO, test_crop_pict_noBSO



def Parcel_Aggregation(df):
    #handle the parcels with more than one GT
    aux = df.drop(df.columns.difference(['code_bbch_surveyed', 'cnn_labels', 'objectid_survey']), 1)
    #print(aux)
    aux = aux.groupby('objectid_survey')['code_bbch_surveyed'].unique()
   
    aux = aux[aux.str.len() >1]
    
    #print('=========================================')
    #print(aux)
    #print('=========================================')
    
    #TODO check the lenghs of the
    for i in aux.index:
        for d in range(len(aux[i])):
            tosum = d/10
            df["objectid_survey"] = np.where(((df['objectid_survey'] == i) & (df['code_bbch_surveyed'] ==aux[i][d])),df['objectid_survey']+tosum, df['objectid_survey'] )

    aux = df.drop(df.columns.difference(['code_bbch_surveyed', 'cnn_labels', 'objectid_survey']), 1)

    ### order aux based on objectid_surveys to compare 
    aux = aux.sort_values(by=['objectid_survey'])

    #print('---------parcel aggregation aux.groupby len > 1 ... + tosum---------')
    #print(aux)

    aux = aux.groupby('objectid_survey')['code_bbch_surveyed'].unique().reset_index()
    aux['code_bbch_surveyed'] = aux['code_bbch_surveyed'].str[0]
    aux["result"] = None
    aux['img'] = 0
    #do the aggregation
    for i in aux["objectid_survey"]:
        parcel = df[df["objectid_survey"] == i]
        aux.loc[aux["objectid_survey"] == i, 'img'] = len(parcel)
        code_bbch_surveyed = pd.unique(parcel['code_bbch_surveyed'])
        result = Majorityvoting(parcel)
        aux.loc[aux["objectid_survey"] == i, 'result'] = result

    #print(aux.head(20))
    #print(aux.shape)
    return aux


def Majorityvoting(df):
    out = df.cnn_labels.mode()
    if len(out) == 1:
        return out.values[0]
    else:
        #select the highest probabliti amog all
        val = df.groupby(['cnn_labels'])['cnn_values'].sum().idxmax()
        return val






if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--results_path", help="Path to txt to the image list to be processed")
    parser.add_argument("--resultstrainval_path", help="Path to txt to the image list to be processed")
    parser.add_argument("--output_fullpath", help="Path to save the data")
    parser.add_argument("--save", help="save the parallel plot?", default='/data/results')
    parser.add_argument("--gt_files", help="csv files with the gt of the files to process the GT has to have code_bbch_surveyed as a key")
    args = parser.parse_args()
    if args.results_path:
        results_path = args.results_path
    if args.output_fullpath:
        output_fullpath = args.output_fullpath
    if args.save:
        save = args.save
    if args.gt_files:
        gt_path = args.gt_files
    if args.resultstrainval_path:
        resultstrainval_path = args.resultstrainval_path

    gt = readcsv_panda(gt_path)

    # create a column with the basename of the files
    #gt['basename'] = gt["name"].apply(lambda x: x[x.find('/NL'):])

    # drop the information that is not needed
    gt.drop(gt.columns.difference(['code_bbch_surveyed', 'basename', 'objectid_survey']), 1, inplace=True)

    overall_stats(results_path, resultstrainval_path, output_fullpath, gt)
    
    print('done')


