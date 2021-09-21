# flevoland
code to setup dedicated dockers and run the processing pipeline

1. make sure the dockerfile is set up properly and running with full access to the machine's GPU (read the dockerfile_readMe.txt for information on how to set this up)

2. if planning on running this on AWS - check out the aws_GPU_instance.txt for detailed instructions on how to properly set the versions of the prerequisite libraries.

3. Run the `random_hyper.py` file in order to get the random space values for learning rate and momentum. Make sure you get a good spread of values within the random space and that there are no obvious clusters of points. Put the values you are happy with in the `hyper_tuning_BBCH_p3.sh` in the brackets without commas under `declare -a learning_rate()` and `declare -a momentum()` respectively

4. run the `hyper_tuning_BBCH_p3.sh` script. NOTE: in order for this script to run smoothly you must run it while in the `/scripts` folder and must have the following directory tree:


```bash
├── inputs
│   ├── dataset_BBCH
│   │   ├── BSO0
│   │   ├── CAR1
│   │   ├── CAR4 
│   │   ├── GMA2
│   │   ├── GRA1
│   │   ├── MAI3
│   │   ├── MAI7
│   │   ├── ONI1
│   │   ├── ONI48
│   │   ├── POT1
│   │   ├── POT6
│   │   ├── POT8
│   │   ├── POT9
│   │   ├── SBT14
│   │   ├── SBT39
│   │   ├── SCR2
│   │   ├── TSH0
│   │   ├── VEG1
│   │   ├── WWH2
│   │   ├── WWH3
│   │   └── WWH7
│   ├── dfExcludeAll_noDup_best.csv
│   ├── flevo5_test_set_scr3000.csv
│   ├── gt_BBCH.csv
├── outputs
├── scripts
│   ├── flevo5_analysisReadyCNNOUTFiles.py
│   ├── flevo5_analysisReadyCNNOUTFiles.R
│   ├── flevo5_bestModel_PA_UA_F1_Evolution_Graph_new_check.R
│   ├── flevo5_excludeBSOnTSH_best.R
│   ├── flevo5_excludeBSOnTSH.R
│   ├── flevo5_exploreParcelResults.R
│   ├── flevo5_f1_ua_pa_total_scatterplot.R
│   ├── flevo5_figsForPresiorPapare.R
│   ├── flevo5_loopBBCHgetAccCompareAll_functions.R
│   ├── flevo5_multiclass_PRC_createInput4PY.R
│   ├── flevo5_multiclass_PRC.py
│   ├── flevo5_multiclassROCnAOC_correct.R
│   ├── hyper-tuning-BBCH_p3.sh
│   ├── __init__.pyc
│   ├── label_image_list.py
│   ├── Label-stats.sh
│   ├── overallstats.py
│   ├── parallel_inference.sh
│   ├── Plot_bestModel.py
│   ├── Plot.py
│   ├── retrain.py
│   ├── Stats-afterparallel_bestModel.sh
│   ├── Stats-afterparallel.sh
│   ├── tesorboard_vals.py
│   └── train_best_model.sh
```
