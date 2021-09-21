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
│   ├── flevo5_test_set_scr3000.csv
│   ├── gt_BBCH.csv
├── outputs
├── scripts
│   ├── flevo5_analysisReadyCNNOUTFiles.py
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

5. After training completes, run the inference part in parallel via `parallel_inference.sh`. NOTE: You must first have installed `GNU parallel` on you machine. A comprehensive guide on how to install and use it can be found here : https://www.youtube.com/watch?v=OpaiGYxkSuQ&list=PL284C9FF2488BC6D1&index=3&t=2s . NOTE2: one shoule set the `JOBS` variable in the bash script to however many processes one wants to run in parallel. This decision should be guided by the amount of processing power available (number of CPUs). NOTE3: make sure you have set the environmental variables right in order to allow `GNU parallel` to be called from any directory, otherwise you would have to specify the entire path when you call it in the final (line 60) of the `parallel_inference.sh` script.

6. After inferencing run `flevo5_analysisReadyCNNOUTFiles.py` in order to format the `cnn_output_data_check.csv` for each model. 

7. After formating is complete run the `flevo5_excludeBSOnTSH.R` in order to create the `dfExcludeAll_noDup_best.csv` which lists all the images from all the runs that have been labelled as either bare soil ot trash/other. This file should then be located at `../inputs/dfExcludeAll_noDup_best.csv`.

8. After formating is complete run the `Stats-afterparallel.sh` script in order to output a single csv with all the results. It should be located in `../outputs/Random_search_BBCH/Results-all-images.csv`.

9. Identify the top performing models and complete steps 4,5,6,7,8 with augmentations in order to push performance higher. NOTE: make sure to change the paths where processing is happening in order not to overwrite the results from the hyper-paramater tuning steps.
