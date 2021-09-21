#!/bin/bash

name_run='../outputs/Random_search_BBCH/'
intermediate_store_frequency=999
how_many_training_steps=3000
declare -a learning_rate=(0.0007726272120552567 0.3040394932417116 0.0006712433559063625 0.47254154836096585 0.008987522693786797 0.02798852424749056 0.11577905486907616 0.0118486808036182 0.001538863149819954 0.0005634819385003901 0.00021036331209194382 0.08990006381198408 0.005824249887973403 0.00042977386296645155 0.33094056193926064 0.0012483891917684762 0.004538789066301824 0.0015286920252756814 0.03274537738764189 0.0208617146137123 0.025102674804231322 0.0011568578556317615 0.0013763852187281816 0.0010335940485878836 0.002042960642222832 0.0003772767132886754 0.00045967406355200565 0.7173316613398796 0.6932821204320934 0.0005671004605727213 0.00012509213359742405 0.0006579910706402429 0.06300427232537394 0.1312381198658241 0.0001235179286230323 0.020447960615912374 0.00010152429912221631 0.011531611431462314 0.036239316669339536 0.8759917176331172)
declare -a momentum=(0.92591 0.98025 0.98705 0.99227 0.90022 0.94695 0.99815 0.93989 0.98137 0.95465 0.97709 0.94849 0.90291 0.90865 0.91115 0.92512 0.99649 0.96318 0.98167 0.95661 0.96354 0.98119 0.99267 0.99126 0.98248 0.90942 0.9361 0.90355 0.95464 0.97961 0.90511 0.91887 0.93655 0.92443 0.97951 0.93521 0.96389 0.94934 0.95835 0.99393)
declare -a train_batch_size=(512 1024)
tfhub_module=('https://tfhub.dev/google/imagenet/mobilenet_v2_140_224/classification/3')
bottleneck_dir='../inputs/bottleneck_BBCH'
image_dir='../inputs/dataset_BBCH/'
output_graph='output_graph'
intermediate_output_graphs_dir='intermediate_graph/'
output_labels='output_labels/'
summaries_dir='retrain_logs/'
saved_model_dir='export_graph/'
checkpoint_path='retrain_checkpoint/'
mean_test=255
std_test=0

#---------------------NOTES
#tfhub_module='https://tfhub.dev/google/imagenet/mobilenet_v2_100_224/classification/3'
#try with the augmented images also
#try with the proper mean values, per image? per test set? I can only pass a value not per channel


mkdir -p $name_run
#counter
count=$((k++))
for i in `seq 0 $(( ${#learning_rate[@]} - 1 ))`
do 
    for l in "${train_batch_size[@]}"
    do
                
        echo "-----------------------------------------------------------------------------"
        echo $count
        #print the command and save it
        #With gradient
        mkdir -p $name_run'commands/'
        echo python3 retrain.py \
          --image_dir $image_dir \
          --output_graph $name_run$output_graph'/'$count'.pb' \
          --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
          --intermediate_store_frequency $intermediate_store_frequency \
          --output_labels $name_run$output_labels'/'$count'.txt' \
          --summaries_dir $name_run$summaries_dir'/'$count \
          --how_many_training_steps $how_many_training_steps \
          --learning_rate ${learning_rate[i]} \
          --train_batch_size $l \
          --momentum 0.000000 \
          --bottleneck_dir $bottleneck_dir \
          --tfhub_module $tfhub_module \
          --saved_model_dir $name_run$saved_model_dir$count'/' \
          --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
          --Adam 'False' > $name_run'commands/'$count'_infocommand.txt'
        
        #if [ $count -eq 180 ]
        #then 
            #create all the folders needed for this run
            mkdir -p $name_run$output_graph'/'
            mkdir -p $name_run$intermediate_output_graphs_dir$count'/'
            mkdir -p $name_run$output_labels'/'
            mkdir -p $name_run$summaries_dir'/'
            mkdir -p $name_run$saved_model_dir
            mkdir -p $name_run$checkpoint_path$count'/'

            python3 retrain.py \
                --image_dir $image_dir \
                --output_graph $name_run$output_graph'/'$count'.pb' \
                --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
                --intermediate_store_frequency $intermediate_store_frequency \
                --output_labels $name_run$output_labels'/'$count'.txt' \
                --summaries_dir $name_run$summaries_dir'/'$count \
                --how_many_training_steps $how_many_training_steps \
                --learning_rate ${learning_rate[i]} \
                --train_batch_size $l \
                --momentum 0.000000 \
                --bottleneck_dir $bottleneck_dir \
                --tfhub_module $tfhub_module \
                --saved_model_dir $name_run$saved_model_dir$count'/' \
                --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
                --Adam 'False'
            wait
        #fi
        count=$((k++))

            
        echo "-----------------------------------------------------------------------------"
        echo $count
        
        
        #with adam
        mkdir -p $name_run'commands/'
        echo python3 retrain.py \
          --image_dir $image_dir \
          --output_graph $name_run$output_graph'/'$count'.pb' \
          --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
          --intermediate_store_frequency $intermediate_store_frequency \
          --output_labels $name_run$output_labels'/'$count'.txt' \
          --summaries_dir $name_run$summaries_dir'/'$count \
          --how_many_training_steps $how_many_training_steps \
          --learning_rate ${learning_rate[i]} \
          --train_batch_size $l \
          --momentum ${momentum[i]} \
          --bottleneck_dir $bottleneck_dir \
          --tfhub_module $tfhub_module \
          --saved_model_dir $name_run$saved_model_dir$count'/' \
          --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
          --Adam 'True' > $name_run'commands/'$count'_infocommand.txt'
        
        #if [ $count -eq 180 ]
        #then 
            #create all the folders needed for this run
            mkdir -p $name_run$output_graph'/'
            mkdir -p $name_run$intermediate_output_graphs_dir$count'/'
            mkdir -p $name_run$output_labels'/'
            mkdir -p $name_run$summaries_dir'/'
            mkdir -p $name_run$saved_model_dir
            mkdir -p $name_run$checkpoint_path$count'/'

            python3 retrain.py \
                --image_dir $image_dir \
                --output_graph $name_run$output_graph'/'$count'.pb' \
                --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
                --intermediate_store_frequency $intermediate_store_frequency \
                --output_labels $name_run$output_labels'/'$count'.txt' \
                --summaries_dir $name_run$summaries_dir'/'$count \
                --how_many_training_steps $how_many_training_steps \
                --learning_rate ${learning_rate[i]} \
                --train_batch_size $l \
                --momentum ${momentum[i]} \
                --bottleneck_dir $bottleneck_dir \
                --tfhub_module $tfhub_module \
                --saved_model_dir $name_run$saved_model_dir$count'/' \
                --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
                --Adam 'True'
            wait
        #fi
        count=$((k++))


	done
done

