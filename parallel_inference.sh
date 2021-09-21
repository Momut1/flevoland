#!/bin/bash

foldertoanalyze='../outputs/Random_search_BBCH/'
gt_path='../inputs/gt_BBCH.csv'
name_files_test='flevo5_test_set_scr.csv'


JOBS=18

inputs='../inputs/'
outputs='../outputs/'
find $foldertoanalyze -maxdepth 5 -type f -name "*_infocommand.txt" > $inputs"list_runs-bbch.txt"
list_commands=$inputs'list_runs-bbch.txt'
commands=`cat $list_commands`
subfolder='Results-all-images/'

#control flag for the inceptiion run
SUB="Inception"


mkdir -p  $foldertoanalyze'infer_commands/'

for c in $commands
do
  com=`cat $c`
  count="$(basename -- $c | cut -d_ -f1)"
  name_run="$(echo $c| cut -d/ -f3- |awk -F /commands '{print $1}')"
  m=`echo $com |grep -oP 'http.?://\S+'`
  hw=`echo $m | cut -d _ -f4 | cut -d / -f1`

  if [ ! -f $outputs$name_run'/'$subfolder$count'/cnn_output_data.csv' ]; then
 	#to handle the inception runs
	if [ "$com" == *"$SUB"* ]; then
	hw=224
	fi

	echo "**********************************************************************"	
	echo $count
	echo $name_run
	echo $hw $m

	mkdir -p $outputs$name_run'/'$subfolder$count'/'
	python3 label_image_list.py \
		--image_list $inputs$name_files_test \
		--graph $outputs$name_run'/output_graph/'$count'.pb' \
		--labels $outputs$name_run'/output_labels/'$count'.txt' \
		--input_height $hw \
		--input_width $hw \
		--output_dir $outputs$name_run'/'$subfolder$count'/'\
		--gt_files $gt_path > $foldertoanalyze'infer_commands/'$count'_infocommand.sh'
                

	wait
  fi
done

##ERASE ON THE GPU RUN
chmod +x $foldertoanalyze'infer_commands/'*'sh'
ls $foldertoanalyze'infer_commands/'*'sh' > ./Main-par.txt
cat Main-par.txt | parallel -j $JOBS {}