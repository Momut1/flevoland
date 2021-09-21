# flevoland
code to setup dedicated dockers and run the processing pipeline

1. make sure the dockerfile is set up properly and running with full access to the machine's GPU (read the dockerfile_readMe.txt for information on how to set this up)

2. if planning on running this on AWS - check out the aws_GPU_instance.txt for detailed instructions on how to properly set the versions of the prerequisite libraries.

3. Run the random_hyper.py file in order to get the random space values for learning rate and momentum. Make sure you get a good spread of values within the random space and that there are no obvious clusters of points. Put the values you are happy with in the hyper_tuning_BBCH_p3.sh in the brackets without commas under "`declare -a learning_rate()`" and "`declare -a momentum()`" respectively

4. 
