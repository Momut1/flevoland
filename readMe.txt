Reads:
------

supported tags:
https://gitlab.com/nvidia/container-images/cuda/blob/master/doc/supported-tags.md


check cuda is installed and nvcc
https://arnon.dk/check-cuda-installed/

#install cuda and cudnn
https://gist.github.com/Mahedi-61/2a2f1579d4271717d421065168ce6a73#file-cuda_10-0_installation_on_ubuntu_18-04

set up nvidia docker
---------------------
https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker

setting up a docker container
-------------------------------
https://towardsdatascience.com/how-to-properly-use-the-gpu-within-a-docker-container-4c699c78c6d1

stack
--------------
https://stackoverflow.com/questions/25185405/using-gpu-from-a-docker-container



1. Make sure you have installed the NVIDIA driver and Docker engine for your Linux distribution.

To check:
run in terminal:

nvidia-smi

output should be something like:

Thu Nov  5 11:30:19 2020       
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 435.21       Driver Version: 435.21       CUDA Version: 10.1     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|===============================+======================+======================|
|   0  GeForce GTX 105...  Off  | 00000000:01:00.0 Off |                  N/A |
| N/A   55C    P0    N/A /  N/A |    451MiB /  4040MiB |      2%      Default |
+-------------------------------+----------------------+----------------------+
                                                                               
+-----------------------------------------------------------------------------+
| Processes:                                                       GPU Memory |
|  GPU       PID   Type   Process name                             Usage      |
|=============================================================================|
|    0      1006      G   /usr/lib/xorg/Xorg                           265MiB |
|    0      1286      G   /usr/bin/gnome-shell                         155MiB |
|    0      2608      G   /usr/lib/firefox/firefox                       1MiB |
|    0      3911      G   /usr/lib/firefox/firefox                       1MiB |
|    0      3977      G   /usr/lib/firefox/firefox                      17MiB |
|    0      5442      G   /usr/lib/firefox/firefox                       1MiB |
|    0      6133      G   /usr/lib/firefox/firefox                       1MiB |
|    0      7919      G   /usr/lib/firefox/firefox                       1MiB |
|    0      7997      G   /usr/lib/firefox/firefox                       1MiB |
|    0      9068      G   /usr/lib/firefox/firefox                       1MiB |
+-----------------------------------------------------------------------------+

2.Install CUDA Toolkit


3. set up system and docker repository key

sudo systemctl start docker
sudo systemctl enable docker

distribution=$(. /etc/os-release;echo $ID$VERSION_ID) \
   && curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add - \
   && curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list


4. Install nvidia docker2
sudo apt-get update
sudo apt-get install -y nvidia-docker2
sudo systemctl restart docker
sudo docker run --rm --gpus all nvidia/cuda:10.1-base nvidia-smi

output should be something like:

Thu Nov  5 12:40:37 2020       
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 435.21       Driver Version: 435.21       CUDA Version: 10.1     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|===============================+======================+======================|
|   0  GeForce GTX 105...  Off  | 00000000:01:00.0 Off |                  N/A |
| N/A   56C    P0    N/A /  N/A |    720MiB /  4040MiB |      0%      Default |
+-------------------------------+----------------------+----------------------+
                                                                               
+-----------------------------------------------------------------------------+
| Processes:                                                       GPU Memory |
|  GPU       PID   Type   Process name                             Usage      |
|=============================================================================|
+-----------------------------------------------------------------------------+


dockerfile:

FROM nvidia/cuda:10.1-base-ubuntu18.04

#install libraries
RUN apt-get update && \
  apt-get install -y \
  libgdal-dev \
  libproj-dev \
  libv8-dev \
  ssh && \
  apt-get clean all

#get Tensorflow in
RUN apt-get update && \
  apt install python3-dev python3-pip -y

RUN pip3 install --upgrade pip
RUN pip3 install argparse
RUN pip3 install pillow
RUN pip3 install --upgrade tensorflow

in directory of dockerfile run:
docker build . -t nvidiadocker

check if image is there:
docker image ls | grep nvidiadocker

run container:
docker run --gpus all -it --name nvidiadocker_c -v '/data':'/data' -p 8887:8887 nvidiadocker


