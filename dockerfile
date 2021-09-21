FROM nvidia/cuda:10.0-base-ubuntu18.04

#install libraries
RUN apt-get update && \
  apt-get install -y \
  libgdal-dev \
  libproj-dev \
  libv8-dev \
  ssh && \
  apt-get clean all

#install cudnn


#get Python and Tensorflow in
RUN apt-get update && \
  apt install python3-dev python3-pip -y

RUN pip3 install --upgrade pip
RUN pip3 install argparse
RUN pip3 install pillow
RUN pip3 install tensorflow-gpu==1.15
RUN pip3 install tensorflow-hub
