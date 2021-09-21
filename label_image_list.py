# Copyright 2017 The TensorFlow Authors. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ==============================================================================
#
# Changes made by Koen Hufkens (2018) in order to allow for list of images to be
# used as data source rather than image directories.
# 
# These changes require additional libraries (pandas) to be installed.
# ==============================================================================

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import numpy as np
import tensorflow as tf
import pandas as pd
import os.path
os.environ["CUDA_VISIBLE_DEVICES"]="-1"


def readcsv_panda(path):
    return pd.read_csv(path)

def load_graph(model_file):
  graph = tf.Graph()
  graph_def = tf.GraphDef()

  with open(model_file, "rb") as f:
    graph_def.ParseFromString(f.read())
  with graph.as_default():
    tf.import_graph_def(graph_def)

  return graph


def read_tensor_from_image_file(file_name,
                                input_height=299,
                                input_width=299,
                                input_mean=0,
                                input_std=255):
  input_name = "file_reader"
  output_name = "normalized"
  file_reader = tf.read_file(file_name, input_name)
  if file_name.endswith(".png"):
    image_reader = tf.image.decode_png(
        file_reader, channels=3, name="png_reader")
  elif file_name.endswith(".gif"):
    image_reader = tf.squeeze(
        tf.image.decode_gif(file_reader, name="gif_reader"))
  elif file_name.endswith(".bmp"):
    image_reader = tf.image.decode_bmp(file_reader, name="bmp_reader")
  else:
    image_reader = tf.image.decode_jpeg(
        file_reader, channels=3, name="jpeg_reader")
  float_caster = tf.cast(image_reader, tf.float32)
  dims_expander = tf.expand_dims(float_caster, 0)
  resized = tf.image.resize_bilinear(dims_expander, [input_height, input_width])
  normalized = tf.divide(tf.subtract(resized, [input_mean]), [input_std])
  sess = tf.Session()
  result = sess.run(normalized)
  return result

def load_labels(label_file):
  label = []
  proto_as_ascii_lines = tf.gfile.GFile(label_file).readlines()
  for l in proto_as_ascii_lines:
    label.append(l.rstrip())
  return label

def printTensors(pb_file):

  # read pb into graph_def
  with tf.gfile.GFile(pb_file, "rb") as f:
    graph_def = tf.GraphDef()
    graph_def.ParseFromString(f.read())

  # import graph_def
  with tf.Graph().as_default() as graph:
    tf.import_graph_def(graph_def)

  # print operations
  for op in graph.get_operations():
    print(op.name)

if __name__ == "__main__":
  input_layer = "Placeholder"
  output_layer = "final_result"
  input_mean=0
  input_std=255
  parser = argparse.ArgumentParser()
  parser.add_argument("--image_list", help="image list to be processed")
  parser.add_argument("--graph", help="graph/model to be executed")
  parser.add_argument("--labels", help="name of file containing labels")
  parser.add_argument("--input_height", type=int, help="input height")
  parser.add_argument("--input_width", type=int, help="input width")
  parser.add_argument("--input_mean", type=int, help="input mean")
  parser.add_argument("--input_std", type=int, help="input std")
  parser.add_argument("--input_layer", help="name of input layer")
  parser.add_argument("--output_layer", help="name of output layer")
  parser.add_argument("--output_dir", help="name of output dir")
  parser.add_argument("--gt_files", help="name of file with all the GT info")
  args = parser.parse_args()

  if args.graph:
    model_file = args.graph
  if args.image_list:
    image_list = args.image_list
  if args.labels:
    label_file = args.labels
  if args.input_height:
    input_height = args.input_height
  if args.input_width:
    input_width = args.input_width
  if args.input_mean:
    input_mean = args.input_mean
  if args.input_std:
    input_std = args.input_std
  if args.input_layer:
    input_layer = args.input_layer
  if args.output_layer:
    output_layer = args.output_layer
  if args.output_dir:
    output_dir = args.output_dir
  if args.gt_files:
    gt_path = args.gt_files

  from absl import logging
  logging.info('=====================================================================')
  logging.info(model_file, image_list, label_file, input_height, input_width, input_mean, input_std)

  # pre-load list of files to cycle through
  #df = pd.read_csv(image_list)
  df = pd.read_csv(image_list)
  file_names = df.values.tolist()

  # these things are static
  graph = load_graph(model_file)

  #printTensors(model_file)
  input_name = "import/" + input_layer
  output_name = "import/" + output_layer
  input_operation = graph.get_operation_by_name(input_name)
  output_operation = graph.get_operation_by_name(output_name)
  
  # initiate empty vectores
  cnn_values = []
  cnn_labels = []

  for file_name in file_names:
    # dynamic component
    file_name = file_name[0]
    print(file_name)
    t = read_tensor_from_image_file(
        file_name,
        input_height=input_height,
        input_width=input_width,
        input_mean=input_mean,
        input_std=input_std)

    # return output
    with tf.Session(graph=graph) as sess:
      results = sess.run(output_operation.outputs[0], {
          input_operation.outputs[0]: t
      })
    results = np.squeeze(results)
    
    #get indexes of results according to ascending order
    top_k = results.argsort()[-len(results):][::-1]
    
    #sort results
    sorted_results = results[top_k]
    
    #sort the labels according to the top_k
    labels = np.array(load_labels(label_file))
    sorted_labels = labels[top_k]

    cnn_values.append(list(sorted_results))
    cnn_labels.append(list(sorted_labels))

#     #todo extract logits! STUPID STUFF THAT COSTED THE WHOLE RUN TO FAIL - WHAT WAS I THINKING - CHECK THIS SHAIT
#     top_k = results.argsort()[-len(results):][::-1]
#     labels = load_labels(label_file)
    
#     #zipp lists
#     zipped_lists = zip(top_k, labels)
#     sorted_zipped_lists = sorted(zipped_lists)
#     sorted_labels = [element for _, element in sorted_zipped_lists]

#     cnn_values.append(list(results[top_k]))
#     cnn_labels.append(sorted_labels)

# concat data into pandas data frame
df = pd.DataFrame({'cnn_labels':cnn_labels,
                   'cnn_values':cnn_values,
                   'files':file_names})

#add the gt label to the preditions
gt = readcsv_panda(gt_path)
#gt['basename'] = gt["name"].apply(lambda x: x[x.find('NL'):])
df['basename'] = df["files"].apply(lambda x: x[0][x[0].find('NL'):])
if 'BBCH' in gt_path:
    print("Doing BBCH")
    gt.drop(gt.columns.difference(['code_bbch_surveyed','basename']), 1, inplace=True)
else:
    print("Doing Crops")
    gt.drop(gt.columns.difference(['code_surveyed','basename']), 1, inplace=True)
df.drop(columns=['files'], axis=1, inplace=True)
df_join = pd.merge(df, gt, how='left', on="basename")

#to erase
out_file = os.path.join(output_dir,"cnn_output_data_check.csv")
df_join.to_csv(out_file, sep=',', index = False)

df_join['files'] = file_names
df.drop(columns=['basename'], axis=1, inplace=True)

# construct path
out_file = os.path.join(output_dir,"cnn_output_data.csv")

# write data to disk
df_join.to_csv(out_file, sep=',', index = False)


