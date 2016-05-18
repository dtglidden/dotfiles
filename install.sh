#!/usr/bin/env bash

cd $(dirname $0)/config_files
DIR=$(pwd)

for file in *; do
  source_file=${DIR}/${file}
  target_file=${HOME}/.${file}
  echo "Linking ${source_file} --> ${target_file}"
  if [ -d ${target_file} ]; then
    echo "Deleting original directory: ${target_file}"
    rm -rf ${target_file}
  fi
  ln -sf ${source_file} ${target_file}
done
