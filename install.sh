#!/usr/bin/env bash

cd $(dirname $0)/config_files
DIR=$(pwd)

for file in *; do
  source_file=${DIR}/${file}
  target_file=${HOME}/.${file}
  if [[ "${file}" =~ \.el$ ]] ; then
    emacsd="${HOME}"/.emacs.d
    target_file="${emacsd}"/"${file}"
    [ -d "${emacsd}" ] || mkdir "${emacsd}"
    echo "Linking ${source_file} --> ${target_file}"
    ln -sf "${source_file}" "${target_file}"
  else
    if [ -d ${target_file} ]; then
      echo "Deleting original directory: ${target_file}"
      rm -rf ${target_file}
    fi
    echo "Linking ${source_file} --> ${target_file}"
    ln -sf ${source_file} ${target_file}
  fi
done
