#!/bin/bash

dictionary=${environment:-dict}
input=${school:-inputFile}

while [ $# -gt 0 ]; do
   if [[ $1 == *"--"* ]]; then
        param="${1/--/}"
        declare $param="$2"
        # echo $1 $2 // Optional to see the parameter:value result
   fi
  shift
done

scala Main.scala $input $dictionary