#!/bin/bash

bk_dst="./resources/backup/`date +%F__%T`"

mkdir -p $bk_dst
cp -vi ./app/*.js $bk_dst;
