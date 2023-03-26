#!/bin/sh

cd data/IAC/data_iac
git add .
git commit -m "removed bug lines"
git pull
cd ../../..
Rscript scripts/update_database_from_git.R
