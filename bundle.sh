#!/bin/bash
mkdir tab_calc_install/src
cp -u src/*.[fF]90 tab_calc_install/src
mkdir tab_calc_install/commands
cp -u commands/*.command tab_calc_install/commands
mkdir tab_calc_install/params
cp -u params/* tab_calc_install/params
mkdir tab_calc_install/variables
cp -u variables/*.vars tab_calc_install/variables
mkdir tab_calc_install/doc
cp -u doc/manual.html tab_calc_install/doc
cp -u Makefile tab_calc_install
cp -u VERSION tab_calc_install
cp -H USAGE tab_calc_install
rm tab_calc_install/lib/*
tar -czvf tab_calc_install.tgz tab_calc_install/