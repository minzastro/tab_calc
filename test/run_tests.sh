export FC=gfortran
mv .hidden/* .
cp -u ../src/*.[fF]90 .
cp -u ../src/*.i .
for file in *.F90; do
  cpp -E ${file} > ${file/F90/f90}
done
funit
#funit --clean
mv *.[oi] .hidden
mv *.mod .hidden
mv *.[fF]90 .hidden