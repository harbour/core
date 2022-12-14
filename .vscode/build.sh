pwd=$(pwd)
pwd
cd ./contrib/gtnap
../../bin/linux/gcc/hbmk2 gtnap.hbp
cd ${pwd}
pwd
cd ./contrib/gtnap/tests/cuademo/gtnap_cualib
pwd
../../../../../bin/linux/gcc/hbmk2 exemplo.hbp
# ./exemplo --hb:gtnap
./exemplo --hb:gttrm
