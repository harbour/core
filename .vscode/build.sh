# Build GTNAP and run exemplo (cuademo) in Linux (Developer mode)

# # Generate GTNAP
mpath=$(pwd)
echo Main path: $mpath
cd ./contrib/gtnap
gtpath=$(pwd)
echo GTNAP path: $gtpath
# bash ./build.sh -b Debug

# Generate exemplo
cd $gtpath/tests/cuademo/gtnap_cualib
echo Exemplo: $gtpath/src/exemplo

if [ ! -d "$gtpath/src/exemplo" ]; then
    mkdir $gtpath/src/exemplo
fi

$mpath/bin/linux/gcc/hbmk2 -debug -trace -keepc -workdir=$gtpath/src/exemplo -o$gtpath/build/exemplo exemplo.hbp || exit 1

# Generate exemplo debug project
if [ ! -d "$gtpath/build_cuademo" ]; then
    mkdir $gtpath/build_cuademo
fi

cd $gtpath/build_cuademo
cmake ../src -DGTNAP_DEVELOPER_MODE=ON

# cd %gtpath%


# # ../../bin/linux/gcc/hbmk2 gtnap.hbp
# # cd ${pwd}
# # pwd
# # cd ./contrib/gtnap/tests/cuademo/gtnap_cualib
# # pwd
# # ../../../../../bin/linux/gcc/hbmk2 exemplo.hbp
# # ./exemplo --hb:gtnap
# # # ./exemplo --hb:gttrm
