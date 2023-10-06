# Build GTNAP and generate developer mode examples (Linux)

# Generate GTNAP
mpath=$(pwd)
echo Main path: $mpath
cd ./contrib/gtnap
gtpath=$(pwd)
echo GTNAP path: $gtpath
bash ./build.sh -b Debug

# Generate exemplo sources
cd $gtpath/tests/cuademo/gtnap_cualib
echo Exemplo: $gtpath/src/exemplo

if [ ! -d "$gtpath/src/exemplo" ]; then
    mkdir $gtpath/src/exemplo
fi

$mpath/bin/linux/gcc/hbmk2 exemplo.hbp -debug -trace -keepc -workdir=$gtpath/src/exemplo -o$gtpath/build/exemplo || exit 1

# Generate hello sources
cd $gtpath/tests/hello
echo Hello: $gtpath/src/hello

if [ ! -d "$gtpath/src/hello" ]; then
    mkdir $gtpath/src/hello
fi

$mpath/bin/linux/gcc/hbmk2 hello.hbp -debug -trace -keepc -workdir=$gtpath/src/hello -o$gtpath/build/hello || exit 1

# Generate CMake solution
if [ ! -d "$gtpath/build-dev" ]; then
    mkdir $gtpath/build-dev
fi

cd $gtpath/build-dev
cmake .. -DGTNAP_DEVELOPER_MODE=ON
