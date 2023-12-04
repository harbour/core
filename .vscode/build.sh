# Build GTNAP and generate developer mode examples (Linux)

# Generate GTNAP
mpath=$(pwd)
HBMK_PATH=bin/linux/gcc/hbmk2

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    HBMK_PATH=bin/darwin/clang/hbmk2
# elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
#     # Do something under GNU/Linux platform
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
#     # Do something under 32 bits Windows NT platform
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
#     # Do something under 64 bits Windows NT platform
fi

echo Main path: $mpath
cd ./contrib/gtnap
gtpath=$(pwd)
echo GTNAP path: $gtpath
echo HBMK path: $HBMK_PATH
bash ./build.sh -b Debug || exit 1

# Generate exemplo sources
cd $gtpath/tests/cuademo/gtnap_cualib
echo Exemplo: $gtpath/src/exemplo

if [ ! -d "$gtpath/src/exemplo" ]; then
    mkdir $gtpath/src/exemplo
fi

$mpath/$HBMK_PATH -debug -trace -keepc -workdir=$gtpath/src/exemplo -o$gtpath/build/exemplo exemplo.hbp || exit 1

# Generate hello sources
cd $gtpath/tests/hello
echo Hello: $gtpath/src/hello

if [ ! -d "$gtpath/src/hello" ]; then
    mkdir $gtpath/src/hello
fi

$mpath/$HBMK_PATH -debug -trace -keepc -workdir=$gtpath/src/hello -o$gtpath/build/hello hello.hbp || exit 1

# Generate CMake solution
if [ ! -d "$gtpath/build-dev" ]; then
    mkdir $gtpath/build-dev
fi

cd $gtpath/build-dev
if [ "$(uname)" == "Darwin" ]; then
    cmake -G Xcode .. -DGTNAP_DEVELOPER_MODE=ON
else
    cmake .. -DGTNAP_DEVELOPER_MODE=ON
fi
