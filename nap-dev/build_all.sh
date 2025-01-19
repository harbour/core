#
# Build all script
# Harbour/GTNAP/HBAWS/HBOFFICE
# Using different compilers (VS, MinGW-GCC, MinGW-Clang)
# Important! This script will remove previous Harbour compilations
# It can take several minutes to finish all tasks
#
# Options: -comp [gcc|clang]  (gcc default)
#          -b [Debug|Release] (Release default)
#
# build_all.bat -comp msvc64 -b Release
#
ALL_BUILD_COMPILER=gcc
BUILD=Release
HBMK_FLAGS=

while [[ $# -gt 0 ]]; do
  case $1 in
    -comp)
      ALL_BUILD_COMPILER="$2"
      shift
      shift
      ;;
    -b)
      BUILD="$2"
      shift
      shift
      ;;
    -*|--*)
      shift
      ;;
  esac
done

cd ..

# Remove previous compilations
rm -rf bin/linux
rm -rf lib/linux
make clean

# Allowed compilers for Linux
if [[ "$ALL_BUILD_COMPILER" != "gcc" && "$ALL_BUILD_COMPILER" != "clang" ]]; then
    echo Error Unknown compiler: $ALL_BUILD_COMPILER
    exit 1
fi

# Compile Harbour with gcc or clang
make -j4 HB_CPU=x86_64 HB_COMPILER=$ALL_BUILD_COMPILER
echo ----------------------------------------
echo Harbour $ALL_BUILD_COMPILER build successfully
echo ----------------------------------------

# Compile LibreOffice
cd contrib/hboffice
rm -rf build

# First, the LibreOffice dll
bash build.sh -dll -comp $ALL_BUILD_COMPILER -b $BUILD || { echo "Error building HBOffice DLL" ; exit 1; }
echo --------------------------------
echo hboffice dll build successfully
echo --------------------------------

# Then, the LibreOffice lib
bash build.sh -lib -comp $ALL_BUILD_COMPILER -b $BUILD || { echo "Error building HBOffice LIB" ; exit 1; }
echo --------------------------------
echo hboffice lib build successfully
echo --------------------------------

# Return to harbour main path after hboffice
cd ..
cd ..

# Build hbaws
cd contrib/hbaws
rm -rf build
bash build.sh -comp $ALL_BUILD_COMPILER -b $BUILD || { echo "Error building HBAWS" ; exit 1; }
echo -------------------------
echo hbaws build successfully
echo -------------------------

# Return to harbour main path after hbaws
cd ..
cd ..

# Build gtnap
cd contrib/gtnap
rm -rf build
bash build.sh -comp $ALL_BUILD_COMPILER -b $BUILD || { echo "Error building GTNAP" ; exit 1; }
echo -------------------------
echo gtnap build successfully
echo -------------------------

cd tests/cuademo/gtnap_cualib
rm exemplo
rm *.so
cp ../../../../hboffice/build/$BUILD/bin/libofficesdk.so .
../../../../../bin/linux/$ALL_BUILD_COMPILER/hbmk2 -comp=$ALL_BUILD_COMPILER exemplo.hbp
export LIBREOFFICE_HOME=/usr/lib/libreoffice
export LD_LIBRARY_PATH=.:/usr/lib/libreoffice/program

./exemplo --hb:gtnap

# Return to gtnap path
cd ..
cd ..
cd ..

# Return to harbour main path after hboffice
cd ..
cd ..

echo ---------------------------------------
echo All build jobs generated successfully
echo ---------------------------------------
