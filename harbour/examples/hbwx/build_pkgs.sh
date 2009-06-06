#!/bin/sh
#
# Build script to generate tgz, rpm, zip files
#
# (C) Teo Fonrouge 2008
#

rootdir="./"
specfile="wxharbour.spec"
fname=`sed -n -e 's/\(%define \+name \+\)\(.*\)/\2/p' ${rootdir}${specfile}`
fversion=`sed -n -e 's/\(%define \+version \+\)\(.*\)/\2/p' ${rootdir}${specfile}`
frelease=`sed -n -e 's/\(%define \+release \+\)\(.*\)/\2/p' ${rootdir}${specfile}`
fullname="${fname}-${fversion}-${frelease}"

srcName=${fullname}.src
srcTarName=${srcName}.tar.gz
srcZipName=${srcName}.zip

rm_dir(){

  [ -d ${fullname} ] && rm -r ${fullname}

}

build_dir() {

  listDir=`cd ${rootdir} ; svn status -v | grep -v '^?' | sed -e 's/.* \([^ ]*\)/\1/g' | sort`

  rm_dir

  for fname in ${listDir} ; do
    if [ -d ${rootdir}${fname} ] ; then
      mkdir -p ${fullname}/${fname}
    elif [ -f ${rootdir}${fname} ] ; then
      cp ${rootdir}${fname} ${fullname}/${fname}
    fi
  done

}

build_tgz() {

  tar -cvzf ${srcTarName} ${fullname}

}

build_zip() {

  zip -r9 ${srcZipName} ${fullname}

}

build_rpm() {

  rpmbuild -ta ${srcTarName}

}

case ${1} in
  dir)
    build_dir
    ;;
  tgz)
    build_dir
    build_tgz
    rm_dir
    ;;
  zip)
    build_dir
    build_zip
    rm_dir
    ;;
  rpm)
    build_dir
    build_tgz
    build_rpm
    rm_dir
    ;;
  all)
    build_dir
    build_tgz
    build_zip
    build_rpm
    ;;
  *)
    echo "Helper script for creating packages"
    echo "Usage: ${0} dir|tgz|zip|rpm|all"
  ;;
esac
