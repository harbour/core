#!/bin/sh
#
# $Id$
#

sed -e "s/\.obj/.o/g"         \
    -e "s/\.lib/.a/g"         \
    -e "s/\.dll/.so/g"        \
    -e "s/\.exe//g"           \
    -e "s/;/ /g"              \
    -e 's!\\\(.\)!/\1!g'      \
    -e "s/^HB_LIB_PREFIX=.*$/HB_LIB_PREFIX=lib/g"                       \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" != "\(.*\)"/ifneq (\1,\2)/g'      \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" == "\(.*\)"/ifeq (\1,\2)/g'       \
    -e 's/^!ifdef /ifdef /g'                                            \
    -e 's/^!ifndef /ifndef /g'                                          \
    -e 's/^!else/else/g'                                                \
    -e 's/^!endif/endif/g'                                              \
    -e 's/^!include/include/g'                                          \
    -e 's/$(HB_GT_LIST)/$(foreach gt, $(HB_GT_LIST), $(GT$(gt)_LIB))/g' \
   common.mak > common.cf
