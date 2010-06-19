#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# simple script run after Harbour make install to finish install
# process
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

if [ -z "${__running_in_posix_sh__}" ]
then
   __running_in_posix_sh__=1
   export __running_in_posix_sh__

   [ -x /usr/bin/posix/sh ] && \
      exec /usr/bin/posix/sh "$0" ${1+"$@"}
   [ -x /usr/xpg4/bin/sh ] && \
      exec /usr/xpg4/bin/sh "$0" ${1+"$@"}

   exec /bin/sh "$0" ${1+"$@"}
fi

unset __running_in_posix_sh__

__builtin_which()
{
   what="$1"
   ret=1
   oIFS="$IFS"
   IFS=:
   for pathcomp in $PATH
   do
      if [ -x "${pathcomp}"/"${what}" ]
      then
         ret=0
         echo "${pathcomp}"/"${what}"
         break
      fi
   done
   IFS="$oIFS"
   return $ret
}

get_solibname()
{
   name="${HB_SHAREDLIB_NAME}"
   [ -z "${name}" ] && name="harbour"
   echo "${name}"
}

get_hbver_so()
{
   if [ "${HB_PLATFORM}" = "win" ] || \
      [ "${HB_PLATFORM}" = "wce" ]; then
      hb_ver_so=`get_hbver_win "${1-.}"`
      if [ "${HB_COMPILER}" = "mingw64" ]; then
         hb_ver_so="${hb_ver_so}-x64"
      elif [ "${HB_COMPILER}" = "mingwarm" ]; then
         hb_ver_so="${hb_ver_so}-wce-arm"
      fi
   else
      hb_ver_so=`get_hbver "${1-.}"`
   fi
   echo "${hb_ver_so}"
}

mk_hbgetlibs()
{
   if [ -z "$@" ]
   then
      libs=""
      if [ "$HB_PLATFORM" != "wce" ]
      then
          libs="$libs gtwin"
      fi
      echo "hbextern hbvm hbpp hbrtl hbrdd rddfpt rddcdx rddnsx rddntx hbhsx hbsix hbusrrdd hbmacro hbcommon hblang hbcpage gtcrs gtsln gtxvt gtxwc gtcgi gtstd gtpca gttrm $libs gtwvt gtgui gtdos gtos2 hbdebug profiler hbcplr hbpcre hbzlib"
   else
      echo "$@"
   fi
}

mk_hblibso()
{
   dir=`pwd`
   name=`get_solibname`

   hb_ver=`get_hbver_so "${1-.}"`
   hb_libs=`mk_hbgetlibs "$2"`
   if [ -n "${HB_USER_DLL_ADDONS}" ]; then
      hb_libs="${hb_libs} ${HB_USER_DLL_ADDONS}"
   fi
   [ -z "${HB_GT_LIB}" ] && HB_GT_LIB="gtstd"

   (cd ${HB_INST_PKGPREF}${HB_LIB_INSTALL}
   LIBS=""
   LIBSMT=""
   gpm="${HB_HAS_GPM}"
   if [ "${HB_PLATFORM}" = "beos" ]; then
      linker_options="-L/system/lib -lroot -lnetwork"
   else
      linker_options="-lm"
   fi
   linker_mtoptions=""
   if echo ${HB_USER_CFLAGS} | grep -q -- -DHB_POSIX_REGEX ; then
      hb_libs="` echo ${hb_libs} | sed 's!hbpcre!!g' `"
   elif [ -z "${HB_HAS_PCRE_LOCAL}" ]; then
      linker_options="-lpcre ${linker_options}"
      hb_libs="` echo ${hb_libs} | sed 's!hbpcre!!g' `"
   fi
   if [ -z "${HB_HAS_ZLIB_LOCAL}" ]; then
      linker_options="-lz ${linker_options}"
      hb_libs="` echo ${hb_libs} | sed 's!hbzlib!!g' `"
   fi
   if [ "${HB_COMPILER}" = "mingw" ] || [ "${HB_COMPILER}" = "mingw64" ]; then
      linker_options="${linker_options} -luser32 -lwinspool -lgdi32 -lcomctl32 -ladvapi32 -lcomdlg32 -lole32 -loleaut32 -luuid -lws2_32"
   elif [ "${HB_COMPILER}" = "mingwarm" ]; then
      linker_options="${linker_options} -lwininet -lws2 -lcommdlg -lcommctrl -luuid -lole32 -loleaut32"
   elif [ "${HB_COMPILER}" = "djgpp" ]; then
      linker_options="${linker_options}"
   elif [ "${HB_PLATFORM}" = "linux" ]; then
      linker_options="${linker_options} -ldl -lrt"
      linker_mtoptions="${linker_mtoptions} -lpthread"
   elif [ "${HB_PLATFORM}" = "sunos" ]; then
      linker_options="${linker_options} -lrt -lsocket -lnsl -lresolv"
      linker_mtoptions="${linker_mtoptions} -lpthread"
   elif [ "${HB_PLATFORM}" = "hpux" ]; then
      linker_options="${linker_options} -lrt"
      linker_mtoptions="${linker_mtoptions} -lpthread"
   elif [ "${HB_PLATFORM}" = "bsd" ]; then
      linker_options="$-L/usr/local/lib {linker_options}"
      linker_mtoptions="${linker_mtoptions} -lpthread"
   elif [ "${HB_PLATFORM}" = "darwin" ]; then
      linker_options="-L/sw/lib -L/opt/local/lib ${linker_options}"
      linker_mtoptions="${linker_mtoptions} -lpthread"
   fi

   for l in ${hb_libs}
   do
      case $l in
         hbdebug|profiler|hbcplr|hbodbc|gtalleg|rddads) ;;
         *)
            ls="lib${l}.a"
            if [ -f lib${l}mt.a ]
            then
               lm="lib${l}mt.a"
            else
               lm="${ls}"
            fi
            if [ -f $lm ]
            then
               LIBSMT="$LIBSMT $lm"
            fi
            if [ -f $ls ]
            then
               LIBS="$LIBS $ls"
               if [ "${l}" = gtcrs ]; then
                  if [ "${HB_PLATFORM}" = "sunos" ] || \
                     [ "${HB_PLATFORM}" = "bsd" ]; then
                     linker_options="$linker_options -lcurses"
                  else
                     linker_options="$linker_options -lncurses"
                  fi
               elif [ "${l}" = gtsln ]; then
                  linker_options="$linker_options -lslang"
               elif [ "${l}" = gtxwc ]; then
                  [ -d "/usr/X11R6/lib64" ] && \
                     linker_options="$linker_options -L/usr/X11R6/lib64"
                  [ -d "/usr/X11R6/lib" ] && \
                     linker_options="$linker_options -L/usr/X11R6/lib"
                  linker_options="$linker_options -lX11"
               fi
               if [ -n "${gpm}" ] && ( [ "${l}" = gtcrs ] || \
                  [ "${l}" = gtsln ] || [ "${l}" = gttrm ] ); then
                  linker_options="$linker_options -lgpm"
                  gpm=""
               fi
            fi
            ;;
      esac
   done
   if [ "${HB_PLATFORM}" = "darwin" ]; then
      lib_ext=".dylib"
      lib_pref="lib"
      lib_suff=".${hb_ver}${lib_ext}"
   elif [ "${HB_PLATFORM}" = "win" ] || \
        [ "${HB_PLATFORM}" = "wce" ]; then
      lib_ext=".dll"
      lib_pref=""
      lib_suff="-${hb_ver}${lib_ext}"
   elif [ "${HB_PLATFORM}" = "hpux" ]; then
      lib_ext=".sl"
      lib_pref="lib"
      lib_suff="-${hb_ver}${lib_ext}"
   else
      lib_ext=".so"
      lib_pref="lib"
      lib_suff="-${hb_ver}${lib_ext}"
   fi
   full_lib_name="${lib_pref}${name}${lib_suff}"
   full_lib_name_mt="${lib_pref}${name}mt${lib_suff}"
   hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/${HB_TOOLS_PREF-hb}-mkdyn"
   if [ -n "${HB_USER_DLL_ADDONS}" ]; then
      echo "Making ${full_lib_name}..."
      ${hb_mkdyn} ${full_lib_name} ${LIBS} ${linker_options}
      if [ "${LIBS}" != "${LIBSMT}" ]; then
         echo "Making ${full_lib_name_mt}..."
         ${hb_mkdyn} ${full_lib_name_mt} ${LIBSMT} ${linker_mtoptions} ${linker_options}
      fi
   fi
   for l in ${full_lib_name} ${full_lib_name_mt}
   do
      if [ -f $l ]
      then
         ll="` echo $l | sed 's!'${lib_suff}'$!!' `${lib_ext}"
         ln -sf $l $ll
         if [ "${HB_PLATFORM}" = "win" ] || \
            [ "${HB_PLATFORM}" = "wce" ]; then
            if [ "${HB_PLATFORM}" = "${HB_HOST_PLAT}" ]; then
               (cd "$dir"
               mv "${HB_INST_PKGPREF}${HB_LIB_INSTALL}/$l" "${HB_INST_PKGPREF}${HB_BIN_INSTALL}"
               mv "${HB_INST_PKGPREF}${HB_LIB_INSTALL}/$ll" "${HB_INST_PKGPREF}${HB_BIN_INSTALL}")
            fi
         else
            case $HB_LIB_INSTALL in
               /usr/lib/${name}|/usr/lib64/${name}|/usr/local/lib/${name}|/usr/local/lib64/${name})
                  ln -sf ${name}/$l ../$ll
                  ln -sf ${name}/$l ../$l
                  ;;
               /usr/local/${name}/lib)
                  ld="/usr/lib"
                  if [ -n "${HB_INST_PKGPREF}" ] || [ -w $ld ]
                  then
                     mkdir -p ${HB_INST_PKGPREF}$ld
                     ln -sf ../local/${name}/lib/$l ${HB_INST_PKGPREF}$ld/$ll
                     ln -sf ../local/${name}/lib/$l ${HB_INST_PKGPREF}$ld/$l
                  fi
                  ;;
               *)
                  ;;
            esac
            ld="/etc/ld.so.conf.d"
            if [ -d $ld ] && ( [ -n "${HB_INST_PKGPREF}" ] || [ -w $ld ] )
            then
               mkdir -p ${HB_INST_PKGPREF}$ld
               echo "$HB_LIB_INSTALL" > ${HB_INST_PKGPREF}/$ld/${name}.conf
            fi
         fi
      fi
   done
   )
   #export LD_LIBRARY_PATH="${HB_INST_PKGPREF}$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
}

if [ -z "$HB_PLATFORM" ] || [ -z "$HB_COMPILER" ] || \
   [ -z "$HB_BIN_INSTALL" ] || \
   [ -z "$HB_INC_INSTALL" ] || \
   [ -z "$HB_LIB_INSTALL" ]
then
   echo "The following envvars must be set:"
   echo "   HB_PLATFORM"
   echo "   HB_COMPILER"
   echo "   HB_BIN_INSTALL"
   echo "   HB_INC_INSTALL"
   echo "   HB_LIB_INSTALL"
   exit 1
fi

hb_root=`dirname "$0"`
if [ "${hb_root}" = "." ]
then
   hb_root=".."
else
   hb_root=`dirname "${hb_root}"`
fi
if [ ! -f ${hb_root}/bin/hb-func.sh ] && [ -f ./bin/hb-func.sh ]
then
   hb_root="."
fi

. ${hb_root}/bin/hb-func.sh

__install="` __builtin_which install `"

# chmod 644 ${HB_INST_PKGPREF}${HB_INC_INSTALL}/*

if [ "$HB_COMPILER" = "gcc" ] || \
   [ "$HB_COMPILER" = "mingw" ] || \
   [ "$HB_COMPILER" = "mingw64" ] || \
   [ "$HB_COMPILER" = "mingwarm" ] || \
   [ "$HB_COMPILER" = "cygwin" ] || \
   [ "$HB_COMPILER" = "djgpp" ] || \
   [ "$HB_COMPILER" = "icc" ] || \
   [ "$HB_COMPILER" = "sunpro" ] || \
   [ "$HB_COMPILER" = "open64" ] || \
   [ "$HB_COMPILER" = "clang" ]
then
   if [ -n "${HB_TOOLS_PREF}" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s!^# HB_PLATFORM=\"\"\$!HB_PLATFORM=\"${HB_PLATFORM}\"!g" \
          -e "s!^# HB_CCPREFIX=\"\"\$![ -n \"\${HB_CCPREFIX}\" ] || HB_CCPREFIX=\"${HB_CCPREFIX}\"!g" \
          -e "s!^# HB_CCPATH=\"\"\$![ -n \"\${HB_CCPATH}\" ] || HB_CCPATH=\"${HB_CCPATH}\"!g" \
          "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "icc" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s/gcc/icc/g" "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "sunpro" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      lnopt=""
      [ "$HB_BUILD_OPTIM" = "no" ] || lnopt="-fast -xnolibmopt $lnopt"
      sed -e "s/gcc -shared/suncc -G ${lnopt} ${HB_ISAOPT}/g" \
          "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "open64" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s/gcc/opencc/g" "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "${HB_PLATFORM}" = "sunos" ] || \
        [ "${HB_PLATFORM}" = "hpux" ] || \
        [ -z "${__install}" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      cp "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "${HB_PLATFORM}" != "dos" ]; then
      hb_mkdyn="${HB_INST_PKGPREF}${HB_BIN_INSTALL}/hb-mkdyn"
      # Without -c some OSes _move_ the file instead of copying it!
      ${__install} -c -m 755 "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}"
   fi

   if [ "${HB_PLATFORM}" != "dos" ]; then
      mk_hblibso "${hb_root}"
   fi
fi
