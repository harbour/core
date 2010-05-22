#!/bin/sh
[ "$BASH" ] || exec bash $0 "$@"
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
# warning: some bash extensions are used
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

get_hbplatform()
{
    local id

    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        id="djgpp"
    else
        # please add your distro suffix if it not belong to the one recognized below
        # and remember that order checking can be important
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release-One 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' redhat-release 2>/dev/null) && echo "rh$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' fedora-release 2>/dev/null) && echo "fc$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' suse-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' openSUSE-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' aurox-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`[ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/'`
        [ "${id}" = "" ] && id=`uname -s | tr '[ A-Z]' '[_a-z]'`
        case "${id}" in
            mingw*) id="mingw" ;;
            *) ;;
        esac
    fi
    echo "${id}"
}

get_hbver()
{
    local FVER MAJOR MINOR RELEA hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    RELEA=`sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo "${MAJOR}.${MINOR}.${RELEA}"
}

get_hbver_win()
{
    local FVER MAJOR MINOR hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo "${MAJOR}${MINOR}"
}

get_hbver_so()
{
    local hb_ver hb_rootdir

    hb_rootdir="${1-.}"
    if [ "${HB_PLATFORM}" = "win" ] || \
       [ "${HB_PLATFORM}" = "wce" ]; then
        hb_ver=`get_hbver_win "${hb_rootdir}"`
        if [ "${HB_COMPILER}" = "mingw64" ]; then
            hb_ver="${hb_ver}-x64"
        elif [ "${HB_COMPILER}" = "mingwarm" ]; then
            hb_ver="${hb_ver}-wce-arm"
        fi
    else
        hb_ver=`get_hbver "${hb_rootdir}"`
    fi
    echo "${hb_ver}"
}

get_hbverstat()
{
    local FVER VERSTAT hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    VERSTAT=`sed -e '/HB_VER_STATUS/ !d' -e 's/[^\"]*\"\([^\"]*\).*/\1/g' "${FVER}"`
    echo "${VERSTAT}"
}

get_solibname()
{
    local name

    name="${HB_SHAREDLIB_NAME}"
    [ -z "${name}" ] && name="harbour"
    echo "${name}"
}

mk_hbgetlibs()
{
    local libs

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
    local LIBS LIBSMT l lm ll ld dir hb_rootdir hb_ver hb_libs full_lib_name full_lib_name_mt linker_options linker_mtoptions gpm lib_ext lib_pref lib_suff

    dir=`pwd`
    name=`get_solibname`
    hb_rootdir="${1-.}"

    hb_ver=`get_hbver_so "${hb_rootdir}"`
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
    if [ "${HB_USER_CFLAGS//-DHB_POSIX_REGEX/}" != "${HB_USER_CFLAGS}" ]; then
        hb_libs="${hb_libs//hbpcre/}"
    elif [ -z "${HB_HAS_PCRE_LOCAL}" ]; then
        linker_options="-lpcre ${linker_options}"
        hb_libs="${hb_libs//hbpcre/}"
    fi
    if [ -z "${HB_HAS_ZLIB_LOCAL}" ]; then
        linker_options="-lz ${linker_options}"
        hb_libs="${hb_libs//hbzlib/}"
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
            ll=${l%${lib_suff}}${lib_ext}
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
