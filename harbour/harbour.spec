#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>,
# Dave Pearson <davep@davep.org>
# Harbour RPM spec file
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

######################################################################
# Conditional build:
# --with static      - link all binaries with static libs
# --with mysql       - build mysql lib
# --with pgsql       - build pgsql lib 
# --with pgsql4      - build pgsql4 lib
# --with gd          - build gd lib 
# --with allegro     - build GTALLEG - Allegro based GT driver
# --with ads         - build ADS RDD
# --with zlib        - build zlib and minizip wrapper
# --with odbc        - build odbc lib
# --without nf       - do not build nanforum lib
# --without gpllib   - do not build libs which needs GPL 3-rd party code
# --without x11      - do not build GTXWC
# --without gpm      - build GTTRM, GTSLN and GTCRS without GPM support
# --without gtsln    - do not build GTSLN
######################################################################

######################################################################
## Definitions.
######################################################################

# please add your distro suffix if it not belong to the one recognized below
# and remember that order checking can be important

%define platform %(release=$(rpm -q --queryformat='%{VERSION}' mandriva-release-common 2>/dev/null) && echo "mdv$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' redhat-release 2>/dev/null) && echo "rh$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' fedora-release 2>/dev/null) && echo "fc$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' suse-release 2>/dev/null) && echo "sus$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' aurox-release 2>/dev/null) && echo "aur$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %([ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/')
%endif
%endif
%endif
%endif
%endif
%endif
%endif


%define name     harbour
%define dname    Harbour
%define version  1.1.1
%define releasen 0
%define hb_pref  hb
%define hb_arch  export HB_ARCHITECTURE=linux
%define hb_cc    export HB_COMPILER=gcc
%define hb_cflag export C_USR="-O3 -DHB_FM_STATISTICS_OFF"
%define hb_lflag export L_USR="${CC_L_USR} %{?_with_static:-static}"
%define hb_mt    export HB_MT=no
%define hb_gt    export HB_GT_LIB=gttrm
%define hb_defgt export HB_GT_DEFAULT="${HB_GT_DEFAULT}"
%define hb_gpm   export HB_GPM_MOUSE=%{!?_without_gpm:yes}
%define hb_sln   export HB_WITHOUT_GTSLN=%{?_without_gtsln:yes}
%define hb_x11   export HB_WITHOUT_X11=%{?_without_x11:yes}
%define hb_bdir  export HB_BIN_INSTALL=%{_bindir}
%define hb_idir  export HB_INC_INSTALL=%{_includedir}/%{name}
%define hb_ldir  export HB_LIB_INSTALL=%{_libdir}/%{name}
%define hb_opt   export HB_GTALLEG=%{?_with_allegro:yes}
%define hb_cmrc  export HB_COMMERCE=%{?_without_gpllib:yes}
%define hb_ctrb  export HB_CONTRIBLIBS="%{?_with_odbc:hbodbc} %{?_with_zlib:hbzlib} %{?_with_ads:rddads} %{?_with_gd:hbgd} %{?_with_pgsql:hbpgsql} %{?_with_mysql:hbmysql}"
%define hb_env   %{hb_arch} ; %{hb_cc} ; %{hb_cflag} ; %{hb_lflag} ; %{hb_mt} ; %{hb_gt} ; %{hb_defgt} ; %{hb_gpm} ; %{hb_sln} ; %{hb_x11} ; %{hb_bdir} ; %{hb_idir} ; %{hb_ldir} ; %{hb_opt} ; %{hb_ctrb} ; %{hb_cmrc}

%define hb_host  www.harbour-project.org
%define readme   README.RPM
######################################################################
## Preamble.
######################################################################

Summary:        Free software Clipper compatible compiler
Summary(pl):    Darmowy kompilator kompatybilny z jЙzykiem Clipper.
Summary(pt_BR): Um compilador Clipper compativel Gratis
Summary(ru):    Свободный компилятор, совместимый с языком Clipper.
Name:           %{name}
Version:        %{version}
Release:        %{releasen}%{platform}
License:        GPL (plus exception)
Group:          Development/Languages
Vendor:         %{hb_host}
URL:            http://%{hb_host}/
Source:         %{name}-%{version}.src.tar.gz
Packager:       PrzemysЁaw Czerpak <druzus@polbox.com> Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
BuildPrereq:    gcc binutils bash ncurses ncurses-devel %{!?_without_gpm: gpm-devel}
Requires:       gcc binutils bash sh-utils %{name}-lib = %{?epoch:%{epoch}:}%{version}-%{release}
Provides:       %{name} harbour
BuildRoot:      /tmp/%{name}-%{version}-root

%define         _noautoreq    'libharbour.*'

%description
%{dname} is a CA-Clipper compatible compiler for multiple platforms. This
package includes a compiler, pre-processor, header files, virtual machine
and documentation.

See README.RPM in the documentation directory for information specific to
this RPM distribution.

%description -l pl
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator rozwijany na
wielu rС©nych platformach. Ten pakiet zawiera kompilator, preprocesor,
zbiory nagЁСwkowe, wirtualn╠ maszynЙ oraz dokumentacjЙ.

%description -l pt_BR
%{dname} И um compilador Clipper compativel para multiplas plataformas.
Esse pacote contem um compilador, um prИ-processador, arquivos de cabeГalho
uma maquina virtual e documentaГЦo.

%description -l ru
%{dname} - многоплатформенный компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит компилятор, препроцессор, файлы заголовков, виртуальную
машину и документацию.


######################################################################
## main shared lib
######################################################################

%package lib
Summary:        Shared runtime libaries for %{dname} compiler
Summary(pl):    Dzielone bilioteki dla kompilatora %{dname}
Summary(ru):    Совместно используемые библиотеки для компилятора %{dname}
Group:          Development/Languages
Provides:       lib%{name}.so lib%{name}mt.so

%description lib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} runtime shared libraries for programs
linked dynamically.

%description -l pl lib
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia dzielone bilioteki kompilatora %{dname}
dla programСw konsolidowanych dynamicznie.

%description -l pt_BR lib
%{dname} И um compilador compativel com o Clipper.
Esse pacote %{dname} provem as bibliotecas compartilhadas para programas
linkados dinamicamente.

%description -l ru lib
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит совместно используемые библиотеки %{dname},
необходимые для работы динамически скомпонованных программ.


######################################################################
## static libs
######################################################################

%package static
Summary:        Static runtime libaries for %{dname} compiler
Summary(pl):    Statyczne bilioteki dla kompilatora %{dname}
Summary(ru):    Статические библиотеки для компилятора %{dname}
Group:          Development/Languages
Requires:       %{name} = %{?epoch:%{epoch}:}%{version}-%{release}

%description static
%{dname} is a Clipper compatible compiler.
This package provides %{dname} static runtime libraries for static
program linking.

%description -l pl static
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia statyczne bilioteki dla kompilatora %{dname}
niezbЙdne do statycznej konsolidacji programСw.

%description -l pt_BR static
%{dname} И um compilador compativel com o clippe.
Esse pacote %{dname} provem as bibliotecas  de run time staticas para linkagem
dos os programas

%description -l ru static
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит статические библиотеки компилятора %{dname},
необходимые для статической компоновки программ.


%package contrib
Summary:        Contrib runtime libaries for %{dname} compiler
Summary(pl):    Bilioteki z drzewa contrib dla kompilatora %{dname}
Summary(pt_BR): Libs contrib para %{dname}
Summary(ru):    Библиотеки из дерева contrib для компилятора %{dname}
Group:          Development/Languages
Requires:       %{name} = %{?epoch:%{epoch}:}%{version}-%{release}

%description contrib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} contrib libraries for program linking.

%description -l pl contrib
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia statyczne bilioteki z drzewa contrib dla
kompilatora %{dname}.

%description -l pt_BR contrib
%{dname} И um compilador compativel com o clippe.
Esse pacote %{dname} provem as bibliotecas contrib para linkagem
dos programas.

%description -l ru contrib
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит статические библиотеки %{dname} из дерева contrib.


######################################################################
## Preperation.
######################################################################

%prep
%setup -c %{name}
rm -rf $RPM_BUILD_ROOT

######################################################################
## Build.
######################################################################

%build
%{hb_env}
case "`uname -m`" in
    *[_@]64)
        export C_USR="$C_USR -fPIC"
        ;;
esac

[ "%{?_with_odbc:1}" ]  || rm -fR contrib/hbodbc
[ "%{?_with_zlib:1}" ]  || rm -fR contrib/hbzlib
[ "%{?_with_ads:1}" ]   || rm -fR contrib/rddads
[ "%{?_without_nf:1}" ] && rm -fR contrib/hbnf

make -r

######################################################################
## Install.
######################################################################

%install

# Install harbour itself.

%{hb_env}
case "`uname -m`" in
    *[_@]64)
        export C_USR="$C_USR -fPIC"
        ;;
esac

export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
export _DEFAULT_INC_DIR=$HB_INC_INSTALL
export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL=$RPM_BUILD_ROOT/$HB_BIN_INSTALL
export HB_INC_INSTALL=$RPM_BUILD_ROOT/$HB_INC_INSTALL
export HB_LIB_INSTALL=$RPM_BUILD_ROOT/$HB_LIB_INSTALL

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL

make -r -i install

[ "%{?_with_odbc:1}" ]     || rm -f $HB_LIB_INSTALL/libhbodbc.a
[ "%{?_with_allegro:1}" ]  || rm -f $HB_LIB_INSTALL/libgtalleg.a
[ "%{?_with_ads:1}" ]      || rm -f $HB_LIB_INSTALL/librddads.a
[ "%{?_with_zlib:1}" ]     || rm -f $HB_LIB_INSTALL/libhbzlib.a
[ "%{?_without_nf:1}" ]    && rm -f $HB_LIB_INSTALL/libhbnf.a
[ "%{?_without_gtsln:1}" ] && rm -f $HB_LIB_INSTALL/libgtsln.a

# Keep the size of the binaries to a minimim.
strip $HB_BIN_INSTALL/harbour
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

mkdir -p $RPM_BUILD_ROOT%{_mandir}/man1
install -m644 doc/man/*.1* $RPM_BUILD_ROOT%{_mandir}/man1/

mkdir -p $RPM_BUILD_ROOT/etc/harbour
install -m644 source/rtl/gtcrs/hb-charmap.def $RPM_BUILD_ROOT/etc/harbour/hb-charmap.def
cat > $RPM_BUILD_ROOT/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O3
VERBOSE=YES
DELTMP=YES
EOF

# check if we should rebuild tools with shared libs
if [ "%{!?_with_static:1}" ]
then
    unset HB_GTALLEG
    export L_USR="${CC_L_USR} -L${HB_LIB_INSTALL} -l%{name} -lncurses %{!?_without_gtsln:-lslang} %{!?_without_gpm:-lgpm} %{!?_without_x11:-L/usr/X11R6/%{_lib} -lX11} %{?_with_pgsql4:/usr/lib/libpq.so.4} %{?_with_pgsql:-lpq} %{?_with_gd:-lgd}"
    export PRG_USR="\"-D_DEFAULT_INC_DIR='${_DEFAULT_INC_DIR}'\" ${PRG_USR}"

    for utl in hbmake hbrun hbdot hbpp hbdoc
    do
        pushd utils/${utl}
        rm -fR "./${HB_ARCHITECTURE}/${HB_COMPILER}"
        make -r install
        strip ${HB_BIN_INSTALL}/${utl}
        popd
    done
fi

# remove unused files
rm -f ${HB_BIN_INSTALL}/{hbdoc,hbtest,hbverfix,hbpptest}

# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of %{dname} includes extra commands to make compiling
and linking with %{dname} a little easier. There are compiler and linker
wrappers called "%{hb_pref}cc", "%{hb_pref}cmp", "%{hb_pref}lnk" and "%{hb_pref}mk".

"%{hb_pref}cc" is a wrapper to the C compiler only. It sets all flags
and paths necessary to compile .c files which include %{dname} header
files. The result of its work is an object file.

Use "%{hb_pref}cmp" exactly as you would use the harbour compiler itself.
The main difference with %{hb_pref}cmp is that it results in an object file,
not a C file that needs compiling down to an object. %{hb_pref}cmp also
ensures that the harbour include directory is seen by the harbour compiler.

"%{hb_pref}lnk" simply takes a list of object files and links them together
with the harbour virtual machine and run-time library to produce an
executable. The executable will be given the basename of the first object
file if not directly set by the "-o" command line switch.

"%{hb_pref}mk" tries to produce an executable from your .prg file. It's a simple
equivalent of cl.bat from the CA-Clipper distribution.

All these scripts accept command line switches:
-o<outputfilename>      # output file name
-static                 # link with static %{dname} libs
-fullstatic             # link with all static libs
-shared                 # link with shared libs (default)
-mt                     # link with multi-thread libs
-gt<hbgt>               # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be
                        #      the default at runtime
-xbgtk                  # link with xbgtk library (xBase GTK+ interface)
-hwgui                  # link with HWGUI library (GTK+ interface)
-l<libname>             # link with <libname> library
-L<libpath>             # additional path to search for libraries
-fmstat                 # link with the memory statistics lib
-nofmstat               # do not link with the memory statistics lib (default)
-[no]strip              # strip (no strip) binaries
-main=<main_func>       # set the name of main program function/procedure.
                        # if not set then 'MAIN' is used or if it doesn't
                        # exist the name of first public function/procedure
                        # in first linked object module (link)

Link options work only with "%{hb_pref}lnk" and "%{hb_pref}mk" and have no effect
in "%{hb_pref}cc" and "%{hb_pref}cmp".
Other options are passed to %{dname}/C compiler/linker.

An example compile/link session looks like:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}cmp foo
Harbour Compiler Alpha build 46.2 (Flex)
Copyright 1999-2006, http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ %{hb_pref}lnk foo.o

druzus@uran:~/tmp$ strip foo

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------

or using %{hb_pref}mk only:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}mk foo
Harbour Compiler Alpha build 46.2 (Flex)
Copyright 1999-2006, http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------


In this RPM you will find additional wonderful tools: /usr/bin/hbrun
You can run clipper/xbase compatible source files with it if you only
put in their first line:
#!/usr/bin/hbrun

For example:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
#!/usr/bin/hbrun
function main()
? "Hello, World!, This is a script !!! :-)"
?
return nil

druzus@uran:~/tmp$ chmod +x foo.prg

druzus@uran:~/tmp$ ./foo.prg

Hello, World!, This is a script !!! :-)

I hope this RPM is useful. Have fun with %{dname}.

Many thanks to Dave Pearson <davep@davep.org>

Przemyslaw Czerpak <druzus@polbox.com>
EOF

######################################################################
## Post install
######################################################################
#%post lib
#/sbin/ldconfig

######################################################################
## Post uninstall
######################################################################
#%postun lib
#/sbin/ldconfig

######################################################################
## Clean.
######################################################################

%clean
rm -rf $RPM_BUILD_ROOT

######################################################################
## File list.
######################################################################

%files
%defattr(-,root,root,755)
%doc ChangeLog*
%doc doc/*.txt
%doc doc/%{readme}
%doc doc/en/
%doc doc/es/

%dir /etc/harbour
%verify(not md5 mtime) %config /etc/harbour.cfg
%verify(not md5 mtime) %config /etc/harbour/hb-charmap.def
%{_bindir}/harbour
%{_bindir}/hb-mkslib
%{_bindir}/%{hb_pref}-build
%{_bindir}/%{hb_pref}cc
%{_bindir}/%{hb_pref}cmp
%{_bindir}/%{hb_pref}lnk
%{_bindir}/%{hb_pref}mk
#%{_bindir}/hbtest
%{_bindir}/hbrun
%{_bindir}/hbdot
%{_bindir}/hbpp
%{_bindir}/hbmake
%{_mandir}/man1/*.1*
%dir %{_includedir}/%{name}
%attr(644,root,root) %{_includedir}/%{name}/*

%files static
%defattr(644,root,root,755)
%dir %{_libdir}/%{name}
%{_libdir}/%{name}/libhbcpage.a
%{_libdir}/%{name}/libhbcommon.a
%{_libdir}/%{name}/libhbcplr.a
%{_libdir}/%{name}/libhbdebug.a
%{_libdir}/%{name}/libfm.a
%{_libdir}/%{name}/librddfpt.a
%{_libdir}/%{name}/librddcdx.a
%{_libdir}/%{name}/librddntx.a
%{_libdir}/%{name}/libgt*.a
%{_libdir}/%{name}/libhblang.a
%{_libdir}/%{name}/libhbmacro.a
%{_libdir}/%{name}/libhbpcre.a
%{_libdir}/%{name}/libhbnulrdd.a
%{_libdir}/%{name}/libhbpp.a
%{_libdir}/%{name}/libhbrdd.a
%{_libdir}/%{name}/libhbhsx.a
%{_libdir}/%{name}/libhbsix.a
%{_libdir}/%{name}/libhbrtl.a
%{_libdir}/%{name}/libhbvm.a
%{_libdir}/%{name}/libhbusrrdd.a

%files contrib
%defattr(644,root,root,755)
%dir %{_libdir}/%{name}
%{!?_without_nf: %{_libdir}/%{name}/libhbnf.a}
%{?_with_ads: %{_libdir}/%{name}/librddads.a}
%{?_with_zlib: %{_libdir}/%{name}/libhbzlib.a}
%{?_with_odbc: %{_libdir}/%{name}/libhbodbc.a}
%{?_with_mysql: %{_libdir}/%{name}/libhbmysql.a}
%{?_with_pgsql: %{_libdir}/%{name}/libhbpgsql.a}
%{?_with_pgsql4: %{_libdir}/%{name}/libhbpgsql.a}
%{?_with_gd: %{_libdir}/%{name}/libhbgd.a}
%{_libdir}/%{name}/libhbbtree.a
%{_libdir}/%{name}/libhbmisc.a
%{_libdir}/%{name}/libhbct.a
%{_libdir}/%{name}/libhbtip.a
%{_libdir}/%{name}/libxhb.a
%{_libdir}/%{name}/libhbgt.a
%{_libdir}/%{name}/libhbbmcdx.a
%{_libdir}/%{name}/libhbclipsm.a

%files lib
%defattr(755,root,root,755)
%dir %{_libdir}/%{name}
%{_libdir}/%{name}/*.so
%{_libdir}/*.so

######################################################################
## Spec file Changelog.
######################################################################

%changelog
* Thu Aug 23 2007 Przemyslaw Czerpak <druzus@priv.onet.pl>
+ added hbdot
- removed PP package

* Fri Mar 23 2005 Przemyslaw Czerpak <druzus@priv.onet.pl>
- removed bison and flex from dependences list

* Sat Aug 09 2003 Przemyslaw Czerpak <druzus@polbox.com>
- removed ${RPM_OPT_FLAGS} from C_USR

* Wed Jul 23 2003 Przemyslaw Czerpak <druzus@polbox.com>
- fixed file (user and group) owner for RPMs builded from non root account
- shared lib names changed from [x]harbour{mt,}.so to
  [x]harbour{mt,}-<version>.so and soft links with short names created
- 0.82 version set

* Wed Apr 30 2003 Przemyslaw Czerpak <druzus@polbox.com>
- new tool "%{hb_pref}-build" (%{hb_pref}cmp, %{hb_pref}lnk, %{hb_pref}mk) added -
  compiler/linker wrapper.
- new tool "hb-mkslib" (build shared libraries from static ones and object
  files).
- shared libraries added.
- binary package divided.

* Fri Mar 08 2002 Dave Pearson <davep@davep.org>
- Fixed gharbour so that it should work no matter the case of the name of
  the PRG file.

* Wed Feb 13 2002 Dave Pearson <davep@davep.org>
- Fixed a bug in harbour-link which meant that, since the environment
  changes of Jan 17 2002, users could not specify which GT library they
  wanted their application linked against.

* Tue Jan 22 2002 Dave Pearson <davep@davep.org>
- Used the "name" macro a lot more, especially in some paths.

* Thu Jan 17 2002 Dave Pearson <davep@davep.org>
- Removed the use of the /etc/profile.d scripts for setting the
  harbour environment variables. The settings are now placed
  directly in gharbour and harbour-link. This means that this .spec
  file should be more useful on RPM using platforms other than RedHat.

* Wed Dec 19 2001 Dave Pearson <davep@davep.org>
- Added a platform ID to the name of the RPM.

* Mon Dec 17 2001 Dave Pearson <davep@davep.org>
- todo.txt is now called TODO.

* Tue Aug 21 2001 Dave Pearson <davep@davep.org>
- Added todo.txt as a doc file.

* Sun Jul 22 2001 Dave Pearson <davep@davep.org>
- harbour-link now fully respects the setting of $HB_GT_LIB.
- HB_GT_LIB wasn't set in the csh startup script. Fixed.

* Fri Jul 20 2001 Dave Pearson <davep@davep.org>
- Added the setting of $HB_GT_LIB to the environment (ncurses is used).
- Added support for installing hbmake.

* Mon Jun 28 2001 Dave Pearson <davep@davep.org>
- Changed the gharbour script so that it only invokes the C compiler if a C
  file was output. This stops any error messages when someone is using the
  -g option to output other target types.

* Mon Mar 19 2001 Dave Pearson <davep@davep.org>
- Reinstated hbrun in the files section.

* Tue Feb 20 2001 Dave Pearson <davep@davep.org>
- Added README.RPM to the documentation directory.

* Sat Jan 06 2001 Dave Pearson <davep@davep.org>
- The gharbour script now passes the harbour include directory, using -I,
  to harbour.

* Thu Aug 24 2000 Dave Pearson <davep@davep.org>
- Changed the files section so that hbrun doesn't get installed. It isn't
  useful on GNU/Linux systems.

* Tue Aug 22 2000 Dave Pearson <davep@davep.org>
- Changed the 'egcs' requirement to 'gcc'.

* Mon Aug 21 2000 Przemyslaw Czerpak <druzus@polbox.com>
- Polish translation added
- BuildRoot marco added. Now you can build the package from normal user
  account.
- bison and flex added to BuildPrereq list
- Debug information is stripped from installed files.

* Wed Aug 02 2000 Dave Pearson <davep@davep.org>
- Removed hbtest from the list of files installed into the bin directory.
- Added 'bash' and 'sh-utils' to the list of required packages.

* Tue Aug 01 2000 Dave Pearson <davep@davep.org>
- Added harbour environment scripts to /etc/profile.d.
- Added generation of gharbour and harbour-link commands.

* Mon Jul 31 2000 Dave Pearson <davep@davep.org>
- Re-worked the layout of the spec file to make it cleaner and easier to
  read and maintain.
- The latest harbour ChangeLog is now installed into the RPM's doc
  directory.
- The content of the RPM's doc directory reflects the layout and content of
  the harbour source's doc directory.
