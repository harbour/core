######################################################################
## Definitions.
######################################################################

%define name     harbour
%define version  0.42
%define release  0
%define platform rh73
%define prefix   /usr
%define hb_cc    export HB_COMPILER=gcc
%define hb_arch  export HB_ARCHITECTURE=linux
%define hb_gt    export HB_GT_LIB=gtcrs
%define hb_bdir  export HB_BIN_INSTALL=%{prefix}/bin/
%define hb_idir  export HB_INC_INSTALL=%{prefix}/include/%{name}/
%define hb_ldir  export HB_LIB_INSTALL=%{prefix}/lib/%{name}/
%define hb_env   %{hb_cc} ; %{hb_arch} ; %{hb_gt} ; %{hb_bdir} ; %{hb_idir} ; %{hb_ldir}
%define readme   README.RPM

######################################################################
## Preamble.
######################################################################

Summary:        Free software Clipper compatible compiler.
Summary(pl):    Darmowy kompilator kompatybilny z jêzykiem Clipper.
Summary(es):    Compilador software-libre compatible con Clipper.
Name:           %{name}
Version:        %{version}
Release:        %{release}%{platform}
Prefix:         %{prefix}
Copyright:      GPL (plus exception)
Group:          Development/Languages
Vendor:         harbour-project.org
URL:            http://www.harbour-project.org/
Source:         %{name}-%{version}.tar.gz
Packager:       Alexander S.Kresin <alex@belacy.belgorod.su>
BuildPrereq:    gcc binutils bash flex bison ncurses ncurses-devel
Requires:       gcc binutils bash ncurses sh-utils
Provides:       %{name}
BuildRoot:      /tmp/%{name}-%{version}-root

%description
Harbour is a CA-Clipper compatible compiler for multiple platforms. This
package includes a compiler, pre-processor, header files, virtual machine,
run-time library and documentation.

You can find out more about harbour at http://www.harbour-project.org/.

See http://www.harbour-project.org/faq/ for the Harbour FAQ.

See README.RPM in the documentation directory for information specific to
this RPM distribution.

%description -l pl
Harbour to kompatybilny z jêzykiem CA-Clipper kompilator rozwijany na
wielu ró¿nych platformach. Ten pakiet zawiera kompilator, preprocesor,
zbiory nag³ówkowe, wirtualn± maszynê, biblioteki uruchomieniowe oraz
dokumentacjê.

Wiêcej inforamcji o projekcie harbour mo¿na znale¼æ na
   http://www.harbour-project.org/ (strona angielska)
lub na
   http://www.harbour.pl.eu.org/ (strona polska)

%description -l es
Harbour es un compilador compatible con CA-Clipper para múltiples
plataformas. Este paquete incluye un compilador, preprocesador, archivos
de cabecera, máquina virtual, librería de tiempo de ejecución (run-time)
y documentación.

Puede encontrar más información sobre harbour en
http://www.harbour-project.org/

Consulte http://www.harbour-project.org/faq/ para leer las FAQ
(preguntas más frecuentes) en inglés, o bien
http://www.iespana.es/todoharbour/documentos/doc_en_linea/index.htm
(en español)

Consulte README.RPM en el directorio de documentación para obtener
información (en inglés) sobre esta distribución RPM en particular.

######################################################################
## Preperation.
######################################################################

%prep 
%setup -n %{name}
rm -rf $RPM_BUILD_ROOT

######################################################################
## Build.
######################################################################

%build
%{hb_env}
make

######################################################################
## Install.
######################################################################

%install

# Install harbour itself.

%{hb_env}
export HB_BIN_INSTALL=$RPM_BUILD_ROOT/$HB_BIN_INSTALL
export HB_INC_INSTALL=$RPM_BUILD_ROOT/$HB_INC_INSTALL
export HB_LIB_INSTALL=$RPM_BUILD_ROOT/$HB_LIB_INSTALL

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL

make -i install

# Keep the size of the binaries to a minimim.
strip --strip-debug $HB_BIN_INSTALL/*
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

# Add a harbour compiler wrapper.
cat > $HB_BIN_INSTALL/gharbour <<EOF
#!/bin/bash

%{hb_env}
INCL="-I\${HB_INC_INSTALL}"
PATHS="\${INCL} -L\${HB_LIB_INSTALL}"
FILE=\$1
BASE=\$FILE
for ext in .{P,p}{R,r}{G,g} 
do
  BASE=\$(basename \$BASE \$ext)
done

[ "\$1" ] && shift

harbour \$FILE \${INCL} \$@ && [ -f \${BASE}.c ] && gcc -g -c \${BASE}.c \$PATHS && rm -f \${BASE}.c
EOF
chmod a+x $HB_BIN_INSTALL/gharbour

# Add a linker command.
cat > $HB_BIN_INSTALL/harbour-link <<EOF
#!/bin/bash

# Attempt to get the GT library setting from the user's environment.
GT_LIB=\${HB_GT_LIB:-gtcrs}

# Now set the full harbour environment.
%{hb_env}

# If we couldn't work out the GT library from the user's environment, set it
# from our local environment.
[ "\$GT_LIB" ] || GT_LIB=\$HB_GT_LIB

# Work out which system library is needed for the choice of GT library.
case "\${GT_LIB}" in
    gtcrs) TERM_LIB="-lncurses";;
    gtsln) TERM_LIB="-lslang";;
    *)     TERM_LIB="";;
esac

SYSTEM_LIBS="-lm \${TERM_LIB}"
HARBOUR_LIBS="-ldebug -lvm -lrtl -l\${GT_LIB} -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"

gcc \$@ -L\${HB_LIB_INSTALL} \${SYSTEM_LIBS} \${HARBOUR_LIBS} -o \$(basename \$1 .o)
EOF
chmod a+x $HB_BIN_INSTALL/harbour-link

# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of harbour includes two extra commands to make
compiling and linking with harbour a little easier. There is a compiler
wrapper called "gharbour" and a linker called "harbour-link".

Use "gharbour" exactly as you would use the harbour compiler itself. The
main difference with gharbour is that it results in an object file, not a C
file that needs compiling down to an object. gharbour also ensures that the
harbour include directory is seen by the harbour compiler.

"harbour-link" simply takes a list of object files and links them together
with the harbour virtual machine and run-time library to produce an
executable. The executable will be given basename of the first object file.

An example compile/link session looks like:

----------------------------------------------------------------------
davep@rama:~/temp$ cat foo.prg
Function Main()

   alert( "Hello, World!" )
   
Return( NIL )

davep@rama:~/temp$ gharbour foo -n
Harbour Compiler Alpha build 40.0 (Flex)
Copyright 1999-2002, http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 7, Functions/Procedures 1
Generating C source output to 'foo.c'... Done.

davep@rama:~/temp$ harbour-link foo.o

davep@rama:~/temp$ ls -l foo
-rwxrwxr-x    1 davep    davep      559076 Aug 16 18:36 foo*
----------------------------------------------------------------------

I hope this RPM is useful. Have fun with harbour.

Dave Pearson <davep@davep.org>
EOF

######################################################################
## File list.
######################################################################

%files
%defattr(-,root,root)
%doc ChangeLog
%doc TODO
%doc ERRATA
%doc doc/*.txt
%doc doc/%{readme}
%doc doc/en/
%doc doc/es/

%{prefix}/bin/harbour
%{prefix}/bin/gharbour
%{prefix}/bin/harbour-link
#%{prefix}/bin/hbtest
%{prefix}/bin/hbrun
%{prefix}/bin/hbpp
%{prefix}/bin/hbmake
%{prefix}/include/%{name}/*
%{prefix}/lib/%{name}/*

######################################################################
## Spec file Changelog.
######################################################################

%changelog
* Wed Aug 21 2002 Dave Pearson <davep@davep.org>
- Spanish translation added. Provided by Carlos Andrés.

* Fri Aug 16 2002 Dave Pearson <davep@davep.org>
- Corrected the example shown in README.RPM so that it demonstrates a 
  fully working program being built (the previous example showed me 
  building a program that wouldn't usefully run).

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
- The latest harbour ChangeLog is now installed into the RPM's doc directory.
- The content of the RPM's doc directory reflects the layout and content of 
  the harbour source's doc directory.
