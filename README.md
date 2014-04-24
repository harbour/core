# Welcome to Harbour [![Build Status](https://travis-ci.org/vszakats/harbour-core.png)](https://travis-ci.org/vszakats/harbour-core)

Harbour is the free software implementation of a multi-platform,
multi-threading, object-oriented, scriptable programming language,
backward compatible with Clipper/xBase. Harbour consists of
a compiler and runtime libraries with multiple UI and database
backends, its own make system and a large collection of libraries
and interfaces to many popular APIs.

# Table of Content

1. [How to Donate](#how-to-donate)
2. [How to Get](#how-to-get)
3. [How to Build](#how-to-build)
4. [How to Do a Partial Build](#how-to-do-a-partial-build)
5. [How to Create Distributable Packages](#how-to-create-distributable-packages)
6. [How to Enable Optional Components](#how-to-enable-optional-components)
7. [Build Options](#build-options)
8. [Build Examples](#build-examples)
9. [Build Your Own Harbour App](#build-your-own-harbour-app)
10. [Debugging Options](#debugging-options)
11. [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)
12. [Platform Matrix](#platform-matrix)
13. [External links](#external-links)
14. [Harbour Links](#harbour-links)
15. [Guarantees and Liability](#guarantees-and-liability)

---

# How to Donate

  You can donate to fund further maintenance of this fork:

  * [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BPSZQYKXMQJYG)
  * [Bitcoin](https://coinbase.com/checkouts/b90e7d8467c3d17f0083f9ad186c3c36)


# How to Get

## Stable versions

### Harbour stable binary download

Download binary archive from this page and unpack or install:

* <https://sourceforge.net/projects/harbour-project/files/><br />
(choose the highest version number)

### Harbour stable source download

Download source archive from this page and unpack:

* <https://sourceforge.net/projects/harbour-project/files/source/><br />
(choose the highest version number)


## Unstable versions

> WARNING:
> Recommended
> [for](https://groups.google.com/forum/#!msg/harbour-users/2fwUzdKwpKA/32nI4WhZLfYJ)
> [users](https://groups.google.com/forum/#!msg/harbour-users/Ro99f8S6my0/KvfjhCx_jE4J)
> contributing to development, following [commits](https://github.com/vszakats/harbour-core/commits/master) and reading
> [ChangeLog.txt](ChangeLog.txt?raw=true).

### Harbour live source repository

You'll need Git version control software installed on your system,
and issue this command:

    git clone https://github.com/vszakats/harbour-core.git harbour-core

You can get subsequent updates using this command:

    git pull

### Harbour unstable sources

Download source archive from any of these links and unpack:

* <https://github.com/vszakats/harbour-core/archive/master.zip>
* <https://github.com/vszakats/harbour-core/archive/master.tar.gz>

### Follow commits using any of these facilities

* [Web](https://github.com/vszakats/harbour-core/commits/master)
* [RSS](https://github.com/vszakats/harbour-core/commits/master.atom)
* [Mac app](https://itunes.apple.com/us/app/committed/id560767719)
* [Mac tool](https://github.com/marcocampana/git-notifier)


# How to Build

For all platforms you'll need:

* Supported ANSI C compiler
* GNU Make (3.81 recommended, minimum 3.79 required, see also platform details)
* Harbour sources (2.0.0 or upper)

## on Windows hosts (possible cross-build targets: Windows CE, MS-DOS, OS/2, Linux)

Platform specific prerequisites:

1. Windows 7 or upper system is recommended to build Harbour.
2. Make sure to have your C compiler of choice installed
   in PATH. Refer to your C compiler installation and setup
   instructions for details. It's recommended to make sure no tools
   in your PATH belonging to other C compilers are interfering with
   your setup. It's also discouraged to keep multiple copies
   of the same compiler, or different versions of the same compiler
   in PATH at the same time. For the list of supported compilers,
   look up [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers).
3. GNU Make 3.81 or upper is required. A copy of this tool
   is included in all Harbour packages, so you don't have to do
   anything.
   If you want to get it separately, you can find it [here](https://sourceforge.net/projects/mingw/files/MinGW/Extension/make/)
   Unpack it to your PATH or Harbour source root directory,
   and run it as `mingw32-make`.

To build:

    > win-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see ``Hello, world!`` on screen.

## on Windows hosts with POSIX shells (MSYS/Cygwin)

> Though you can use these alternative shells to build Harbour on Windows,
> it's recommended to use the native one.

To build:

    > sh -c make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see ``Hello, world!`` on screen.

> When building for Borland C++ make sure that GNU Make
> is executed when typing ``make``, Borland Make has the same name.

## on MS-DOS hosts (possible cross-build targets: Windows, OS/2, Linux)

Make sure to have your C compiler of choice installed in PATH.

To build:

    > dos-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

 You should see ``Hello, world!`` on screen.

## on OS/2 hosts (possible cross-build targets: MS-DOS, OS/2, Linux)

To build:

    > os2-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see ``Hello, world!`` on screen.

## on Linux hosts (possible cross-build targets: Windows, Windows CE, MS-DOS, OS/2)

To build:

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see ``Hello, world!`` on screen.

## on Darwin (OS X) hosts (possible cross-build targets: Windows, Windows CE, MS-DOS)

Platform specific prerequisite:
   Xcode or Command Line Tools for Xcode installed

To build:

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see ``Hello, world!`` on screen.

> You can override default (host) architecture by adding
> values below to `HB_USER_CFLAGS`, `HB_USER_LDFLAGS` envvars,
> you can use multiple values:<br />
> <br />
> Intel 32-bit: `-arch i386`<br />
> Intel 64-bit: `-arch x86_64`<br />
> PPC 32-bit:   `-arch ppc`<br />
> PPC 64-bit:   `-arch ppc64`

## on FreeBSD hosts

Platform specific prerequisites:

1. You'll need to have the developer tools installed.
2. Then you'll need to install gmake and optionally bison.
   If you installed the ports collection, then all you need
   to do to install bison and gmake is to run the following
   commands, which may require that you run su root first to
   get the correct permissions:

        $ cd /usr/ports/devel/gmake
        $ make
        $ make install
        $ make clean
        $ cd /usr/ports/devel/bison
        $ make
        $ make install
        $ make clean

To build:

    $ gmake

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see ``Hello, world!`` on screen.

## on Minix hosts

Install GNU make from the Minix pkgsrc repository; for details see [here](http://wiki.minix3.org/en/UsersGuide/InstallingBinaryPackages).

Optionally, GCC may also be installed if you wish to use that instead
of Clang, the Minix system compiler.

## on other *nix hosts (possible cross-build targets: Windows, Windows CE, MS-DOS)

To build:

    $ gmake [HB_PLATFORM=<...>]

Or

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see ``Hello, world!`` on screen.

> For sunpro on Solaris:<br />
> If you have any GNU binutils stuff installed, do make sure
> `/usr/ccs/bin` (the location of the native Sun C compilation
> system tools) come *before* the GNU binutils components in
> your `$PATH`.


# How to Do a Partial Build

If you want to build only a specific part of Harbour, like
one core library or all core libraries, or all contrib packages,
you have to do everything the same way as for a full build, the
only difference is that you first have to go into the specific
source directory you want to build. When starting GNU Make,
all components under that dir will be built:

    cd src/rtl
    <make> [clean]

If you want to rebuild one specific contrib package, use this:

    cd contrib
    hbmk2 make.hb <name> [clean] [custom hbmk2 options]


# How to Create Distributable Packages

## Source .tgz on *nix

    $ package/mpkg_src.sh

## Binary .tgz on *nix

    $ export HB_BUILD_PKG=yes
    $ make clean install

## Binary .deb on Linux

    $ fakeroot debian/rules binary

## Binary .rpm on Linux

    $ package/mpkg_rpm.sh

You can fine-tune the build with these options:

    --with static      - link all binaries with static libs
    --with localzlib   - build local copy of zlib library
    --with localpcre   - build local copy of pcre library
    --without x11      - do not build components dependent on x11 (gtxwc)
    --without curses   - do not build components dependent on curses (gtcrs)
    --without slang    - do not build components dependent on slang (gtsln)
    --without gpllib   - do not build components dependent on GPL 3rd party code
    --without gpm      - build components without gpm support (gttrm, gtsln, gtcrs)

## Binary .rpm on Linux (cross-builds)

### for Windows:

    $ package/mpkg_rpm_win.sh

### for Windows CE:

    $ package/mpkg_rpm_wce.sh

## Binary .zip and .exe on Windows for all targets (except Linux)

    $ set HB_DIR_NSIS=%ProgramFiles%\NSIS\
    $ set HB_DIR_ZIP=C:\info-zip\
    $ set HB_BUILD_PKG=yes

Then run build as usual with `clean install` options.
See: [How to Build](#how-to-build)

## Binary .zip on MS-DOS for all targets (except Linux)

    $ set HB_DIR_ZIP=C:\info-zip\
    $ set HB_BUILD_PKG=yes

Then run build as usual with `clean install` options.
See: [How to Build](#how-to-build)

## Unified .7z and .exe installer for Windows

    $ package\winuni\mpkg_win_uni.bat

> Read in-file instructions and do the necessary
> steps before calling the script.


# How to Enable Optional Components

Certain Harbour parts &ndash; typically contrib packages &ndash; depend
on 3rd party components. To make these Harbour parts built,you need
to tell Harbour where to find the headers for these 3rd party
components.

On *nix systems most of these 3rd party components will
automatically be used if installed on well-known standard
system locations.

You only need to use manual setup if the dependency isn't available
on your platform on a system location, or you wish to use
a non-standard location. Typically you need to do this on non-*nix
(Windows, MS-DOS, OS/2) systems for all packages and for a few packages
on *nix which aren't available through official package managers
(f.e. ADS Client).

Note that Harbour is tuned to use 3rd party binary packages in their
default, unmodified &ndash; "vanilla" &ndash; install layout created by their
official/mainstream install kits. If you manually move, rename,
delete or add files under the 3rd party packages' root directory,
the default Harbour build process (especially Windows implib
generation) might not work as expected.

You can set these environment variables before starting
the build. Make sure to adjust them to your own directories:

    HB_WITH_CURSES= (on *nix systems and DJGPP, where it's autodetected)
    HB_WITH_GPM= (on Linux only)
    HB_WITH_JPEG=C:\jpeglib (defaults to locally hosted version if not found)
    HB_WITH_PCRE=C:\pcre (defaults to locally hosted version if not found)
    HB_WITH_PNG=C:\libpng (defaults to locally hosted version if not found)
    HB_WITH_SLANG= (on *nix systems)
    HB_WITH_TIFF=C:\libtiff (defaults to locally hosted version if not found)
    HB_WITH_WATT= (on MS-DOS systems)
    HB_WITH_X11= (on *nix systems)
    HB_WITH_ZLIB=C:\zlib (defaults to locally hosted version if not found)

To explicitly disable any given components, use the value `no`.
This may be useful to avoid autodetection of installed packages
on *nix systems. You may also use the value `local` to force using the
locally hosted copy (inside Harbour source repository) of these packages,
where applicable. `nolocal` will explicitly disable using locally hosted
copy.

See contrib-specific dependencies and build notes in the projects'
.hbp file and find occasional link notes inside their .hbc files.


> NOTES:
>
>    * you need to use native path format to your shell/OS
>    * spaces in directory names aren't supported
>      (you *can* use 8.3 name alias on Windows platform, though)
>    * don't put directory names inside double quotes
>    * use absolute paths

## Darwin (OS X)

1. Install [Homebrew](http://brew.sh/)
2. Install packages:

        $ brew install pcre slang
        $ brew install upx uncrustify ack optipng jpegoptim

3. Install [X11](https://xquartz.macosforge.org/landing/) (optional, for gtxwc)


## Linux (.deb based distros: Debian, Ubuntu)

You'll need these base packages to build/package/test/use Harbour:

    $ sudo apt-get install bash git gcc binutils fakeroot debhelper valgrind upx uncrustify

You'll need these packages to compile certain optional Harbour features:

      for gtcrs terminal lib:    $ sudo apt-get install libncurses-dev
      for gtsln terminal lib:    $ sudo apt-get install libslang2-dev OR
                                 $ sudo apt-get install libslang1-dev
      for gtxwc terminal lib:    $ sudo apt-get install libx11-dev
      for console mouse support: $ sudo apt-get install libgpm-dev OR
                                 $ sudo apt-get install libgpmg1-dev

Optional, to override locally hosted sources:

      for zlib support:          $ sudo apt-get install zlib1g-dev
      for pcre (regex) support:  $ sudo apt-get install libpcre3-dev

## Linux (.rpm based distros: openSUSE, Fedora, CentOS, Mandriva)

You'll need these base packages to build/package/test/use Harbour:

    bash git gcc make glibc-devel rpm valgrind upx uncrustify

You'll need these packages to compile certain optional Harbour features:

      for gtcrs terminal lib:    ncurses-devel ncurses
      for gtsln terminal lib:    slang-devel slang
      for gtxwc terminal lib:    xorg-x11-devel
                                 XFree86-devel
      for console mouse support: gpm-devel OR
                                 gpm

> NOTES:
>
>    * You can use following commands on different distros to install packages:
>
>         openSUSE:       $ sudo zypper install <pkg>
>         Fedora, CentOS: $ sudo yum install <pkg>
>         Mandriva:       $ sudo urpmi <pkg>
>
>   * Check [this](http://distrowatch.com/dwres.php?resource=package-management) for more
>   * On openSUSE, if you want to build 32-bit Harbour on a 64-bit host, install
>       above packages with `-32bit` appended to their names, f.e. `slang-devel-32bit`

## OpenSolaris

    $ pkg install SUNWgit SUNWgcc SUNWgmake

## FreeBSD

If you want to use the gtsln library instead of gtstd or gtcrs,
then you also need to install libslang. If you installed the ports
collection, then all you need to do to install libslang is to run
the following commands, which may require that you run su first to
get the correct permissions:

    $ cd /usr/ports/devel/libslang
    $ make
    $ make install
    $ make clean


# Build Options

You can fine-tune Harbour builds with below listed
environment variables. You can add most of these via the
GNU Make command-line also, using `make VARNAME=value` syntax.
All of these settings are optional and all settings are case
sensitive.

## General

   - `HB_INSTALL_PREFIX`

     Target root directory to install Harbour files.
     On \*nix systems the default is set to `/usr/local/`
     or `$(PREFIX)` if specified, and
     `/usr/local/harbour-<arch>-<comp>` for cross-builds.
     It's always set to `./pkg/<arch>/<comp>` when
     `HB_BUILD_PKG` is set to `yes`. On non-*nix systems,
     you must set it to a valid directory when using
     ``install``. Use absolute paths only. For a peace of
     mind, avoid using spaces and quotes in the name.
     You have to use path format native to your shell.
     F.e. to specify `C:\dir` on Windows, with Cygwin
     you should use `/cygdrive/c/dir`, with MSYS `/c/dir`.

   - `HB_USER_PRGFLAGS`        User Harbour compiler options
   - `HB_USER_CFLAGS`          User C compiler options
   - `HB_USER_RESFLAGS`        User resource compiler options (on win, wce, os2)
   - `HB_USER_LDFLAGS`         User linker options for executables
   - `HB_USER_AFLAGS`          User linker options for libraries
   - `HB_USER_DFLAGS`          User linker options for dynamic libraries

   Set these only if autodetection doesn't suit your purpose:

   - `HB_PLATFORM`             Override platform autodetection
   - `HB_COMPILER`             Override C compiler autodetection

     See this for possible values:
     [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)
     See also: `HB_CC*` settings.

## Special

   - `HB_BUILD_NAME=[<name>]`

     Create named build. This allows to keep multiple builds in parallel for any
     given platform/compiler. F.e. debug / release.

     > In current implementation it's appended to compiler directory name, so
     > all filesystem/platform name rules and limits apply. (Back)slashes will be
     > stripped from the name though.

   - `HB_BUILD_PKG=yes`

     Create release package. Default: `no`
     Requires `clean install` in root source dir.

   - `HB_BUILD_DYN=no`

     Create Harbour dynamic libraries. Default: `yes`

   - `HB_BUILD_CONTRIB_DYN=yes`

     Create contrib dynamic libraries. Default: `no`,
     except Windows platform, where it's `yes`.

   - `HB_BUILD_SHARED=yes`

     Create Harbour executables in shared mode.
     Default: `yes` when `HB_INSTALL_PREFIX` points
     to a *nix system location, otherwise `no`.

   - `HB_BUILD_DEBUG=yes`

     Create a debug build. Default: `no`

   - `HB_BUILD_STRIP=[all|bin|lib|no]`

     Strip symbols and debug information from binaries.
     Default: `no`

   - `HB_BUILD_OPTIM=no`

     Enable C compiler optimizations. Default: `yes`

   - `HB_BUILD_MODE=[cpp|c]`

     Change default build mode to C++ or C.
     Default: `c`, except for msvc* compilers, where it's `cpp`.

   - `HB_BUILD_PARTS=[all|compiler|lib]`

     Build only specific part of Harbour.

   - `HB_BUILD_NOGPLLIB=yes`

     Disable components dependent on GPL 3rd party code,
     to allow Harbour for commercial (closed source)
     projects. Default: `no`

   - `HB_BUILD_3RDEXT=no`

     Enable autodetection of 3rd party components
     on default system locations. Default: `yes`

   - `HB_BUILD_CONTRIBS=no [<l>]`

     Don't build any, or space separated `<l>` list of,
     contrib packages. Please note it won't prevent
     building packages which are dependencies of
     other &ndash; enabled &ndash; packages.

   - `HB_BUILD_CONTRIBS=[<l>]`

     Build space separated `<l>` list of contrib
     libraries. Build all if left empty (default).

   - `HB_BUILD_ADDONS=<l>`

     Build space separated <l> list of additional .hbp
     projects.

   - `HB_COMPILER_VER=[<ver>]`

     Set C compiler version. This is used with win/msvc,
     win/mingw and cygwin/gcc targets currently.
     `<ver>` format:

         <15><0>[<0>] = <major><minor>[.<revision>]

     Default: filled by compiler autodetection or empty

   - `HB_USER_LIBS=[<list>]`

     Add space separated `<list>` of libs to link process.
     Lib names should be without extension and path.
     You only need this in special cases, like CodeGuard
     build with win/bcc.

   - `HB_INSTALL_IMPLIB=no`

     Copy import libraries created for external .dll
     dependencies to the library install directory in
     ``install`` build phase. Default: `yes`
     For Windows/OS/2 targets only. Please note
     that this feature doesn't work with all possible
     binary distributions of 3rd party packages.
     We test only the official/mainstream ones. Also
     note that the generated implibs will require .dlls
     compatible with the ones used at build time.

   - `HB_INSTALL_3RDDYN=yes`

     Copy dynamic libraries of external .dll dependencies
     to the dynamic library directory in ``install`` build
     phase. Default: no

   - `HB_SRC_ROOTPATH=<dir>`

     When using GNU Make older than 3.81, you shall set
     the root directory of Harbour source tree as an
     absolute path. If not set, some build functionality
     may fail, like detection of 3rd party packages with
     locally hosted sources.
     With newer make versions, this variable is ignored.

   - `HB_REBUILD_EXTERN=yes`

     Rebuild extern headers. It is meant for developers
     doing Harbour code modifications and releases.
     Default: no

   - `HB_REBUILD_PARSER=yes`

     Rebuild language parser sources. Typically
     you only need this if your are Harbour core
     developer modifying grammar rules (.y).
     Requires GNU Bison 1.28 or upper in PATH.
     Default: no

   - `HB_CCPATH=[<dir>/]`

     Used with non-*nix gcc family compilers (and
     sunpro) to specify path to compiler/linker/archive
     tool to help them run from *nix hosts as cross-build
     tools. Ending slash must be added.

   - `HB_CCPREFIX=[<prefix>]`

     Used with gcc compiler family to specify
     compiler/linker/archive tool name prefix.

   - `HB_CCSUFFIX=[<suffix>]`

     Used with gcc compiler family to specify
     compiler/linker tool name suffix &ndash; usually
     version number.

## Cross-builds

You can build Harbour for target platforms different than host
platform. F.e. you can create Windows build on *nix systems, Linux
builds on Windows systems, etc. It's also possible to build targets
for different than host CPU architectures. F.e. you can create
Windows 64-bit build on 32-bit Windows platform, or Linux x86-64
build on x86 hosts, or Linux MIPS build on x86 host, etc.

Point this envvar to the directory where native Harbour executables
for your host platform can be found:

      HB_HOST_BIN=<harbour_native_build_dir>\bin

If you leave this value empty, the make system will try to autodetect it,
so in practice all you have to do is to create a native build first (no
``install`` required), then create the cross-build. If you set this value
manually, it may be useful to know that harbour, hbpp and hbmk2
executables are required for a cross-build process to succeed.


# Build Examples

## on Windows 32-bit hosts

> NOTES:
>
> - All code below should be copied to batch files or typed at command
>   line.
> - Naturally, you'll need to adapt dirs to valid ones on your system.
>   Don't use spaces in dirs.
> - You can use additional `clean`, `install` or `clean install`
>   make parameters depending on what you want to do.
> - To redirect all output to a log file, append this after the make
>   command: `> log.txt 2>&1`

```batchfile
rem clang (alpha)
call "%ProgramFiles%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"
set PATH="%ProgramFiles%\LLVM 3.4.svn";%PATH%
win-make
```

```batchfile
rem MSVC 2013
call "%ProgramFiles%\Microsoft Visual Studio 12.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2013 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem MSVC 2012
call "%ProgramFiles%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2012 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem MSVC 2010 and Windows SDK 7.1
call "%ProgramFiles%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2010 (Professional or above) and Windows SDK 7.1 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem Windows SDK 7
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
win-make
```

```batchfile
rem Windows SDK 7 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\bin\vcvarsx86_amd64.bat"
win-make
```

```batchfile
rem MSVC 2008 + SDK
set WindowsSDKDir=%ProgramFiles%\Microsoft SDKs\Windows\v6.0A\
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2008
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2008 (Standard or above) for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem MSVC 2008 (Team Suite) for Windows IA-64 Itanium
rem (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_ia64
win-make
```

```batchfile
rem MSVC 2008 for Windows CE ARM
rem (requires preceding build for native target)
set INCLUDE=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\include;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Include\Armv4i
set LIB=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\lib\armv4i;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Lib\ARMV4I
set PATH=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\bin\x86_arm;%ProgramFiles%\Microsoft Visual Studio 9.0\Common7\IDE;%PATH%
win-make
```

```batchfile
rem MSVC 2005
call "%ProgramFiles%\Microsoft Visual Studio 8\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2005 for Windows CE ARM
rem (requires preceding build for native target)
set INCLUDE=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\include;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Include\Armv4i
set LIB=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\lib\armv4i;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Lib\ARMV4I
set PATH=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\bin\x86_arm;%ProgramFiles%\Microsoft Visual Studio 8\Common7\IDE;%PATH%
win-make
```

```batchfile
rem MSVC .NET 2003 (untested)
call "%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7\vcvarsall.bat"
win-make
```

```batchfile
rem MinGW GCC
set PATH=C:\mingw\bin;%PATH%
win-make
```

```batchfile
rem MinGW GCC using MSYS shell
set PATH=C:\msys\1.0.11\bin;C:\mingw\bin;%PATH%
sh -c make
```

```batchfile
rem MinGW GCC for Windows x86-64
rem (requires preceding build for native target)
set PATH=C:\mingw64\bin;%PATH%
win-make
```

```batchfile
rem MinGW GCC for Windows CE ARM
rem (requires Cygwin + preceding build for native target)
set PATH=C:\mingwce\opt\mingw32ce\bin;C:\cygwin\bin;%PATH%
rem optional:
set CYGWIN=nodosfilewarning
win-make
```

```batchfile
rem Intel(R) C++
call "%ProgramFiles%\Intel\Compiler\C++\10.1.014\IA32\Bin\iclvars.bat"
win-make
```

```batchfile
rem Intel(R) C++ for Windows IA-64 Itanium
rem (requires preceding build for native target)
call "%ProgramFiles%\Intel\Compiler\C++\10.1.025\Itanium\Bin\iclvars.bat"
win-make
```

```batchfile
rem Borland C++ 5.5.1
set PATH=C:\Borland\BCC55\Bin;%PATH%
win-make
```

```batchfile
rem Pelles C
set PATH=%ProgramFiles%\PellesC\Bin;%PATH%
set INCLUDE=%ProgramFiles%\PellesC\Include;%ProgramFiles%\PellesC\Include\Win;%INCLUDE%
set LIB=%ProgramFiles%\PellesC\Lib;%ProgramFiles%\PellesC\Lib\Win;%LIB%
win-make
```

```batchfile
rem Pelles C for Windows x86-64
rem (requires preceding build for native target)
set PATH=%ProgramFiles%\PellesC\Bin;%PATH%
set INCLUDE=%ProgramFiles%\PellesC\Include;%ProgramFiles%\PellesC\Include\Win;%INCLUDE%
set LIB=%ProgramFiles%\PellesC\Lib;%ProgramFiles%\PellesC\Lib\Win64;%LIB%
win-make
```

```batchfile
rem Pelles C for Windows CE ARM
rem (requires preceding build for native target)
set PATH=%ProgramFiles%\PellesC\Bin;%PATH%
set INCLUDE=%ProgramFiles%\PellesC\Include\WinCE;%ProgramFiles%\PellesC\Include;%INCLUDE%
set LIB=%ProgramFiles%\PellesC\Lib;%ProgramFiles%\PellesC\Lib\WinCE;%LIB%
win-make
```

```batchfile
rem Delorie GNU C for MS-DOS (on Intel 32-bit Windows hosts only)
set DJGPP=C:\djgpp\djgpp.env
set PATH=C:\djgpp\bin;%PATH%
win-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
win-make
```

```batchfile
rem Open Watcom C/C++ for MS-DOS
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H
win-make
```

```batchfile
rem Open Watcom C/C++ for OS/2
rem (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2
set BEGINLIBPATH=%WATCOM%\BINP\DLL
win-make
```

```batchfile
rem Open Watcom C/C++ for Linux
rem (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\LH
win-make
```

```batchfile
rem VxWorks GCC x86
rem (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
win-make
```

```batchfile
rem VxWorks GCC ARM
rem (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
set HB_CPU=arm
set HB_BUILD_NAME=arm
win-make
```

```batchfile
rem VxWorks Wind River Compiler x86
rem (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=diab
win-make
```

```batchfile
rem Symbian OS
rem (requires preceding build for Windows target)
set PATH=C:\Symbian\CSL Arm Toolchain\bin;%PATH%
set HB_PLATFORM=symbian
set HB_COMPILER=gcc
set HB_CCPREFIX=arm-none-symbianelf-
set HB_USER_CFLAGS=-IC:\Symbian\SDK\S60\devices\S60_5th_Edition_SDK_v1.0\epoc32\include\stdapis -IC:\Symbian\SDK\S60\devices\S60_5th_Edition_SDK_v1.0\epoc32\include -D__GCC32__ -D__SYMBIAN32__
win-make
```

```batchfile
rem Cygwin GCC using Cygwin shell
set PATH=C:\cygwin\bin
sh -c make
```

## on Windows x86-64 (64-bit) hosts

Same as 32-bit Windows, with the difference that you'll have to change
`%ProgramFiles%` to `%ProgramFiles(x86)%` for 32-bit and mixed tools,
that you can build for both x86 and x86_64 without building a native
target first, and potential differences with some compilers in using
native binaries if they are available.

```batchfile
rem MinGW GCC for Windows x86-64
set PATH=C:\mingw64\bin;%PATH%
win-make
```

```batchfile
rem clang (alpha)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"
set PATH="%ProgramFiles(x86)%\LLVM 3.4.svn";%PATH%
win-make
```

```batchfile
rem MSVC 2013 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2013 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem MSVC 2012 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2012 for Windows x86-64
rem (requires preceding build for native target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
rem MSVC 2010 and Windows SDK 7.1 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2010 (Professional or above) and Windows SDK 7.1 for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64
win-make
```

```batchfile
rem Windows SDK 7 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
win-make
```

```batchfile
rem Windows SDK 7 for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars64.bat"
win-make
```

```batchfile
rem MSVC 2008 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
rem MSVC 2008 (Standard or above) for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" amd64
win-make
```

```batchfile
rem MSVC 2008 (Team Suite) for Windows IA-64 Itanium
rem (requires preceding build for native target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_ia64
win-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT64;%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
win-make
```

## on MS-DOS hosts

```batchfile
rem Delorie GNU C
set DJGPP=C:\djgpp\djgpp.env
set PATH=C:\djgpp\bin;%PATH%
dos-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H
dos-make
```

## on OS/2 hosts

```batchfile
rem GCC 3.3.4 and GCC 3.3.5
C:\usr\bin\gccenv.cmd
os2-make
```

```batchfile
rem GCC 4.x
C:\usr\local433\gcc440.cmd
set HB_COMPILER=gccomf
os2-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINP;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2
set BEGINLIBPATH=%WATCOM%\BINP\DLL
os2-make
```

## on Linux hosts

```bash
# Open Watcom C/C++ for OS/2
# (requires preceding build for Linux target)
export WATCOM="/opt/lng/watcom"
export PATH="${WATCOM}/binl:$PATH"
export INCLUDE="${WATCOM}/h:${WATCOM}/h/os2"
export HB_BUILD_3RDEXT=no
make
```

```bash
# Borland C++ 5.5.1
export PATH=~/.wine/drive_c/Borland/BCC55/Bin:$PATH
export HB_PLATFORM=win
export HB_COMPILER=bcc
export HB_BUILD_3RDEXT=no
make
```

## on Darwin (OS X) hosts

```bash
# To create "Universal" binaries, compatible with pre-Snow Leopard PowerPC and Intel systems
export HB_USER_LDFLAGS="-arch i386 -arch ppc"
export HB_USER_CFLAGS="$HB_USER_LDFLAGS"
export HB_COMPILER=gcc
make
```

## on *nix hosts in general

```bash
[g]make
```

```bash
# MinGW GCC for Windows x86
[g]make HB_PLATFORM=win
```

```bash
# MinGW GCC for Windows CE ARM
[g]make HB_PLATFORM=wce
```


# Build Your Own Harbour App

For all platforms you'll need two things:

* Harbour binaries

    Either a Harbour binary distribution or a local Harbour
    build will be okay. If you're reading this text, it's
    likely you have one of these already.

* Supported ANSI C compiler

    Your compiler of choice has to be placed in the PATH &ndash; and
    configured appropriately according to instructions.
    If you use official Harbour binary distribution on Windows,
    you already have MinGW compiler embedded in the installation,
    which will automatically be used, so you don't have to
    make any extra steps here.

Use hbmk2 to build your app from source. It's recommended to put
it in the PATH (f.e. by using `set PATH=C:\harbour\bin;%PATH%` on Windows).

See hbmk2 [documentation, with examples](utils/hbmk2/doc/hbmk2.en.md).


# Debugging Options

## Tracing

Build Harbour with:

    HB_BUILD_DEBUG=yes

Run app with:

    HB_TR_LEVEL=debug
    # to override default stderr output:
    HB_TR_OUTPUT=<filename>
    # to enable additional system specific logging output,
    # OutputDebugString() on Windows, syslog() on *nix systems:
    HB_TR_SYSOUT=yes

## Memory statistics/tracking

Build Harbour with:

    HB_USER_CFLAGS=-DHB_FM_STATISTICS

## Valgrind (on linux and darwin targets)

Build Harbour with:

    HB_BUILD_DEBUG=yes

Build app with:

    $ hbmk2 myapp -debug

Run app with:

    $ valgrind --tool=memcheck --leak-check=yes --num-callers=16 -v ./myapp 2> myapp.log

## CodeGuard (on win/bcc target only)

Build Harbour with:

    HB_USER_CFLAGS=-vG
    HB_USER_LIBS=cg32

## Harbour Debugger

Build app with:

    $ hbmk2 myapp -b -run

or run script with:

    $ hbrun myapp --hb:debug

Press `<Alt+D>` in the app.


# Supported Platforms and C Compilers

## You can override target platform autodetection with these `HB_PLATFORM` values:

* linux    - Linux
* darwin   - OS X
* bsd      - FreeBSD / OpenBSD / NetBSD / DragonFly BSD / *BSD
* beos     - BeOS / Haiku
* hpux     - HP-UX
* sunos    - Sun Solaris / OpenSolaris
* qnx      - QNX
* android  - Android
* vxworks  - VxWorks
* symbian  - Symbian OS (experimental)
* minix    - Minix 3 (tested on 3.2.1; earlier releases won't work)
* aix      - IBM AIX
* win      - MS Windows (all flavors)
             (see [External links](#external-links) for Win9x requirements)
* wce      - MS Windows CE
* dos      - MS-DOS (32-bit protected mode only)
             (MS-DOS compatible systems also work, like dosemu)
* os2      - OS/2 Warp 4 / eComStation

## You can override C compiler autodetection with these `HB_COMPILER` values:

### linux
* gcc      - GNU C
* clang    - Clang
* watcom   - Open Watcom C/C++
* icc      - Intel(R) C/C++
* sunpro   - Sun Studio C/C++
* open64   - Open64 C/C++

### darwin
* gcc      - GNU C
* clang    - Clang
* icc      - Intel(R) C/C++

### bsd
* gcc      - GNU C
* clang    - Clang
* pcc      - Portable C Compiler (experimental)

### hpux
* gcc      - GNU C

### beos
* gcc      - GNU C

### qnx
* gcc      - GNU C

### android
* gcc      - GNU C x86
* gccarm   - GNU C ARM

### vxworks
* gcc      - GNU C
* diab     - Wind River Compiler

### symbian
* gcc      - GNU C

### minix
* clang    - Clang
* gcc      - GNU C

### aix
* gcc      - GNU C

### cygwin
* gcc      - GNU C

### sunos
* gcc      - GNU C
* sunpro   - Sun Studio C/C++

### win
* mingw    - MinGW GNU C 3.4.2 and above
* mingw64  - MinGW GNU C x86-64
* msvc     - Microsoft Visual C++
* msvc64   - Microsoft Visual C++ x86-64
* msvcia64 - Microsoft Visual C++ IA-64 (Itanium)

### win (partial support, some features may be missing)
* clang    - Clang
* watcom   - Open Watcom C/C++
* bcc      - Borland/CodeGear/Embarcadero C++ 4.x and above
* bcc64    - Embarcadero C++ 6.5 and above
* icc      - Intel(R) C/C++
* iccia64  - Intel(R) C/C++ IA-64 (Itanium)
* pocc     - Pelles C 4.5 and above
* pocc64   - Pelles C x86-64 5.0 and above
* xcc      - Pelles C for xhb

### wce
* mingw    - MinGW GNU C x86
* mingwarm - MinGW GNU C ARM (CEGCC 0.55 and above)
* msvcarm  - Microsoft Visual C++ ARM
* poccarm  - Pelles C ARM 5.0 and above

### dos
* djgpp    - Delorie GNU C
* watcom   - Open Watcom C/C++

### os2
* gcc      - EMX GNU C 3.3.5 or lower
* gccomf   - EMX GNU C 3.3.5 or upper
* watcom   - Open Watcom C/C++


# Platform Matrix

 &nbsp;| host<br />platform | target<br />platform/compiler | target cpu
 :---- | :------- | :---------------- | :---------------------------------------
       | win      | win/bcc           | x86
       | win      | win/bcc64         | x86-64
       | win      | win/clang         | x86
       | win      | win/gcc           | x86
       | win      | win/icc           | x86
       | win      | win/icc64         | x86-64 (not supported yet)
       | win      | win/iccia64       | ia64
       | win      | win/mingw         | x86
       | win      | win/mingw64       | x86-64
       | win      | win/msvc          | x86
       | win      | win/msvc64        | x86-64
       | win      | win/msvcia64      | ia64
       | win      | win/pocc          | x86
       | win      | win/pocc64        | x86-64
       | win      | win/watcom        | x86
       | win      | win/xcc           | x86
     x | win      | wce/mingwarm      | arm
     x | win      | wce/mingw         | x86   (not fully supported yet)
     x | win      | wce/poccarm       | arm
     x | win      | wce/msvcarm       | arm
     x | win      | wce/msvcmips      | mips  (not supported yet)
     x | win      | wce/msvcsh        | sh    (not supported yet)
     x | win      | wce/msvc          | x86   (not supported yet)
     x | win      | dos/djgpp         | x86   (on Windows x86 hosts only)
     x | win      | dos/watcom        | x86
     x | win      | os2/watcom        | x86
     x | win      | linux/watcom      | x86
     x | win      | android/gcc       | x86
     x | win      | android/gccarm    | arm
     x | win      | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | win      | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
     x | win      | symbian/gcc       | arm
     x | win      | cygwin/gcc        | x86
       | dos      | dos/djgpp         | x86
       | dos      | dos/watcom        | x86
     x | dos      | win/watcom        | x86
     x | dos      | os2/watcom        | x86
     x | dos      | linux/watcom      | x86
       | os2      | os2/gcc           | x86
       | os2      | os2/watcom        | x86
     x | os2      | win/watcom        | x86
     x | os2      | dos/watcom        | x86
     x | os2      | linux/watcom      | x86
       | linux    | linux/gcc         | (CPU cross-builds possible)
       | linux    | linux/clang       | (CPU cross-builds possible)
       | linux    | linux/icc         | (CPU cross-builds possible: x86, x86-64, ia64)
       | linux    | linux/sunpro      | (CPU cross-builds possible: x86, x86-64)
       | linux    | linux/open64      | (CPU cross-builds possible: x86-64, ia64, ...)
     x | linux    | wce/mingwarm      | arm
     x | linux    | wce/mingw         | x86
     x | linux    | win/mingw         | x86
     x | linux    | win/mingw64       | x86-64
     x | linux    | win/watcom        | x86
     x | linux    | win/bcc           | x86 (requires WINE)
     x | linux    | win/bcc64         | x86-64 (requires WINE)
     x | linux    | os2/watcom        | x86
     x | linux    | dos/watcom        | x86
     x | linux    | dos/djgpp         | x86
     x | linux    | android/gcc       | x86
     x | linux    | android/gccarm    | arm
     x | linux    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | linux    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
       | bsd      | bsd/gcc           | (CPU cross-builds possible)
       | bsd      | bsd/clang         | (CPU cross-builds possible)
       | bsd      | bsd/pcc           | (experimental)
     x | bsd      | wce/mingwarm      | arm
     x | bsd      | wce/mingw         | x86
     x | bsd      | win/mingw         | x86
     x | bsd      | dos/djgpp         | x86
       | darwin   | darwin/clang      | (CPU cross-builds possible: x86, x86-64, unibin)
       | darwin   | darwin/gcc        | (CPU cross-builds possible: x86, x86-64, ppc, ppc64, unibin)
       | darwin   | darwin/icc        | (CPU cross-builds possible: x86, x86-64)
     x | darwin   | wce/mingwarm      | arm
     x | darwin   | wce/mingw         | x86
     x | darwin   | win/mingw         | x86
     x | darwin   | win/mingw64       | x86-64
     x | darwin   | dos/djgpp         | x86
     x | darwin   | android/gcc       | x86
     x | darwin   | android/gccarm    | arm
       | hpux     | hpux/gcc          | (CPU cross-builds possible)
       | qnx      | qnx/gcc           | (CPU cross-builds possible - no tested)
       | beos     | beos/gcc          | x86
     x | hpux     | wce/mingwarm      | arm
     x | hpux     | wce/mingw         | x86
     x | hpux     | win/mingw         | x86
     x | hpux     | dos/djgpp         | x86
       | minix    | minix/clang       | x86
       | minix    | minix/gcc         | x86
       | aix      | aix/gcc           | (CPU cross-builds possible: ppc, ppc64)
       | sunos    | sunos/gcc         | (CPU cross-builds possible)
       | sunos    | sunos/sunpro      | (CPU cross-builds possible: x86, x86-64, sparc32, sparc64)
     x | sunos    | wce/mingwarm      | arm
     x | sunos    | wce/mingw         | x86
     x | sunos    | win/mingw         | x86
     x | sunos    | dos/djgpp         | x86
     x | sunos    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | sunos    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)

> Leading **x** marks cross-platform scenarios.

Supported shells per host platforms:

* win  / NT shell (cmd.exe)
* win  / POSIX shell (MSYS or Cygwin sh.exe)
* win  / MS-DOS shell (command.com)
* dos  / MS-DOS shell (command.com)
* dos  / POSIX shell (bash.exe)
* os/2 / OS/2 shell (cmd.exe)
* os/2 / POSIX shell (bash.exe)
* *nix / POSIX shell


# External links

* C/C++ Compilers/Shells:

     * MinGW/MinGW-64 [win, *nix, free, open-source]
        * <https://sourceforge.net/projects/mingwbuilds/> (unofficial, recommended)
        * <http://tdm-gcc.tdragon.net/>, <https://sourceforge.net/projects/tdm-gcc/> (unofficial) [NOTE: 4.5.1 is broken, 4.5.2, 4.6.1 x86 OK, for x86_64 use the official release]
        * <http://mingw.org/>, <https://sourceforge.net/projects/mingw/> (official 32-bit, MSYS home, broken as of 4.5.0-1)
        * <http://mingw-w64.sourceforge.net/>, <https://sourceforge.net/projects/mingw-w64/> (official 64-bit, MSYS home)
        * <http://nuwen.net/mingw.html> (unofficial)
     * MinGW CEGCC [win, *nix, free, open-source]
        * <http://cegcc.sourceforge.net/>
     * Cygwin [win, free, open-source]
        * <http://cygwin.com/>
     * Clang [multiplatform, free, open-source]
        * <http://llvm.org/builds/> (Windows snapshots)
     * OS/2 GCC [os2, free, open-source]
        * <http://os2ports.smedley.id.au/index.php?page=gcc-v4-4-x>
     * DJGPP [dos, *nix, free, open-source]
        * <http://www.delorie.com/djgpp/>
     * Open Watcom [win, dos, os2, linux, free, open-source]
        * <https://github.com/open-watcom>, <http://openwatcom.org/>
     * Xcode / Command Line Tools for Xcode [darwin, free, hybrid-source]
        * <https://itunes.apple.com/us/app/xcode/id497799835>
        * <https://developer.apple.com/downloads/>
     * MS Windows SDK [win, free, closed-source]
        * <https://msdn.microsoft.com/en-us/windowsserver/bb980924.aspx>
         (7.0 and above contains compilers for x86, x86_64 and IA-64)
     * MS Windows Mobile SDK [wce, free]
        * <https://www.microsoft.com/en-us/download/details.aspx?id=42>
     * MS Visual Studio Express [win, free, closed-source]
        * <https://www.visualstudio.com/en-US/products/visual-studio-express-vs>
     * MS Visual Studio [win, commercial, closed-source]
        * <https://www.visualstudio.com/en-us>
     * Pelles C [win, wce, free, closed-source]
        * <http://smorgasbordet.com/pellesc/>
     * Borland/CodeGear/Embarcadero Compiler [win, free, closed-source]
        * <https://downloads.embarcadero.com/free/c_builder>
     * Intel Compiler [win, linux, darwin, commercial, closed-source]
        * <http://software.intel.com/en-us/intel-compilers/>

* Libraries:

     * HB_WITH_JPEG - jpeglib [multiplatform, free, open-source]
        * <http://ijg.org/>
     * HB_WITH_PCRE - Perl Compatible Regular Expressions [multiplatform, free, open-source]
        * <http://pcre.org/>
     * HB_WITH_PNG - libpng [multiplatform, free, open-source]
        * <http://libpng.org/pub/png/libpng.html>
     * HB_WITH_TIFF - libtiff [multiplatform, free, open-source]
        * <http://remotesensing.org/libtiff/>
     * HB_WITH_WATT - Watt-32 (TCP/IP sockets) [dos, free, open-source]
        * <http://home.broadpark.no/~gvanem/>
     * HB_WITH_ZLIB - zlib [multiplatform, free, open-source]
        * <http://zlib.net/>

     * Windows 95 Winsock2 [win, free, closed-source]
        * Information: Microsoft KB182108 "Availability of Windows Sockets 2.0 for Windows 95"
        * Search for `w95ws2setup.exe`: <https://duckduckgo.com/?q=w95ws2setup.exe>
            (required for Win95 support to run applications built with Harbour)
     * Windows UNICOWS .dll [win, free, closed-source]
        * <https://www.microsoft.com/en-us/download/details.aspx?id=4237>
            (required for Win9x/ME support to run applications built with Harbour in UNICODE mode)
     * Windows UNICOWS runtime/import library [win, free, open-source]
        * <http://libunicows.sourceforge.net/>
            (required for Win9x/ME support at application built-time)

* Tools:

     * Git (1.7 or upper) [multiplatform, free, open-source]
        * <http://git-scm.com/>
        * <http://msysgit.github.io> (Windows binaries)
     * GitHub Client [multiplatform, free]
        * <https://windows.github.com/>
        * <https://mac.github.com/>
     * Travis CI [continuous integration, web service, free]
        * <https://travis-ci.org/>
     * Read the Docs [online documentation creator, web service, free]
        * <https://readthedocs.org/>
     * GNU Bison (grammer paser generator) [multiplatform, free, open-source]
        * Windows binary:
           * <http://gnuwin32.sourceforge.net/packages/bison.htm>
               (not verified with current Harbour version)
     * Valgrind (dynamic executable analysis tool) [linux, darwin-x86, free, open-source]
        * <http://valgrind.org/>
     * ack (programmer grep) [multiplatform, free, open-source]
        * <http://beyondgrep.com/>
     * Uncrustify (source formatter) [multiplatform, free, open-source]
        * <http://uncrustify.sourceforge.net/>
     * UPX (executable compressor) [win, dos, *nix, free, open-source]
        * <http://upx.sourceforge.net/>
     * Nullsoft Installer [win, free, open-source]
        * <http://nsis.sourceforge.net/>
     * 7-Zip [multiplatform, free, open-source]
        * <http://7-zip.org/>
     * Info-ZIP [multiplatform, free, open-source]
        * <http://info-zip.org/>
     * bzip2 [multiplatform, free, open-source]
         Windows binary:
           * <http://bzip.org/downloads.html>
     * Chocolatey (Windows package manager) [free, open-source]
        * <https://chocolatey.org/>
     * GNU Make

        Windows binary + source:

         * <https://sourceforge.net/projects/mingw/files/MinGW/Extension/make/>
         * <https://downloads.sourceforge.net/mingw/MinGW/Extension/make/make-3.82-mingw32/make-3.82-5-mingw32-bin.tar.lzma>
         * <https://downloads.sourceforge.net/mingw/MinGW/Extension/make/make-3.82-mingw32/make-3.82-5-mingw32-src.tar.lzma>
         * `cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/make co make`
          <br />(included in Harbour as [win-make.exe](win-make.exe))

        MS-DOS binary + source:

         * <http://www.delorie.com/pub/djgpp/beta/v2gnu/>
         * <http://www.delorie.com/pub/djgpp/beta/v2gnu/mak381b.zip>
         * <http://www.delorie.com/pub/djgpp/beta/v2gnu/mak381s.zip>
          <br />(included in Harbour as [dos-make.exe](dos-make.exe))

        OS/2 binary + source (3.81r3 or upper):

         * <http://hobbes.nmsu.edu/download/pub/os2/dev/util/make-3.81-r3-bin-static.zip>
         * <http://hobbes.nmsu.edu/h-browse.php?dir=/pub/os2/dev/util>
          <br />(included in Harbour as [os2-make.exe](os2-make.exe))
     * GNU core utils (mkdir, rm, cp, echo)

        MS-DOS binary + source:

         * <http://www.delorie.com/pub/djgpp/current/v2gnu/>
         * <http://www.delorie.com/pub/djgpp/current/v2gnu/fil41b.zip>
         * <http://www.delorie.com/pub/djgpp/current/v2gnu/fil41s.zip>
         * <http://www.delorie.com/pub/djgpp/current/v2gnu/shl2011b.zip>
         * <http://www.delorie.com/pub/djgpp/current/v2gnu/shtl208s.zip>
         * <http://www.delorie.com/pub/djgpp/current/v2/djdev203.zip>
         * <http://www.delorie.com/pub/djgpp/current/v2/djlsr203.zip>
          <br />(included in Harbour as [dosmkdir.exe](config/dosmkdir.exe), [dosrm.exe](config/dosrm.exe), [doscp.exe](config/doscp.exe), [dosecho.exe](config/dosecho.exe))

        OS/2 binary:

         * <http://os2ports.smedley.info/index.php?page=build-environment>
         * <http://download.smedley.info/buildenv_20071022.zip>
          <br />(included in Harbour as [os2mkdir.exe](config/os2mkdir.exe), [os2rm.exe](config/os2rm.exe), [os2cp.exe](config/os2cp.exe))

* Documentation:

     * [Netiquette Guidelines](https://tools.ietf.org/html/rfc1855)
     * [Setting up Git](https://help.github.com/articles/set-up-git)
     * [Git book](http://git-scm.com/book) [free]
     * [Git Reference](http://gitref.org)
     * [Git crash course for Subversion users](http://git.or.cz/course/svn.html)
     * Using gettext (.po files)
       * <http://support.transifex.com/customer/portal/articles/913371-po-files>
       * <http://heiner-eichmann.de/autotools/using_gettext.html>
     * [GitHub Guides](https://guides.github.com)
     * [GitHub flavoured Markdown](https://help.github.com/articles/github-flavored-markdown)


# Harbour Links

  * [Homepage - for non-fork/mainstream](http://harbour-project.org/)
  * [Users' Mailing List - for non-fork/mainstream](https://groups.google.com/group/harbour-users/) (English language)
  * [Development Mailing List - for non-fork/mainstream](https://groups.google.com/group/harbour-devel/) (English language)
  * [Source code](https://github.com/vszakats/harbour-core)
  * [Localization](https://www.transifex.com/projects/p/harbour/)
  * [Issues](https://github.com/vszakats/harbour-core/issues)
  * Documents:
     * [hbmk2 documentation](utils/hbmk2/doc/hbmk2.en.md)
     * [hbrun documentation](contrib/hbrun/doc/hbrun.en.md)
     * [ChangeLog](ChangeLog.txt?raw=true)
     * Comparing [Harbour to xHarbour](doc/xhb-diff.txt?raw=true)
     * CA-Cl*pper 5.3 [online documentation](http://x-hacker.org/ng/53guide/)
     * Harbour [online documentation](http://harbour.github.io/doc/)
     * Harbour [internal documents](doc/)
     * [Wikipedia](https://en.wikipedia.org/wiki/Harbour_compiler)
     * [Stack Exchange](https://stackoverflow.com/questions/tagged/clipper)


# Guarantees and Liability

   This document and all other parts of Harbour are distributed in the
   hope they will be useful, but WITHOUT GUARANTEE that they are complete,
   accurate, non-infringing or usable for any purpose whatsoever.
   Contributors are NOT LIABLE for any damages that result from using
   Harbour in any ways. For more legal details, see [COPYING](COPYING.txt).

   If you feel you can make Harbour better: contribute.
   [See how](CONTRIBUTING.md).

   The information this document is subject to change without notice
   and does not represent any future commitment by the participants
   of the project.


---
This document Copyright &copy;&nbsp;2009&ndash;2014 Viktor Szakáts (vszakats.net/harbour)<br />
Licensed under [Creative Commons Attribution-ShareAlike 4.0](https://creativecommons.org/licenses/by-sa/4.0/)<br />
See [COPYING](COPYING.txt).
