Harbour Make \(hbmk2\) 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 1999\-2016, Viktor Szakáts  
<https://github\.com/vszakats/harbour\-core/>  
Magyar \(hu\) fordítás: Copyright &copy; 2009\-2016, Szakáts Viktor  

Használat:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.hbc|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Description:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will auto\-detect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Kapcsolók:  


 - **\-o&lt;outname&gt;** kimeneti fájlnév
 - **\-l&lt;libname&gt;** link with &lt;libname&gt; library\. &lt;libname&gt; should be without path, extension and 'lib' prefix \(unless part of the name\)\. Do not add core Harbour libraries, they are automatically added as needed\. If &lt;libname&gt; starts with a '\-' character, the library will be removed from the list of libraries at link time\.
 - **\-L&lt;libpath&gt;** további keresési útvonal függvénykönyvtárakhoz
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** további keresési útvonal a fejlécekhez
 - **\-static|\-shared** használjon a szerkesztéskor statikus/dinamikus függvénykönyvtárakat
 - **\-gt&lt;name&gt;** link with GT&lt;name&gt; GT driver, can be repeated to link with more GTs\. First one will be the default at run\-time
 - **\-inc\[\-\]** kapcsolja be/ki a lépcsőzetes \(incremental\) üzemmódot \(alapértelmezés: kikapcsolva\)
 - **\-hbexe** create executable \(default\)
 - **\-hblib** statikus függvénykönyvtár létrehozása
 - **\-hbdyn** dinamikus függvénykönyvtár létrehozása \(Harbour VM nélkül\)
 - **\-hbdynvm** dinamikus függvénykönyvtár létrehozása \(Harbour VM\-mel\)
 - **\-strip\[\-\]** strip \(or don't\) debugging \(and other extra\) information from target binary\. They are included by default by certain C compilers, f\.e\.: gcc\*, clang, mingw\*, djgpp\.


 - **\-mt|\-st** többszálú/egyszálú virtuális gép használata
 - **\-gui|\-std|\-cli** create GUI/console/command\-line executable
 - **\-main=&lt;mainfunc&gt;** állítson be alternatív kezdő/belépési függvénynevet
 - **\-request=&lt;func&gt;** force function/procedure to be linked
 - **\-fullstatic** csak statikus függvénykönyvtárak használata
 - **\-pic\[\-\]** create position independent object code \(always enabled in \-hbdyn/\-hbdynvm modes\)
 - **\-\[full|fix\]shared** használjon a szerkesztéskor dinamikus függvénykönyvtárakat közvetlen hivatkozás nélkül/hivtakozással a dinamikus Harbour függvénykönyvtár felé\. \(alapértelmezés: 'fullshared', ha a Harbour telepítve van\) \(fix/full opció csak \*nix rendszereken\)
 - **\-nulrdd\[\-\]** nulrdd használata
 - **\-debug\[\-\]** C fordító debug információ hozzáadása \(vagy sem\)\. Harbour szintű debughoz használja a \-b kapcsolót
 - **\-optim\[\-\]** C fordító optimalizálási szintjét állítja \(alapértelmezés: be\)
 - **\-cpp\[\-\]** force C\+\+/C mode
 - **\-cpp=&lt;value&gt;** select C\+\+ mode\. Allowed values are: def, yes, no
 - **\-c=&lt;value&gt;** select C standard\. Allowed values are: iso90, iso99, iso11, gnu90, gnu99, gnu11
 - **\-cpp=&lt;value&gt;** select C\+\+ mode or standard\. Allowed values are: def, yes, no, iso98, iso11, iso14, gnu98, gnu11, gnu14
 - **\-map\[\-\]** készítsen \(vagy ne\) térkép \(map\) fájlt
 - **\-implib\[\-\]** create \(or not\) an import library \(in \-hbdyn/\-hbexe mode\)\. The name will have a suffix added\.
 - **\-implib=&lt;output&gt;** create import library \(in \-hbdyn/\-hbexe mode\) name to &lt;output&gt; \(default: same as output\)
 - **\-ln=&lt;link&gt;** create symbolic link pointing to &lt;output&gt; \(&lt;link&gt; is considered relative to &lt;output&gt;\)
 - **\-trace\[\-\]** mutassa a végrehajtott parancsokat
 - **\-beep\[\-\]** egyszeri sípolás sikeres végrehajtás esetén, dupla sípolás hiba esetén
 - **\-ignore\[\-\]** külső parancsok hibáinak figyelmen hagyása \(alapértelmezés: ki\)
 - **\-hbcppmm\[\-\]** override standard C\+\+ memory management functions with Harbour ones
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI Windows API usage for C/C\+\+ input files \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** do not use static core Harbour libraries when linking
 - **\-nodefgt\[\-\]** do not link default GTs \(effective in \-static mode\)
 - **\-nolibgrouping\[\-\]** disable library grouping on gcc based compilers
 - **\-nomiscsyslib\[\-\]** do not add extra list of system libraries to default library list
 - **\-traceonly** mutassa a végrehajtandó parancsokat, anélkül hogy végrehajtaná őket
 - **\-warn=&lt;level&gt;** set C compiler warning level  
&lt;level&gt; can be: max, yes, low, no, def \(default: yes\)
 - **\-harden\[\-\]** enable hardening options in C compiler/linker \(default: enabled on Windows, disabled on other systems\)
 - **\-vcsts\[\-\]** set timestamp of output file\(s\) to the last repository commit \(Supported with: Git\)
 - **\-compr=&lt;level&gt;** compress executable/dynamic lib \(needs UPX tool\)  
&lt;level&gt; can be: yes, no, min, high, max
 - **\-run\[\-\]** futtassa/ne az elkészült alkalmazást
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. VCS specific information is added as \_HBMK\_VCS\_&lt;TYPE&gt;\_\*\_ constants, where supported\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-bldhead=&lt;file&gt;** generate \.ch header file with build information, like build sequence number and timestamp\. Generated header will define preprocessor constants \_HBMK\_BUILD\_ID\_ and \_HBMK\_BUILD\_ID\_NUM\_ with sequence number \(incremented on each build\), \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build and \_HBMK\_BUILD\_RANDSTR\_32\_ with a random string of 32 bytes in hexadecimal format
 - **\-haltrev\[\-\]** do not increase revision numbers in \-bldhead= \(\_HBMK\_BUILD\_ID\_\) and \-vcshead= \(\_HBMK\_VCS\_ID\_\) options \(default: do increase\)
 - **\-icon=&lt;file&gt;** set &lt;file&gt; as application icon\. &lt;file&gt; should be a supported format on the target platform \(not supported by some platforms/compilers\)\. On Windows, it is implemented by generating and linking a resource file\.
 - **\-manifest=&lt;file&gt;** embed manifest &lt;file&gt; in executable/dynamic lib \(Windows only\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both auto\-detected\.
 - **\-signpw=&lt;pw&gt;** use &lt;pw&gt; as password when signing executable \(Windows and Darwin only\)
 - **\-signts=&lt;\[std:\]url&gt;** use &lt;url&gt; as trusted timestamp server\. Optional &lt;std&gt; might specify the standard as 'rfc3161' or 'authenticode' \(without quotes\)\. The default is 'rfc3161'\. Empty value resets it to the default: http://timestamp\.globalsign\.com/scripts/timstamp\.dll
 - **\-instfile=&lt;g:file&gt;** add &lt;file&gt; in to the list of files to be copied to path specified by \-instpath option\. &lt;g&gt; is an optional copy group \(case sensitive\), it must be at least two characters long\. In case you do not specify &lt;file&gt;, the list of files in that group will be emptied\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** enable \(or disable\) import library generation for import library sources specified in \-depimplibs= options \(default: yes\)
 - **\-stop\[=&lt;text&gt;\]** álljon meg anélkül hogy bármit csinálna
 - **\-echo=&lt;text&gt;** echo text on screen
 - **\-skip** skip processing the rest of the project file \(filters not supported\)
 - **\-pause** várjon billentyűre sikertelen befejezés esetén\. \(csak alternatív GT használata esetén\)
 - **\-exitstr** show error result as human readable text on exit
 - **\-info** tájékoztató üzenetek bekapcsolása
 - **\-quiet\[\-\]** semmit ne írjon a képernyőre


 - **\-bldf\[\-\]** örököljön minden/semmi \(alapért\.\) kapcsolót a Harbourtól
 - **\-bldf=\[p\]\[c\]\[l\]** örökölje a \.prg/\.c/szerkesztő kapcsolókat \(vagy ne\) a Harbourtól
 - **\-F&lt;framework&gt;** link with &lt;framework&gt; framework \(Darwin only\)
 - **\-prgflag=&lt;f&gt;** kapcsoló továbbítása Harbour fordítónak
 - **\-cflag=&lt;f&gt;** kapcsoló továbbítása C fordítónak
 - **\-resflag=&lt;f&gt;** továbbítsa a kapcsolót az erőforrás fordítónak \(csak Windows\-on\)
 - **\-ldflag=&lt;f&gt;** kapcsoló továbbítása \(alkalmazás\) szerkesztőnek
 - **\-dflag=&lt;f&gt;** kapcsoló továbbítása \(dinamikus függvénykönyvtár\) szerkesztőnek
 - **\-aflag=&lt;f&gt;** kapcsoló továbbítása \(statikus függvénykönyvtár\) szerkesztőnek
 - **\-iflag=&lt;f&gt;** pass single flag to import library creation command
 - **\-signflag=&lt;f&gt;** pass single flag to code sign command
 - **\-runflag=&lt;f&gt;** továbbítsa a kapcsolót a létrehozott alkalmazásnak, \-run kapcsoló használatakor
 - **\-cflag\+=&lt;f&gt;** pass single flag to C compiler overriding C compiler flags added by hbmk2 itself\. Use with caution\.
 - **\-ldflag\+=&lt;f&gt;** pass single raw option to linker \(executable\) after the library list\. Use with caution\.
 - **\-dflag\+=&lt;f&gt;** pass single raw option to linker \(dynamic library\) after the library list\. Use with caution\.
 - **\-3rd=&lt;f&gt;** options/flags reserved for 3rd party tools, always ignored by hbmk2 itself
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alter local environment\. &lt;e&gt; is the name of the environment variable to alter\. &lt;o&gt; can be '=' to set/override, '\-' to delete, '\+' to append to the end of existing value, '\#' to insert to the beginning of existing value\. &lt;v&gt; is the value to set/append/insert\.
 - **\-jobs=&lt;n&gt;** start n compilation threads \(multiprocess platforms only\) \(default: number of processors available or 1 if not detectable/applicable; on this system: 2\)
 - **\-head=&lt;m&gt;** control source header parsing \(in incremental build mode\)  
&lt;m&gt; can be: native \(uses compiler to extract dependencies\), full \(default, uses simple text parser on the whole file\), dep, off
 - **\-rebuild** rebuild \(in incremental build mode\)
 - **\-rebuildall** rebuild with sub\-projects \(in incremental build mode\)
 - **\-clean** törlés \(lépcsőzetes üzemmódban\)
 - **\-workdir=&lt;dir&gt;** working directory  
\(default: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] in incremental mode, OS temp directory otherwise\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** create import library \(Windows only\)


 - **\-hbl\[=&lt;output&gt;\]** kimeneti \.hbl fájlnév\. %\{hb\_lng\} makró használható a fájlnévben
 - **\-lng=&lt;languages&gt;** nyelvek listája, amelyek a \.pot/\.po és \.hbl/\.po állományokban levő %\{hb\_lng\} makrókba kerülnek behelyettesítésre\. Vesszővel elválasztott lista:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** hozzon létre/frissítse a \.po fálj a forráskód alapján\. Használja a meglévő \.po fájlban levő fordításokat
 - **\-minipo\[\-\]** do \(not\) add source file reference to \.po \(default: add them\)
 - **\-rebuildpo** Generálja újra a \.po fájlt, törölve ezáltal minden szükségtelen bejegyzést


 - **\-hbx=&lt;n\[\.hbx&gt;\]&gt;** create Harbour header \(in \.hbx format\) with all external symbols\. Empty parameter will disable it\. Default extension is \.hbx\. If set, &lt;n&gt; will be automatically added to the list of Harbour input files and built into the project\. Therefore, the name part of &lt;n&gt; must not be the same as any other input file present in the project\.
 - **\-hbx\[\-\]** update \(or don't\) \.hbx file specified in \-hbx= option \(default: update\)
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; is a header file name\. &lt;\.hbc&gt; is a \.hbc filename to be automatically included in case the header is found in any of the compiled sources\. \(EXPERIMENTAL\)


 - **\-depurlbase=&lt;d:u&gt;** &lt;d&gt; is the name of the dependency\. &lt;u&gt; is the URL of the project\. Can be specified multiple times\.
 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; is the name of the dependency\. &lt;n&gt; name of the package dependency\. Can be specified multiple times\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; is the name of the dependency\. &lt;h&gt; is the key header \(\.h\) of the package dependency\. Multiple alternative headers can be specified\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; is the name of the dependency\. &lt;f&gt; can be 'yes' or 'no', specifies whether the dependency is optional\. Default: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; is the name of the dependency\. Set &lt;r&gt; as root directory for paths specified in \-depincpath options\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll\[:lib\]&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;dll&gt; to the import library source list\. Optionally override the name of the generated implib to become &lt;lib&gt;\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; is the name of the dependency\. Set generated import library name to &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** add plugin\. &lt;filename&gt; can be: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** pass input file to plugins
 - **\-pflag=&lt;f&gt;** pass single flag to plugins
  
Options below are available on command\-line:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** stop after running Harbour compiler
 - **\-hbcmp|\-clipper** álljon meg az object állományok létrehozása után  
A hbmk2 program hbcmp/clipper nevekre való másolásával/átnevezésével hasonló hatás érhet el
 - **\-hbcc** accept raw C flags  
create link/copy hbmk2 to hbcc for the same effect
 - **\-hblnk** kapcsolókat adja tovább a szerkesztőnek
 - **\-autohbm\[\-\]** enable \(or disable\) processing of hbmk\.hbm in current directory \(default: yes\)
 - **\-hb10** enable Harbour 1\.0\.x compatibility mode
 - **\-hb20** enable Harbour 2\.0\.x compatibility mode
 - **\-hb30** enable Harbour 3\.0\.x compatibility mode
 - **\-hb32** enable Harbour 3\.2\.0dev compatibility mode
 - **\-xhb** kapcsolja be az xhb üzemmódot
 - **\-hbc** enable pure C mode
 - **\-blinker** emulate Cl\*pper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect
 - **\-exospace** see above
 - **\-rtlink** see above


 - **\-hbreg\[=global\]** register Harbour Script \(\.hb\) with hbmk2 \(Windows only\)
 - **\-hbunreg\[=global\]** unregister Harbour Script \(\.hb\) from hbmk2 \(Windows only\)


 - **\-find &lt;text&gt;** list all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)
 - **\-doc &lt;text&gt;** show documentation for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-docjson &lt;text&gt;** output documentation in JSON format for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-fixcase &lt;file\[s\]&gt;** fix casing of Harbour function names to their 'official' format\. Core functions and functions belonging to all active contribs/addons with an \.hbx file will be processed\.
 - **\-sanitize &lt;file\[s\]&gt;** convert filenames to lowercase, EOLs to platform native and remove EOF character, if present\.


 - **\-hbmake=&lt;file&gt;** convert hbmake project &lt;file&gt; to \.hbp file
 - **\-xbp=&lt;file&gt;** \.xbp \(xbuild\) project állomány konvertálása \.hbp állományba
 - **\-xhp=&lt;file&gt;** \.xhp \(xMate\) project állomány konvertálása \.hbp állományba


 - **\-\-hbdirbin** output Harbour binary directory to stdout
 - **\-\-hbdirdyn** output Harbour dynamic library directory to stdout
 - **\-\-hbdirlib** output Harbour static library directory to stdout
 - **\-\-hbdirinc** output Harbour header directory to stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** felülbírálja az alapértelmezett cél platformot \(alapértelmezés: automatikus\)
 - **\-cpu=&lt;cpu&gt;** override default target CPU \(default: automatic\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler auto\-detection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** specify a build name
 - **\-lang=&lt;lang&gt;** override default language\. &lt;lang&gt; is an ISO language code\.
 - **\-width=&lt;n&gt;** set output width to &lt;n&gt; characters \(0=unlimited\)\.
 - **\-shl** show sub\-project level in output lines
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** Harbour compiler credits
 - **\-build** Harbour compiler build information
 - **\-version** csak a verziószám kijelzése
  
Options below are internal/developer ones \(compatibility not guaranteed\):  


 - **\-debugtime** measure time spent on the build
 - **\-debuginc** display internals of incremental build
 - **\-debugstub** display content of all internally generated source files
 - **\-debugi18n** display internals on translation file generation
 - **\-debugdepd** display internals of dependency detection
 - **\-debugpars** display all input parameters in processing order
 - **\-debugrte** generate a run\-time error


You can sym\-link/copy/rename hbmk2 to the following names to alter default mode of operation:


 - **hbrun\*|\*hbrun** mode script runner / interactive shell
 - **hbrund|hbrun\*d** mode script runner / interactive shell in debug mode
 - **harbour** mode \-hbraw \(emulate \- raw \- Harbour compiler\)
 - **clipper** mode \-hbcmp \(emulate Cl\*pper compiler\)
 - **rtlink** mode \-rtlink \(emulate Cl\*pper linker\)
 - **exospace** mode \-rtlink \(emulate Cl\*pper linker\)
 - **blinker** mode \-rtlink \(emulate Cl\*pper linker\)
 - **\*10** \-hb10 kapcsoló
 - **\*20** \-hb20 kapcsoló
 - **\*30** \-hb30 kapcsoló
 - **\*32** \-hb32 kapcsoló
 - **x\*** \-xhb kapcsoló
 - **hbcmp\*|\*hbcmp** mode \-hbcmp \(emulate Harbour compiler producing a binary object\)
 - **hbcc\*|\*hbcc** mode \-hbcc \(emulate C compiler\)
 - **hblnk\*|\*hblnk** mode \-hblnk \(emulate C linker\)
 - **hbexe\*|\*hbexe** mode \-hbexe
 - **hblib\*|\*hblib** mode \-hblib
 - **hbdyn\*|\*hbdyn** mode \-hbdyn
  
Fájlok:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** collection of options\. Can be used to collect common ones into a file and include that into project files\. Uses same format as \.hbp files\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate line\.
 - **\*\.ch** if passed directly as a source file, it will be used as additional standard header
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: $HOME/\.harbour, /etc/harbour, &lt;hbmk2 mappa&gt;/\.\./etc/harbour, &lt;hbmk2 mappa&gt;/\.\./etc, &lt;hbmk2 mappa&gt;
 - **hbmk\.hbm** optional \.hbm file residing in current working directory, which gets automatically processed before other options
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Harbour script
 - **\*\.hrb** Harbour portable binary \(aka precompiled Harbour script\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;hbmk2 mappa&gt;/\.\./etc/harbour, &lt;hbmk2 mappa&gt;/\.\./etc, &lt;hbmk2 mappa&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** stores command history for interactive Harbour shell\. You can disable history by making the first line 'no' \(without quotes and with newline\)\. Resides in \[\*\]: $HOME/\.harbour/
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: $HOME/\.harbour/
  
Makró változók:  


 - **$\{hb\_root\}** directory of hbmk2
 - **$\{hb\_dir\}** directory of the filename it is used in
 - **$\{hb\_dirname\}** top directory of the filename it is used in
 - **$\{hb\_name\}** name of the filename it is used in \(without directory and extension\)
 - **$\{hb\_self\}** full filename it is used in
 - **$\{hb\_curdir\}** current working directory
 - **$\{hb\_tempdir\}** OS directory for temporary files
 - **$\{hb\_targetname\}** name of the project \(without directory and extension\)\. Returns \.adhoc\. if there is not project file\.
 - **$\{hb\_targettype\}** type of the project \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** kiválasztott platform
 - **$\{hb\_comp\}** selected C compiler
 - **$\{hb\_comp\_ver\}** C compiler version
 - **$\{hb\_build\}** build name
 - **$\{hb\_cpu\}** kiválasztott CPU
 - **$\{hb\_work\}** default base workdir name
 - **$\{hb\_workdynsub\}** default workdir subdirectory for dynamic library targets
 - **$\{hb\_dynprefix\}** dynamic library prefix
 - **$\{hb\_dynsuffix\}** dynamic library suffix
 - **$\{hb\_dynext\}** dynamic library extension
 - **$\{hb\_ver\}** Harbour version in hexadecimal triple byte format\. F\.e\.: 030400
 - **$\{hb\_verstr\}** Harbour version in human readable format &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. F\.e\.: 3\.4\.0dev
 - **$\{hb\_major\}** Harbour major version number
 - **$\{hb\_minor\}** Harbour minor version number
 - **$\{hb\_release\}** Harbour release version number
 - **$\{hb\_status\}** Harbour version status
 - **$\{hb\_ver\_id\}** Harbour version ID
 - **$\{hb\_revision\}** Harbour revision
 - **$\{hb\_host\_plat\}** Harbour host platform
 - **$\{hb\_host\_plat\_unix\}** returns '1' if Harbour host platform is \*nix compatible
 - **$\{hb\_bin\}** Harbour binary directory
 - **$\{hb\_lib\}** Harbour static library directory
 - **$\{hb\_lib3rd\}** Harbour 3rd party static library directory
 - **$\{hb\_dyn\}** Harbour dynamic library directory
 - **$\{hb\_inc\}** Harbour header directory
 - **$\{hb\_addons\}** Harbour add\-ons base directory
 - **$\{hb\_first\}** name of source file that holds the entry function \(without directory and extension\)
 - **$\{hb\_outputdir\}** directory of the output
 - **$\{hb\_outputname\}** name of the output \(without extension\)
 - **$\{hb\_level\}** sub\-project recursion level
 - **$\{&lt;depname&gt;\}** returns the header directory of dependency &lt;depname&gt;, or '1' if it is not detected
 - **$\{&lt;envvar&gt;\}** returns the value of the environment variable &lt;envvar&gt;
  
Filters \(you can combine and/or negate them\):  


 - **\{&lt;platform&gt;\}** target platform\. Where &lt;platform&gt; can be any value accepted by \-plat= option\.
 - **\{&lt;compiler&gt;\}** target C compiler\. Where &lt;compiler&gt; can be any value accepted by \-comp= option\.
 - **\{&lt;cpu&gt;\}** target CPU\. Where &lt;cpu&gt; can be any of: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{&lt;package\-manager&gt;\}** package manager\. Where &lt;package\-manager&gt; can be any of: deb, rpm, portage, homebrew, rudix, macports, fink, pkg, cygwin
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** GUI target \(see \-gui option\)
 - **\{std\}** console target \(see \-console option\)
 - **\{debug\}** C level debugging is enabled \(see \-debug option\)
 - **\{nodebug\}** C level debugging is disabled \(see \-debug\- option\)
 - **\{shared\}** shared build \(see \-shared and related options\)
 - **\{static\}** static build \(see \-static and related options\)
 - **\{lngcpp\}** forced C\+\+ mode \(see \-cpp option\)
 - **\{lngc\}** forced C mode \(see \-cpp\- option\)
 - **\{winuni\}** Windows UNICODE \(WIDE\) mode \(see \-winuni option\)
 - **\{winansi\}** Windows ANSI mode \(see \-winuni\- option\)
 - **\{unix\}** target platform is \*nix compatible \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** target platform is Windows compatible \(win, wce\)
 - **\{allgcc\}** target C compiler belongs to gcc family \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** target C compiler is mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** target C compiler is msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** target C compiler is bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** target C compiler is pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** target C compiler is icc\* \(icc, iccia64\)
 - **\{hb10\}** Harbour 1\.0\.x compatibility mode \(see \-hb10 option\)
 - **\{hb20\}** Harbour 2\.0\.x compatibility mode \(see \-hb20 option\)
 - **\{hb30\}** Harbour 3\.0\.x compatibility mode \(see \-hb30 option\)
 - **\{hb32\}** Harbour 3\.2\.0dev compatibility mode \(see \-hb32 option\)
 - **\{xhb\}** xhb mode \(see \-xhb option\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** filter will pass if &lt;file&gt; or &lt;dir&gt; name exists on disk\.
 - **\{MACRO\}** filter will pass if $\{MACRO\} value is not empty and not equal to '0' or 'no' \(case insensitive\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is larger than &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is smaller than &lt;value&gt; \(case insensitive\)\.


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** when an \.hb script is compiled as hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** when an \.hbx source file is present in a project \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected \(available in C sources\)


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\) \(do not define them manually\):


 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected
 - **HBMK\_DIR\_&lt;depname&gt;** return the header directory where &lt;depname&gt; was detected, or empty if it was not\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** when &lt;depname&gt; dependency was detected in a location configured by \-depincpathlocal= option
  
Environment variables:  


 - **HBMK\_OPTIONS** accepts any options as if they were passed in the beginning of the command\-line
 - **HB\_PLATFORM** accepts same values as \-plat= option
 - **HB\_COMPILER** accepts same values as \-comp= option
 - **HB\_CPU** accepts same values as \-cpu= option
 - **HB\_BUILD\_NAME** accepts same values as \-build= option
 - **HB\_LANG** accepts same values as \-lang= option
 - **HB\_USER\_LIBS** accepts same values \(space separated\) as \-l option
 - **HB\_USER\_LIBPATHS** accepts same values \(space separated\) as \-L option
 - **HB\_USER\_PRGFLAGS** options to be passed to Harbour compiler \(before command\-line options\)
 - **HB\_USER\_CFLAGS** options to be passed to C compiler \(before command\-line options\)
 - **HB\_USER\_RESFLAGS** options to be passed to resource compiler \(before command\-line options\) \(Windows only\)
 - **HB\_USER\_LDFLAGS** options to be passed to linker \(executable\) \(before command\-line options\)
 - **HB\_USER\_DFLAGS** options to be passed to linker \(dynamic library\) \(before command\-line options\)
 - **HB\_USER\_AFLAGS** options to be passed to linker \(static library\) \(before command\-line options\)
 - **HB\_CCPATH** override C compiler executable directory \(gcc compiler families only\)
 - **HB\_CCPREFIX** override C compiler executable prefix \(gcc compiler families only\)
 - **HB\_CCSUFFIX** override C compiler executable suffix \(gcc compiler families only\)
 - **HB\_INSTALL\_PREFIX** override Harbour base installation directory
 - **HB\_INSTALL\_ADDONS** override Harbour base add\-ons directory


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
\.hbc directives \(they should be written in separate lines\):  


 - **echo=&lt;msg&gt;** display &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** skip processing the rest of the \.hbc file\. Display &lt;msg&gt;, if specified\.
 - **stop=\[&lt;msg&gt;\]** stop the build\. Display &lt;msg&gt;, if specified\.
 - **sources=** add space separated list of files as input files
 - **headers=** add space separated list of \.ch format headers as standard header
 - **libs=** add space separated list of libraries \(see more at \-l option\)
 - **frameworks=** add space separated list of frameworks \(Darwin only\)
 - **requests=** add space separated list of symbols to force link to the build target
 - **syslibs=** add space separated list of libraries as system libraries \(before regular libraries\)
 - **hbcs=** embed space separated list of \.hbc files\. Names without the extension is accepted\. These references are processed in place\.
 - **autohbcs=** space separated list of values as in \-autohbc= option
 - **libpaths=** space separated list of additional library paths
 - **incpaths=** add space separated list of additional header paths \(for both Harbour and C\)
 - **instfiles=** space separated list of values as in \-instfile= option
 - **instpaths=** space separated list of values as in \-instpath= option
 - **prgflags=** space separated list of values as in \-prgflag= option
 - **cflags=** space separated list of values as in \-cflag= option
 - **resflags=** space separated list of values as in \-resflag= option
 - **ldflags=** space separated list of values as in \-ldflag= option
 - **ldflags\+=** space separated list of values as in \-ldflag\+= option
 - **dflags=** space separated list of values as in \-dflag= option
 - **dflags\+=** space separated list of values as in \-dflag\+= option
 - **pflags=** space separated list of values as in \-pflag= option
 - **psources=** space separated list of values as in \-pi= option
 - **gui=&lt;bool&gt;** 'yes' = \-gui, 'no' = \-std kapcsoló
 - **mt=&lt;bool&gt;** 'yes' = \-mt, 'no' = \-st kapcsoló
 - **pic=&lt;bool&gt;** 'yes' = \-pic, 'no' = \-pic\- kapcsoló
 - **shared=&lt;bool&gt;** 'yes' = \-shared, 'no' = \-static kapcsoló
 - **shareddef=&lt;bool&gt;** similar to shared=, but works only if shared/static mode was not set before
 - **fullstatic=&lt;bool&gt;** 'yes' = \-fullstatic, 'no' = \-static kapcsoló
 - **debug=&lt;bool&gt;** 'yes' = \-debug, 'no' = \-debug\- kapcsoló
 - **optim=** 'yes' = \-optim, 'no' = \-optim\- kapcsoló
 - **nulrdd=&lt;bool&gt;** 'yes' = \-nulrdd, 'no' = \-nulrdd\- kapcsoló
 - **nodefgt=&lt;bool&gt;** 'yes' = \-nodefgt, 'no' = \-nodefgt\- kapcsoló
 - **map=&lt;bool&gt;** 'yes' = \-map, 'no' = \-map\- kapcsoló
 - **hbcppmm=&lt;bool&gt;** 'yes' = \-hbcpmm, 'no' = \-hbcpmm\- kapcsoló
 - **implib=&lt;bool&gt;** 'yes' = \-implib, 'no' = \-implib\- kapcsoló
 - **winuni=&lt;bool&gt;** 'yes' = \-winuni, 'no' = \-winuni\- kapcsoló
 - **strip=&lt;bool&gt;** 'yes' = \-strip, 'no' = \-strip\- kapcsoló
 - **run=&lt;bool&gt;** 'yes' = \-run, 'no' = \-run\- kapcsoló
 - **inc=&lt;bool&gt;** 'yes' = \-inc, 'no' = \-inc\- kapcsoló
 - **harden=&lt;bool&gt;** 'yes' = \-harden, 'no' = \-harden\- kapcsoló
 - **cpp=** ugyanaz, mint \-cpp= kapcsoló
 - **warn=** ugyanaz, mint \-warn= kapcsoló
 - **compr=** ugyanaz, mint \-compr= kapcsoló
 - **head=** ugyanaz, mint \-head= kapcsoló
 - **plugins=** space separated list of hbmk2 plugins to load
 - **gt=&lt;name&gt;** ugyanaz, mint \-gt&lt;name&gt; kapcsoló
 - **gtdef=&lt;name&gt;** set the default GT to be used
 - **env=** ugyanaz, mint \-env: kapcsoló
 - **depurlbase=** ugyanaz, mint \-depurlbase= kapcsoló
 - **deppkgname=** ugyanaz, mint \-deppkgname= kapcsoló
 - **depkeyhead=** ugyanaz, mint \-depkeyhead= kapcsoló
 - **depoptional=** ugyanaz, mint \-depoptional= kapcsoló
 - **depcontrol=** ugyanaz, mint \-depcontrol= kapcsoló
 - **depincroot=** ugyanaz, mint \-depincroot= kapcsoló
 - **depincpath=** ugyanaz, mint \-depincpath= kapcsoló
 - **depincpathlocal=** ugyanaz, mint \-depincpathlocal= kapcsoló
 - **depimplibs=** ugyanaz, mint \-depimplibs= kapcsoló
 - **depimplibd=** ugyanaz, mint \-depimplibd= kapcsoló
 - **depfinish=** ugyanaz, mint \-depfinish= kapcsoló
 - **signts=** ugyanaz, mint \-signts= kapcsoló
 - **name=** package name
 - **description=** package description
 - **version=&lt;x\.y\.z&gt;** package version number, where x,y,z &gt;= 0 &lt;= 255\. Defaults to 0\.0\.1, if not specified\.
 - **keywords=** space separated list of keywords
 - **licences=** licencek szóközzel elválasztott listája
 - **repository=** space separated list of source repository references


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unrecognized file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Add a Harbour input file to the project\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Add a C input file to the project\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Add a C\+\+ input file to the project\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Add a Windows resource input file to the project\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Add a binary object file to the project\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Add a file to be installed, with an optional \-instpath= group name\.
 - **hbmk\_AddOption\_PRG\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Add a Harbour compiler option\.
 - **hbmk\_AddOption\_C\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Add a C compiler option\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Output text to stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Output text to stderr\.
 - **hbmk\_OutStdRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Output text to stdout without any formatting\.
 - **hbmk\_OutErrRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Output text to stderr without any formatting\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Evaluate hbmk2 macro expression\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Escape/quote filename for using it as external command parameter\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target platform/C compiler\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Convert filename to have forward slash directory separators\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Return relative path of \-workdir= value from current working directory\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Find file in &lt;xPath&gt; \(array or pathsep delimited string are accepted\) with list of &lt;aExtDef&gt; alternate extensions \(defaults to executable binaries\)\. Returns filename if found and NIL if not\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Change directory and/or extension in filename\.
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Encode function name according to Harbour compiler rules for forming HB\_FUNC\(\) function names in C code\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Strip double quote enclosure from a string\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convert array of strings to a string\. Default separator is a single space\.


Plugin variables:  
\('hbmk' context hash items, case\-sensitive, read\-only unless marked otherwise\)


 - **"apiver"** API version as an integer
 - **"cSTATE"** callback state\. Can be: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** array of parameters passed to plugins via \-pflag=/pi= options or having an extension registered via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** hash of plugin custom variables\. Writable, local to each plugin
 - **"cPLAT"** \-plat value
 - **"cCOMP"** \-comp value
 - **"nCOMPVer"** detected compiler version in &lt;MMmm&gt; format
 - **"cCPU"** \-cpu value
 - **"cBUILD"** \-build= value
 - **"cOUTPUTNAME"** \-o value
 - **"cTARGETNAME"** see $\{hb\_targetname\} macro
 - **"cTARGETTYPE"** see $\{hb\_targettype\} macro
 - **"lREBUILD"** \-rebuild option status
 - **"lCLEAN"** \-clean option status
 - **"lDEBUG"** \-debug option status
 - **"lMAP"** \-map option status
 - **"lSTRIP"** \-strip option status
 - **"lDONTEXEC"** \-traceonly option status
 - **"lIGNOREERROR"** \-ignore option status
 - **"lTRACE"** \-trace option status
 - **"lQUIET"** \-q option status
 - **"lINFO"** \-info option status
 - **"lBEEP"** \-beep option status
 - **"lRUN"** \-run option status
 - **"lINC"** \-inc option status
 - **"cCCPATH"** see HB\_CCPATH envvar
 - **"cCCPREFIX"** see HB\_CCPREFIX envvar
 - **"cCCSUFFIX"** see HB\_CCSUFFIX envvar
 - **"cCCEXT"** see HB\_CCEXT envvar
 - **"cWorkDir"** \-workdir= value
 - **"nExitCode"** Aktuális kilépési érték
  
Shell API available in Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Display list of loaded Harbour header\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load package\. Similar to \#request PP directive\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload package\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
List of loaded packages\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.


Examples to start with hbmk2:


 - **To run the interactive shell \('dot' prompt\)**  
$ hbmk2 \.
 - **To run a Harbour script**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Examples to build and run Harbour portable binary \(aka precompiled Harbour script\):


 - **To build**  
$ hbmk2 \-gh myscript\.hb
 - **To run result of above**  
$ hbmk2 myscript\.hrb \[&lt;parameter\[s\]&gt;\]


Examples to build a Harbour application:


 - **To build one simple \.prg**  
$ hbmk2 hello\.prg
 - **To build multiple \.prg sources into one application in incremental mode**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **To build an application using a project file**  
$ hbmk2 myapp\.hbp
 - **To build an application using incremental mode**  
$ hbmk2 myapp\.hbp \-inc
 - **To build an application which uses a contrib package or 3rd party \(add\-on\) package that ships with an \.hbc file**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **To build an application which uses a raw library**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **To build an application which uses a Windows resource**  
$ hbmk2 mymain\.prg myres\.rc
 - **To build an application which links against Harbour dynamic libraries**  
$ hbmk2 \-shared myapp\.prg
 - **To build an application out of all \.prg and \.c sources residing in 'source' subdir**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Examples to build a Harbour static library:


 - **To build library 'mylib' from sources**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **To build library 'mylib' from sources using incremental mode**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Exit codes \("errorlevels"\):  


 - **0** no error
 - **1** unrecognized platform
 - **2** unrecognized compiler
 - **3** failed Harbour detection
 - **5** failed stub creation
 - **6** failed in compilation phase
 - **7** failed in final assembly \(linker or library manager\)
 - **8** unsupported
 - **9** failed to create working directory
 - **19** súgó
 - **10** dependency missing or disabled
 - **20** beépülő\-modul inicializáció
 - **30** túl mély rekurzió
 - **50** stop requested
 - **&lt;egyéb&gt;** when \-run option is used, the exit code will be the one returned by the target executable
  
Megjegyzések:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Source filename without extension will load the \.hbp file, if such \.hbp file exists in current directory\. If not, \.prg extension will be used\.
  - Több \-l, \-L, \-i és &lt;parancsállomány&gt; kapcsoló/paraméter is megengedett\.
  - Normál Harbour fordító kapcsolók is megadhatók\.
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/harbour, /etc/\.harbour, &lt;base&gt;/etc/\.harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - hbmk\.hbm make script in current directory is always processed if it exists\.
  - Using forwards slashes is recommended in option values as directory separator, but backslashes are also equally accepted\.
  - A szűrők az egyes \.hbc sorokban használhatók és számos opció esetén támogatottak\.  
Szűrő formátum: \{\[\!\]\[&lt;platform&gt;|&lt;compiler&gt;|&lt;cpu&gt;|&lt;keyword&gt;\]\}\. Szűrők kombinálhatók '&amp;', '|' operátorokkal és zárójelekkel csoportosíthatók\. Pl\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - A legtöbb \.hbc opcióban és parancssori megfelelőikben \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) használhatók makró változókat\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Options accepting macro variables also support command substitution\. Enclose command inside \`\`, and, if the command contains space, also enclose in double quotes\. Standard output of the command will be used as the value\. F\.e\. "\-cflag=\`wx\-config \-\-cflags\`", or ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Libraries and object files built with/for CA\-Cl\*pper will not work with any supported platform/compiler\.
  - Defaults and feature support may vary by platform/compiler\.
  - GNU Make or any C compiler specific make tool and MSYS \(on Windows\) are not needed to run hbmk2\.
  - '\.' \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'darwin' host platform\.


Az egyes &lt;platform&gt; értékekhez tartozó &lt;compiler&gt; értékek a következők:


 - **linux** gcc, clang, icc, watcom, sunpro, open64
 - **darwin** gcc, clang, icc
 - **win** mingw, msvc, clang, bcc, bcc64, watcom, icc, pocc, xcc, mingw64, msvc64, msvcia64, iccia64, pocc64
 - **wce** mingwarm, mingw, msvcarm, poccarm
 - **os2** gcc, gccomf, watcom
 - **dos** djgpp, watcom
 - **bsd** gcc, clang, pcc
 - **hpux** gcc
 - **beos** gcc
 - **qnx** gcc
 - **android** gcc, gccarm
 - **vxworks** gcc, diab
 - **symbian** gcc
 - **cygwin** gcc
 - **minix** clang, gcc
 - **aix** gcc
 - **sunos** gcc, sunpro
  
Licenc \(angolul\):  


  This program is free software; you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation; either version 2 of the License, or  
\(at your option\) any later version\.  
  
This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE\.  See the  
GNU General Public License for more details\.  
  
You should have received a copy of the GNU General Public License  
along with this program; if not, write to the Free Software  
Foundation, Inc\., 675 Mass Ave, Cambridge, MA 02139, USA \(or visit  
their website at https://www\.gnu\.org/\)\.  
  
License extensions:  
  \- This source code must be kept and distributed as part  
    of the Harbour package and/or the placement of the tool sources  
    and files must reflect that it is part of Harbour Project\.  
  \- Copyright information must always be presented by  
    projects including this tool or help text\.  
  \- Modified versions of the tool must clearly state this  
    fact on the copyright screen\.  
  \- Source code modifications shall always be made available  
    along with binaries\.  
  \- Help text and documentation is licensed under  
    Creative Commons Attribution\-ShareAlike 4\.0 International:  
    https://creativecommons\.org/licenses/by\-sa/4\.0/  

  
Szerző:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
