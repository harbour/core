Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-04\-03 03:33\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  

Syntax:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descripción:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will autodetect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Options:  


 - **\-o&lt;outname&gt;** output file name
 - **\-l&lt;libname&gt;** link with &lt;libname&gt; library\. &lt;libname&gt; should be without path, extension and 'lib' prefix \(unless part of the name\)\. Do not add core Harbour libraries, they are automatically added as needed\. If &lt;libname&gt; starts with a '\-' character, the library will be removed from the list of libraries at link time\.
 - **\-L&lt;libpath&gt;** additional path to search for libraries
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** additional path to search for headers
 - **\-static|\-shared** link with static/shared libs
 - **\-gt&lt;name&gt;** link with GT&lt;name&gt; GT driver, can be repeated to link with more GTs\. First one will be the default at run\-time
 - **\-inc\[\-\]** enable/disable incremental build mode \(default: disabled\)
 - **\-hbexe** Crear executable \(predeterminado\)
 - **\-hblib** create static library
 - **\-hbdyn** create dynamic library \(without linked Harbour VM\)
 - **\-hbdynvm** create dynamic library \(with linked Harbour VM\)


 - **\-mt|\-st** link with multi/single\-thread Harbour VM
 - **\-gui|\-std** create GUI/console executable
 - **\-main=&lt;mainfunc&gt;** override the name of starting function/procedure
 - **\-request=&lt;func&gt;** Forzar enlazado de función/procedemento
 - **\-fullstatic** link with all static libs
 - **\-pic\[\-\]** create position independent object code \(always enabled in \-hbdyn/\-hbdynvm modes\)
 - **\-\[full|fix\]shared** create shared Harbour binaries without/with absolute dir reference to Harbour library \(default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise\) \(fix/full option in \*nix only\)
 - **\-nulrdd\[\-\]** link with nulrdd
 - **\-debug\[\-\]** add/exclude C compiler debug info\. For Harbour level debug, use Harbour option \-b as usual
 - **\-optim\[\-\]** toggle C compiler optimizations \(default: on\)
 - **\-cpp\[\-\]** force C\+\+/C mode
 - **\-cpp=&lt;value&gt;** Selecciona o modo C\+\+\. Os valores posibles son: def, yes, no
 - **\-map\[\-\]** create \(or not\) a map file
 - **\-implib\[\-\]** Crear \(ou non\) a biblioteca de imporación \(en modo \-hbdyn/\-hbexe\)\. Engadirase un sufixo ao nome\.
 - **\-implib=&lt;output&gt;** crear nome de biblioteca de importación \(en modo \-hbdyn/\-hbexe\) para a saída a &lt;output&gt; \(predeterminado: o mesmo que a saída\)
 - **\-ln=&lt;link&gt;** crear enlace simbólico para &lt;output&gt; \(&lt;link&gt; considérase relativo a &lt;output&gt;\)
 - **\-strip\[\-\]** strip \(no strip\) binaries
 - **\-trace\[\-\]** show commands executed
 - **\-beep\[\-\]** enable \(or disable\) single beep on successful exit, double beep on failure
 - **\-ignore\[\-\]** ignore errors when running compiler tools \(default: off\)
 - **\-hbcppmm\[\-\]** override standard C\+\+ memory management functions with Harbour ones
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI compilation modes \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** Non usar as bibliotecas estáticas de Harbour ao enlazar
 - **\-nodefgt\[\-\]** Non enlazar GTs predeterminados \(eficaz en modo \-static\)
 - **\-nolibgrouping\[\-\]** disable library grouping on gcc based compilers
 - **\-nomiscsyslib\[\-\]** do not add extra list of system libraries to default library list
 - **\-traceonly** show commands to be executed, but do not execute them
 - **\-warn=&lt;level&gt;** set C compiler warning level  
&lt;level&gt; can be: max, yes, low, no, def \(default: yes\)
 - **\-safe\[\-\]** enable safety options in C compiler/linker \(default: enabled on Windows, disabled on other systems\)
 - **\-compr=&lt;level&gt;** compress executable/dynamic lib \(needs UPX tool\)  
&lt;level&gt; can be: yes, no, min, max
 - **\-run\[\-\]** run/do not run output executable
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-tshead=&lt;file&gt;** generate \.ch header file with timestamp information\. Generated header will define preprocessor constants \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build
 - **\-icon=&lt;file&gt;** set &lt;file&gt; as application icon\. &lt;file&gt; should be a supported format on the target platform \(not supported by some platforms/compilers\)\. On Windows, it is implemented by generating and linking a resource file\.
 - **\-manifest=&lt;file&gt;** Incrustar arquivo de manifesto &lt;file&gt; en executable/biblioteca dinámica \(só Windows\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both autodetected\.
 - **\-signpw=&lt;pw&gt;** Use &lt;pw&gt; como chave de acceso ao asinar executables \(só en Windows e Darwin\)
 - **\-instfile=&lt;g:file&gt;** add &lt;file&gt; in to the list of files to be copied to path specified by \-instpath option\. &lt;g&gt; is an optional copy group \(case sensitive\), it must be at least two characters long\. In case you do not specify &lt;file&gt;, the list of files in that group will be emptied\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** enable \(or disable\) import library generation for import library sources specified in \-depimplibs= options \(default: yes\)
 - **\-stop\[=&lt;text&gt;\]** stop without doing anything and display &lt;text&gt; if specified
 - **\-echo=&lt;text&gt;** echo text on screen
 - **\-pause** force waiting for a key on exit in case of failure \(with alternate GTs only\)
 - **\-exitstr** Mostrar resultado de error como texto lexible
 - **\-info** turn on informational messages
 - **\-quiet\[\-\]** suppress all screen messages


 - **\-bldf\[\-\]** inherit all/no \(default\) flags from Harbour build
 - **\-bldf=\[p\]\[c\]\[l\]** inherit \.prg/\.c/linker flags \(or none\) from Harbour build
 - **\-F&lt;framework&gt;** link with &lt;framework&gt; framework \(Darwin only\)
 - **\-prgflag=&lt;f&gt;** pass single flag to Harbour compiler
 - **\-cflag=&lt;f&gt;** pass single flag to C compiler
 - **\-resflag=&lt;f&gt;** pass single flag to resource compiler \(Windows only\)
 - **\-ldflag=&lt;f&gt;** pass single flag to linker \(executable\)
 - **\-dflag=&lt;f&gt;** pass single flag to linker \(dynamic library\)
 - **\-aflag=&lt;f&gt;** pass single flag to linker \(static library\)
 - **\-iflag=&lt;f&gt;** Pasar a opción ao comando de creación de bibliotecas de importación
 - **\-signflag=&lt;f&gt;** pass single flag to code sign command
 - **\-runflag=&lt;f&gt;** pass single flag to output executable when \-run option is used
 - **\-cflag\+=&lt;f&gt;** pass single flag to C compiler overriding C compiler flags added by hbmk2 itself\. Use with caution\.
 - **\-ldflag\+=&lt;f&gt;** pass single raw option to linker \(executable\) after the library list\. Use with caution\.
 - **\-dflag\+=&lt;f&gt;** pasar a opción tal cuál ao enlazador \(biblioteca dinámica\), tra\-la lista de bibliotecas\. Usar con precaución\.
 - **\-3rd=&lt;f&gt;** opcións/parámetros reservados para ferramentas externas, ignoradas sempre por hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alter local environment\. &lt;e&gt; is the name of the environment variable to alter\. &lt;o&gt; can be '=' to set/override, '\-' to delete, '\+' to append to the end of existing value, '\#' to insert to the beginning of existing value\. &lt;v&gt; is the value to set/append/insert\.
 - **\-jobs=&lt;n&gt;** start n compilation threads \(multiprocess platforms only\)
 - **\-head=&lt;m&gt;** control source header parsing \(in incremental build mode\)  
&lt;m&gt; can be: native \(uses compiler to extract dependencies\), full \(default, uses simple text parser on the whole file\), dep, off
 - **\-rebuild** Reconstruir \(en modo construcción incremental\)
 - **\-rebuildall** Reconstruir con subproxectos \(en modo de construcción incremental\)
 - **\-clean** clean \(in incremental build mode\)
 - **\-workdir=&lt;dir&gt;** working directory  
\(default: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] in incremental mode, OS temp directory otherwise\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** Crear biblioteca de importación \(só Windows\)


 - **\-hbl\[=&lt;output&gt;\]** output \.hbl filename\. %\{hb\_lng\} macro is accepted in filename
 - **\-lng=&lt;languages&gt;** list of languages to be replaced in %\{hb\_lng\} macros in \.pot/\.po filenames and output \.hbl/\.po filenames\. Comma separated list:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** create/update \.po file from source\. Merge it with previous \.po file of the same name
 - **\-minipo\[\-\]** do \(not\) add Harbour version number and source file reference to \.po \(default: add them\)
 - **\-rebuildpo** recreate \.po file, thus removing all obsolete entries in it


 - **\-hbx=\[&lt;\.ch&gt;\]** Create Harbour header \(in \.hbx format\) with all external symbols\. Empty parameter will disable it\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; is a header file name\. &lt;\.hbc&gt; is a \.hbc filename to be automatically included in case the header is found in any of the compiled sources\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; is the name of the dependency\. &lt;n&gt; name of the package dependency\. Can be specified multiple times\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; is the name of the dependency\. &lt;h&gt; is the key header \(\.h\) of the package dependency\. Multiple alternative headers can be specified\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; é o nome da dependencia\. &lt;f&gt; pose ser 'yes' ou 'no' e indica se a dependencia é opcional\. Predeterminado: 'no'
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; is the name of the dependency\. Set &lt;r&gt; as root directory for paths specified in \-depincpath options\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;i&gt; á lista de rutas de busca de arquivos de cabeceira\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;dll&gt; á lista de arquivos de bibliotecas de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; é o nome de dependencia\. Establecer o nome da biblioteca de importación xenerada a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** add plugin\. &lt;filename&gt; can be: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** Pasar arquivos de entrada a os complementos
 - **\-pflag=&lt;f&gt;** pass single flag to plugins
  
As seguintes opcións están dispoñibles na liña de comandos:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** Deter despois de executar o compilador Harbour
 - **\-hbcmp|\-clipper** stop after creating the object files  
create link/copy hbmk2 to hbcmp/clipper for the same effect
 - **\-hbcc** accept raw C flags  
create link/copy hbmk2 to hbcc for the same effect
 - **\-hblnk** accept raw linker flags
 - **\-autohbm\[\-\]** enable \(or disable\) processing of hbmk\.hbm in current directory \(default: yes\)
 - **\-hb10** Habilitar modo compatibilidade Harbour 1\.0\.x
 - **\-hb20** enable Harbour 2\.0\.x compatibility mode
 - **\-hb30** enable Harbour 3\.0\.x compatibility mode
 - **\-xhb** enable xhb mode
 - **\-hbc** enable pure C mode
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emulate Clipper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect


 - **\-hbreg\[=global\]** registrar Harbour Script \(\.hb\) con hbmk2 \(Só Windows\)
 - **\-hbunreg\[=global\]** unregister Harbour Script \(\.hb\) from hbmk2 \(Windows only\)


 - **\-find &lt;text&gt;** lists all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)


 - **\-hbmake=&lt;file&gt;** convert hbmake project &lt;file&gt; to \.hbp file
 - **\-xbp=&lt;file&gt;** convert \.xbp \(xbuild\) project &lt;file&gt; to \.hbp file
 - **\-xhp=&lt;file&gt;** convert \.xhp \(xMate\) project &lt;file&gt; to \.hbp file


 - **\-\-hbdirbin** output Harbour binary directory to stdout
 - **\-\-hbdirdyn** output Harbour dynamic library directory to stdout
 - **\-\-hbdirlib** output Harbour static library directory to stdout
 - **\-\-hbdirinc** output Harbour header directory to stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** override default target platform \(default: automatic\)
 - **\-cpu=&lt;cpu&gt;** override default target CPU \(default: automatic\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler autodetection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** specify a build name
 - **\-lang=&lt;lang&gt;** override default language\. &lt;lang&gt; is an ISO language code\.
 - **\-width=&lt;n&gt;** Fixar ancho da saída a &lt;n&gt; caracteres \(0=ilimitado\)\.
 - **\-shl** show sub\-project level in output lines
 - **\-viewhelp** long help in text viewer
 - **\-longhelp** long help
 - **\-longhelpmd** long help in [Markdown](http://daringfireball.net/projects/markdown/) format
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** Créditos do compilador Harbour
 - **\-build** Información da construcción do compilador Harbour
 - **\-version** display version header only
  
Options below are internal/developer ones \(compatibility not guaranteed\):  


 - **\-debugtime** measure time spent on the build
 - **\-debuginc** display internals of incremental build
 - **\-debugstub** display content of all internally generated source files
 - **\-debugi18n** display internals on translation file generation
 - **\-debugdepd** display internals of dependency detection
 - **\-debugpars** display all input parameters in processing order
 - **\-debugrte** generate a run\-time error


Pode engadir un enlace simbólico/copia/renomear hbmk2 a os seguintes nomes para alterar o modo predeterminado de operación:


 - **hbrun\*|\*hbrun** modo secuencia de comandos / consola interactiva
 - **hbrund|hbrun\*d** Modo secuencia de comandos / consola interactiva en modo depuración
 - **harbour** mode \-hbraw \(emulate \- raw \- Harbour compiler\)
 - **clipper** mode \-hbcmp \(emulación do compilador Clipper\)
 - **rtlink** Modo \-rtlink \(emular enlazador Clipper\)
 - **exospace** Modo \-rtlink \(emular enlazador Clipper\)
 - **blinker** Modo \-rtlink \(emular enlazador Clipper\)
 - **\*10** option \-hb10
 - **\*20** option \-hb20
 - **\*30** option \-hb30
 - **x\*** option \-xhb
 - **hbcmp\*|\*hbcmp** mode \-hbcmp \(emulate Harbour compiler producing a binary object\)
 - **hbcc\*|\*hbcc** mode \-hbcc \(emulate C compiler\)
 - **hblnk\*|\*hblnk** mode \-hblnk \(emulate C linker\)
 - **hbexe\*|\*hbexe** mode \-hbexe
 - **hblib\*|\*hblib** mode \-hblib
 - **hbdyn\*|\*hbdyn** mode \-hbdyn
  
Arquivos:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** collection of options\. Can be used to collect common ones into a file and include that into project files\. Uses same format as \.hbp files\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate line\.
 - **\*\.ch** if passed directly as a source file, it will be used as additional standard header
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **hbmk\.hbm** arquivo opcional \.hbm no directorio de traballo actual, que é automaticamente procesado antes de outras opcións
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Harbour script
 - **\*\.hrb** Harbour portable binary \(aka precompiled Harbour script\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** stores command history for interactive Harbour shell\. You can disable history by making the first line 'no' \(without quotes and with newline\)\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
  
Macro variables:  


 - **$\{hb\_root\}** directory of hbmk2
 - **$\{hb\_dir\}** directory of the filename it is used in
 - **$\{hb\_dirname\}** top directory of the filename it is used in
 - **$\{hb\_name\}** name of the filename it is used in \(without directory and extension\)
 - **$\{hb\_self\}** full filename it is used in
 - **$\{hb\_curdir\}** current working directory
 - **$\{hb\_tempdir\}** Directorio do sistema operativo para arquivos temporais\.
 - **$\{hb\_targetname\}** name of the project \(without directory and extension\)\. Returns \.adhoc\. if there is not project file\.
 - **$\{hb\_targettype\}** type of the project \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** selected platform
 - **$\{hb\_comp\}** selected C compiler
 - **$\{hb\_comp\_ver\}** C compiler version
 - **$\{hb\_build\}** build name
 - **$\{hb\_cpu\}** selected CPU
 - **$\{hb\_work\}** default base workdir name
 - **$\{hb\_workdynsub\}** default workdir subdirectory for dynamic library targets
 - **$\{hb\_dynprefix\}** dynamic library prefix
 - **$\{hb\_dynsuffix\}** dynamic library suffix
 - **$\{hb\_dynext\}** dynamic library extension
 - **$\{hb\_ver\}** Harbour version in hexadecimal triple byte format\. F\.e\.: 030200
 - **$\{hb\_verstr\}** Harbour version in human readable format &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. F\.e\.: 3\.2\.0dev
 - **$\{hb\_major\}** Harbour major version number
 - **$\{hb\_minor\}** Harbour minor version number
 - **$\{hb\_release\}** Harbour release version number
 - **$\{hb\_status\}** Harbour version status
 - **$\{hb\_revision\}** Harbour revision
 - **$\{hb\_host\_plat\}** Harbour host platform
 - **$\{hb\_host\_plat\_unix\}** Retorna '1' se a plataforma anfitrión e \*nix compatible
 - **$\{hb\_bin\}** Harbour binary directory
 - **$\{hb\_lib\}** Directorio para bibliotecas estáticas de Harbour
 - **$\{hb\_lib3rd\}** Harbour 3rd party static library directory
 - **$\{hb\_dyn\}** Directorio para bibliotecas de enlace dinámico de Harbour
 - **$\{hb\_inc\}** Harbour header directory
 - **$\{hb\_addons\}** Harbour add\-ons base directory
 - **$\{hb\_first\}** name of source file that holds the entry function \(without directory and extension\)
 - **$\{hb\_outputdir\}** directory of the output
 - **$\{hb\_outputname\}** Nome do arquivo de saída \(sen extensión\)
 - **$\{hb\_level\}** sub\-project recursion level
 - **$\{&lt;depname&gt;\}** returns the header directory of dependency &lt;depname&gt;, or '1' if it is not detected
 - **$\{&lt;envvar&gt;\}** Retorna o valor da variable de entorno &lt;envvar&gt;
  
Filters \(you can combine and/or negate them\):  


 - **\{&lt;platform&gt;\}** target platform\. Where &lt;platform&gt; can be any value accepted by \-plat= option\.
 - **\{&lt;compiler&gt;\}** target C compiler\. Where &lt;compiler&gt; can be any value accepted by \-comp= option\.
 - **\{&lt;cpu&gt;\}** CPU destino\. Onde &lt;cpu&gt; pode ser: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** GUI target \(see \-gui option\)
 - **\{std\}** console target \(see \-console option\)
 - **\{debug\}** C level debugging is enabled \(see \-debug option\)
 - **\{nodebug\}** Depuración a nivel C está desactivada \(ver opción \-debug\-\)
 - **\{shared\}** shared build \(see \-shared and related options\)
 - **\{static\}** static build \(see \-static and related options\)
 - **\{lngcpp\}** Forzado modo C\+\+ \(ver opción \-cpp\)
 - **\{lngc\}** Forzado modo C \(ver opción \-cpp\-\)
 - **\{winuni\}** Modo Windows UNICODE \(WIDE\) \(ver opción \-winuni\)
 - **\{winansi\}** Modo Windows ANSI \(ver opción \-winuni\-\)
 - **\{unix\}** target platform is \*nix compatible \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** Plataforma destino e compatible con Windows \(win, wce\)
 - **\{allgcc\}** target C compiler belongs to gcc family \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** O compilador C é mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** target C compiler is msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** target C compiler is bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** target C compiler is pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** target C compiler is icc\* \(icc, iccia64\)
 - **\{hb10\}** Harbour 1\.0\.x compatibility mode \(see \-hb10 option\)
 - **\{hb20\}** Harbour 2\.0\.x compatibility mode \(see \-hb20 option\)
 - **\{hb30\}** Harbour 3\.0\.x compatibility mode \(see \-hb30 option\)
 - **\{xhb\}** xhb mode \(see \-xhb option\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** filter will pass if &lt;file&gt; or &lt;dir&gt; name exists on disk\.
 - **\{MACRO\}** filtro pasará se o valor de $\{MACRO\} non está valeiro e é diferente de cero '0' or 'non' \(non se diferencian maiúsculas e minúsculas\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is larger than &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is smaller than &lt;value&gt; \(case insensitive\)\.


Constantes predefinidas nos fontes:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** when an \.hb script is compiled as hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** when an \.hbx source file is present in a project \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected \(available in C sources\)


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\):


 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected
 - **HBMK\_DIR\_&lt;depname&gt;** return the header directory where &lt;depname&gt; was detected, or empty if it was not\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** cando a dependencia &lt;depname&gt; se detectou nun lugar configurado pola opción \-depincpathlocal=
  
Environment variables:  


 - **HBMK\_OPTIONS** accepts any options as if they were passed in the beginning of the command\-line
 - **HB\_PLATFORM** accepts same values as \-plat= option
 - **HB\_COMPILER** accepts same values as \-comp= option
 - **HB\_CPU** accepts same values as \-cpu= option
 - **HB\_BUILD\_NAME** accepts same values as \-build= option
 - **HB\_LANG** accepts same values as \-lang= option
 - **HB\_USER\_LIBS** Acepta os mesmos valores \(separados por espacios\) que a opción \-l
 - **HB\_USER\_LIBPATHS** Acepta os mesmos valores \(separados por espacios\) que a opción \-L
 - **HB\_USER\_PRGFLAGS** opcións para o compilador Harbour \(antes das opcións de liña de comandos\)
 - **HB\_USER\_CFLAGS** opcións para o compilador C \(antes das opcións de liña de comandos\)
 - **HB\_USER\_RESFLAGS** options to be passed to resource compiler \(before command\-line options\) \(Windows only\)
 - **HB\_USER\_LDFLAGS** options to be passed to linker \(executable\) \(before command\-line options\)
 - **HB\_USER\_DFLAGS** options to be passed to linker \(dynamic library\) \(before command\-line options\)
 - **HB\_USER\_AFLAGS** options to be passed to linker \(static library\) \(before command\-line options\)
 - **HB\_COMPILER\_VER** override C compiler version autodetection \(gcc and msvc compiler families only\)\. Format: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** override C compiler executable directory \(gcc compiler families only\)
 - **HB\_CCPREFIX** override C compiler executable prefix \(gcc compiler families only\)
 - **HB\_CCSUFFIX** override C compiler executable suffix \(gcc compiler families only\)
 - **HB\_INSTALL\_PREFIX** override Harbour base installation directory
 - **HB\_INSTALL\_ADDONS** override Harbour base add\-ons directory


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
\.hbc directives \(they should be written in separate lines\):  


 - **echo=&lt;msg&gt;** Amosar &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** Omitir o procesamento do resto do arquivo \.hbc\. Mostrarase &lt;msg&gt; se se especificou\.
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
 - **gui=&lt;bool&gt;** 'yes' = \-gui, 'no' = \-std option
 - **mt=&lt;bool&gt;** 'yes' = \-mt, 'no' = \-st option
 - **pic=&lt;bool&gt;** 'yes' = \-pic, 'no' = \-pic\- option
 - **shared=&lt;bool&gt;** 'yes' = \-shared, 'no' = \-static option
 - **shareddef=&lt;bool&gt;** similar to shared=, but works only if shared/static mode was not set before
 - **fullstatic=&lt;bool&gt;** 'yes' = \-fullstatic, 'no' = \-static option
 - **debug=&lt;bool&gt;** 'yes' = \-debug, 'no' = \-debug\- option
 - **optim=** 'yes' = \-optim, 'no' = \-optim\- option
 - **nulrdd=&lt;bool&gt;** 'yes' = \-nulrdd, 'no' = \-nulrdd\- option
 - **nodefgt=&lt;bool&gt;** 'yes' = \-nodefgt, 'no' = \-nodefgt\- option
 - **map=&lt;bool&gt;** 'yes' = \-map, 'no' = \-map\- option
 - **hbcppmm=&lt;bool&gt;** 'yes' = \-hbcpmm, 'no' = \-hbcpmm\- option
 - **implib=&lt;bool&gt;** 'yes' = \-implib, 'no' = \-implib\- option
 - **winuni=&lt;bool&gt;** 'yes' = \-winuni, 'no' = \-winuni\- option
 - **strip=&lt;bool&gt;** 'yes' = \-strip, 'no' = \-strip\- option
 - **run=&lt;bool&gt;** 'yes' = \-run, 'no' = \-run\- option
 - **inc=&lt;bool&gt;** 'yes' = \-inc, 'no' = \-inc\- option
 - **safe=&lt;bool&gt;** 'yes' = \-safe, 'no' = \-safe\- option
 - **cpp=** O mesmo que a opción \-cpp=
 - **warn=** O mesmo que a opción \-warn=
 - **compr=** O mesmo que a opción \-compr=
 - **head=** O mesmo que a opción \-head=
 - **plugins=** space separated list of hbmk2 plugins to load
 - **gt=&lt;name&gt;** O mesmo que a opción \-gt&lt;name&gt;
 - **gtdef=&lt;name&gt;** set the default GT to be used
 - **env=** O mesmo que a opción \-env:
 - **deppkgname=** O mesmo que a opción \-deppkgname=
 - **depkeyhead=** O mesmo que a opción \-depkeyhead=
 - **depoptional=** O mesmo que a opción \-depoptional=
 - **depcontrol=** O mesmo que a opción \-depcontrol=
 - **depincroot=** O mesmo que a opción \-depincroot=
 - **depincpath=** O mesmo que a opción \-depincpath=
 - **depincpathlocal=** O mesmo que a opción \-depincpathlocal=
 - **depimplibs=** O mesmo que a opción \-depimplibs=
 - **depimplibd=** O mesmo que a opción \-depimplibd=
 - **depfinish=** O mesmo que a opción \-depfinish=
 - **name=** Nome do paquete
 - **description=** Descripción do paquete
 - **version=&lt;x\.y\.z&gt;** package version number, where x,y,z &gt;= 0 &lt;= 255\. Defaults to 0\.0\.1, if not specified\.
 - **keywords=** Lista de palabras chave separadas por espacios
 - **licences=** Lista de licencias separadas por espacios
 - **repository=** space separated list of source repository references


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unknown file extensions are passed to Harbour compiler\)\.
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
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Output text to stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Output text to stderr\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Output text to stdout without any formatting\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
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
 - **"nCOMPVer"** see HB\_COMPILER\_VER envvar
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
 - **"nExitCode"** Código actual de saída
  
Shell API available in Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Clipper compatibility \(non\-Unicode\) mode\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Display list of loaded Harbour header\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load package\. Similar to \#request PP directive\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar paquete\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
List of loaded packages\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.


Examples to start with hbmk2:


 - **To run the interactive shell \('dot' prompt\)**  
$ hbmk2 \.
 - **To run a Harbour script**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Examples to build and run Harbour portable binary \(aka precompiled Harbour script\):


 - **Para construir**  
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


 - **0** Sen error
 - **1** Plataforma descoñecida
 - **2** Compilador descoñecido
 - **3** Fallou a detección de Harbour
 - **5** failed stub creation
 - **6** Fallou a compilación \(Harbour, compilador C, compilador de recursos\)
 - **7** Fallo na montaxe final \(o enlazador ou o xestor de bibliotecas\)
 - **8** Non soportado
 - **9** Fallo ao crear directorio de traballo
 - **19** axuda
 - **10** Dependencia non atopada ou desactivada
 - **20** Inicialización de complemento
 - **30** Aniñamento demasiado profundo\.
 - **50** stop requested
 - **&lt;other&gt;** cando a opción \-run e usada, o código de saída será retornado polo executable
  
Notes:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Source filename without extension will load the \.hbp file, if such \.hbp file exists in current directory\. If not, \.prg extension will be used\.
  - Multiple \-l, \-L, \-i and &lt;script&gt; parameters are accepted\.
  - Regular Harbour compiler options are also accepted as is\.  
\(see them with \-harbourhelp option\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - hbmk\.hbm make script in current directory is always processed if it exists\.
  - Using forwards slashes is recommended in option values as directory separator, but backslashes are also equally accepted\.
  - Filters are accepted in each \.hbc line and most options\.  
Filters can be combined using '&amp;' \(and\), '|' \(or\) operators, negated by '\!' operator and grouped by parentheses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Most \.hbc lines \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) and corresponding command\-line parameters will accept macro variables\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Options accepting macro variables also support command substitution\. Enclose command inside \`\`, and, if the command contains space, also enclose in double quotes\. Standard output of the command will be used as the value\. F\.e\. "\-cflag=\`wx\-config \-\-cflags\`", or ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Libraries and object files built with/for CA\-Cl\*pper will not work with any supported platform/compiler\.
  - Defaults and feature support may vary by platform/compiler\.
  - Non se necesita GNU Make, ningunha ferramenta de tipo make específica do compilador C, nin MSYS \(en Windows\) para executar hbmk2\.
  - \. \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Pose usar &lt;Alt\+V&gt; no modo de consola interactiva de Harbour para pegar texto dende o portapapeis\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'win' host platform\.


Supported &lt;compiler&gt; values for each supported &lt;platform&gt; value:


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
  
License:  


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
their web site at http://www\.gnu\.org/\)\.  
  
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
    Creative Commons Attribution\-ShareAlike 3\.0:  
    http://creativecommons\.org/licenses/by\-sa/3\.0/  

  
Autor:  


 - Viktor Szakáts \(harbour syenar\.net\) 
