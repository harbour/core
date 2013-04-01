Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  
Traduzione \(it\): \(inserisci qui il tuo nome\)  

Sintassi:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descrizione:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will autodetect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Opzioni:  


 - **\-o&lt;outname&gt;** nome del file di output
 - **\-l&lt;libname&gt;** Il link con la libreria &lt;libname&gt;\. &lt;libname&gt; dovrebbe essere senza percorso, estensione e prefisso 'lib' \(a meno che non faccia parte del nome\)\. Non aggiungere librerie Harbour di base, esse vengono aggiunte automaticamente, se necessario\. Se &lt;libname&gt; inizia con il carattere '\-', la libreria verrà rimossa dalla lista delle librerie durante la fase del link\.
 - **\-L&lt;libpath&gt;** additional path to search for libraries
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** additional path to search for headers
 - **\-static|\-shared** link with static/shared libs
 - **\-gt&lt;name&gt;** link with GT&lt;name&gt; GT driver, can be repeated to link with more GTs\. First one will be the default at run\-time
 - **\-inc\[\-\]** abilita/disabilita la compilazione incrementale \(default: disabilitata\)
 - **\-hbexe** create executable \(default\)
 - **\-hblib** create static library
 - **\-hbdyn** create dynamic library \(without linked Harbour VM\)
 - **\-hbdynvm** create dynamic library \(with linked Harbour VM\)


 - **\-mt|\-st** link with multi/single\-thread Harbour VM
 - **\-gui|\-std** create GUI/console executable
 - **\-main=&lt;mainfunc&gt;** override the name of starting function/procedure
 - **\-request=&lt;func&gt;** force function/procedure to be linked
 - **\-fullstatic** linkare con una libreria statica
 - **\-pic\[\-\]** create position independent object code \(always enabled in \-hbdyn/\-hbdynvm modes\)
 - **\-\[full|fix\]shared** create shared Harbour binaries without/with absolute dir reference to Harbour library \(default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise\) \(fix/full option in \*nix only\)
 - **\-nulrdd\[\-\]** link with nulrdd
 - **\-debug\[\-\]** add/exclude C compiler debug info\. For Harbour level debug, use Harbour option \-b as usual
 - **\-optim\[\-\]** toggle C compiler optimizations \(default: on\)
 - **\-cpp\[\-\]** forza il modo C\+\+/C
 - **\-cpp=&lt;value&gt;** select C\+\+ mode\. Allowed values are: def, yes, no
 - **\-map\[\-\]** create \(or not\) a map file
 - **\-implib\[\-\]** crea \(o non\) una libreria d'importazione \(in modalità \-hbdyn/\-hbexe\)\. Al nome verrà aggiunto un suffisso\.
 - **\-implib=&lt;output&gt;** crea la libreria di importazione \(in modalità \-hbdyn/\-hbexe\) di nome &lt;output&gt; \(default: identica all'output\)
 - **\-ln=&lt;link&gt;** create symbolic link pointing to &lt;output&gt; \(&lt;link&gt; is considered relative to &lt;output&gt;\)
 - **\-strip\[\-\]** strip \(no strip\) binaries
 - **\-trace\[\-\]** mostra comandi eseguiti
 - **\-beep\[\-\]** attiva \(o disattiva\) un beep singolo in caso di successo in uscita, doppio beep in caso di errore
 - **\-ignore\[\-\]** ignore errors when running compiler tools \(default: off\)
 - **\-hbcppmm\[\-\]** override standard C\+\+ memory management functions with Harbour ones
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI compilation modes \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** non si devono usare librerie statiche principali di Harbour durante il linking
 - **\-nodefgt\[\-\]** do not link default GTs \(effective in \-static mode\)
 - **\-nolibgrouping\[\-\]** disable library grouping on gcc based compilers
 - **\-nomiscsyslib\[\-\]** non aggiungere un'ulteriore elenco di librerie di sistema alla lista della libreria di default
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
 - **\-manifest=&lt;file&gt;** embed manifest &lt;file&gt; in executable/dynamic lib \(Windows only\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both autodetected\.
 - **\-signpw=&lt;pw&gt;** use &lt;pw&gt; as password when signing executable \(Windows and Darwin only\)
 - **\-instfile=&lt;g:file&gt;** add &lt;file&gt; in to the list of files to be copied to path specified by \-instpath option\. &lt;g&gt; is an optional copy group \(case sensitive\), it must be at least two characters long\. In case you do not specify &lt;file&gt;, the list of files in that group will be emptied\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** abilita \(o disabilita\) la generazione della libreria d'importazione per i sorgenti della libreria d'importazione specificata nelle opzioni \-deimpslib= \(default: si\)
 - **\-stop\[=&lt;text&gt;\]** stop senza alcuna azione e visualizzazione di &lt;text&gt; se specificato
 - **\-echo=&lt;text&gt;** visualizzazione del testo sullo schermo
 - **\-pause** force waiting for a key on exit in case of failure \(with alternate GTs only\)
 - **\-exitstr** mostra gli errori all'uscita come testo leggibile
 - **\-info** turn on informational messages
 - **\-quiet\[\-\]** elimina tutti i messaggi sullo schermo


 - **\-bldf\[\-\]** inherit all/no \(default\) flags from Harbour build
 - **\-bldf=\[p\]\[c\]\[l\]** inherit \.prg/\.c/linker flags \(or none\) from Harbour build
 - **\-F&lt;framework&gt;** link with &lt;framework&gt; framework \(Darwin only\)
 - **\-prgflag=&lt;f&gt;** pass single flag to Harbour compiler
 - **\-cflag=&lt;f&gt;** pass single flag to C compiler
 - **\-resflag=&lt;f&gt;** pass single flag to resource compiler \(Windows only\)
 - **\-ldflag=&lt;f&gt;** pass single flag to linker \(executable\)
 - **\-dflag=&lt;f&gt;** pass single flag to linker \(dynamic library\)
 - **\-aflag=&lt;f&gt;** pass single flag to linker \(static library\)
 - **\-iflag=&lt;f&gt;** pass single flag to import library creation command
 - **\-signflag=&lt;f&gt;** pass single flag to code sign command
 - **\-runflag=&lt;f&gt;** pass single flag to output executable when \-run option is used
 - **\-cflag\+=&lt;f&gt;** pass single flag to C compiler overriding C compiler flags added by hbmk2 itself\. Use with caution\.
 - **\-ldflag\+=&lt;f&gt;** pass single raw option to linker \(executable\) after the library list\. Use with caution\.
 - **\-dflag\+=&lt;f&gt;** passa una singola opzione direttamente al linker \(libreria dinamica\) dopo la lista di libreria\. Usare con cautela\.
 - **\-3rd=&lt;f&gt;** options/flags reserved for 3rd party tools, always ignored by hbmk2 itself
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alter local environment\. &lt;e&gt; is the name of the environment variable to alter\. &lt;o&gt; can be '=' to set/override, '\-' to delete, '\+' to append to the end of existing value, '\#' to insert to the beginning of existing value\. &lt;v&gt; is the value to set/append/insert\.
 - **\-jobs=&lt;n&gt;** start n compilation threads \(multiprocess platforms only\)
 - **\-head=&lt;m&gt;** control source header parsing \(in incremental build mode\)  
&lt;m&gt; can be: native \(uses compiler to extract dependencies\), full \(default, uses simple text parser on the whole file\), dep, off
 - **\-rebuild** rebuild \(in incremental build mode\)
 - **\-rebuildall** Ricompila con i sotto\-progetti \(in compilazione incrementale\)
 - **\-clean** clean \(in incremental build mode\)
 - **\-workdir=&lt;dir&gt;** working directory  
\(default: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] in incremental mode, OS temp directory otherwise\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** create import library \(Windows only\)


 - **\-hbl\[=&lt;output&gt;\]** output \.hbl filename\. %\{hb\_lng\} macro is accepted in filename
 - **\-lng=&lt;languages&gt;** list of languages to be replaced in %\{hb\_lng\} macros in \.pot/\.po filenames and output \.hbl/\.po filenames\. Comma separated list:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** create/update \.po file from source\. Merge it with previous \.po file of the same name
 - **\-minipo\[\-\]** do \(not\) add Harbour version number and source file reference to \.po \(default: add them\)
 - **\-rebuildpo** recreate \.po file, thus removing all obsolete entries in it


 - **\-hbx=\[&lt;\.ch&gt;\]** Creare l'intestazione Harbour \(in formato\. HBX\), con tutti i simboli esterni\. Il parametro vuoto lo disattiva\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; is a header file name\. &lt;\.hbc&gt; is a \.hbc filename to be automatically included in case the header is found in any of the compiled sources\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; is the name of the dependency\. &lt;n&gt; name of the package dependency\. Can be specified multiple times\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; is the name of the dependency\. &lt;h&gt; is the key header \(\.h\) of the package dependency\. Multiple alternative headers can be specified\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; è il nome della dipendenza\. &lt;f&gt; può essere 'yes' o 'no', specifica se la dipendenza è opzionale\. Default: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; is the name of the dependency\. Set &lt;r&gt; as root directory for paths specified in \-depincpath options\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;dll&gt; to the import library source list\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; is the name of the dependency\. Set generated import library name to &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** add plugin\. &lt;filename&gt; can be: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** pass input file to plugins
 - **\-pflag=&lt;f&gt;** pass single flag to plugins
  
Opzioni riportate di seguito sono disponibili da riga di comando:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** stop after running Harbour compiler
 - **\-hbcmp|\-clipper** stop after creating the object files  
create link/copy hbmk2 to hbcmp/clipper for the same effect
 - **\-hbcc** accept raw C flags  
create link/copy hbmk2 to hbcc for the same effect
 - **\-hblnk** accept raw linker flags
 - **\-autohbm\[\-\]** enable \(or disable\) processing of hbmk\.hbm in current directory \(default: yes\)
 - **\-hb10** abilita la compatibilità Harbour 1\.0\.x
 - **\-hb20** enable Harbour 2\.0\.x compatibility mode
 - **\-hb30** enable Harbour 3\.0\.x compatibility mode
 - **\-xhb** enable xhb mode
 - **\-hbc** enable pure C mode
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emulate Clipper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect


 - **\-hbreg\[=global\]** register Harbour Script \(\.hb\) with hbmk2 \(Windows only\)
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
 - **\-build=&lt;name&gt;** specificare un nome di compilazione
 - **\-lang=&lt;lang&gt;** override default language\. &lt;lang&gt; is an ISO language code\.
 - **\-width=&lt;n&gt;** imposta la larghezza dell'output a &lt;n&gt; caratteri \(0=illimitato\)
 - **\-shl** show sub\-project level in output lines
 - **\-viewhelp** Help esteso nel visualizzatore di testo
 - **\-longhelp** aiuto esteso
 - **\-longhelpmd** Help esteso in formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** Harbour compiler credits
 - **\-build** Harbour compiler build information
 - **\-version** display version header only
  
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
 - **clipper** modo \-hbcmp \(emulazione compilatore Clipper\)
 - **rtlink** modo \-rtlink \(emulazione linker di Clipper\)
 - **exospace** modo \-rtlink \(emulazione linker di Clipper\)
 - **blinker** modo \-rtlink \(emulazione linker di Clipper\)
 - **\*10** opzione \-hb10
 - **\*20** opzione \-hb20
 - **\*30** opzione \-hb30
 - **x\*** opzione \-xhb
 - **hbcmp\*|\*hbcmp** mode \-hbcmp \(emulate Harbour compiler producing a binary object\)
 - **hbcc\*|\*hbcc** mode \-hbcc \(emulate C compiler\)
 - **hblnk\*|\*hblnk** mode \-hblnk \(emulate C linker\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Files:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** collection of options\. Can be used to collect common ones into a file and include that into project files\. Uses same format as \.hbp files\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate lines\.
 - **\*\.ch** if passed directly as a source file, it will be used as additional standard header
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: %APPDATA%\\\.harbour, &lt;hbmk2 cartella&gt;
 - **hbmk\.hbm** optional \.hbm file residing in current working directory, which gets automatically processed before other options
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Script di Harbour
 - **\*\.hrb** Harbour portable binary \(aka precompiled Harbour script\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbmk2 cartella&gt;
 - **shell plugins** plugins \.hb e \.hrb per la shell interattiva di Harbour\. Possono essere situati in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** stores command history for interactive Harbour shell\. You can disable history by making the first line 'no' \(without quotes and with newline\)\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
  
Macro variables:  


 - **$\{hb\_root\}** directory of hbmk2
 - **$\{hb\_dir\}** directory of the filename it is used in
 - **$\{hb\_dirname\}** top directory of the filename it is used in
 - **$\{hb\_name\}** nome del file in uso \(senza cartella ed estensione\)
 - **$\{hb\_self\}** nome completo del file utilizzato
 - **$\{hb\_curdir\}** attuale cartella di lavoro
 - **$\{hb\_tempdir\}** OS directory for temporary files
 - **$\{hb\_targetname\}** name of the project \(without directory and extension\)\. Returns \.adhoc\. if there is not project file\.
 - **$\{hb\_targettype\}** type of the project \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** piattaforma selezionata
 - **$\{hb\_comp\}** Compilatore C selezionato
 - **$\{hb\_comp\_ver\}** Versione del compilatore C
 - **$\{hb\_build\}** build name
 - **$\{hb\_cpu\}** CPU selezionata
 - **$\{hb\_work\}** nome di default della cartella di lavoro
 - **$\{hb\_workdynsub\}** sottocartella di lavoro di default per la libreria dinamica di destinazione
 - **$\{hb\_dynprefix\}** prefisso della libreria dinamica
 - **$\{hb\_dynsuffix\}** suffisso della libreria dinamica
 - **$\{hb\_dynext\}** estensione della libreria dinamica
 - **$\{hb\_ver\}** Harbour version in hexadecimal triple byte format\. F\.e\.: 030200
 - **$\{hb\_verstr\}** Harbour version in human readable format &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. F\.e\.: 3\.2\.0dev
 - **$\{hb\_major\}** numero primario di versione Harbour
 - **$\{hb\_minor\}** numero secondario di versione Harbour
 - **$\{hb\_release\}** numero di versione di rilascio di Harbour
 - **$\{hb\_status\}** Harbour version status
 - **$\{hb\_revision\}** Revisione di Harbour
 - **$\{hb\_host\_plat\}** Harbour host platform
 - **$\{hb\_host\_plat\_unix\}** ritorna "1" se la piattaforma host Harbour è \*nix compatibile
 - **$\{hb\_bin\}** cartella dei file binari Harbour
 - **$\{hb\_lib\}** Cartella di Harbour della libreria statica
 - **$\{hb\_lib3rd\}** Harbour 3rd party static library directory
 - **$\{hb\_dyn\}** Cartella di Harbour della libreria dinamica
 - **$\{hb\_inc\}** cartella degli header Harbour
 - **$\{hb\_addons\}** cartella base degli add\-ons Harbour
 - **$\{hb\_first\}** name of source file that holds the entry function \(without directory and extension\)
 - **$\{hb\_outputdir\}** directory dell'output
 - **$\{hb\_outputname\}** nome dell'output \(senza estensione\)
 - **$\{hb\_level\}** livello di recursione del sub\-progetto
 - **$\{&lt;depname&gt;\}** restituisce la directory di intestazione della dipendenza &lt;depname&gt;, o '1 ', se essa non viene rilevata
 - **$\{&lt;envvar&gt;\}** ritorna il valore della variabile d'ambiente &lt;envvar&gt;
  
Filters \(you can combine and/or negate them\):  


 - **\{&lt;platform&gt;\}** target platform\. Where &lt;platform&gt; can be any value accepted by \-plat= option\.
 - **\{&lt;compiler&gt;\}** target C compiler\. Where &lt;compiler&gt; can be any value accepted by \-comp= option\.
 - **\{&lt;cpu&gt;\}** CPU di destinazione\. Dove &lt;cpu&gt; può essere una tra i seguenti: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** destinazione GUI \(vedi l'opzone \-gui\)
 - **\{std\}** console target \(see \-console option\)
 - **\{debug\}** è abilitato il debug a livello C \(vedi l'opzione \-debug\)
 - **\{nodebug\}** C level debugging is disabled \(see \-debug\- option\)
 - **\{shared\}** shared build \(see \-shared and related options\)
 - **\{static\}** static build \(see \-static and related options\)
 - **\{lngcpp\}** forced C\+\+ mode \(see \-cpp option\)
 - **\{lngc\}** modalità C forzata \(vedere l'opzione \-cpp\-\)
 - **\{winuni\}** Modo Windows UNICODE \(WIDE\) \(vedere l'opzione \-winuni\)
 - **\{winansi\}** Modalità ANSI Windows \(vedere l'opzione \-winuni\-\)
 - **\{unix\}** target platform is \*nix compatible \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** la piattaforma di destinazione è compatibile con Windows \(win, wce\)
 - **\{allgcc\}** target C compiler belongs to gcc family \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** target C compiler is mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** target C compiler is msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** target C compiler is bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** il compilatore dell'eseguibile C è pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** il compilatore dell'eseguibile C è icc\* \(icc, iccia64\)
 - **\{hb10\}** modalità di comaptibilità Harbour 1\.0\.x \(vedi l'opzione \-hb10\)
 - **\{hb20\}** modalità di comaptibilità Harbour 2\.0\.x \(vedi l'opzione \-hb20\)
 - **\{hb30\}** modalità di comaptibilità Harbour 3\.0\.x \(vedi l'opzione \-hb30\)
 - **\{xhb\}** modalità xhb \(vedi l'opzione \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** filter will pass if &lt;file&gt; or &lt;dir&gt; name exists on disk\.
 - **\{MACRO\}** filter will pass if $\{MACRO\} value is not empty and not equal to '0' or 'no' \(case insensitive\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is larger than &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is smaller than &lt;value&gt; \(case insensitive\)\.


Predefined constants in sources:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** quando uno scripr \.hb viene compilato come plugin hbmk2
 - **\_\_HBEXTREQ\_\_** when an \.hbx source file is present in a project \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** dove la dipendenza &lt;depname&gt; è stata rilevata \(disponibile nei sorgenti C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** quando un file sorgente Harbour è eseguito come uno script di shell
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, ecc\.\.\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\):


 - **HBMK\_HAS\_&lt;depname&gt;** quando la dipendenza &lt;depname&gt; è stata rilevata
 - **HBMK\_DIR\_&lt;depname&gt;** return the header directory where &lt;depname&gt; was detected, or empty if it was not\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** quando la dipendenza &lt;depname&gt; è stata rilevata nella locazione configurata da \-depincpathlocal= opzione
  
Variabili d'ambiente:  


 - **HBMK\_OPTIONS** accepts any options as if they were passed in the beginning of the command\-line
 - **HB\_PLATFORM** accetta gli stessi valori come l'opzione \-plat=
 - **HB\_COMPILER** accetta gli stessi valori come l'opzione \-comp=
 - **HB\_CPU** accetta gli stessi valori come l'opzione \-cpu=
 - **HB\_BUILD\_NAME** accetta gli stessi valori come l'opzione \-build=
 - **HB\_LANG** accetta gli stessi valori come l'opzione \-lang=
 - **HB\_USER\_LIBS** accetta gli stessi valori \(separati da uno spazio\) come l'opzione \-l
 - **HB\_USER\_LIBPATHS** accetta gli stessi valori \(separati da uno spazio\) come l'opzione \-L
 - **HB\_USER\_PRGFLAGS** opzioni da passare al compilatore Harbour \(prima delle opzioni nella riga di comando\)
 - **HB\_USER\_CFLAGS** opzioni da passare al compilatore C \(prima delle opzioni nella riga di comando\)
 - **HB\_USER\_RESFLAGS** opzioni da passare al compilatore di risorse \(prima delle opzioni nella riga di comando\) \(solo Windows\)
 - **HB\_USER\_LDFLAGS** options to be passed to linker \(executable\) \(before command\-line options\)
 - **HB\_USER\_DFLAGS** options to be passed to linker \(dynamic library\) \(before command\-line options\)
 - **HB\_USER\_AFLAGS** options to be passed to linker \(static library\) \(before command\-line options\)
 - **HB\_COMPILER\_VER** override C compiler version autodetection \(gcc and msvc compiler families only\)\. Format: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** override C compiler executable directory \(gcc compiler families only\)
 - **HB\_CCPREFIX** override C compiler executable prefix \(gcc compiler families only\)
 - **HB\_CCSUFFIX** override C compiler executable suffix \(gcc compiler families only\)
 - **HB\_INSTALL\_PREFIX** override Harbour base installation directory
 - **HB\_INSTALL\_ADDONS** override Harbour base add\-ons directory


 - **HB\_EXTENSION** lista di estensioni separate da spazio da caricare nella shell interattiva di Harbour
  
\.hbc direttive \(essi dovrebbero essere scritte in righe separate\):  


 - **echo=&lt;msg&gt;** display &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** skip processing the rest of the \.hbc file\. Display &lt;msg&gt;, if specified\.
 - **stop=\[&lt;msg&gt;\]** ferma la compilazione\. Visualizza &lt;msg&gt;, se specificato\.
 - **sources=** aggiunge come file di input una lista di file separati da spazio
 - **headers=** aggiungere un elenco di intestazioni \.ch separate da spazio come intestazione standard
 - **libs=** Aggiungi un elenco di librerie, separate da uno spazio \(più informazioni con l'opzione \-l\)
 - **frameworks=** aggiunge una lista di strutture separate da spazio \(solo Darwin\)
 - **requests=** add space separated list of symbols to force link to the build target
 - **syslibs=** add space separated list of libraries as system libraries \(before regular libraries\)
 - **hbcs=** embed space separated list of \.hbc files\. Names without the extension is accepted\. These references are processed in place\.
 - **autohbcs=** lista di valori separati da spazio come nell'opzione \-autohbc=
 - **libpaths=** lista di ulteriori percorsi di libreria separati da spazio
 - **incpaths=** add space separated list of additional header paths \(for both Harbour and C\)
 - **instfiles=** lista di valori separati da spazio come nell'opzione \-instfile=
 - **instpaths=** lista di valori separati da spazio come nell'opzione \-instpath=
 - **prgflags=** lista di valori separati da spazio come nell'opzione \-prgflag=
 - **cflags=** lista di valori separati da spazio come nell'opzione \-cflag=
 - **resflags=** lista di valori separati da spazio come nell'opzione \-resflag=
 - **ldflags=** lista di valori separati da spazio come nell'opzione \-ldflag=
 - **ldflags\+=** lista di valori separati da spazio come nell'opzione \-ldflag\+=
 - **dflags=** lista di valori separati da spazio come nell'opzione \-dflag=
 - **dflags\+=** lista di valori separati da spazio come nell'opzione \-dflag\+=
 - **pflags=** lista di valori separati da spazio come nell'opzione \-pflag=
 - **psources=** lista di valori separati da spazio come nell'opzione \-pi=
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
 - **cpp=** come l'opzione \-cpp=
 - **warn=** come l'opzione \-warn=
 - **compr=** come l'opzione \-compr=
 - **head=** come l'opzione \-head=
 - **plugins=** lista separata da spazi di plugin hbmk2 da caricare
 - **gt=&lt;name&gt;** come l'opzione \-gt&lt;name&gt;
 - **gtdef=&lt;name&gt;** setta il GT di default da utilizzare
 - **env=** come l'opzione \-env:
 - **deppkgname=** come l'opzione \-deppkgname=
 - **depkeyhead=** come l'opzione \-depkeyhead=
 - **depoptional=** come l'opzione \-depoptional=
 - **depcontrol=** come l'opzione \-depcontrol=
 - **depincroot=** come l'opzione \-depincroot=
 - **depincpath=** come l'opzione \-depincpath=
 - **depincpathlocal=** come l'opzione \-depincpathlocal=
 - **depimplibs=** come l'opzione \-depimplibs=
 - **depimplibd=** come l'opzione \-depimplibd=
 - **name=** Nome del pacchetto
 - **description=** descrizione del pacchetto
 - **version=&lt;x\.y\.z&gt;** package version number, where x,y,z &gt;= 0 &lt;= 255\. Defaults to 0\.0\.1, if not specified\.
 - **keywords=** lista di parole chiave separate da spazio
 - **licences=** space separated list of licenses
 - **repository=** lista di riferimenti, separati da spazio, ai repository sorgenti


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unknown file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Aggiunge al progetto un file Harbour di input
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Aggiungere in input un file \.C al progetto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Aggiungere al progetto un file C\+\+ in input\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Aggiunge al progetto un file di risorsa di input
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Aggiunge al progetto un file binario oggetto\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Add a file to be installed, with an optional \-instpath= group name\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Invio del testo su stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Invio del testo su stderr
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Invia testo a stdout senza alcuna formattazione\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Invia testo a stderr senza alcuna formattazione\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Esegue la macro espressione hbmk2\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Escape/quote filename for using it as external command parameter\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target platform/C compiler\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Convertire il nome del file con lo "slash" come separatore di cartella
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Return relative path of \-workdir= value from current working directory\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Find file in &lt;xPath&gt; \(array or pathsep delimited string are accepted\) with list of &lt;aExtDef&gt; alternate extensions \(defaults to executable binaries\)\. Returns filename if found and NIL if not\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Cambia cartella e/o estensione nel nome file
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Encode function name according to Harbour compiler rules for forming HB\_FUNC\(\) function names in C code\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Elimina da una stringa la coppia di delimitatori
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convert array of strings to a string\. Default separator is a single space\.


Plugin variables:  
\('hbmk' context hash items, case\-sensitive, read\-only unless marked otherwise\)


 - **"apiver"** varsione API come intero
 - **"cSTATE"** callback state\. Can be: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** array of parameters passed to plugins via \-pflag=/pi= options or having an extension registered via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** hash of plugin custom variables\. Writable, local to each plugin
 - **"cPLAT"** valore \-plat
 - **"cCOMP"** valore \-comp
 - **"nCOMPVer"** see HB\_COMPILER\_VER envvar
 - **"cCPU"** valore \-cpu
 - **"cBUILD"** valore \-build=
 - **"cOUTPUTNAME"** valore \-o
 - **"cTARGETNAME"** see $\{hb\_targetname\} macro
 - **"cTARGETTYPE"** see $\{hb\_targettype\} macro
 - **"lREBUILD"** \-rebuild stato dell'opzione
 - **"lCLEAN"** \-clean stato dell'opzione
 - **"lDEBUG"** \-debug stato dell'opzione
 - **"lMAP"** \-map stato dell'opzione
 - **"lSTRIP"** \-strip stato dell'opzione
 - **"lDONTEXEC"** \-traceonly stato dell'opzione
 - **"lIGNOREERROR"** \-ignore stato dell'opzione
 - **"lTRACE"** \-trace stato dell'opzione
 - **"lQUIET"** \-q stato dell'opzione
 - **"lINFO"** \-info stato dell'opzione
 - **"lBEEP"** \-beep stato dell'opzione
 - **"lRUN"** \-run stato dell'opzione
 - **"lINC"** \-inc stato dell'opzione
 - **"cCCPATH"** see HB\_CCPATH envvar
 - **"cCCPREFIX"** see HB\_CCPREFIX envvar
 - **"cCCSUFFIX"** see HB\_CCSUFFIX envvar
 - **"cCCEXT"** see HB\_CCEXT envvar
 - **"cWorkDir"** valore \-workdir=
 - **"nExitCode"** Codice di uscita attuale
  
Shell API available in Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambia GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Abilita la modialità di compatibilità Clipper \(non\-Unicode\)
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Visualizza l'elenco di intestazione di Harbour caricato\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica un pacchetto\. Simile alla direttiva PP \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload package\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista dei pacchetti caricati
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.


Esempi per partire con hbmk2:


 - **To run the interactive shell \('dot' prompt\)**  
$ hbmk2 \.
 - **To run a Harbour script**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Esempi per compilare e lanciare un eseguibile portabile Harbour \(ossia uno script Harbour precompilato\):


 - **To build**  
$ hbmk2 \-gh myscript\.hb
 - **To run result of above**  
$ hbmk2 myscript\.hrb \[&lt;parameter\[s\]&gt;\]


Examples to build a Harbour application:


 - **Per compilare un singolo \.prg**  
$ hbmk2 hello\.prg
 - **Per compilare in modalità incrementale più di un sorgente \.prg in una singola applicazione**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **To build an application using a project file**  
$ hbmk2 myapp\.hbp
 - **To build an application using incremental mode**  
$ hbmk2 myapp\.hbp \-inc
 - **To build an application which uses a contrib package or 3rd party \(add\-on\) package that ships with an \.hbc file**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Per compilare un'applicazione che utilizza direttamente una libreria**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Per compilare un'applicazione che usa una risorsa Windows**  
$ hbmk2 mymain\.prg myres\.rc
 - **Per compilare un'applicazione che si linka a librerie dinamiche Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Per compilare un'applicazione esterna a tutti i sorgenti \.prg e \.c residenti nella sottocartella 'source'**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Esempi per la compilazione di una libreria statica Harbour:


 - **To build library 'mylib' from sources**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Per compilare la libreria 'mylib' dai sorgenti usando il modo incrementale**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Exit codes \("errorlevels"\):  


 - **0** Nessun errore
 - **1** piattaforma sconosciuta
 - **2** compilatore sconosciuto
 - **3** Individuazione fallita di Harbour
 - **5** failed stub creation
 - **6** failed in compilation \(Harbour, C compiler, Resource compiler\)
 - **7** failed in final assembly \(linker or library manager\)
 - **8** non sopportato
 - **9** Impossibile creare la cartella di lavoro
 - **19** aiuto
 - **10** Dipendenza omessa o disabilitata
 - **20** plugin initialization
 - **30** nidificazione troppo profonda
 - **50** richiesta di stop
 - **&lt;other&gt;** when \-run option is used, the exit code will be the one returned by the target executable
  
Note:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Source filename without extension will load the \.hbp file, if such \.hbp file exists in current directory\. If not, \.prg extension will be used\.
  - Multiple \-l, \-L, \-i and &lt;script&gt; parameters are accepted\.
  - Regular Harbour compiler options are also accepted as is\.  
\(see them with \-harbourhelp option\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - se esistente, lo script make hbmk\.hbm nella cartella corrente è sempre processato
  - Using forwards slashes is recommended in option values as directory separator, but backslashes are also equally accepted\.
  - Filters are accepted in each \.hbc line and most options\.  
Filters can be combined using '&amp;' \(and\), '|' \(or\) operators, negated by '\!' operator and grouped by parentheses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Most \.hbc lines \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) and corresponding command\-line parameters will accept macro variables\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Options accepting macro variables also support command substitution\. Enclose command inside \`\`, and, if the command contains space, also enclose in double quotes\. Standard output of the command will be used as the value\. F\.e\. "\-cflag=\`wx\-config \-\-cflags\`", or ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Libraries and object files built with/for CA\-Cl\*pper will not work with any supported platform/compiler\.
  - Defaults and feature support may vary by platform/compiler\.
  - GNU Make or any C compiler specific make tool and MSYS \(on Windows\) are not needed to run hbmk2\.
  - \. \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Puoi usare i tasti &lt;Alt\+V&gt; nella shell interativa di Harbour per incollare il testo dalla clipboard\.
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
  
Licenza:  


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

  
Autore:  


 - Viktor Szakáts \(harbour syenar\.net\) 
