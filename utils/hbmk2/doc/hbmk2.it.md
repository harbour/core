Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-11\-26 05:07\)  
Copyright &copy; 1999\-2014, Viktor Szakáts  
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
 - **\-static|\-shared** linkare con una libreria statica/condivisa
 - **\-gt&lt;name&gt;** link with GT&lt;name&gt; GT driver, can be repeated to link with more GTs\. First one will be the default at run\-time
 - **\-inc\[\-\]** abilita/disabilita la compilazione incrementale \(default: disabilitata\)
 - **\-hbexe** creazione dell'eseguibile \(predefinita\)
 - **\-hblib** creare la libreria statica
 - **\-hbdyn** create dynamic library \(without linked Harbour VM\)
 - **\-hbdynvm** create dynamic library \(with linked Harbour VM\)


 - **\-mt|\-st** link with multi/single\-thread Harbour VM
 - **\-gui|\-std** creare l'eseguibile GUI/console
 - **\-main=&lt;mainfunc&gt;** override the name of starting function/procedure
 - **\-request=&lt;func&gt;** forza la funzione/procedura da essere linkata
 - **\-fullstatic** linkare con una libreria statica
 - **\-pic\[\-\]** create position independent object code \(always enabled in \-hbdyn/\-hbdynvm modes\)
 - **\-\[full|fix\]shared** create shared Harbour binaries without/with absolute dir reference to Harbour library \(default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise\) \(fix/full option in \*nix only\)
 - **\-nulrdd\[\-\]** link with nulrdd
 - **\-debug\[\-\]** add/exclude C compiler debug info\. For Harbour level debug, use Harbour option \-b as usual
 - **\-optim\[\-\]** toggle C compiler optimizations \(default: on\)
 - **\-cpp\[\-\]** forza il modo C\+\+/C
 - **\-cpp=&lt;value&gt;** seleziona la modalità C\+\+\. Sono ammessi i valori: def, yes, no
 - **\-map\[\-\]** creare \(o no\) un file mappa
 - **\-implib\[\-\]** crea \(o non\) una libreria d'importazione \(in modalità \-hbdyn/\-hbexe\)\. Al nome verrà aggiunto un suffisso\.
 - **\-implib=&lt;output&gt;** crea la libreria di importazione \(in modalità \-hbdyn/\-hbexe\) di nome &lt;output&gt; \(default: identica all'output\)
 - **\-ln=&lt;link&gt;** create symbolic link pointing to &lt;output&gt; \(&lt;link&gt; is considered relative to &lt;output&gt;\)
 - **\-strip\[\-\]** strip \(no strip\) binaries
 - **\-trace\[\-\]** mostra comandi eseguiti
 - **\-beep\[\-\]** attiva \(o disattiva\) un beep singolo in caso di successo in uscita, doppio beep in caso di errore
 - **\-ignore\[\-\]** ignore errors when running compiler tools \(default: off\)
 - **\-hbcppmm\[\-\]** override standard C\+\+ memory management functions with Harbour ones
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI Windows API usage for C/C\+\+ input files \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** non usare librerie statiche principali di Harbour durante il linking
 - **\-nodefgt\[\-\]** do not link default GTs \(effective in \-static mode\)
 - **\-nolibgrouping\[\-\]** disable library grouping on gcc based compilers
 - **\-nomiscsyslib\[\-\]** non aggiungere un'ulteriore elenco di librerie di sistema alla lista della libreria di default
 - **\-traceonly** mostrare i comandi da essere eseguiti, ma non vengono eseguiti
 - **\-warn=&lt;level&gt;** set C compiler warning level  
&lt;level&gt; can be: max, yes, low, no, def \(default: yes\)
 - **\-safe\[\-\]** enable safety options in C compiler/linker \(default: enabled on Windows, disabled on other systems\)
 - **\-compr=&lt;level&gt;** compress executable/dynamic lib \(needs UPX tool\)  
&lt;level&gt; can be: yes, no, min, max
 - **\-run\[\-\]** eseguire/non eseguire l'eseguibile prodotto
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. VCS specific information is added as \_HBMK\_VCS\_&lt;TYPE&gt;\_\*\_ constants, where supported\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-bldhead=&lt;file&gt;** generate \.ch header file with build information, like build sequence number and timestamp\. Generated header will define preprocessor constants \_HBMK\_BUILD\_ID\_ with sequence number \(incremented on each build\) and \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build
 - **\-icon=&lt;file&gt;** set &lt;file&gt; as application icon\. &lt;file&gt; should be a supported format on the target platform \(not supported by some platforms/compilers\)\. On Windows, it is implemented by generating and linking a resource file\.
 - **\-manifest=&lt;file&gt;** embed manifest &lt;file&gt; in executable/dynamic lib \(Windows only\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both autodetected\.
 - **\-signpw=&lt;pw&gt;** Usare &lt;pw&gt; come password quando si accede all'eseguibile \(Solo Windows e Darwin\)
 - **\-signts=&lt;url&gt;** use &lt;url&gt; as trusted timestamp server\. Empty value resets it to the default: http://timestamp\.verisign\.com/scripts/timstamp\.dll
 - **\-instfile=&lt;g:file&gt;** add &lt;file&gt; in to the list of files to be copied to path specified by \-instpath option\. &lt;g&gt; is an optional copy group \(case sensitive\), it must be at least two characters long\. In case you do not specify &lt;file&gt;, the list of files in that group will be emptied\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** abilita \(o disabilita\) la generazione della libreria d'importazione per i sorgenti della libreria d'importazione specificata nelle opzioni \-deimpslib= \(default: si\)
 - **\-stop\[=&lt;text&gt;\]** stop senza alcuna azione e visualizzazione di &lt;text&gt; se specificato
 - **\-echo=&lt;text&gt;** visualizzazione del testo sullo schermo
 - **\-pause** force waiting for a key on exit in case of failure \(with alternate GTs only\)
 - **\-exitstr** mostra gli errori all'uscita come testo leggibile
 - **\-info** abilitare i messaggi di informazione
 - **\-quiet\[\-\]** elimina tutti i messaggi sullo schermo


 - **\-bldf\[\-\]** inherit all/no \(default\) flags from Harbour build
 - **\-bldf=\[p\]\[c\]\[l\]** inherit \.prg/\.c/linker flags \(or none\) from Harbour build
 - **\-F&lt;framework&gt;** Collega al &lt;framework&gt; framework \(Solamente Darwin\)
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
 - **\-rebuild** ricompilazione \(in modo incrementale\)
 - **\-rebuildall** Ricompila con i sotto\-progetti \(in compilazione incrementale\)
 - **\-clean** clean \(in compilazione incrementale\)
 - **\-workdir=&lt;dir&gt;** cartella di lavoro \(predefinita: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] in modo incrementale, altrimenti la cartella temporanea OS\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** creare la libreria d'importazione \(solamente WIndows\)


 - **\-hbl\[=&lt;output&gt;\]** output \.hbl filename\. %\{hb\_lng\} macro is accepted in filename
 - **\-lng=&lt;languages&gt;** list of languages to be replaced in %\{hb\_lng\} macros in \.pot/\.po filenames and output \.hbl/\.po filenames\. Comma separated list:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** create/update \.po file from source\. Merge it with previous \.po file of the same name
 - **\-minipo\[\-\]** do \(not\) add Harbour version number and source file reference to \.po \(default: add them\)
 - **\-rebuildpo** recreate \.po file, thus removing all obsolete entries in it


 - **\-hbx=\[&lt;\.ch&gt;\]** Creare l'intestazione Harbour \(in formato\. HBX\), con tutti i simboli esterni\. Il parametro vuoto lo disattiva\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; is a header file name\. &lt;\.hbc&gt; is a \.hbc filename to be automatically included in case the header is found in any of the compiled sources\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; è il nome della dipendenza\. &lt;n&gt; nome del pacchetto della dipendenza\. Può essere specificato più volte\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; is the name of the dependency\. &lt;h&gt; is the key header \(\.h\) of the package dependency\. Multiple alternative headers can be specified\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; è il nome della dipendenza\. &lt;f&gt; può essere 'yes' o 'no', specifica se la dipendenza è opzionale\. Default: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; p il nome della dipendenza\. Fissare &lt;r&gt; a cartella principale per i percorsi specificati nelle opzioni \-depincpath\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;dll&gt; to the import library source list\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; is the name of the dependency\. Set generated import library name to &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** aggiungere il plugin\. &lt;filename&gt; può essere: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** specificare il file di input ai plugin
 - **\-pflag=&lt;f&gt;** pass single flag to plugins
  
Le opzioni riportate di seguito sono disponibili da riga di comando:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** esecuzione fermata dopo l'esecuzione del compilatore Harbour
 - **\-hbcmp|\-clipper** stop after creating the object files  
create link/copy hbmk2 to hbcmp/clipper for the same effect
 - **\-hbcc** accept raw C flags  
create link/copy hbmk2 to hbcc for the same effect
 - **\-hblnk** accept raw linker flags
 - **\-autohbm\[\-\]** enable \(or disable\) processing of hbmk\.hbm in current directory \(default: yes\)
 - **\-hb10** abilita la compatibilità Harbour 1\.0\.x
 - **\-hb20** abilita la compatibilità Harbour 2\.0\.x
 - **\-hb30** abilita la compatibilità Harbour 3\.0\.x
 - **\-xhb** abilita modo xhb
 - **\-hbc** abilita modo C puro
 - **\-blinker** emulate Clipper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect
 - **\-exospace** see above
 - **\-rtlink** see above


 - **\-hbreg\[=global\]** register Harbour Script \(\.hb\) with hbmk2 \(Windows only\)
 - **\-hbunreg\[=global\]** elimina la registrazione dello Script Harbour \(\.hb\) da hbmk2 \(solo Windows\)


 - **\-find &lt;text&gt;** elenca tutte le funzioni conosciute di Harbour che contengono &lt;text&gt; nel loro nome, insieme con il loro pacchetto \(ignora maiuscole e minuscole, accetta più valori, può contenere caratteri jolly\)


 - **\-hbmake=&lt;file&gt;** Converte il progetto hbmake &lt;file&gt; in file \.hbp
 - **\-xbp=&lt;file&gt;** convert \.xbp \(xbuild\) project &lt;file&gt; to \.hbp file
 - **\-xhp=&lt;file&gt;** convert \.xhp \(xMate\) project &lt;file&gt; to \.hbp file


 - **\-\-hbdirbin** output Harbour binary directory to stdout
 - **\-\-hbdirdyn** output Harbour dynamic library directory to stdout
 - **\-\-hbdirlib** output Harbour static library directory to stdout
 - **\-\-hbdirinc** output Harbour header directory to stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** override default target platform \(default: automatic\)
 - **\-cpu=&lt;cpu&gt;** ignora la CPU predefinita di destinazione \(predefinito: automatico\) \(SPERIMENTALE\)
 - **\-comp=&lt;compiler&gt;** sovrascrive il riconoscimento automatico del compilatore C  
Valore speciale:  
\- bld: utilizza i settaggi originali \(default su \*nix\)
 - **\-build=&lt;name&gt;** specificare un nome di compilazione
 - **\-lang=&lt;lang&gt;** ignora la lingua predefinita\. &lt;lang&gt; è un codice ISO di lingua\.
 - **\-width=&lt;n&gt;** imposta la larghezza dell'output a &lt;n&gt; caratteri \(0=illimitato\)
 - **\-shl** show sub\-project level in output lines
 - **\-viewhelp** Help esteso nel visualizzatore di testo
 - **\-longhelp** aiuto esteso
 - **\-longhelpmd** Help esteso in formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** Riconoscimenti del compilatore Harbour
 - **\-build** Informazioni del compilatore Harbour
 - **\-version** visualizza solamente la versione dell'intestazione
  
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
 - **harbour** modo \-hbraw \(emulazione del compilatore Harbour \- raw \-\)
 - **clipper** modalità \-hbcmp \(emulazione compilatore Clipper\)
 - **rtlink** modalità \-rtlink \(emulazione linker di Clipper\)
 - **exospace** modalità \-rtlink \(emulazione linker di Clipper\)
 - **blinker** modalità \-rtlink \(emulazione linker di Clipper\)
 - **\*10** opzione \-hb10
 - **\*20** opzione \-hb20
 - **\*30** opzione \-hb30
 - **x\*** opzione \-xhb
 - **hbcmp\*|\*hbcmp** modalità \-hbcmp \(emula il compilatore Harbour producendo un oggetto binario\)
 - **hbcc\*|\*hbcc** modo \-hbcc \(emulazione compilatore Clipper\)
 - **hblnk\*|\*hblnk** modo \-hblnk \(emulazione linker di Clipper\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Files:  


 - **\*\.hbp** file di progetto\. Può contenere un numero qualsiasi di opzioni della linea di comando, che sono attesi nella creazione dell'output\. Le linee che iniziano con il carattere '\#' sono ignorate, il carattere di fine linea è opzionale e le opzioni sono separate da spazi, esattamente come per la linea di comando\. Occorre chiudere tra doppie virgolette le opzioni che contengono spazi\. Ciascun riferimento a file \.hbp sarà eseguito come sub\-progetto\.
 - **\*\.hbm** insieme di opzioni\. Può essere utilizzata per raccoglierle in un file ed includerle nei files di progetto\. Esso usa lo stesso formato \.hbp\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate line\.
 - **\*\.ch** se passato direttamente come file sorgente, esso sarà utilizzato come intestazione standard addizionale
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: %APPDATA%\\\.harbour, &lt;cartella hbmk2&gt;
 - **hbmk\.hbm** optional \.hbm file residing in current working directory, which gets automatically processed before other options
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Script di Harbour
 - **\*\.hrb** Harbour binario portabile \(conosciuto anche come script Harbour precompilato\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;cartella hbmk2&gt;
 - **shell plugins** plugins \.hb e \.hrb per la shell interattiva di Harbour\. Possono essere situati in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** memorizza la cronologia dei comandi per la shell interattiva di Harbour\. E' possibile disattivare la cronologia creando la prima linea 'no' \(senza virgolette e ritorno a capo\)\. E' contenuta in \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
  
Variabili macro:  


 - **$\{hb\_root\}** cartella di hbmk2
 - **$\{hb\_dir\}** la cartella del file è usata
 - **$\{hb\_dirname\}** cartella principale del nome del file è utilizzata
 - **$\{hb\_name\}** nome del file in uso \(senza cartella ed estensione\)
 - **$\{hb\_self\}** nome completo del file utilizzato
 - **$\{hb\_curdir\}** attuale cartella di lavoro
 - **$\{hb\_tempdir\}** Directory di sistema per i files temporanei
 - **$\{hb\_targetname\}** nome del progetto \(senza nome cartella ed estensione\)\. Ritorna \.adhoc\. se non c'è il file di progetto\.
 - **$\{hb\_targettype\}** tipologia del progetto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** piattaforma selezionata
 - **$\{hb\_comp\}** Compilatore C selezionato
 - **$\{hb\_comp\_ver\}** Versione del compilatore C
 - **$\{hb\_build\}** nome di compilazione
 - **$\{hb\_cpu\}** CPU selezionata
 - **$\{hb\_work\}** nome di default della cartella di lavoro
 - **$\{hb\_workdynsub\}** sottocartella di lavoro di default per la libreria dinamica di destinazione
 - **$\{hb\_dynprefix\}** prefisso della libreria dinamica
 - **$\{hb\_dynsuffix\}** suffisso della libreria dinamica
 - **$\{hb\_dynext\}** estensione della libreria dinamica
 - **$\{hb\_ver\}** Harbour version in hexadecimal triple byte format\. F\.e\.: 030200
 - **$\{hb\_verstr\}** Versione di Harbour in un formato leggibile dall'uomo &lt;maggiore&gt;\.&lt;minore&gt;\.&lt;rilascio&gt;\.&lt;stato&gt;\. Per esempio: 3\.2\.0dev
 - **$\{hb\_major\}** numero primario di versione Harbour
 - **$\{hb\_minor\}** numero secondario di versione Harbour
 - **$\{hb\_release\}** numero di versione di rilascio di Harbour
 - **$\{hb\_status\}** Stato della versione di Harbour
 - **$\{hb\_revision\}** Revisione di Harbour
 - **$\{hb\_host\_plat\}** Motore Harbour
 - **$\{hb\_host\_plat\_unix\}** ritorna '1' se la piattaforma host Harbour è \*nix compatibile
 - **$\{hb\_bin\}** cartella dei file binari Harbour
 - **$\{hb\_lib\}** Cartella della libreria statica di Harbour
 - **$\{hb\_lib3rd\}** Cartella di Harbour della libreria statica di terze parti
 - **$\{hb\_dyn\}** Cartella della libreria dinamica di Harbour
 - **$\{hb\_inc\}** cartella degli header Harbour
 - **$\{hb\_addons\}** cartella base degli add\-ons Harbour
 - **$\{hb\_first\}** name of source file that holds the entry function \(without directory and extension\)
 - **$\{hb\_outputdir\}** directory dell'output
 - **$\{hb\_outputname\}** nome dell'output \(senza estensione\)
 - **$\{hb\_level\}** livello di recursione del sub\-progetto
 - **$\{&lt;depname&gt;\}** restituisce la directory di intestazione della dipendenza &lt;depname&gt;, o '1 ', se essa non viene rilevata
 - **$\{&lt;envvar&gt;\}** ritorna il valore della variabile d'ambiente &lt;envvar&gt;
  
Filtri \(è possibile combinarli e/o negarli\):  


 - **\{&lt;platform&gt;\}** piattaforma di sestinazione\. Dove &lt;platform&gt; può essere qualsiasi valore accettato dall'opzione \-plat=
 - **\{&lt;compiler&gt;\}** compilatore C di destinazione\. Dove &lt;compiler&gt; può essere qualunque valore accettato dall'opzione \-comp=\.
 - **\{&lt;cpu&gt;\}** CPU di destinazione\. Dove &lt;cpu&gt; può essere una tra i seguenti: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** destinazione GUI \(vedi l'opzone \-gui\)
 - **\{std\}** console target \(see \-console option\)
 - **\{debug\}** è abilitato il debug a livello C \(vedi l'opzione \-debug\)
 - **\{nodebug\}** il debug a livello C è disabilitato \(consulta l'opzione \-debug\-\)
 - **\{shared\}** shared build \(see \-shared and related options\)
 - **\{static\}** static build \(see \-static and related options\)
 - **\{lngcpp\}** modalità C\+\+ forzata \(vedere l'opzione \-cpp\)
 - **\{lngc\}** modalità C forzata \(vedere l'opzione \-cpp\-\)
 - **\{winuni\}** Modalità Windows UNICODE \(WIDE\) \(vedere l'opzione \-winuni\)
 - **\{winansi\}** Modalità ANSI Windows \(vedere l'opzione \-winuni\-\)
 - **\{unix\}** La piattaforma di destinazione è compatibile \*nix \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** la piattaforma di destinazione è compatibile con Windows \(win, wce\)
 - **\{allgcc\}** target C compiler belongs to gcc family \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** il compilatore C di destinazione è mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** il compilatore C di destinazione è msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** il compilatore C di destinazione è bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** il compilatore dell'eseguibile C è pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** il compilatore dell'eseguibile C è icc\* \(icc, iccia64\)
 - **\{hb10\}** modalità di comaptibilità Harbour 1\.0\.x \(vedi l'opzione \-hb10\)
 - **\{hb20\}** modalità di comaptibilità Harbour 2\.0\.x \(vedi l'opzione \-hb20\)
 - **\{hb30\}** modalità di comaptibilità Harbour 3\.0\.x \(vedi l'opzione \-hb30\)
 - **\{xhb\}** modalità xhb \(vedi l'opzione \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** il filtro sarà soddisfatto se il esiste il nome &lt;file&gt; o &lt;dir&gt; sul disco\.
 - **\{MACRO\}** verrà soddisfatto il filtro se $\{MACRO\} è valorizzato e diverso da '0' or 'no' \(ignora maiuscole e minuscole\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** il filtro passerà se il valore $\{MACRO\} è più grande di &lt;value&gt; \(ignora maiuscole e minuscole\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** il filtro passerà se il valore $\{MACRO\} è più piccolo di &lt;value&gt; \(ignora maiuscole e minuscole\)\.


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** quando uno scripr \.hb viene compilato come plugin hbmk2
 - **\_\_HBEXTREQ\_\_** dove un file sorgente \.hbx è presente in un progetto \(disponibile nei sorgenti Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** quando la dipendenza &lt;depname&gt; è stata rilevata \(disponibile nei sorgenti C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** quando un file sorgente Harbour è eseguito come uno script di shell
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\) \(do not define them manually\):


 - **HBMK\_HAS\_&lt;depname&gt;** quando la dipendenza &lt;depname&gt; è stata rilevata
 - **HBMK\_DIR\_&lt;depname&gt;** ritorna la cartella di intestazione dove &lt;depname&gt; è stato rilevato, o un valore vuoto se non è stato rilevato\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** quando la dipendenza &lt;depname&gt; è stata rilevata nella locazione configurata da \-depincpathlocal= opzione
  
Variabili d'ambiente:  


 - **HBMK\_OPTIONS** accepts any options as if they were passed in the beginning of the command\-line
 - **HB\_PLATFORM** accetta gli stessi valori dell'opzione \-plat=
 - **HB\_COMPILER** accetta gli stessi valori dell'opzione \-comp=
 - **HB\_CPU** accetta gli stessi valori dell'opzione \-cpu=
 - **HB\_BUILD\_NAME** accetta gli stessi valori dell'opzione \-build=
 - **HB\_LANG** accetta gli stessi valori dell'opzione \-lang=
 - **HB\_USER\_LIBS** accetta gli stessi valori \(separati da uno spazio\) come l'opzione \-l
 - **HB\_USER\_LIBPATHS** accetta gli stessi valori \(separati da uno spazio\) come l'opzione \-L
 - **HB\_USER\_PRGFLAGS** opzioni da passare al compilatore Harbour \(prima delle opzioni nella riga di comando\)
 - **HB\_USER\_CFLAGS** opzioni da passare al compilatore C \(prima delle opzioni nella riga di comando\)
 - **HB\_USER\_RESFLAGS** opzioni da passare al compilatore di risorse \(prima delle opzioni nella riga di comando\) \(solo Windows\)
 - **HB\_USER\_LDFLAGS** opzioni da passare al linker \(eseguibile\) \(prima delle opzioni da linea di comando\)
 - **HB\_USER\_DFLAGS** opzioni da passare al linker \(libreria dinamica\) \(prima delle opzioni da linea di comando\)
 - **HB\_USER\_AFLAGS** opzioni da passare al linker \(libreria statica\) \(prima delle opzioni da linea di comando\)
 - **HB\_COMPILER\_VER** override C compiler version autodetection \(gcc and msvc compiler families only\)\. Format: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** ignora la cartella del compilatore C \(solamente per la famiglia dei compilatori gcc\)
 - **HB\_CCPREFIX** ignora il prefisso del compilatore C \(solamente per la famiglia dei compilatori gcc\)
 - **HB\_CCSUFFIX** ignora il suffisso del compilatore C \(solamente per la famiglia dei compilatori gcc\)
 - **HB\_INSTALL\_PREFIX** ignora la cartella base di installazione di Harbour
 - **HB\_INSTALL\_ADDONS** override Harbour base add\-ons directory


 - **HB\_EXTENSION** lista di estensioni separate da spazio da caricare nella shell interattiva di Harbour
  
direttive \.hbc \(dovrebbero essere scritte in righe separate\):  


 - **echo=&lt;msg&gt;** visualizzare &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** salta l'elaborazione del resto del file\. hbc\. Visualizza &lt;msg&gt;, se specificato\.
 - **stop=\[&lt;msg&gt;\]** ferma la compilazione\. Visualizza &lt;msg&gt;, se specificato\.
 - **sources=** aggiunge come file di input una lista di file separati da spazio
 - **headers=** aggiungere un elenco di intestazioni \.ch separate da spazio come intestazione standard
 - **libs=** Aggiungi un elenco di librerie, separate da uno spazio \(vedi l'opzione \-l per maggiori informazioni\)
 - **frameworks=** aggiunge una lista di strutture separate da spazio \(solo Darwin\)
 - **requests=** add space separated list of symbols to force link to the build target
 - **syslibs=** aggiungere un elenco di librerie separate da uno spazio, come sistema di librerie \(prime delle normali librerie\)
 - **hbcs=** embed space separated list of \.hbc files\. Names without the extension is accepted\. These references are processed in place\.
 - **autohbcs=** lista di valori separati da spazio come nell'opzione \-autohbc=
 - **libpaths=** lista di ulteriori percorsi di libreria separati da spazio
 - **incpaths=** aggiungere un elenco di percorsi di intestazione addizionali, separati da uno spazio \(sia per Harbour e C\)
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
 - **gui=&lt;bool&gt;** opzione 'yes' = \-gui, 'no' = \-std
 - **mt=&lt;bool&gt;** opzione 'yes' = \-mt, 'no' = \-st
 - **pic=&lt;bool&gt;** opzione 'yes' = \-pic, 'no' = \-pic\-
 - **shared=&lt;bool&gt;** opzione 'yes' = \-shared, 'no' = \-static
 - **shareddef=&lt;bool&gt;** simile a shared=, ma funziona solo se la modalità shared/static non è stata impostata prima
 - **fullstatic=&lt;bool&gt;** opzione 'yes' = \-fullstatic, 'no' = \-static
 - **debug=&lt;bool&gt;** opzione 'yes' = \-debug, 'no' = \-debug\-
 - **optim=** opzione 'yes' = \-optim, 'no' = \-optim\-
 - **nulrdd=&lt;bool&gt;** opzione 'yes' = \-nulrdd, 'no' = \-nulrdd\-
 - **nodefgt=&lt;bool&gt;** opzione 'yes' = \-nodefgt, 'no' = \-nodefgt\-
 - **map=&lt;bool&gt;** opzione 'yes' = \-map, 'no' = \-map\-
 - **hbcppmm=&lt;bool&gt;** opzione 'yes' = \-hbcpmm, 'no' = \-hbcpmm\-
 - **implib=&lt;bool&gt;** opzione 'yes' = \-implib, 'no' = \-implib\-
 - **winuni=&lt;bool&gt;** opzione 'yes' = \-winuni, 'no' = \-winuni\-
 - **strip=&lt;bool&gt;** opzione 'yes' = \-strip, 'no' = \-strip\-
 - **run=&lt;bool&gt;** opzione 'yes' = \-run, 'no' = \-run\-
 - **inc=&lt;bool&gt;** opzione 'yes' = \-inc, 'no' = \-inc\-
 - **safe=&lt;bool&gt;** opzione 'yes' = \-safe, 'no' = \-safe\-
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
 - **depfinish=** come l'opzione \-depfinish=
 - **signts=** come l'opzione \-signts=
 - **name=** Nome del pacchetto
 - **description=** descrizione del pacchetto
 - **version=&lt;x\.y\.z&gt;** numero di versione del pacchetto, dove x, y, z &gt;=0 &lt;=255\. Il valore predefinito è 0\.0\.1, se non specificato\.
 - **keywords=** lista di parole chiave separate da spazio
 - **licences=** Elenco di licenze separate da uno spazio
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
 - **hbmk\_OutStdRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Invia testo a stdout senza alcuna formattazione\.
 - **hbmk\_OutErrRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Invia testo a stderr senza alcuna formattazione\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Esegue la macro espressione hbmk2\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Escape/quote filename for using it as external command parameter\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target platform/C compiler\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Converte il nome del file usando la barra dritta come separatore di percorso
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Ritorna il percorso relativo di \-workdir= valore dalla cartella di lavoro corrente\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Find file in &lt;xPath&gt; \(array or pathsep delimited string are accepted\) with list of &lt;aExtDef&gt; alternate extensions \(defaults to executable binaries\)\. Returns filename if found and NIL if not\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Cambia cartella e/o estensione nel nome file
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Encode function name according to Harbour compiler rules for forming HB\_FUNC\(\) function names in C code\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Elimina da una stringa la coppia di delimitatori
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Converte un array di stringa in una stringa\. Il separatore predefinito è un singolo spazio\.


Plugin variables:  
\('hbmk' context hash items, case\-sensitive, read\-only unless marked otherwise\)


 - **"apiver"** varsione API come intero
 - **"cSTATE"** callback state\. Can be: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** array of parameters passed to plugins via \-pflag=/pi= options or having an extension registered via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** hash of plugin custom variables\. Writable, local to each plugin
 - **"cPLAT"** valore \-plat
 - **"cCOMP"** valore \-comp
 - **"nCOMPVer"** vedere variabile d'ambiente HB\_COMPILER\_VER
 - **"cCPU"** valore \-cpu
 - **"cBUILD"** valore \-build=
 - **"cOUTPUTNAME"** valore \-o
 - **"cTARGETNAME"** vedere la macro $\{hb\_targetname\}
 - **"cTARGETTYPE"** vedere la macro $\{hb\_targettype\}
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
 - **"cCCPATH"** vedere variabile d'ambiente HB\_CCPATH
 - **"cCCPREFIX"** vedere variabile d'ambiente HB\_CCPREFIX
 - **"cCCSUFFIX"** vedere variabile d'ambiente HB\_CCSUFFIX
 - **"cCCEXT"** vedere variabile d'ambiente HB\_CCEXT
 - **"cWorkDir"** valore \-workdir=
 - **"nExitCode"** Codice di uscita attuale
  
Shell API disponibile negli Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambia GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Abilita la modialità di compatibilità Clipper \(non\-Unicode\)
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica l'intestazione Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scarica l'intestazione Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Visualizza l'elenco di intestazione di Harbour caricato\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica un pacchetto\. Simile alla direttiva PP \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scaricare pacchetto\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista dei pacchetti caricati
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.


Esempi per partire con hbmk2:


 - **Per eseguire la shell interattiva \(prompt 'punto'\)**  
$ hbmk2 \.
 - **Per eseguire uno script di Harbour**  
$ hbmk2 myscript\.hb \[&lt;parametro\[i\]&gt;\]


Esempi per compilare e lanciare un eseguibile portabile Harbour \(ossia uno script Harbour precompilato\):


 - **Per compilare**  
$ hbmk2 \-gh myscript\.hb
 - **Per eseguire risultato di cui sopra**  
$ hbmk2 myscript\.hrb \[&lt;parametro\[i\]&gt;\]


Esempi per la compilazione di un'applicazione Harbour


 - **Per compilare un singolo \.prg**  
$ hbmk2 hello\.prg
 - **Per compilare in modalità incrementale più di un sorgente \.prg in una singola applicazione**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Per compilare un'applicazione che utilizza un file di progetto**  
$ hbmk2 myapp\.hbp
 - **Per compilare un'applicazione che utilizza il modo incrementale**  
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


 - **Per compilare la libreria 'mylib' dai sorgenti**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Per compilare la libreria 'mylib' dai sorgenti usando il modo incrementale**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Codice di uscita \("errorlevels"\):  


 - **0** Nessun errore
 - **1** piattaforma sconosciuta
 - **2** compilatore sconosciuto
 - **3** Individuazione fallita di Harbour
 - **5** failed stub creation
 - **6** failed in compilation phase
 - **7** failed in final assembly \(linker or library manager\)
 - **8** non sopportato
 - **9** Impossibile creare la cartella di lavoro
 - **19** aiuto
 - **10** Dipendenza omessa o disabilitata
 - **20** Inizializzazione del plugin
 - **30** nidificazione troppo profonda
 - **50** richiesta di stop
 - **&lt;altro&gt;** quando è utilizzata l'opzione \-run, il codice di uscita sarà quello riportato dall'eseguibile
  
Note:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Il nome del file sorgente senza estensione caricherà il file\. hbp, se tale file\. hbp esiste nella cartella corrente\. Altrimenti, verrà utilizzata l'estensione \.prg\.
  - Multiple \-l, \-L, \-i and &lt;script&gt; parameters are accepted\.
  - Regular Harbour compiler options are also accepted as is\.  
\(see them with \-harbourhelp option\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/harbour, /etc/\.harbour, &lt;base&gt;/etc/\.harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - se esistente, lo script make hbmk\.hbm nella cartella corrente è sempre processato
  - E' consigliato l''utilizzo delle normali barre nei valori opzionali, come separatore per le cartelle, ma le barre rovesciate sono egualmente accettate\.
  - Filters are accepted in each \.hbc line and most options\.  
Filters can be combined using '&amp;' \(and\), '|' \(or\) operators, negated by '\!' operator and grouped by parentheses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Most \.hbc lines \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) and corresponding command\-line parameters will accept macro variables\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Options accepting macro variables also support command substitution\. Enclose command inside \`\`, and, if the command contains space, also enclose in double quotes\. Standard output of the command will be used as the value\. F\.e\. "\-cflag=\`wx\-config \-\-cflags\`", or ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Libraries and object files built with/for CA\-Cl\*pper will not work with any supported platform/compiler\.
  - Comportamenti predefiniti e caratteristiche potrebbero essere differenti in altre piattaforme/compilatori\.
  - Per eseguire hbmk2 non sono richiesti né GNU Make, o qualsiasi altro strumento make specifico per il compilatore C, né MSYS \(in Windows\)\.
  - '\.' \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
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
their web site at https://www\.gnu\.org/\)\.  
  
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
    https://creativecommons\.org/licenses/by\-sa/3\.0/  

  
Autore:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
