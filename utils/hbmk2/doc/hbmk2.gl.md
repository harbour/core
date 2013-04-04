Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-04\-03 03:33\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  

Sintaxe:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descripción:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will autodetect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Opcións:  


 - **\-o&lt;outname&gt;** nome do arquivo de saída
 - **\-l&lt;libname&gt;** link with &lt;libname&gt; library\. &lt;libname&gt; should be without path, extension and 'lib' prefix \(unless part of the name\)\. Do not add core Harbour libraries, they are automatically added as needed\. If &lt;libname&gt; starts with a '\-' character, the library will be removed from the list of libraries at link time\.
 - **\-L&lt;libpath&gt;** additional path to search for libraries
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** additional path to search for headers
 - **\-static|\-shared** enlazar con bibliotecas estáticas/dinámicas
 - **\-gt&lt;name&gt;** enlazar co controlador de terminal GT&lt;name&gt;, pode repetirse para enlazar varios GTs\. O primero será o predeterminado en tempo de execución
 - **\-inc\[\-\]** permitir/impedir o modo de construcción incremental \(predeterminado: desactivado\)
 - **\-hbexe** Crear executable \(predeterminado\)
 - **\-hblib** crear biblioteca estática
 - **\-hbdyn** Crear biblioteca dinámica \(sen incluir a máquina virtual \-VM\- de Harbour\)
 - **\-hbdynvm** Crear biblioteca dinámica \(coa máquina virtual \-VM\- enlazada\)


 - **\-mt|\-st** enlazar coa máquina virtual de Harbour para multifío/simple fío
 - **\-gui|\-std** crear executable para GUI/consola
 - **\-main=&lt;mainfunc&gt;** substituír o nome da función/procedemento de arranque
 - **\-request=&lt;func&gt;** Forzar enlazado de función/procedemento
 - **\-fullstatic** enlazar con todas as bibliotecas estáticas
 - **\-pic\[\-\]** create position independent object code \(always enabled in \-hbdyn/\-hbdynvm modes\)
 - **\-\[full|fix\]shared** create shared Harbour binaries without/with absolute dir reference to Harbour library \(default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise\) \(fix/full option in \*nix only\)
 - **\-nulrdd\[\-\]** enlazar con nulrdd
 - **\-debug\[\-\]** engadir/quitar información de depuración no compilador C\. Para depurar a nivel de Harbour use a opción \-b estándar
 - **\-optim\[\-\]** toggle C compiler optimizations \(default: on\)
 - **\-cpp\[\-\]** forzar modo C\+\+/C
 - **\-cpp=&lt;value&gt;** Selecciona o modo C\+\+\. Os valores posibles son: def, yes, no
 - **\-map\[\-\]** crear \(ou non\) o arquivo \.map
 - **\-implib\[\-\]** Crear \(ou non\) a biblioteca de imporación \(en modo \-hbdyn/\-hbexe\)\. Engadirase un sufixo ao nome\.
 - **\-implib=&lt;output&gt;** crear nome de biblioteca de importación \(en modo \-hbdyn/\-hbexe\) para a saída a &lt;output&gt; \(predeterminado: o mesmo que a saída\)
 - **\-ln=&lt;link&gt;** crear enlace simbólico para &lt;output&gt; \(&lt;link&gt; considérase relativo a &lt;output&gt;\)
 - **\-strip\[\-\]** limpar \(ou non\) os binarios \(strip\)
 - **\-trace\[\-\]** mostrar comandos executados
 - **\-beep\[\-\]** permitir \(ou impedir\) un sinal sonoro en caso de éxito e doble en caso de erro
 - **\-ignore\[\-\]** ignorar erros ao executar ferramentas do compilador \(predeterminado: off\)
 - **\-hbcppmm\[\-\]** substituír as funcións estándar de xestión de memoria de C\+\+ coas propias de Harbour
 - **\-winuni\[\-\]** selecciona entre os modos de compilación UNICODE \(WIDE\) ou ANSI \(Predeterminado: ANSI\) \(Só para Windows\. En WinCE é sempre UNICODE\)
 - **\-nohblib\[\-\]** Non usar as bibliotecas estáticas de Harbour ao enlazar
 - **\-nodefgt\[\-\]** Non enlazar GTs predeterminados \(eficaz en modo \-static\)
 - **\-nolibgrouping\[\-\]** desactivar agrupamento de bibliotecas para compiladores basados en gcc
 - **\-nomiscsyslib\[\-\]** non engadir lista extra de bibliotecas do sistema á lista predeterminada de bibliotecas
 - **\-traceonly** mostrar comandos para executar pero sen executalos
 - **\-warn=&lt;level&gt;** set C compiler warning level  
&lt;level&gt; can be: max, yes, low, no, def \(default: yes\)
 - **\-safe\[\-\]** activa as opcións de seguridade no compilador de C/enlazador \(Predeterminado: activado en Windows, desactivado en outros sistemas\)
 - **\-compr=&lt;level&gt;** compress executable/dynamic lib \(needs UPX tool\)  
&lt;level&gt; can be: yes, no, min, max
 - **\-run\[\-\]** executar/non executar o executable de saída
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-tshead=&lt;file&gt;** generate \.ch header file with timestamp information\. Generated header will define preprocessor constants \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build
 - **\-icon=&lt;file&gt;** set &lt;file&gt; as application icon\. &lt;file&gt; should be a supported format on the target platform \(not supported by some platforms/compilers\)\. On Windows, it is implemented by generating and linking a resource file\.
 - **\-manifest=&lt;file&gt;** Incrustar arquivo de manifesto &lt;file&gt; en executable/biblioteca dinámica \(só Windows\)
 - **\-sign=&lt;key&gt;** asinar executable con &lt;key&gt; \(Só Windows e Darwin\)\. En Windows úsase a ferramenta signtool\.exe \(pertence ao SDK de Windows\) ou posign\.exe \(pertence a Pelles C 7\), nesa orde, ambos autodetectados\.
 - **\-signpw=&lt;pw&gt;** Use &lt;pw&gt; como chave de acceso ao asinar executables \(só en Windows e Darwin\)
 - **\-instfile=&lt;g:file&gt;** add &lt;file&gt; in to the list of files to be copied to path specified by \-instpath option\. &lt;g&gt; is an optional copy group \(case sensitive\), it must be at least two characters long\. In case you do not specify &lt;file&gt;, the list of files in that group will be emptied\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** activa \(ou desactiva\) a xeneración de bibliotecas de importación para as bibliotecas indicadas nas opcións \-depimplibs= \(Predeterminado: yes\)
 - **\-stop\[=&lt;text&gt;\]** deter sen facer nada e mostrar &lt;text&gt; cando se especifica
 - **\-echo=&lt;text&gt;** Resoar textos na pantalla
 - **\-pause** force waiting for a key on exit in case of failure \(with alternate GTs only\)
 - **\-exitstr** Mostrar resultado de error como texto lexible
 - **\-info** mostrar mensajes informativos
 - **\-quiet\[\-\]** suprimir todos los mensajes de pantalla


 - **\-bldf\[\-\]** inherit all/no \(default\) flags from Harbour build
 - **\-bldf=\[p\]\[c\]\[l\]** inherit \.prg/\.c/linker flags \(or none\) from Harbour build
 - **\-F&lt;framework&gt;** enlazar co marco de traballo &lt;framework&gt; \(Só Darwin\)
 - **\-prgflag=&lt;f&gt;** pass single flag to Harbour compiler
 - **\-cflag=&lt;f&gt;** pasar parámetro al compilador C
 - **\-resflag=&lt;f&gt;** pass single flag to resource compiler \(Windows only\)
 - **\-ldflag=&lt;f&gt;** pass single flag to linker \(executable\)
 - **\-dflag=&lt;f&gt;** pass single flag to linker \(dynamic library\)
 - **\-aflag=&lt;f&gt;** pass single flag to linker \(static library\)
 - **\-iflag=&lt;f&gt;** Pasar a opción ao comando de creación de bibliotecas de importación
 - **\-signflag=&lt;f&gt;** pasar a opción para a ferramenta de asinado de código
 - **\-runflag=&lt;f&gt;** pass single flag to output executable when \-run option is used
 - **\-cflag\+=&lt;f&gt;** pass single flag to C compiler overriding C compiler flags added by hbmk2 itself\. Use with caution\.
 - **\-ldflag\+=&lt;f&gt;** pass single raw option to linker \(executable\) after the library list\. Use with caution\.
 - **\-dflag\+=&lt;f&gt;** pasar a opción tal cuál ao enlazador \(biblioteca dinámica\), tra\-la lista de bibliotecas\. Usar con precaución\.
 - **\-3rd=&lt;f&gt;** opcións/parámetros reservados para ferramentas externas, ignoradas sempre por hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alter local environment\. &lt;e&gt; is the name of the environment variable to alter\. &lt;o&gt; can be '=' to set/override, '\-' to delete, '\+' to append to the end of existing value, '\#' to insert to the beginning of existing value\. &lt;v&gt; is the value to set/append/insert\.
 - **\-jobs=&lt;n&gt;** lanzar n fíos de compilación \(só en plataformas con multitarefa/multiproceso\)
 - **\-head=&lt;m&gt;** control source header parsing \(in incremental build mode\)  
&lt;m&gt; can be: native \(uses compiler to extract dependencies\), full \(default, uses simple text parser on the whole file\), dep, off
 - **\-rebuild** Reconstruir \(en modo construcción incremental\)
 - **\-rebuildall** Reconstruir con subproxectos \(en modo de construcción incremental\)
 - **\-clean** limpar \(en modo de construcción incremental\)
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


 - **\-hbx=\[&lt;\.ch&gt;\]** Crear cabeceira de Harbour \(en formato \.hbx\) con todos os símbolos externos\. Un parámetro vacío desactiva a opción\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; is a header file name\. &lt;\.hbc&gt; is a \.hbc filename to be automatically included in case the header is found in any of the compiled sources\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; é o nome da dependencia\. &lt;n&gt; é o nome da dependencia de un paquete\. Pode indicarse varias veces\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; is the name of the dependency\. &lt;h&gt; is the key header \(\.h\) of the package dependency\. Multiple alternative headers can be specified\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; é o nome da dependencia\. &lt;f&gt; pose ser 'yes' ou 'no' e indica se a dependencia é opcional\. Predeterminado: 'no'
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; é o nome da dependencia\. Use &lt;r&gt; como directorio base para as rutas indicadas nas opcións \-depincpath\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;i&gt; á lista de rutas de busca de arquivos de cabeceira\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;dll&gt; á lista de arquivos de bibliotecas de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; é o nome de dependencia\. Establecer o nome da biblioteca de importación xenerada a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** engadir complemento\. &lt;filename&gt; pode ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** Pasar arquivos de entrada a os complementos
 - **\-pflag=&lt;f&gt;** pasar opción para os complementos
  
As seguintes opcións están dispoñibles na liña de comandos:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** Deter despois de executar o compilador Harbour
 - **\-hbcmp|\-clipper** stop after creating the object files  
create link/copy hbmk2 to hbcmp/clipper for the same effect
 - **\-hbcc** accept raw C flags  
create link/copy hbmk2 to hbcc for the same effect
 - **\-hblnk** accept raw linker flags
 - **\-autohbm\[\-\]** permitir \(ou impedir\) o procesamento de hbmk\.hbm no directorio actual \(Predeterminado: yes\)
 - **\-hb10** Habilitar modo compatibilidade Harbour 1\.0\.x
 - **\-hb20** Activar modo de compatibilidade con Harbour 2\.0\.x
 - **\-hb30** activar modo de compatibilidade Harbour 3\.0\.x
 - **\-xhb** activar o modo xhb
 - **\-hbc** activar modo C puro
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emulate Clipper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect


 - **\-hbreg\[=global\]** registrar Harbour Script \(\.hb\) con hbmk2 \(Só Windows\)
 - **\-hbunreg\[=global\]** anular rexistro de Harbour Script \(\.hb\) de hbmk2 \(Só Windows\)


 - **\-find &lt;text&gt;** lists all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)


 - **\-hbmake=&lt;file&gt;** converter arquivo de proxecto hbmake a arquivo \.hbp
 - **\-xbp=&lt;file&gt;** converter arquivo de proxecto \.xbp \(xbuild\) para \.hbp
 - **\-xhp=&lt;file&gt;** converter arquivo de proxecto \.xhp \(xMate\) para \.hbp


 - **\-\-hbdirbin** output Harbour binary directory to stdout
 - **\-\-hbdirdyn** output Harbour dynamic library directory to stdout
 - **\-\-hbdirlib** output Harbour static library directory to stdout
 - **\-\-hbdirinc** output Harbour header directory to stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** substituír a plataforma destino predeterminada \(predeterminado: automático\)
 - **\-cpu=&lt;cpu&gt;** substituír CPU predeterminado de destino \(predeterminado: automático\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler autodetection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** Indique un nome de construcción
 - **\-lang=&lt;lang&gt;** substituír idioma predeterminado\. &lt;lang&gt; é o código ISO do idioma\.
 - **\-width=&lt;n&gt;** Fixar ancho da saída a &lt;n&gt; caracteres \(0=ilimitado\)\.
 - **\-shl** mostrar nivéis de subproxecto en liñas de saída
 - **\-viewhelp** axuda extendida no visor de texto
 - **\-longhelp** axuda extendida
 - **\-longhelpmd** axuda extendia en formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** Axuda do compilador Harbour \(todas as opcións do compilador Harbour son aceptadas tal cuál por hbmk2
 - **\-credits** Créditos do compilador Harbour
 - **\-build** Información da construcción do compilador Harbour
 - **\-version** mostrar só información de versión
  
Options below are internal/developer ones \(compatibility not guaranteed\):  


 - **\-debugtime** measure time spent on the build
 - **\-debuginc** display internals of incremental build
 - **\-debugstub** display content of all internally generated source files
 - **\-debugi18n** display internals on translation file generation
 - **\-debugdepd** display internals of dependency detection
 - **\-debugpars** display all input parameters in processing order
 - **\-debugrte** generate a run\-time error


Pode engadir un enlace simbólico/copia/renomear hbmk2 a os seguintes nomes para alterar o modo predeterminado de operación:


 - **hbrun\*|\*hbrun** modo secuencia de comandos / intérprete interactivo
 - **hbrund|hbrun\*d** Modo secuencia de comandos / intérprete interactivo en modo depuración
 - **harbour** modo \-hbraw \(simular compilador Harbour \-tal cuál\-\)
 - **clipper** mode \-hbcmp \(emulación do compilador Clipper\)
 - **rtlink** Modo \-rtlink \(emular enlazador Clipper\)
 - **exospace** Modo \-rtlink \(emular enlazador Clipper\)
 - **blinker** Modo \-rtlink \(emular enlazador Clipper\)
 - **\*10** opción \-hb10
 - **\*20** opción \-hb20
 - **\*30** opción \-hb30
 - **x\*** opción \-xhb
 - **hbcmp\*|\*hbcmp** modo \-hbcmp \(simular compilador Harbour producindo un obxeto binario\)
 - **hbcc\*|\*hbcc** modo \-hbcc \(simular compilador C\)
 - **hblnk\*|\*hblnk** modo \-hblnk \(simular enlazador C\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Arquivos:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** colección de opcións\. Pode usarse para agrupar opcións comúns no mesmo arquivo e incluílo nos arquivos de proxecto\. Usa o mesmo formato que os arquivos \.hbp\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate line\.
 - **\*\.ch** if passed directly as a source file, it will be used as additional standard header
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **hbmk\.hbm** arquivo opcional \.hbm no directorio de traballo actual, que é automaticamente procesado antes de outras opcións
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Guión de Harbour
 - **\*\.hrb** Binario portable de Harbour \(coñecido como guión de Harbour precompilado\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** stores command history for interactive Harbour shell\. You can disable history by making the first line 'no' \(without quotes and with newline\)\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\
  
Macro variables:  


 - **$\{hb\_root\}** directorio de hbmk2
 - **$\{hb\_dir\}** o directorio do nome do arquivo é usado en
 - **$\{hb\_dirname\}** directorio superior do nome do arquivo é usado en
 - **$\{hb\_name\}** nome do arquivo usado \(sen directorio nin extensión\)
 - **$\{hb\_self\}** nome completo é usado en
 - **$\{hb\_curdir\}** directorio de traballo actual
 - **$\{hb\_tempdir\}** Directorio do sistema operativo para arquivos temporais\.
 - **$\{hb\_targetname\}** nome do proxecto \(sen directorio nin extensión\)\. Retorna \.adhoc\. se non hay arquivo de proxecto\.
 - **$\{hb\_targettype\}** type of the project \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** plataforma seleccionada
 - **$\{hb\_comp\}** compilador C seleccionado
 - **$\{hb\_comp\_ver\}** Versión do compilador C
 - **$\{hb\_build\}** nome da construcción
 - **$\{hb\_cpu\}** CPU seleccionada
 - **$\{hb\_work\}** nome predeterminado do directorio base de traballo
 - **$\{hb\_workdynsub\}** subdirectorio por defecto para o destino das bibliotecas dinámicas
 - **$\{hb\_dynprefix\}** prefixo para bibliotecas dinámicas
 - **$\{hb\_dynsuffix\}** sufixo para bibliotecas dinámicas
 - **$\{hb\_dynext\}** extensión para bibliotecas dinámicas
 - **$\{hb\_ver\}** Versión de Harbour en formato hexadecimal de tres bytes\. Por exemplo: 030200
 - **$\{hb\_verstr\}** Harbour version in human readable format &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. F\.e\.: 3\.2\.0dev
 - **$\{hb\_major\}** Número de versión principal de Harbour
 - **$\{hb\_minor\}** Número de versión secundario de Harbour
 - **$\{hb\_release\}** Número de versión da publicación de Harbour
 - **$\{hb\_status\}** Estado da versión de Harbour
 - **$\{hb\_revision\}** Revisión de Harbour
 - **$\{hb\_host\_plat\}** Plataforma anfitriona de Harbour
 - **$\{hb\_host\_plat\_unix\}** Retorna '1' se a plataforma anfitrión e \*nix compatible
 - **$\{hb\_bin\}** Directorio dos binarios de Harbour
 - **$\{hb\_lib\}** Directorio para bibliotecas estáticas de Harbour
 - **$\{hb\_lib3rd\}** Directorio para bibliotecas externas de terceiros para Harbour
 - **$\{hb\_dyn\}** Directorio para bibliotecas de enlace dinámico de Harbour
 - **$\{hb\_inc\}** Directorio das cabeceiras de Harbour
 - **$\{hb\_addons\}** Directorio base dos complementos de Harbour
 - **$\{hb\_first\}** name of source file that holds the entry function \(without directory and extension\)
 - **$\{hb\_outputdir\}** directorio para a saída
 - **$\{hb\_outputname\}** Nome do arquivo de saída \(sen extensión\)
 - **$\{hb\_level\}** nivel de recursión para subproxectos
 - **$\{&lt;depname&gt;\}** returns the header directory of dependency &lt;depname&gt;, or '1' if it is not detected
 - **$\{&lt;envvar&gt;\}** Retorna o valor da variable de entorno &lt;envvar&gt;
  
Filtros \(poden combinarse e/ou negarse\):  


 - **\{&lt;platform&gt;\}** plataforma destino\. Donde &lt;platform&gt; pode ser calquera valor aceptado pola opción \-plat=\.
 - **\{&lt;compiler&gt;\}** compilador C de destino\. Donde &lt;compiler&gt; pode ser calquera valor aceptado pola opción \-comp=\.
 - **\{&lt;cpu&gt;\}** CPU destino\. Onde &lt;cpu&gt; pode ser: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** Destino GUI \(ver opción \-gui\)
 - **\{std\}** tipo de consola \(ver opción \-console\)
 - **\{debug\}** Depuración a nivel de C activada \(ver opción \-debug\)
 - **\{nodebug\}** Depuración a nivel C está desactivada \(ver opción \-debug\-\)
 - **\{shared\}** Construcción compartida \(ver opción \-shared e relacionadas\)
 - **\{static\}** construcción estática \(ver opción \-static e relacionadas\)
 - **\{lngcpp\}** Forzado modo C\+\+ \(ver opción \-cpp\)
 - **\{lngc\}** Forzado modo C \(ver opción \-cpp\-\)
 - **\{winuni\}** Modo Windows UNICODE \(WIDE\) \(ver opción \-winuni\)
 - **\{winansi\}** Modo Windows ANSI \(ver opción \-winuni\-\)
 - **\{unix\}** target platform is \*nix compatible \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** Plataforma destino e compatible con Windows \(win, wce\)
 - **\{allgcc\}** O compilador de C pertence á familia gcc \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** O compilador C é mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** compilador C de destino é msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** Compilador C destino é bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** Compilador C destino é pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** compilador C de destino é icc\* \(icc, iccia64\)
 - **\{hb10\}** Modo compatibilidade Harbour 1\.0\.x \(ver opción \-hb10\)
 - **\{hb20\}** Modo compatibilidade Harbour 2\.0\.x \(ver opción \-hb20\)
 - **\{hb30\}** Modo compatibilidade Harbour 3\.0\.x \(ver opción \-hb30\)
 - **\{xhb\}** modo xhb \(ver opción \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** o filtro pasará se o nome de &lt;file&gt; ou &lt;dir&gt; existe no disco\.
 - **\{MACRO\}** filtro pasará se o valor de $\{MACRO\} non está valeiro e é diferente de cero '0' or 'non' \(non se diferencian maiúsculas e minúsculas\)
 - **\{MACRO='&lt;value&gt;'\}** o filtro pasará se o valor de $\{MACRO\} é igual que &lt;value&gt; \(non se diferencian maiúsculas/minúsculas\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** o filtro pasará se o valor de $\{MACRO\} é maior que &lt;value&gt; \(non se diferencian maiúsculas/minúsculas\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** o filtro pasará se o valor de $\{MACRO\} é menor que &lt;value&gt; \(non se diferencian maiúsculas/minúsculas\)\.


Constantes predefinidas nos fontes:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** when an \.hb script is compiled as hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** cando un arquivo \.hbx está presente nun proxecto \(permitido nos fontes de Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** cando a dependencia &lt;depname&gt; se detectou \(dispoñible nas fontes en C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** cando unha fonte Harbour se executa coma un guión do intérprete de comandos
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Constantes predefinidas en arquivos de construcción \(están dispoñibles despois de '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\):


 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected
 - **HBMK\_DIR\_&lt;depname&gt;** return the header directory where &lt;depname&gt; was detected, or empty if it was not\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** cando a dependencia &lt;depname&gt; se detectou nun lugar configurado pola opción \-depincpathlocal=
  
Variables de ámbito:  


 - **HBMK\_OPTIONS** acepta calquera opción como se foran pasadas ao principio da liña de comando
 - **HB\_PLATFORM** acepta os mesmos valores que a opción \-plat=
 - **HB\_COMPILER** acepta os mesmos valores que a opción \-comp=
 - **HB\_CPU** acepta os mesmos valores que a opción \-cpu=
 - **HB\_BUILD\_NAME** acepta os mesmos valores que a opción \-build=
 - **HB\_LANG** acepta os mesmos valores que a opción \-lang=
 - **HB\_USER\_LIBS** Acepta os mesmos valores \(separados por espacios\) que a opción \-l
 - **HB\_USER\_LIBPATHS** Acepta os mesmos valores \(separados por espacios\) que a opción \-L
 - **HB\_USER\_PRGFLAGS** opcións para o compilador Harbour \(antes das opcións de liña de comandos\)
 - **HB\_USER\_CFLAGS** opcións para o compilador C \(antes das opcións de liña de comandos\)
 - **HB\_USER\_RESFLAGS** options to be passed to resource compiler \(before command\-line options\) \(Windows only\)
 - **HB\_USER\_LDFLAGS** opcións para enviar ao enlazador \(executable\) \(antes das opcións da liña de comando\)
 - **HB\_USER\_DFLAGS** opcións para enviar ao enlazador \(biblioteca dinámica\) \(antes das opcións da liña de comando\)
 - **HB\_USER\_AFLAGS** opcións para enviar ao enlazador \(biblioteca estática\) \(antes das opcións da liña de comando\)
 - **HB\_COMPILER\_VER** override C compiler version autodetection \(gcc and msvc compiler families only\)\. Format: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** substituír o directorio do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_CCPREFIX** substituír o prefixo do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_CCSUFFIX** substituír o sufixo do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_INSTALL\_PREFIX** Substituír o directorio base da instalación de Harbour
 - **HB\_INSTALL\_ADDONS** substituír o directorio base dos complementos de Harbour


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
directivas \.hbc \(deben ser escritas en liñas separadas\):  


 - **echo=&lt;msg&gt;** Amosar &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** Omitir o procesamento do resto do arquivo \.hbc\. Mostrarase &lt;msg&gt; se se especificou\.
 - **stop=\[&lt;msg&gt;\]** Deter a construcción\. Mostra &lt;msg&gt; cando se especifica\.
 - **sources=** engadir lista separada por espazos de arquivos como arquivos de entrada
 - **headers=** engadir lista de cabeceiras \.ch separada por espazos como cabeceira estándar
 - **libs=** engada lista de bibliotecas separadas por espacios \(ver opción \-l\)
 - **frameworks=** engadir lista de entornos de traballo \(frameworks\) separados por espazos \(Só Darwin\)
 - **requests=** add space separated list of symbols to force link to the build target
 - **syslibs=** add space separated list of libraries as system libraries \(before regular libraries\)
 - **hbcs=** embed space separated list of \.hbc files\. Names without the extension is accepted\. These references are processed in place\.
 - **autohbcs=** lista de valores separada por espazos como na opción \-autohbc=
 - **libpaths=** lista separada por espazos de rutas para bibliotecas adicionais
 - **incpaths=** engadir lista separada por espazos de rutas a arquivos de cabeceira \(para Harbour e C\)
 - **instfiles=** lista de valores separada por espazos como na opción \-instfile=
 - **instpaths=** lista de valores separada por espazos como na opción \-instpath=
 - **prgflags=** lista de valores separada por espazos como na opción \-prgflag=
 - **cflags=** lista de valores separada por espazos como na opción \-cflag=
 - **resflags=** lista de valores separada por espazos como na opción \-resflag=
 - **ldflags=** lista de valores separada por espazos como na opción \-ldflag=
 - **ldflags\+=** lista de valores separada por espazos como na opción \-ldflag\+=
 - **dflags=** lista de valores separada por espazos como na opción \-dflag=
 - **dflags\+=** lista de valores separada por espazos como na opción \-dflag\+=
 - **pflags=** lista de valores separada por espazos como na opción \-pflag=
 - **psources=** lista de valores separada por espazos como na opción \-pi=
 - **gui=&lt;bool&gt;** 'yes' = \-gui, 'no' = \-std option
 - **mt=&lt;bool&gt;** 'yes' = \-mt, 'no' = \-st option
 - **pic=&lt;bool&gt;** 'yes' = \-pic, 'no' = \-pic\- option
 - **shared=&lt;bool&gt;** 'yes' = \-shared, 'no' = \-static option
 - **shareddef=&lt;bool&gt;** similar a shared= pero funciona só cando o modo shared/static non foi establecido antes
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
 - **gtdef=&lt;name&gt;** establecer o controlador de terminal \(GT\) predeterminado
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
 - **repository=** lista separada por espazos de referencias a repositorios de fontes


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unknown file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo de Harbour ao proxecto\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo C ao proxecto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engade un arquivo C\+\+ a o proxecto\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo de recursos Windows ao proxecto\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo binario ao proxecto\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Engadir un arquivo para ser instalado, con un nome de grupo opcional para \-instpath=\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Enviar texto a saída de estándar 'stdout'\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Enviar texto a saída de erro estándar 'stderr'\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Enviar texto á saída estándar \(stdout\) sen ningún formato\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Enviar texto á saída de erro estándar \(stderr\) sen ningún formato\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Avaliar a macro expresión hbmk2\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Citar/Entrecomillar o nome do arquivo para ser usado como parámetro para comandos externos\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target platform/C compiler\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Converter nome de arquivo para usar a barra invertida coma separadores de directorio\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Retorna a ruta relativa do valor de \-workdir= dende o directorio de traballo actual\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Find file in &lt;xPath&gt; \(array or pathsep delimited string are accepted\) with list of &lt;aExtDef&gt; alternate extensions \(defaults to executable binaries\)\. Returns filename if found and NIL if not\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Cambiar directorio e/ou extensión no nome do arquivo\.
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Codificar nomes de funcións de acordo ás normas do compilador Harbour para xenerar funcións de tipo HB\_FUNC\(\) en código C\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Limpar redundante/dobre entrecomillado en literais\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convirte unha lista de literais en un literal\. O separador predeterminado é un espazo\.


Variables para os complementos: \(elementos hash do contexto de 'hbmk', sen distinción maiúsculas/minúsculas, só lectura se non se indica o contrario\)


 - **"apiver"** Versión do API como número enteiro
 - **"cSTATE"** aviso de estado\. Pode ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** array of parameters passed to plugins via \-pflag=/pi= options or having an extension registered via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** lista tipo hash de variables personalizadas para o complemento\. Modificables e locais para cada complemento
 - **"cPLAT"** valor \-plat
 - **"cCOMP"** valor \-comp
 - **"nCOMPVer"** ver variable de ámbito HB\_COMPILER\_VER
 - **"cCPU"** valor \-cpu
 - **"cBUILD"** valor \-build=
 - **"cOUTPUTNAME"** valor \-o
 - **"cTARGETNAME"** ver macro $\{hb\_targetname\}
 - **"cTARGETTYPE"** ver macro $\{hb\_targettype\}
 - **"lREBUILD"** Estado da opción \-rebuild
 - **"lCLEAN"** Estado da opción \-clean
 - **"lDEBUG"** Estado da opción \-debug
 - **"lMAP"** Estado da opción \-map
 - **"lSTRIP"** Estado da opción \-strip
 - **"lDONTEXEC"** Estado da opción \-traceonly
 - **"lIGNOREERROR"** Estado da opción \-ignore
 - **"lTRACE"** Estado da opción \-trace
 - **"lQUIET"** Estado da opción \-q
 - **"lINFO"** Estado da opción \-info
 - **"lBEEP"** Estado da opción \-beep
 - **"lRUN"** Estado da opción \-run
 - **"lINC"** Estado da opción \-inc
 - **"cCCPATH"** ver variable de ámbito HB\_CCPATH
 - **"cCCPREFIX"** ver variable de ámbito HB\_CCPREFIX
 - **"cCCSUFFIX"** ver variable de ámbito HB\_CCSUFFIX
 - **"cCCEXT"** ver variable de ámbito HB\_CCEXT
 - **"cWorkDir"** valor \-workdir=
 - **"nExitCode"** Código actual de saída
  
Shell API available in Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambiar controlador de terminal \(GT\)\. Predeterminado \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Permitir modo de compatibilidade con Clipper \(non Unicode\)\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Cargar cabeceira de Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar cabeceira de Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Mostra a lista de cabeceiras de Harbour cargadas\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Cargar paquete\. Similar á directiva \#request do preprocesador\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar paquete\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de paquetes cargados\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.


Exemplos para empezar con hbmk2:


 - **Para executar o intérprete interactivo \('punto' interactivo\)**  
$ hbmk2 \.
 - **Para executar un guión de Harbour**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Exemplos para construir e executar binarios portables de Harbour \(conocidos coma guións de Harbour\)


 - **Para construir**  
$ hbmk2 \-gh myscript\.hb
 - **Para executar resultado anterior**  
$ hbmk2 myscript\.hrb \[&lt;parameter\[s\]&gt;\]


Exemplos para construir unha aplicación con Harbour:


 - **Para construir un único arquivo \.prg**  
$ hbmk2 hello\.prg
 - **Para construir unha aplicación con varias fontes \.prg en modo incremental**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Para construir unha aplicación usando un arquivo de proxecto**  
$ hbmk2 myapp\.hbp
 - **Para construir unha aplicación usando o modo incremental**  
$ hbmk2 myapp\.hbp \-inc
 - **To build an application which uses a contrib package or 3rd party \(add\-on\) package that ships with an \.hbc file**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Para construir unha aplicación que usa unha biblioteca**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Para construir unha aplicación que usa recursos de Windows**  
$ hbmk2 mymain\.prg myres\.rc
 - **Para construir unha aplicación enlazada coas bibliotecas dinámicas de Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Para construir unha aplicación con todos os arquivos fonte \.prg e \.c do subdirectorio 'source'**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Exemplos para construir unha biblioteca estática de Harbour:


 - **Para construir a biblioteca 'mylib' dende os fontes**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Para construir a biblioteca 'mylib' desde as fontes usando o modo incremental**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Códigos de saída \("errorlevels"\):  


 - **0** Sen error
 - **1** Plataforma descoñecida
 - **2** Compilador descoñecido
 - **3** Fallou a detección de Harbour
 - **5** Fallou a creación do arquivo auxiliar
 - **6** Fallou a compilación \(Harbour, compilador C, compilador de recursos\)
 - **7** Fallo na montaxe final \(o enlazador ou o xestor de bibliotecas\)
 - **8** Non soportado
 - **9** Fallo ao crear directorio de traballo
 - **19** axuda
 - **10** Dependencia non atopada ou desactivada
 - **20** Inicialización de complemento
 - **30** Aniñamento demasiado profundo\.
 - **50** parada solicitada
 - **&lt;other&gt;** cando a opción \-run e usada, o código de saída será retornado polo executable
  
Notas:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Source filename without extension will load the \.hbp file, if such \.hbp file exists in current directory\. If not, \.prg extension will be used\.
  - Acéptanse parámetros \-l, \-L, \-i e &lt;script&gt; múltiples\.
  - Regular Harbour compiler options are also accepted as is\.  
\(see them with \-harbourhelp option\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - hbmk\.hbm make script in current directory is always processed if it exists\.
  - Recoméndase o uso da barra invertida como separador de directorios pero a barra estándar tamén se acepta\.
  - Filters are accepted in each \.hbc line and most options\.  
Filters can be combined using '&amp;' \(and\), '|' \(or\) operators, negated by '\!' operator and grouped by parentheses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Most \.hbc lines \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) and corresponding command\-line parameters will accept macro variables\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Options accepting macro variables also support command substitution\. Enclose command inside \`\`, and, if the command contains space, also enclose in double quotes\. Standard output of the command will be used as the value\. F\.e\. "\-cflag=\`wx\-config \-\-cflags\`", or ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Bibliotecas e arquivos obxeto compilados con/para CA\-Cl\*pper non funcionarán en ningunha plataforma ou compilador soportados\.
  - Os valores predeterminados e o soporte de características pode variar para cada plataforma/compilador\.
  - Non se necesita GNU Make, ningunha ferramenta de tipo make específica do compilador C, nin MSYS \(en Windows\) para executar hbmk2\.
  - \. \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb ou \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Pose usar &lt;Alt\+V&gt; no modo de intérprete interactivo de Harbour para pegar texto dende o portapapéis\.
  - Os valores marcados con \[\*\] poden depender da plataforma anfitriona e/ou da configuración\. Esta axuda foi xenerada na plataforma 'win'\.


Valor soportado en &lt;compiler&gt; para cada valor de &lt;platform&gt;:


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
  
Licencia:  


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
