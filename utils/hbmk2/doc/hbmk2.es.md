Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  
Traducción \(es\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintáxis:  
  
  hbmk2 \[opciones\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Description:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will autodetect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Opciones:  


 - **\-o&lt;outname&gt;** nombre de archivo de salida
 - **\-l&lt;libname&gt;** link with &lt;libname&gt; library\. &lt;libname&gt; should be without path, extension and 'lib' prefix \(unless part of the name\)\. Do not add core Harbour libraries, they are automatically added as needed\. If &lt;libname&gt; starts with a '\-' character, the library will be removed from the list of libraries at link time\.
 - **\-L&lt;libpath&gt;** PATH adicional para buscar librerías
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** rutass adicionales para buscar para cabeceras
 - **\-static|\-shared** enlazar con librerías estáticas/compartidas
 - **\-gt&lt;name&gt;** link with GT&lt;name&gt; GT driver, can be repeated to link with more GTs\. First one will be the default at run\-time
 - **\-inc\[\-\]** habilita modo de compilación incremental
 - **\-hbexe** crea ejecutable \(por defecto\)
 - **\-hblib** crear librería estática
 - **\-hbdyn** crea biblioteca dinámica \(sin enlace a Harbour VM\)
 - **\-hbdynvm** crear librería dinámica \(con enlace a Harbour VM\)


 - **\-mt|\-st** enlazar con soporte multi/single\-thread Harbour VM
 - **\-gui|\-std** crear ejecutable GUI/console
 - **\-main=&lt;mainfunc&gt;** sobreescribe nombre de función/procedimento inicial
 - **\-request=&lt;func&gt;** fuerza función/procedimiento a enlazarse
 - **\-fullstatic** enlazar con todas las librerías estáticas
 - **\-pic\[\-\]** crear código objeto independiente de la posición \(siempre activado en los modos \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** crear para compartir archivos binarios Harbour sin/con referencia absoluta a librerías de Harbour \(por defecto: 'fullshared' cuando Harbour se instala en ubicación del sistema, 'fixshared' en otro caso\) \(opción fix/full en \*nix solamente\)
 - **\-nulrdd\[\-\]** enlazar con nulrdd
 - **\-debug\[\-\]** adicionar/excluir información de debug de compilador C\. Para activar el debug de Harbour utilize la Opción \-b como de costumbre\.
 - **\-optim\[\-\]** alternar las optimizaciones del compilador C \(por defecto: on\)
 - **\-cpp\[\-\]** fuerza modo C\+\+/C
 - **\-cpp=&lt;value&gt;** select C\+\+ mode\. Allowed values are: def, yes, no
 - **\-map\[\-\]** crear \(o no\) un archivo map
 - **\-implib\[\-\]** crear \(o no\) una biblioteca de importación \(en modo \-hbdyn/\-hbexe\)\. El nombre tendrá un sufijo anadido\.
 - **\-implib=&lt;output&gt;** crear biblioteca de importación \(en modo \-hbdyn/\-hbexe\) Nombre de &lt;output&gt; \(por defecto: igual que la salida\)
 - **\-ln=&lt;link&gt;** crea enlace simbólico apuntando a &lt;output&gt; \(&lt;link&gt; se asocia a &lt;output&gt;\)
 - **\-strip\[\-\]** desmontar \(no desmontar\) binarios
 - **\-trace\[\-\]** mostrar comandos ejecutados
 - **\-beep\[\-\]** activa \(o desactiva\) beep simple en caso de éxito, doble beep en caso de falla
 - **\-ignore\[\-\]** ignore errores cuando ejecute herramienta de compilador \(por defecto: off\)
 - **\-hbcppmm\[\-\]** reemplaza las funciones de administración de memoria estandar de C\+\+ con las de Harbour
 - **\-winuni\[\-\]** seleccionar modo de compilación entre Unicode \(WIDE\) y ANSI \(por defecto: ANSI\) \(sólo Windows\. Para WinCE siempre se establece en UNICODE\.\)
 - **\-nohblib\[\-\]** no use librerías estáticas del núcleo de Harbour al enlazar
 - **\-nodefgt\[\-\]** no enlaza GTs por defecto \(efectivo en modo \-static\)
 - **\-nolibgrouping\[\-\]** desactivar agrupamiento de LIBs en compiladores basados en gcc\.
 - **\-nomiscsyslib\[\-\]** no agregue lista adicional de librerías del sistema a lista de librerías por defecto
 - **\-traceonly** mostrar comandos a ser ejecutados, pero no ejecutarlos
 - **\-warn=&lt;lev&gt;** Configure nivel de advertencia \(warnings\) del compilador C  
&lt;lev&gt; puede ser: max, yes, low, no, def \(default: yes\)
 - **\-safe\[\-\]** enable safety options in C compiler/linker \(default: enabled on Windows, disabled on other systems\)
 - **\-compr=&lt;lev&gt;** comprime executable/librería dinamica \(necesita UPX\)  
&lt;lev&gt; puede ser: yes, no, min, max
 - **\-run\[\-\]** ejecutar/no ejecutar aplicativo generado\.
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-tshead=&lt;file&gt;** generar archivo de cabecera \.ch con información de fecha/hora\. Cabecera generado definirá macros \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ con fecha/hora de creación de archivo\.
 - **\-icon=&lt;file&gt;** set &lt;file&gt; as application icon\. &lt;file&gt; should be a supported format on the target platform \(not supported by some platforms/compilers\)\. On Windows, it is implemented by generating and linking a resource file\.
 - **\-manifest=&lt;file&gt;** incrustar manifiesto &lt;file&gt; en ejecutable/lib dinámica \(sólo Windows\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both autodetected\.
 - **\-signpw=&lt;pw&gt;** use &lt;pw&gt; as password when signing executable \(Windows and Darwin only\)
 - **\-instfile=&lt;g:file&gt;** anadir &lt;archivo&gt; a la lista de archivos que desea copiar a la ruta especificada por la opción \-instpath\. &lt;g&gt; es un grupo opcional de copia \(distingue mayúsculas y minúsculas\), debe haber al menos dos caracteres\. En caso de que no se especifica &lt;archivo&gt;, la lista de archivos en ese grupo se vaciará\.
 - **\-instpath=&lt;g:path&gt;** copy target to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copia destino a ruta de instalación incluso si está actualizado
 - **\-depimplib\[\-\]** activar \(o desactivar\) la generación de Bibliotecas de Importación para fuentes de biblioteca de importación especificados en la opción \-depimplibs= \(por defecto: yes\)
 - **\-stop\[=&lt;text&gt;\]** parar sin hacer nada y mostrar &lt;text&gt; si se ha especificado
 - **\-echo=&lt;text&gt;** eco de texto en la pantalla
 - **\-pause** forzar pause para presionar una tecla en caso de error \(solo con driver GT alternativo\)
 - **\-exitstr** show error result as human readable text on exit
 - **\-info** activar los mensajes informativos
 - **\-quiet\[\-\]** suprimir todos los mensajes en pantalla


 - **\-bldf\[\-\]** heredar flags de Harbour: todos/ninguno \(por defecto\)
 - **\-bldf=\[p\]\[c\]\[l\]** heredar todos los flags \.prg/\.c/linker \(o ninguno\) desde construcción de Harbour 
 - **\-F&lt;framework&gt;** Enlace con marco &lt;framework&gt; \(Sólo Darwin \)
 - **\-prgflag=&lt;f&gt;** pasar flags a Harbour
 - **\-cflag=&lt;f&gt;** pasar flags a compilador C
 - **\-resflag=&lt;f&gt;** pasar flags a compilador de recursos \(sólo para Windows\)
 - **\-ldflag=&lt;f&gt;** pasar flags a linkeditor \(ejecutable\)
 - **\-dflag=&lt;f&gt;** pasar flags a linkeditor \(librería dinánica\)
 - **\-aflag=&lt;f&gt;** pasa flag a linkeditor \(lib estática\)
 - **\-iflag=&lt;f&gt;** pasar solo bandera a comando de creación de biblioteca de importación
 - **\-signflag=&lt;f&gt;** pass single flag to code sign command
 - **\-runflag=&lt;f&gt;** pasar flag a ejecutable de salida cuando opción \-run es utilizada
 - **\-cflag\+=&lt;f&gt;** pasar solo bandera del compilador de C para reemplazar las opciones del compilador C anadida por hbmk2 mismo\. Usar con precaución\.
 - **\-ldflag\+=&lt;f&gt;** pass single raw option to linker \(executable\) after the library list\. Use with caution\.
 - **\-dflag\+=&lt;f&gt;** pass single raw option to linker \(dynamic library\) after the library list\. Use with caution\.
 - **\-3rd=&lt;f&gt;** opciones/flags reservados para herramientas de terceros, siempre ignorado por hbmk2 en sí
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alterar el ambiente local\. &lt;e&gt; es el nombre de la variable de entorno a alterar\. &lt;o&gt; puede ser '=' para establecer/reemplazar, '\-' para borrar, '\+' para anadir al final de valor existente, '\#' para insertar al principio del valor existente\. &lt;v&gt; es el valor a poner/agregar/insertar\.
 - **\-jobs=&lt;n&gt;** Inicia &lt;n&gt; threads de compilación \(solo para plataformas multiproceso\)
 - **\-head=&lt;m&gt;** analizando fuente de control de encabezado \(en construcción en modo incremental\)  
&lt;m&gt; puede ser: nativo \(compilador utilizado para extraer las dependencias\), completa \(por defecto, utiliza analizador de texto simple en el fichero entero\), dep, off
 - **\-rebuild** reconstrucción \(en modo incremental\)
 - **\-rebuildall** reconstruir con sub\-proyectos \(contrucción en modo incremental\)
 - **\-clean** compilación limpia \(en modo incremental\)
 - **\-workdir=&lt;dir&gt;** working directory  
\(default: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] in incremental mode, OS temp directory otherwise\)


 - **\-hbcontainer** virtual target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** create import library \(Windows only\)


 - **\-hbl\[=&lt;output&gt;\]** nombre\-de\-archivo \.hbl resultante\. macro %\{hb\_lng\} es aceptada en nombre\-de\-archivo\.
 - **\-lng=&lt;languages&gt;** lista de idiomas a ser reemplazados en %\{hb\_lng\} macros en archivos \.pot/\.po y nombres de archivos y salida \.hbl/\.po\. Lista separada por comas:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** crear/actualizar archivo \.po desde fuentes\. Combinar con anterior archivo \.po del mismo nombre\.
 - **\-minipo\[\-\]** adicionar \(o no\) número de versión y referencia de archivo de origen a po\. \(por defecto: anadirlos\)
 - **\-rebuildpo** recrea archivo \.po, eliminando todas las entradas obsoletas en el mismo\.


 - **\-hbx=\[&lt;\.ch&gt;\]** Create Harbour header \(in \.hbx format\) with all external symbols\. Empty parameter will disable it\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; es un nombre de archivo de cabecera\. &lt;\.hbc&gt; es un nombre de archivo \.hbc para ser incluido automáticamente en caso la cabecera se encuentra en cualquiera de las fuentes compiladas\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;n&gt; nombre de la dependencia de un paquete\. Se puede especificar varias veces\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;h&gt; es la cabecera clave \(\.h\) de la dependencia de un paquete\. Varios encabezados alternativos pueden ser especificados\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;f&gt; puede ser 'yes' o 'no', especifica si la dependencia es opcional\. Por defecto: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;v&gt; es un valor que controla como se hace la detección\. Valores aceptados: no, yes, force, nolocal, local\. Por defecto: contenido de envvar HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; es el nombre de la dependencia\. Establecer &lt;r&gt; como directorio raíz para las rutas especificadas en la opción \-depincpath\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; es el nombre de la dependencia\. Anadir &lt;i&gt; a la lista de rutas de detección de encabezados\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;i&gt; to the header detection path list, where &lt;i&gt; is pointing to a directory local to the project and containing an embedded \(aka\. 'locally hosted'\) dependency\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; es el nombre de la dependencia\. Anadir &lt;dll&gt; a la lista fuente de la biblioteca de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; es el nombre de la dependencia\. Establecer nombre generado de biblioteca de importación a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** agregar plugin \(módulo\)\. &lt;filename&gt; puede ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** pasar archivo de entrada a plugins
 - **\-pflag=&lt;f&gt;** pasar solo bandera para plugins
  
Options below are available on command\-line:  


 - **\-target=&lt;script&gt;** especifica un nuevo destino de construcción\. &lt;script&gt; puede ser \.prg \(o sin extensión\) o \.hbp archivo\. Tenga en cuenta que los archivos \.hbp son automáticamente considerados como destinos separados\.


 - **\-hbrun** ejecutar destino
 - **\-hbraw** parar después de ejecutar compilador Harbour
 - **\-hbcmp|\-clipper** para después de la creación de los archivos objeto  
crear un enlace/copia a hbmk2 para hbcmp/clipper resultará el mismo efecto
 - **\-hbcc** acepta raw C flags  
create enlace/copia hbmk2 para hbcc para el mismo efecto
 - **\-hblnk** aceptar flags primas del enlazador
 - **\-autohbm\[\-\]** activar \(o desactivar\) procesamiento de hbmk\.hbm en el directorio actual \(por defecto: yes\)
 - **\-hb10** habilita modo de compatibilidad 'Harbour 1\.0\.x'
 - **\-hb20** activa el modo de compatibilidad Harbour 2\.0\.x
 - **\-hb30** enable Harbour 3\.0\.x compatibility mode
 - **\-xhb** habilitar modo xHb
 - **\-hbc** activa modo puro C
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emula comportamiento de linkeditor compatible con clipper  
crear link o copiar hbmk2 para rtlink/blinker/exospace resultará el mismo efecto


 - **\-hbreg\[=global\]** register Harbour Script \(\.hb\) with hbmk2 \(Windows only\)
 - **\-hbunreg\[=global\]** unregister Harbour Script \(\.hb\) from hbmk2 \(Windows only\)


 - **\-find &lt;text&gt;** lists all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)


 - **\-hbmake=&lt;file&gt;** convierte proyecto hbmake en un archivo \.hbp
 - **\-xbp=&lt;file&gt;** convierte proyecto \.xbp \(xbuild\) en un archivo \.hbp
 - **\-xhp=&lt;file&gt;** convierte un proyecto \.xhp \(xMate\) en un archivo \.hbp


 - **\-\-hbdirbin** directorio de binarios de Harbour
 - **\-\-hbdirdyn** directorio de salida de librerías dinámicas de Harbour
 - **\-\-hbdirlib** directorio de salida de librerías estáticas de Harbour
 - **\-\-hbdirinc** directorio de cabeceras de Harbour
 - **\-\-hbinfo\[=nested\]** output Harbour build information\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** override default target platform \(default: automatic\)
 - **\-cpu=&lt;cpu&gt;** selecciona la CPU de destino\. \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler autodetection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** utilizar un nombre de build especifico
 - **\-lang=&lt;lang&gt;** override default language\. &lt;lang&gt; is an ISO language code\.
 - **\-width=&lt;n&gt;** establecer el ancho de salida a &lt;n&gt; caracteres \(0=sin límite\)\.
 - **\-shl** mostrar nivel de subproyecto en las líneas de salida
 - **\-viewhelp** long help in text viewer
 - **\-longhelp** ayuda detallada
 - **\-longhelpmd** long help in [Markdown](http://daringfireball.net/projects/markdown/) format
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** Harbour compiler credits
 - **\-build** Harbour compiler build information
 - **\-version** muestra solo versión de cabecera
  
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
 - **clipper** mode \-hbcmp \(emulate Clipper compiler\)
 - **rtlink** mode \-rtlink \(emulate Clipper linker\)
 - **exospace** mode \-rtlink \(emulate Clipper linker\)
 - **blinker** mode \-rtlink \(emulate Clipper linker\)
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
  
Files:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** collection of options\. Can be used to collect common ones into a file and include that into project files\. Uses same format as \.hbp files\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate lines\.
 - **\*\.ch** if passed directly as a source file, it will be used as additional standard header
 - **hbmk\.hbc** standard \.hbc file that gets automatically processed, if present\. Possible location\(s\) \(in order of precedence\) \[\*\]: %APPDATA%\\\.harbour, &lt;hbmk2 directory&gt;
 - **hbmk\.hbm** optional \.hbm file residing in current working directory, which gets automatically processed before other options
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Harbour script
 - **\*\.hrb** Harbour portable binary \(aka precompiled Harbour script\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbmk2 directory&gt;
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
 - **$\{hb\_tempdir\}** OS directory for temporary files
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
 - **\{&lt;targettype&gt;\}** target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** target is multi\-threaded \(see \-mt option\)
 - **\{st\}** target is single\-threaded \(see \-st option\)
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
 - **\{xhb\}** xhb mode \(see \-xhb option\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** filter will pass if &lt;file&gt; or &lt;dir&gt; name exists on disk\.
 - **\{MACRO\}** filter will pass if $\{MACRO\} value is not empty and not equal to '0' or 'no' \(case insensitive\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is larger than &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is smaller than &lt;value&gt; \(case insensitive\)\.


Predefined constants in sources:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** when an \.hb script is compiled as hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** when an \.hbx source file is present in a project \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** when &lt;depname&gt; dependency was detected \(available in C sources\)


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\):


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
 - **HB\_COMPILER\_VER** override C compiler version autodetection \(gcc and msvc compiler families only\)\. Format: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
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
 - **requests=** add space separated list of symbols to force link to the target
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
 - **cpp=** same as \-cpp= option
 - **warn=** same as \-warn= option
 - **compr=** same as \-compr= option
 - **head=** same as \-head= option
 - **plugins=** space separated list of hbmk2 plugins to load
 - **gt=&lt;name&gt;** same as \-gt&lt;name&gt; option
 - **gtdef=&lt;name&gt;** set the default GT to be used
 - **env=** same as \-env: option
 - **deppkgname=** same as \-deppkgname= option
 - **depkeyhead=** same as \-depkeyhead= option
 - **depoptional=** same as \-depoptional= option
 - **depcontrol=** same as \-depcontrol= option
 - **depincroot=** same as \-depincroot= option
 - **depincpath=** same as \-depincpath= option
 - **depincpathlocal=** same as \-depincpathlocal= option
 - **depimplibs=** same as \-depimplibs= option
 - **depimplibd=** same as \-depimplibd= option
 - **name=** package name
 - **description=** package description
 - **version=&lt;x\.y\.z&gt;** package version number, where x,y,z &gt;= 0 &lt;= 255\. Defaults to 0\.0\.1, if not specified\.
 - **keywords=** space separated list of keywords
 - **licences=** space separated list of licenses
 - **repository=** space separated list of source repository references


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, cExt \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unknown file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, cFileName \) \-&gt; NIL**  
Add a Harbour input file to the project\.
 - **hbmk\_AddInput\_C\( hbmk, cFileName \) \-&gt; NIL**  
Add a C input file to the project\.
 - **hbmk\_AddInput\_CPP\( hbmk, cFileName \) \-&gt; NIL**  
Add a C\+\+ input file to the project\.
 - **hbmk\_AddInput\_RC\( hbmk, cFileName \) \-&gt; NIL**  
Add a Windows resource input file to the project\.
 - **hbmk\_AddInput\_OBJ\( hbmk, cFileName \) \-&gt; NIL**  
Add a binary object file to the project\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, cFileName, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Add a file to be installed, with an optional \-instpath= group name\.
 - **hbmk\_OutStd\( hbmk, cText \) \-&gt; NIL**  
Output text to stdout\.
 - **hbmk\_OutErr\( hbmk, cText \) \-&gt; NIL**  
Output text to stderr\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Output text to stdout without any formatting\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Output text to stderr without any formatting\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Evaluate hbmk2 macro expression\.
 - **hbmk\_FNameEscape\( hbmk, cFileName \) \-&gt; &lt;cFileName&gt;**  
Escape/quote filename for using it as external command parameter\.
 - **hbmk\_PathSepToTarget\( hbmk, cFileName \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target toolchain\.
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
 - **hbmk\_StrStripQuote\( cString \) \-&gt; &lt;cString&gt;**  
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
 - **"nExitCode"** Current exit code
  
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
Unload package\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
List of loaded packages\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
ProgName\(\) not mapped to script\.


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
 - **1** unknown platform
 - **2** unknown compiler
 - **3** failed Harbour detection
 - **5** failed stub creation
 - **6** failed in compilation \(Harbour, C compiler, Resource compiler\)
 - **7** failed in final assembly \(linker or library manager\)
 - **8** unsupported
 - **9** failed to create working directory
 - **19** help
 - **10** dependency missing or disabled
 - **20** plugin initialization
 - **30** too deep nesting
 - **50** stop requested
 - **&lt;other&gt;** when \-run option is used, the exit code will be the one returned by the target executable
  
Notas:  


  - &lt;script&gt; puede ser:  
 &lt;@script&gt; o &lt;script\.hbm&gt;: comandos de opciones de línea en el archivo  
 &lt;script\.hbp&gt;: comandos de opciones de línea en el archivo, también marca un nuevo destino si se especifica en la línea de comandos  
 &lt;script\.hbc&gt;: archivo de configuración de paquetes
  - Source filename without extension will load the \.hbp file, if such \.hbp file exists in current directory\. If not, \.prg extension will be used\.
  - Múltiples parámetros son aceptados \-l, \-L, \-i y &lt;script&gt;\.
  - las opciones regulares de compilador Harbour también son aceptadas\.  
\(Verlos con la opción \-harbourhelp\)
  - archivo de opciones hbmk\.hbc en directorio de hbmk2 siempre es procesado si existe\. En plataformas \*nix este archivo es chequeado \(en este orden\) ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc antes de pasar a directorio de hbmk2\.
  - hbmk\.hbm hace script en el directorio actual siempre se procesa, si existe\.
  - Using forwards slashes is recommended in option values as directory separator, but backslashes are also equally accepted\.
  - filtros para plataformas son aceptados en cada linea de archivo \.hbc y con varias opciones\.  
Formato de filtro: \{\[\!\]\[&lt;plataforma&gt;|&lt;compilador&gt;|&lt;cpu&gt;|&lt;palabra\-clave&gt;\]\}\. Filtros pueden ser combinados usando los operadores '&amp;', '|' y agrupados en parénteses\. Ej\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Most \.hbc lines \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) and corresponding command\-line parameters will accept macro variables\. libpaths= also accepts %\{hb\_name\} which translates to the name of the \.hbc file under search\.
  - Tambien acepta Opciones de macros sustitución de comandos\. Incluya comando dentro de \`\`, y, si el comando contiene espacios, también entre comillas dobles\. F\.e\. "\-cflag==\`wx\-config \-cflags\`", o ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Libraries and object files built with/for CA\-Cl\*pper will not work with any supported platform/compiler\.
  - Defaults and feature support may vary by platform/compiler\.
  - GNU Make or any C compiler specific make tool and MSYS \(on Windows\) are not needed to run hbmk2\.
  - \. \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Alt\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'win' host platform\.


Valores suportados para &lt;compiler&gt; conforme a &lt;platform&gt; disponible:


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

  
Author:  


 - Viktor Szakáts \(harbour syenar\.net\) 
