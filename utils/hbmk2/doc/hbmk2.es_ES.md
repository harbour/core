Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  
Traducción \(es\_ES\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintáxis:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descripción:  


  hbmk2 es una herramienta integrada y portátil de generación o automatización de código, haciendo posible la creación de varios tipos de ejecutables binarios \(ejecutable, biblioteca dinámica, biblioteca estática, binario portátil de Harbour\) de múltiples tipos de código fuente \(C, C\+\+, Objective\-C, Harbour, traducciones de 'gettext', recursos de Windows\)\. 'Integrada' significa que un solo fichero de proyecto hbmk2 puede controlar todos, o casi todos, los aspectos del proceso de construcción\. 'Portátil' significa que un solo fichero de proyecto hbmk2 puede controlar la construcción del ejecutable binario en todas las plataformas de los sistemas operativos soportados y a través de todos los compiladores de C soportados\. Ayuda también en la mayoría de los procesos de construcción por medio de cortos y simples ficheros de proyecto \(opciones\)\. hbmk2 soporta ficheros de proyecto para C/C\+\+/Objetive\-C sin relación con Harbour\. Para conseguir esos objetivos, hbmk2 detecta automáticamente a Harbour, al compilador de C y a las demás herramientas necesarias, las configura y luego las ejecuta convenientemente\. hbmk2 permite extender los tipos de código fuente soportados por medio de complementos\.  
 Además de construir ejecutables, hbmk2 puede ejecutar archivos de órdenes de Harbour \(tanto en código fuente como precompilado\) directamente, y otra característica es que dispone de un intérprete de comandos interactivo\.
  
Opciones:  


 - **\-o&lt;outname&gt;** nombre de archivo de salida
 - **\-l&lt;libname&gt;** enlaza con la biblioteca &lt;libname&gt;\. &lt;libname&gt; se especificará sin ruta, extensión y sin el prefijo 'lib' \(excepto que sea parte del nombre\)\. No incluyas las bibliotecas del núcleo de Harbour, ya que se añaden automáticamente según se necesitan\. Si &lt;libname&gt; comienza con el caracter '\-', esta se quitará de la lista de bibliotecas a la hora de enlazar\.
 - **\-L&lt;libpath&gt;** rutas adicionales para buscar archivos de bibliotecas
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** rutas adicionales para buscar archivos de cabecera
 - **\-static|\-shared** enlazar con librerías estáticas/compartidas
 - **\-gt&lt;name&gt;** enlazar con el controlador GT GT&lt;name&gt;, se puede repetir para enlazar más GTs\. El primero de ellos será el GT por defecto en tiempo de ejecución\.
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
 - **\-cpp=&lt;value&gt;** selecciona el modo C\+\+\. Los valores permitidos son: def, yes, no
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
 - **\-warn=&lt;level&gt;** set C compiler warning level  
&lt;level&gt; can be: max, yes, low, no, def \(default: yes\)
 - **\-safe\[\-\]** enable safety options in C compiler/linker \(default: enabled on Windows, disabled on other systems\)
 - **\-compr=&lt;level&gt;** compress executable/dynamic lib \(needs UPX tool\)  
&lt;level&gt; can be: yes, no, min, max
 - **\-run\[\-\]** ejecutar/no ejecutar aplicativo generado\.
 - **\-vcshead=&lt;file&gt;** generar archivo de cabecera \.ch con información del repositorio local\. Actualmente están soportados Git, SVN, Mercurial, Bazaar, Fossil, CVS y Monotone\. El archivo de cabecera definirá la constante de preprocesador \_HBMK\_VCS\_TYPE\_ con el nombre del VCS detectado, y \_HBMK\_VCS\_ID\_ con el ID único del repositorio local\. Si no se detecta un VCS, un número secuencial será incrementado cada vez que se construya\.
 - **\-tshead=&lt;file&gt;** generar archivo de cabecera \.ch con información de fecha/hora\. Cabecera generado definirá macros \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ con fecha/hora de creación de archivo\.
 - **\-icon=&lt;file&gt;** establece &lt;file&gt; como icono de la aplicación\. &lt;file&gt; debe ser de un formato soportado por la plataforma de destino \(no soportado por algunas plataformas/compiladores\)\. En Windows, está implementado generando y enlazando un archivo de recurso\.
 - **\-manifest=&lt;file&gt;** incrustar manifiesto &lt;file&gt; en ejecutable/lib dinámica \(sólo Windows\)
 - **\-sign=&lt;key&gt;** firma del ejecutable con &lt;key&gt; \(solo en Windows y Darwin\)\. En Windows se usa signtool\.exe \(incluido en MS Windows SDK\) o posign\.exe \(incluido en Pelles C 7\), por ese orden, se detectan ambos automaticamente\.
 - **\-signpw=&lt;pw&gt;** utilice &lt;pw&gt; como contraseña cuando firme el ejecutable \(sólo Windows y Darwin\)
 - **\-instfile=&lt;g:file&gt;** anadir &lt;archivo&gt; a la lista de archivos que desea copiar a la ruta especificada por la opción \-instpath\. &lt;g&gt; es un grupo opcional de copia \(distingue mayúsculas y minúsculas\), debe haber al menos dos caracteres\. En caso de que no se especifica &lt;archivo&gt;, la lista de archivos en ese grupo se vaciará\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copy target file\(s\) to install path even if already up to date
 - **\-depimplib\[\-\]** activar \(o desactivar\) la generación de Bibliotecas de Importación para fuentes de biblioteca de importación especificados en la opción \-depimplibs= \(por defecto: yes\)
 - **\-stop\[=&lt;text&gt;\]** parar sin hacer nada y mostrar &lt;text&gt; si se ha especificado
 - **\-echo=&lt;text&gt;** eco de texto en la pantalla
 - **\-pause** forzar pause para presionar una tecla en caso de error \(solo con driver GT alternativo\)
 - **\-exitstr** mostrar el error resultante al salir como texto reconocible
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
 - **\-signflag=&lt;f&gt;** pasar un solo indicador al comando de firma de código
 - **\-runflag=&lt;f&gt;** pasar flag a ejecutable de salida cuando opción \-run es utilizada
 - **\-cflag\+=&lt;f&gt;** pasar solo bandera del compilador de C para reemplazar las opciones del compilador C anadida por hbmk2 mismo\. Usar con precaución\.
 - **\-ldflag\+=&lt;f&gt;** pasar una sola opción al enlazador \(ejecutable\) después de la lista de bibliotecas\. Usar con precaución\.
 - **\-dflag\+=&lt;f&gt;** pasar una sola opción al enlazador \(biblioteca dinámica\) después de la lista de bibliotecas\. Usar con precaución\.
 - **\-3rd=&lt;f&gt;** opciones/flags reservados para herramientas de terceros, siempre ignorado por hbmk2 en sí
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alterar el ambiente local\. &lt;e&gt; es el nombre de la variable de entorno a alterar\. &lt;o&gt; puede ser '=' para establecer/reemplazar, '\-' para borrar, '\+' para anadir al final de valor existente, '\#' para insertar al principio del valor existente\. &lt;v&gt; es el valor a poner/agregar/insertar\.
 - **\-jobs=&lt;n&gt;** Inicia &lt;n&gt; threads de compilación \(solo para plataformas multiproceso\)
 - **\-head=&lt;m&gt;** analizando fuente de control de encabezado \(en construcción en modo incremental\)  
&lt;m&gt; puede ser: nativo \(compilador utilizado para extraer las dependencias\), completa \(por defecto, utiliza analizador de texto simple en el fichero entero\), dep, off
 - **\-rebuild** reconstrucción \(en modo incremental\)
 - **\-rebuildall** reconstruir con sub\-proyectos \(contrucción en modo incremental\)
 - **\-clean** compilación limpia \(en modo incremental\)
 - **\-workdir=&lt;dir&gt;** directorio de trabajo  
\(por defecto: \.hbmk/&lt;plataforma&gt;/&lt;compilador&gt; \[\*\] en modo incremental, si no, directorio temporal del SO\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** crear librería de importación \(solo en Windows\)


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
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; es el nombre de la dependencia\. Añadir &lt;i&gt; a la lista de rutas de detección de cabeceras, donde &lt;i&gt; apunta a un directorio local del proyecto, conteniendo una dependencia embebida \(conocido como 'alojado localmente'\)\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; es el nombre de la dependencia\. Anadir &lt;dll&gt; a la lista fuente de la biblioteca de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; es el nombre de la dependencia\. Establecer nombre generado de biblioteca de importación a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; is the name of the dependency\. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly\. Optional, if omitted, detection will take place after processing all options\.


 - **\-plugin=&lt;filename&gt;** agregar plugin \(módulo\)\. &lt;filename&gt; puede ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** pasar archivo de entrada a plugins
 - **\-pflag=&lt;f&gt;** pasar solo bandera para plugins
  
Las siguientes opciones están disponibles en línea de comandos:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** run build target
 - **\-hbraw** parar después de ejecutar compilador Harbour
 - **\-hbcmp|\-clipper** para después de la creación de los archivos objeto  
crear un enlace/copia a hbmk2 para hbcmp/clipper resultará el mismo efecto
 - **\-hbcc** acepta raw C flags  
create enlace/copia hbmk2 para hbcc para el mismo efecto
 - **\-hblnk** aceptar flags primas del enlazador
 - **\-autohbm\[\-\]** activar \(o desactivar\) procesamiento de hbmk\.hbm en el directorio actual \(por defecto: yes\)
 - **\-hb10** habilita modo de compatibilidad 'Harbour 1\.0\.x'
 - **\-hb20** activa el modo de compatibilidad Harbour 2\.0\.x
 - **\-hb30** activar modo de compatibilidad con Harbour 3\.0\.x
 - **\-xhb** habilitar modo xHb
 - **\-hbc** activa modo puro C
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emula comportamiento de linkeditor compatible con clipper  
crear link o copiar hbmk2 para rtlink/blinker/exospace resultará el mismo efecto


 - **\-hbreg\[=global\]** registrar Harbour Script \(\.hb\) con hbmk2 \(solo en Windows\)
 - **\-hbunreg\[=global\]** unregister Harbour Script \(\.hb\) from hbmk2 \(Windows only\)


 - **\-find &lt;text&gt;** lists all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)


 - **\-hbmake=&lt;file&gt;** convierte proyecto hbmake en un archivo \.hbp
 - **\-xbp=&lt;file&gt;** convierte proyecto \.xbp \(xbuild\) en un archivo \.hbp
 - **\-xhp=&lt;file&gt;** convierte un proyecto \.xhp \(xMate\) en un archivo \.hbp


 - **\-\-hbdirbin** output Harbour binary directory to stdout
 - **\-\-hbdirdyn** output Harbour dynamic library directory to stdout
 - **\-\-hbdirlib** output Harbour static library directory to stdout
 - **\-\-hbdirinc** output Harbour header directory to stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** override default target platform \(default: automatic\)
 - **\-cpu=&lt;cpu&gt;** selecciona la CPU de destino\. \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler autodetection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** utilizar un nombre de build especifico
 - **\-lang=&lt;lang&gt;** sobreescribir idioma por defecto\. &lt;lang&gt; es un código ISO de idioma\.
 - **\-width=&lt;n&gt;** establecer el ancho de salida a &lt;n&gt; caracteres \(0=sin límite\)\.
 - **\-shl** mostrar nivel de subproyecto en las líneas de salida
 - **\-viewhelp** abrir ayuda completa en visor de texto
 - **\-longhelp** ayuda detallada
 - **\-longhelpmd** ayuda completa en formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** Harbour compiler help \(all Harbour compiler options are accepted as is by hbmk2\)
 - **\-credits** créditos del compilador Harbour
 - **\-build** información de la construcción del compilador Harbour
 - **\-version** muestra solo versión de cabecera
  
Options below are internal/developer ones \(compatibility not guaranteed\):  


 - **\-debugtime** measure time spent on the build
 - **\-debuginc** display internals of incremental build
 - **\-debugstub** display content of all internally generated source files
 - **\-debugi18n** display internals on translation file generation
 - **\-debugdepd** display internals of dependency detection
 - **\-debugpars** display all input parameters in processing order
 - **\-debugrte** generate a run\-time error


Puedes crear un enlace simbólico/copiar/renombrar hbmk2 a los siguientes nombres para cambiar el modo de ejecución por defecto:


 - **hbrun\*|\*hbrun** mode script runner / interactive shell
 - **hbrund|hbrun\*d** mode script runner / interactive shell in debug mode
 - **harbour** modo \-hbraw \(emular el compilador Harbour plano\)
 - **clipper** modo \-hbcmp \(emular compilador Clipper\)
 - **rtlink** modo \-rtlink \(emular enlazador de Clipper\)
 - **exospace** modo \-rtlink \(emular enlazador de Clipper\)
 - **blinker** modo \-rtlink \(emular enlazador de Clipper\)
 - **\*10** opción \-hb10
 - **\*20** opción \-hb20
 - **\*30** opción \-hb30
 - **x\*** opción \-xhb
 - **hbcmp\*|\*hbcmp** modo \-hbcmp \(emular compilador Harbour creando un objeto binario\)
 - **hbcc\*|\*hbcc** modo \-hbcc \(emular compilador de C\)
 - **hblnk\*|\*hblnk** modo \-hblnk \(emular enlazador de C\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Ficheros:  


 - **\*\.hbp** project file\. Can contain any number of command\-line options, which are expected to create an output\. Lines beginning with '\#' character are ignored, otherwise newline is optional and options are space separated, just like on the command\-line\. You must enclose option containing space in double quotes\. Each \.hbp file reference will be executed as a sub\-project\.
 - **\*\.hbm** colección de opciones\. Puede ser utilizada para recoger opciones por defecto en un archivo e incluirlo dentro de un archivo de proyecto\. Utiliza el mismo formato que los archivos \.hbp\.
 - **\*\.hbc** collection of options that accompany components \(aka 'libs', aka packages\)\. Use different syntax than command\-line and \.hbp/\.hbm files\. Lines beginning with '\#' character are ignored, each directive must be placed in separate lines\.
 - **\*\.ch** si se pasa directamente como un archivo fuente, se utilizará como una cabecera estándar adicional
 - **hbmk\.hbc** archivo \.hbc estandar que es procesado automáticamente si está presente\. Posible\(s\) localizacion\(es\) \(en orden de preferencia\) \[\*\]: %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **hbmk\.hbm** archivo \.hbm situado en el directorio de trabajo actual, que es procesado automáticamente antes que otras opciones
 - **$hb\_pkg\_dynlib\.hbm** archivo especial \.hbm incrustado en hbmk2\. Se encarga de la creación de una biblioteca dinámica \(al estilo de las contribuciones de Harbour\)
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Archivo de órdenes de Harbour
 - **\*\.hrb** binario portable de Harbour \(aka archivo de comandos pre\-compilado de Harbour\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puedes deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de linea\)\. Se guarda en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensiones para cargar en el interprete de comandos interactivo de Harbour\. Una extensión por línea, y se ignora todo lo que hay detrás del caracter '\#'\. Nombre de fichero alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\
  
Variables de macro:  


 - **$\{hb\_root\}** directorio de hbmk2
 - **$\{hb\_dir\}** directorio del fichero en el que es usado
 - **$\{hb\_dirname\}** top directory of the filename it is used in
 - **$\{hb\_name\}** nombre del archivo que se ha utilizado \(sin ruta ni extensión\)
 - **$\{hb\_self\}** full filename it is used in
 - **$\{hb\_curdir\}** directorio de trabajo actual
 - **$\{hb\_tempdir\}** Directorio para archivos temporales del sistema operativo
 - **$\{hb\_targetname\}** name of the project \(without directory and extension\)\. Returns \.adhoc\. if there is not project file\.
 - **$\{hb\_targettype\}** tipo de proyecto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** plataforma seleccionada
 - **$\{hb\_comp\}** Compilador de C seleccionado
 - **$\{hb\_comp\_ver\}** versión del compilador C
 - **$\{hb\_build\}** build name
 - **$\{hb\_cpu\}** CPU seleccionada
 - **$\{hb\_work\}** default base workdir name
 - **$\{hb\_workdynsub\}** subdirectorio de trabajo por defecto para bibliotecas dinámicas de destino
 - **$\{hb\_dynprefix\}** prefijo de la librería dinámica
 - **$\{hb\_dynsuffix\}** sufijo de la librería dinámica
 - **$\{hb\_dynext\}** dynamic library extension
 - **$\{hb\_ver\}** Versión de Harbour en formato hexadecimal de tres bytes\. P\.ej\.: 030200
 - **$\{hb\_verstr\}** Versión de Harbour en un formato legible para humanos &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. Por ejemplo: 3\.2\.0dev
 - **$\{hb\_major\}** Harbour major version number
 - **$\{hb\_minor\}** Harbour minor version number
 - **$\{hb\_release\}** Harbour release version number
 - **$\{hb\_status\}** Estado de la versión de Harbour
 - **$\{hb\_revision\}** Revisión de Harbour
 - **$\{hb\_host\_plat\}** Harbour host platform
 - **$\{hb\_host\_plat\_unix\}** devuelve '1' si la plataforma anfitriona es compatible \*nix\.
 - **$\{hb\_bin\}** Harbour binary directory
 - **$\{hb\_lib\}** Directorio de las bibliotecas estáticas de Harbour
 - **$\{hb\_lib3rd\}** Directorio de las librerías estáticas de terceros de Harbour
 - **$\{hb\_dyn\}** Directorio de las bibliotecas de enlace dinámico de Harbour
 - **$\{hb\_inc\}** Harbour header directory
 - **$\{hb\_addons\}** Harbour add\-ons base directory
 - **$\{hb\_first\}** nombre del fichero de código fuente que contiene la función de entrada \(sin el directorio ni la extensión\)
 - **$\{hb\_outputdir\}** directorio de salida
 - **$\{hb\_outputname\}** Nombre del archivo de salida \(sin extensión\)
 - **$\{hb\_level\}** sub\-project recursion level
 - **$\{&lt;depname&gt;\}** devuelve el directorio de archivos de cabecera de la dependencia &lt;depname&gt;, o '1' si no se ha detectado
 - **$\{&lt;envvar&gt;\}** devolver el valor de la variable de entorno &lt;envvar&gt;
  
Filtros \(puedes combinarlos y/o negarlos\):  


 - **\{&lt;platform&gt;\}** target platform\. Where &lt;platform&gt; can be any value accepted by \-plat= option\.
 - **\{&lt;compiler&gt;\}** target C compiler\. Where &lt;compiler&gt; can be any value accepted by \-comp= option\.
 - **\{&lt;cpu&gt;\}** CPU destino\. &lt;cpu&gt; puede ser una de: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** build target type\. Where &lt;targettype&gt; is any of the values returned by macro variable $\{hb\_targettype\}\.
 - **\{mt\}** build target is multi\-threaded \(see \-mt option\)
 - **\{st\}** build target is single\-threaded \(see \-st option\)
 - **\{gui\}** GUI target \(see \-gui option\)
 - **\{std\}** el objetivo es una consola de linea de comandos \(ver opción \-console\)
 - **\{debug\}** C level debugging is enabled \(see \-debug option\)
 - **\{nodebug\}** La depuración a nivel C está desactivada \(ver la opción \-debug\-\)
 - **\{shared\}** shared build \(see \-shared and related options\)
 - **\{static\}** static build \(see \-static and related options\)
 - **\{lngcpp\}** modo C\+\+ forzado \(ver la opción \-cpp\)
 - **\{lngc\}** modo C forzado \(ver la opción \-cpp\-\)
 - **\{winuni\}** Modo UNICODE \(WIDE\) de Windows \(ver la opción \-winuni\)
 - **\{winansi\}** modo ANSI de Windows \(ver la opción \-winuni\-\)
 - **\{unix\}** la plataforma de destino es compatible \*nix \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** la plataforma de destino es compatible con Windows \(win, wce\)
 - **\{allgcc\}** el compilador de C de destino pertenece a la familia gcc \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** el compilador de C de destino es mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** el compilador de C de destino es msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** el compilador de C de destino es bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** target C compiler is pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** target C compiler is icc\* \(icc, iccia64\)
 - **\{hb10\}** Modo de compatibilidad 'Harbour 1\.0\.x' \(ver opción \-hb10\)
 - **\{hb20\}** Modo de compatibilidad 'Harbour 2\.0\.x' \(ver opción \-hb20\)
 - **\{hb30\}** Modo de compatibilidad 'Harbour 3\.0\.x' \(ver opción \-hb30\)
 - **\{xhb\}** xhb mode \(see \-xhb option\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** el filtro pasará si el nombre &lt;file&gt; o &lt;dir&gt; existe en el disco\.
 - **\{MACRO\}** el filtro se pasará si el valor de $\{MACRO\} no está vacio y no es igual a '0' o 'no' \(en mayúsculas o minúsculas\)
 - **\{MACRO='&lt;value&gt;'\}** filter will pass if $\{MACRO\} value equals to &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is larger than &lt;value&gt; \(case insensitive\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** filter will pass if $\{MACRO\} value is smaller than &lt;value&gt; \(case insensitive\)\.


Constantes predefinidas en el código fuente:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** when an \.hb script is compiled as hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** when an \.hbx source file is present in a project \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** when &lt;hbcname&gt;\.hbc package is linked to the build target\. The value is the version= value from the \.hbc file, converted to a decimal number, which is '1', if not specified\. \(available in Harbour sources\)
 - **HBMK\_HAS\_&lt;depname&gt;** cuando la dependencia &lt;depname&gt; se detectó \(disponible en código fuente C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Constantes predefinidas en ficheros de construcción \(disponibles después de '\-depfinish=&lt;depname&gt;'/'depfinish=&lt;depname&gt;'\):


 - **HBMK\_HAS\_&lt;depname&gt;** cuando la dependencia &lt;depname&gt; se detectó
 - **HBMK\_DIR\_&lt;depname&gt;** devuelve el directorio de los archivos de cabecera donde &lt;depname&gt; ha sido detectado, o vacio si no\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** cuando la dependencia &lt;depname&gt; se detectó en la localización configurada por la opción \-decincpathlocal=
  
Variables de entorno:  


 - **HBMK\_OPTIONS** accepts any options as if they were passed in the beginning of the command\-line
 - **HB\_PLATFORM** admite los mismos valores que la opción \-plat=
 - **HB\_COMPILER** admite los mismos valores que la opción \-comp=
 - **HB\_CPU** admite los mismos valores que la opción \-cpu=
 - **HB\_BUILD\_NAME** admite los mismos valores que la opción \-build=
 - **HB\_LANG** admite los mismos valores que la opción \-lang=
 - **HB\_USER\_LIBS** acepta los mismos valores \(separados por espacios\) que la opción \-l
 - **HB\_USER\_LIBPATHS** acepta los mismos valores \(separados por espacios\) que la opción \-L
 - **HB\_USER\_PRGFLAGS** opciones para pasar al compilador Harbour \(antes de las opciones de línea de comandos\)
 - **HB\_USER\_CFLAGS** opciones para pasar al compilador de C \(antes de las opciones de línea de comandos\)
 - **HB\_USER\_RESFLAGS** opciones a pasar al compilador de recursos \(antes de las opciones de línea de comandos\)\(sólo Windows\)
 - **HB\_USER\_LDFLAGS** opciones para ser enviadas al enlazador \(ejecutable\) \(antes de las opciones de la linea de comandos\)
 - **HB\_USER\_DFLAGS** opciones para ser enviadas al enlazador \(librería dinámica\) \(antes de las opciones de la linea de comandos\)
 - **HB\_USER\_AFLAGS** opciones para ser enviadas al enlazador \(librería estática\) \(antes de las opciones de la linea de comandos\)
 - **HB\_COMPILER\_VER** sustituye la autodetección de la versión del compilador de C \(solo en las familias de compiladores gcc y msvc\)\. Formato: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** Sustituir el directorio del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_CCPREFIX** Sustituir el prefijo del ejecutable del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_CCSUFFIX** Sustituir el sufijo del ejecutable del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_INSTALL\_PREFIX** Sustituir el directorio base de la instalación de Harbour
 - **HB\_INSTALL\_ADDONS** sustituye el directorio base de los complementos de Harbour


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
directivas \.hbc \(tienen que ser escritas en líneas separadas\):  


 - **echo=&lt;msg&gt;** mostrar &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** Omitir el procesado del resto del archivo \.hbc\. Mostrar &lt;msg&gt;, si se especifica\.
 - **stop=\[&lt;msg&gt;\]** stop the build\. Display &lt;msg&gt;, if specified\.
 - **sources=** add space separated list of files as input files
 - **headers=** añadir una lista de archivos de cabecera \.ch separados por espacios como cabeceras estándar
 - **libs=** añadir lista de bibliotecas separadas por espacios \(ver más en la opción \-l\)
 - **frameworks=** add space separated list of frameworks \(Darwin only\)
 - **requests=** add space separated list of symbols to force link to the build target
 - **syslibs=** añadir lista de bibliotecas separadas por espacios como bibliotecas del sistema \(antes de las bibliotecas normales\)
 - **hbcs=** incrusta una lista de archivos \.hbc separados por espacios\. Se aceptan nombres sin extensión\. Estas referencias se procesan en el sitio\.
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
 - **gui=&lt;bool&gt;** opción 'sí' = \-gui, 'no' = \-std
 - **mt=&lt;bool&gt;** opción 'sí' = \-mt, 'no' = \-st
 - **pic=&lt;bool&gt;** opción 'sí' = \-pic, 'no' = \-pic\-
 - **shared=&lt;bool&gt;** opción 'sí' = \-shared, 'no' = \-static
 - **shareddef=&lt;bool&gt;** similar to shared=, but works only if shared/static mode was not set before
 - **fullstatic=&lt;bool&gt;** opción 'sí' = \-fullstatic, 'no' = \-static
 - **debug=&lt;bool&gt;** opción 'sí' = \-debug, 'no' = \-debug\-
 - **optim=** opción 'sí' = \-optim, 'no' = \-optim\-
 - **nulrdd=&lt;bool&gt;** opción 'sí' = \-nulrdd, 'no' = \-nulrdd\-
 - **nodefgt=&lt;bool&gt;** opción 'sí' = \-nodefgt, 'no' = \-nodefgt\-
 - **map=&lt;bool&gt;** opción 'sí' = \-map, 'no' = \-map\-
 - **hbcppmm=&lt;bool&gt;** opción 'sí' = \-hbcpmm, 'no' = \-hbcpmm\-
 - **implib=&lt;bool&gt;** opción 'sí' = \-implib, 'no' = \-implib\-
 - **winuni=&lt;bool&gt;** opción 'sí' = \-winuni, 'no' = \-winuni\-
 - **strip=&lt;bool&gt;** opción 'sí' = \-strip, 'no' = \-strip\-
 - **run=&lt;bool&gt;** opción 'sí' = \-run, 'no' = \-run\-
 - **inc=&lt;bool&gt;** opción 'sí' = \-inc, 'no' = \-inc\-
 - **safe=&lt;bool&gt;** opción 'sí' = \-safe, 'no' = \-safe\-
 - **cpp=** el mismo que la opción \-cpp=
 - **warn=** el mismo que la opción \-warn=
 - **compr=** el mismo que la opción \-compr=
 - **head=** el mismo que la opción \-head=
 - **plugins=** space separated list of hbmk2 plugins to load
 - **gt=&lt;name&gt;** el mismo que la opción \-gt&lt;name&gt;
 - **gtdef=&lt;name&gt;** set the default GT to be used
 - **env=** el mismo que la opción \-env:
 - **deppkgname=** el mismo que la opción \-deppkgname=
 - **depkeyhead=** el mismo que la opción \-depkeyhead=
 - **depoptional=** el mismo que la opción \-depoptional=
 - **depcontrol=** el mismo que la opción \-depcontrol=
 - **depincroot=** el mismo que la opción \-depincroot=
 - **depincpath=** el mismo que la opción \-depincpath=
 - **depincpathlocal=** el mismo que la opción \-depincpathlocal=
 - **depimplibs=** el mismo que la opción \-depimplibs=
 - **depimplibd=** el mismo que la opción \-depimplibd=
 - **name=** nombre del paquete
 - **description=** descripción del paquete
 - **version=&lt;x\.y\.z&gt;** número de versión del paquete, donde x\.y\.z &gt;= 0 &lt;= 255\. Si no se especifica se usa el valor por defecto '0\.0\.1'\.
 - **keywords=** lista de palabras clave separadas por espacios
 - **licences=** lista de licencias separadas por espacios
 - **repository=** space separated list of source repository references


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Registra la extensión de archivo de entrada para ser pasada a un complemento \(por defecto, todos los archivos con extensión desconocida se pasan al compilador Harbour\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añadir un archivo de entrada de Harbour al proyecto\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añadir un archivo de entrada de C al proyecto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añadir un archivo de entrada de C\+\+ al proyecto\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añadir un archivo de entrda de recursos de Windows al proyecto\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añadir un archivo objeto binario al proyecto\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Añadir un archivo para instalar, con un nombre de grupo \-instpath= opcional\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Enviar el texto de salida a stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Enviar el texto de salida a stderr\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Output text to stdout without any formatting\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Output text to stderr without any formatting\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Evaluate hbmk2 macro expression\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
El nombre de archivo tiene que estar entrecomillado para usarlo como parámetro de comando externo\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convert filename to the format required for the target platform/C compiler\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Convierte el nombre de archivo para que tenga barras como separadores de directorios\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Devolver la ruta relativa al valor de \-workdir= para el directorio de trabajo actual\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Buscar el archivo en &lt;xPath&gt; \(se admite una matriz o una cadena delimitada con separadores de ruta\) con una lista de extensiones alternativas &lt;aExtDef&gt; \(por defecto ejecutables binarios\)\. Devuelve un nombre de archivo si se encuentra, NIL si no\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Change directory and/or extension in filename\.
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Codifica el nombre de la función de acuerdo a las reglas del compilador Harbour para formar los nombres de función HB\_FUNC\(\) en el código C\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Strip double quote enclosure from a string\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convierte una lista de cadenas a una cadena\. Por defecto el separador es un solo espacio\.


Plugin variables:  
\('hbmk' context hash items, case\-sensitive, read\-only unless marked otherwise\)


 - **"apiver"** API version as an integer
 - **"cSTATE"** estado de retrollamada\. Puede ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** matriz de parámetros pasada al plugin vía opción \-pflag=/pi= o que tenga una extensión registrada vía hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** 'hash' de variables para uso del complemento\. Modificable, local para cada complemento\.
 - **"cPLAT"** valor de \-plat
 - **"cCOMP"** valor de \-comp
 - **"nCOMPVer"** ver la variable de entorno HB\_COMPILER\_VER
 - **"cCPU"** valor de \-cpu
 - **"cBUILD"** valor de \-build=
 - **"cOUTPUTNAME"** valor de \-o
 - **"cTARGETNAME"** ver la macro $\{hb\_targetname\}
 - **"cTARGETTYPE"** ver la macro $\{hb\_targettype\}
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
 - **"cCCPATH"** ver la variable de entorno HB\_CCPATH
 - **"cCCPREFIX"** ver la variable de entorno HB\_CCPREFIX
 - **"cCCSUFFIX"** ver la variable de entorno HB\_CCSUFFIX
 - **"cCCEXT"** ver la variable de entorno HB\_CCEXT
 - **"cWorkDir"** valor de \-workdir=
 - **"nExitCode"** Código de salida actual
  
API del intérprete de comandos disponible en los archivos de órdenes de Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Activa modo de compatibilidad 'Clipper' \(sin Unicode\)\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Cargar cabecera de Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar cabecera de Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Mostrar la lista de cabeceras de Harbour cargadas\.
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


 - **Ejecutar el intérprete de comandos interactivo \('dot' prompt\)**  
$ hbmk2 \.
 - **Ejecutar un archivo de órdenes de Harbour**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Examples to build and run Harbour portable binary \(aka precompiled Harbour script\):


 - **Para construir**  
$ hbmk2 \-gh myscript\.hb
 - **To run result of above**  
$ hbmk2 myscript\.hrb \[&lt;parameter\[s\]&gt;\]


Ejemplos para construir una aplicación Harbour:


 - **To build one simple \.prg**  
$ hbmk2 hello\.prg
 - **To build multiple \.prg sources into one application in incremental mode**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Construir una aplicación utilizando un archivo de proyecto**  
$ hbmk2 myapp\.hbp
 - **To build an application using incremental mode**  
$ hbmk2 myapp\.hbp \-inc
 - **Para construir una aplicación que utilice un paquete de contribuciones o de terceros que incorporen un archivo \.hbc**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **To build an application which uses a raw library**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **To build an application which uses a Windows resource**  
$ hbmk2 mymain\.prg myres\.rc
 - **To build an application which links against Harbour dynamic libraries**  
$ hbmk2 \-shared myapp\.prg
 - **To build an application out of all \.prg and \.c sources residing in 'source' subdir**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Ejemplos para construir una biblioteca estática con Harbour:


 - **Construir la librería 'mylib' desde el código fuente**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Construir la biblioteca 'mylib' desde el código fuente usando el modo incremental**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Códigos de salida \("errorlevels"\):  


 - **0** sin errores
 - **1** plataforma desconocida
 - **2** compilador desconocido
 - **3** no se pudo detectar Harbour
 - **5** failed stub creation
 - **6** fallo al compilar \(Harbour, compilador de C, compilador de recursos\)
 - **7** falló en el ensamblaje final \(enlazador o gestor de bibliotecas\)
 - **8** no soportado
 - **9** error al crear el directorio de trabajo
 - **19** ayuda
 - **10** dependencia no encontrada o desactivada
 - **20** inicio del complemento
 - **30** too deep nesting
 - **50** parada solicitada
 - **&lt;other&gt;** cuando se utilice la opción \-run, el código de salida será el que devuelva el ejecutable de destino
  
Notas:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - Un nombre de archivo fuente sin extensión cargará el archivo \.hbp, si este existe en el directorio actual\. Si no, la extensión \.prg será usada\.
  - Múltiples parámetros son aceptados \-l, \-L, \-i y &lt;script&gt;\.
  - las opciones regulares de compilador Harbour también son aceptadas\.  
\(Verlos con la opción \-harbourhelp\)
  - archivo de opciones hbmk\.hbc en directorio de hbmk2 siempre es procesado si existe\. En plataformas \*nix este archivo es chequeado \(en este orden\) ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc antes de pasar a directorio de hbmk2\.
  - hbmk\.hbm hace script en el directorio actual siempre se procesa, si existe\.
  - Se recomienda usar barras en los valores de opciones de directorios, pero tambien se aceptan igualmente barras invertidas\.
  - filtros para plataformas son aceptados en cada linea de archivo \.hbc y con varias opciones\.  
Formato de filtro: \{\[\!\]\[&lt;plataforma&gt;|&lt;compilador&gt;|&lt;cpu&gt;|&lt;palabra\-clave&gt;\]\}\. Filtros pueden ser combinados usando los operadores '&amp;', '|' y agrupados en parénteses\. Ej\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - La mayoría de la líneas de un fichero \.hbc \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) y sus correspondientes parámetros de línea de comandos aceptan variables de macro\. libpaths= también acepta %\{hb\_name\} que se transforma al nombre del fichero \.hbc que se busca\.
  - Tambien acepta Opciones de macros sustitución de comandos\. Incluya comando dentro de \`\`, y, si el comando contiene espacios, también entre comillas dobles\. F\.e\. "\-cflag==\`wx\-config \-cflags\`", o ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Bibliotecas y archivos objeto construidos con/para CA\-Cl\*pper no funcionarán con ningún compilador/plataforma soportados\.
  - Valores por defecto y caracteristicas soportadas pueden variar entre plataformas/compiladores\.
  - No es necesario para ejecutar hbmk2 la herramienta GNU Make o cualquier otra utilidad 'make' específica de un compilador C o de MSYS \(en Windows\)\.
  - si se pasa el \. \(punto\) como primer parámetro se entrará en el intérprete de comandos interactivo\.


  - el archivo \.hb, \.hrb o \.dbf pasado como primer parámetro será ejecutado como un archivo de órdenes\. Si el nombre del archivo no contiene componentes de una ruta, será buscado en el directorio actual y en el PATH\. Si no se especifica una extensión, se buscarán las extensiones \.hb y \.hrb en ese orden\. Los archivos \.dbf se abrirán automáticamente en modo compartido y el intérprete de comandos de Harbour será iniciado\. Las extensiones no\-estandar se autodetectarán para archivos de tipo fuente y archivos de órdenes precompilados\. Nótese, que para los archivos de órdenes de Harbour, la página de códigos \(codepage\) establecida por defecto es la UTF\-8\. El archivo de cabecera principal 'hb\.ch' es incluido \(\#include\) automáticamente\. El formato de fecha por defecto es el estandar ISO: yyyy\-mm\-dd\. El GT por defecto es 'gtcgi', excepto que se detecten llamadas CUI de pantalla completa, en cuyo caso el GT 'gtwin' \[\*\] se selecciona automáticamente \(excepto para INIT PROCEDURESs\)\.
  - Puede utilizar las teclas &lt;Alt\+V&gt; en el indicador de comandos interactivo de Harbour para pegar texto desde el portapapeles\.
  - Los valores marcados con \[\*\] pueden depender de la plataforma anfitriona y/o la configuración\. Esta ayuda se generó en una plataforma afitriona 'win'\.


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
