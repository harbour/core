Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-04\-03 03:33\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  
Traducción \(es\_ES\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintaxis:  
  
  hbmk2 \[opciones\] \[&lt;archivosdeorden\[es\]&gt;\] &lt;fuente\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descripción:  


  hbmk2 es una herramienta integrada y portable de generación o automatización de código, haciendo posible la creación de varios tipos de ejecutables binarios \(ejecutable, biblioteca dinámica, biblioteca estática, binario portátil de Harbour\) desde múltiples tipos de ficheros de código fuente \(C, C\+\+, Objective\-C, Harbour, traducciones de 'gettext', recursos de Windows\)\. 'Integrada' significa que un solo fichero de proyecto hbmk2 puede controlar todos, o casi todos, los aspectos del proceso de construcción\. 'Portable' significa que un solo fichero de proyecto hbmk2 puede controlar la construcción del ejecutable binario en todas las plataformas de los sistemas operativos soportados y a través de todos los compiladores de C soportados\. Ayuda también en la mayoría de los procesos de construcción por medio de pequeños y simples ficheros de proyecto \(opciones\)\. hbmk2 soporta ficheros de proyecto para C/C\+\+/Objetive\-C sin relación con Harbour\. Para conseguir esos objetivos, hbmk2 detecta automáticamente a Harbour, al compilador de C y a las demás herramientas necesarias, las configura y luego las ejecuta convenientemente\. hbmk2 permite extender los tipos de código fuente soportados por medio de complementos\.  
Además de construir ejecutables, hbmk2 puede ejecutar archivos de órdenes de Harbour \(tanto en código fuente como precompilado\) directamente, y otra característica de la que dispone es de un intérprete de comandos interactivo\.
  
Opciones:  


 - **\-o&lt;outname&gt;** nombre de archivo de salida
 - **\-l&lt;libname&gt;** enlaza con la biblioteca &lt;libname&gt;\. &lt;libname&gt; se especificará sin ruta, extensión y sin el prefijo 'lib' \(excepto que sea parte del nombre\)\. No incluyas las bibliotecas del núcleo de Harbour, ya que se añaden automáticamente según se necesitan\. Si &lt;libname&gt; comienza con el caracter '\-', esta se quitará de la lista de bibliotecas a la hora de enlazar\.
 - **\-L&lt;libpath&gt;** rutas adicionales para buscar archivos de bibliotecas
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** rutas adicionales para buscar archivos de cabecera
 - **\-static|\-shared** enlaza con librerías estáticas/compartidas
 - **\-gt&lt;name&gt;** enlaza con el controlador GT GT&lt;name&gt;, se puede repetir para enlazar más GTs\. El primero de ellos será el GT por defecto en tiempo de ejecución\.
 - **\-inc\[\-\]** habilita modo de compilación incremental
 - **\-hbexe** crea ejecutable \(por defecto\)
 - **\-hblib** crea una librería estática
 - **\-hbdyn** crea biblioteca dinámica \(sin enlace a Harbour VM\)
 - **\-hbdynvm** crea una librería dinámica \(con enlace a Harbour VM\)


 - **\-mt|\-st** enlaza con soporte multihilo/monohilo en Harbour VM
 - **\-gui|\-std** crea un ejecutable GUI/console
 - **\-main=&lt;mainfunc&gt;** sustituye el nombre de la función o procedimento inicial
 - **\-request=&lt;func&gt;** fuerza función/procedimiento a enlazarse
 - **\-fullstatic** enlaza con todas las librerías estáticas
 - **\-pic\[\-\]** crea código objeto independiente de la posición \(siempre activado en los modos \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** crea archivos binarios Harbour para compartir sin/con referencia absoluta a librerías de Harbour \(por defecto: 'fullshared' cuando Harbour se instala en ubicación del sistema, 'fixshared' en otro caso\) \(opción fix/full en \*nix solamente\)
 - **\-nulrdd\[\-\]** enlaza con 'nulrdd'
 - **\-debug\[\-\]** añade/excluye información de debug de compilador C\. Para activar el debug de Harbour utilize la opción \-b como de costumbre\.
 - **\-optim\[\-\]** conmuta las optimizaciones del compilador C \(por defecto: on\)
 - **\-cpp\[\-\]** fuerza modo C\+\+/C
 - **\-cpp=&lt;value&gt;** selecciona el modo C\+\+\. Los valores permitidos son: def, yes, no
 - **\-map\[\-\]** crea \(o no\) un archivo map
 - **\-implib\[\-\]** crea \(o no\) una biblioteca de importación \(en modo \-hbdyn/\-hbexe\)\. Se le añade un sufijo al nombre\.
 - **\-implib=&lt;output&gt;** crea una biblioteca de importación \(en modo \-hbdyn/\-hbexe\) denominada &lt;output&gt; \(por defecto: igual que la salida\)
 - **\-ln=&lt;link&gt;** crea enlace simbólico apuntando a &lt;output&gt; \(&lt;link&gt; se asocia a &lt;output&gt;\)
 - **\-strip\[\-\]** desmonta \(no desmonta\) binarios
 - **\-trace\[\-\]** muestra los comandos ejecutados
 - **\-beep\[\-\]** activa \(o desactiva\) beep simple en caso de éxito, doble beep en caso de falla
 - **\-ignore\[\-\]** ignore errores cuando ejecute herramienta de compilador \(por defecto: off\)
 - **\-hbcppmm\[\-\]** sustituye las funciones de administración de memoria estandar de C\+\+ con las de Harbour
 - **\-winuni\[\-\]** selecciona el modo de compilación entre Unicode \(WIDE\) y ANSI \(por defecto: ANSI\) \(sólo Windows\. Para WinCE siempre se establece en UNICODE\.\)
 - **\-nohblib\[\-\]** no use librerías estáticas del núcleo de Harbour al enlazar
 - **\-nodefgt\[\-\]** no enlaza GTs por defecto \(efectivo en modo \-static\)
 - **\-nolibgrouping\[\-\]** desactiva el agrupamiento de bibliotecas en compiladores basados en gcc
 - **\-nomiscsyslib\[\-\]** no se añade lista adicional de librerías del sistema a lista de librerías por defecto
 - **\-traceonly** muestra comandos a ser ejecutados, pero no ejecutarlos
 - **\-warn=&lt;level&gt;** establece el nivel advertencias del compilar de C  
&lt;level&gt; puede ser: max, yes, low, no, def \(por defecto: yes\)
 - **\-safe\[\-\]** activa opciones seguras en el compilador/enlazador de código fuente en C \(por defecto: activado en Windows, desactivado en otros sistemas\)
 - **\-compr=&lt;level&gt;** comprime el ejecutable/biblioteca dinámica \(es necesaria la herramienta UPX\)  
&lt;level&gt; puede ser: yes, no, min, max
 - **\-run\[\-\]** ejecuta/no ejecuta el objetivo generado\.
 - **\-vcshead=&lt;file&gt;** genera un archivo de cabecera \.ch con información del repositorio local\. Actualmente están soportados Git, SVN, Mercurial, Bazaar, Fossil, CVS y Monotone\. El archivo de cabecera definirá la constante de preprocesador \_HBMK\_VCS\_TYPE\_ con el nombre del VCS detectado, y \_HBMK\_VCS\_ID\_ con el ID único del repositorio local\. Si no se detecta un VCS, un número secuencial será incrementado cada vez que se construya\.
 - **\-tshead=&lt;file&gt;** genera archivo de cabecera \.ch con información de fecha/hora\. Cabecera generado definirá macros \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ con fecha/hora de creación de archivo\.
 - **\-icon=&lt;file&gt;** establece &lt;file&gt; como icono de la aplicación\. &lt;file&gt; debe ser de un formato soportado por la plataforma de destino \(no soportado por algunas plataformas/compiladores\)\. En Windows, está implementado generando y enlazando un archivo de recurso\.
 - **\-manifest=&lt;file&gt;** incrusta el fichero de manifiesto &lt;file&gt; en el ejecutable/biblioteca dinámica \(sólo Windows\)
 - **\-sign=&lt;key&gt;** firma del ejecutable con &lt;key&gt; \(sólo en Windows y Darwin\)\. En Windows se usa signtool\.exe \(incluido en MS Windows SDK\) o posign\.exe \(incluido en Pelles C 7\), por ese orden, se detectan ambos automaticamente\.
 - **\-signpw=&lt;pw&gt;** utilice &lt;pw&gt; como contraseña cuando firme el ejecutable \(sólo Windows y Darwin\)
 - **\-instfile=&lt;g:file&gt;** añade &lt;archivo&gt; a la lista de archivos que desea copiar a la ruta especificada por la opción \-instpath\. &lt;g&gt; es un grupo opcional de copia \(distingue mayúsculas y minúsculas\), debe haber al menos dos caracteres\. En caso de que no se especifica &lt;archivo&gt;, la lista de archivos en ese grupo se vaciará\.
 - **\-instpath=&lt;g:path&gt;** copia el/los archivo\(s\) objetivo a la ruta &lt;path&gt;\. Si &lt;path&gt; es un directorio, debería terminar con un separador de rutas, en este caso los archivos especificados en la opción '\-instfile' también son copiados\. Puede ser especificado varias veces\. &lt;g&gt; es grupo de copia opcional, debe tener al menos dos caracteres de largo\. El objetivo final será copiado automaticamente al grupo de copia por defecto \(sin asignación de &lt;g&gt;\)\. Existen los siguientes grupos de copia ya incluidos: 'depimplib' para las bibliotecas importadas y 'depimplibsrc' para los archivos fuente de las bibliotecas importadas \(\.dll\), ambos pertenecientes a las dependencias\.
 - **\-instforce\[\-\]** copia el/los archivo\(s\) objetivos a la ruta de instalación aunque esten al día
 - **\-depimplib\[\-\]** activa \(o desactiva\) la generación de Bibliotecas de Importación para fuentes de biblioteca de importación especificados en la opción \-depimplibs= \(por defecto: yes\)
 - **\-stop\[=&lt;text&gt;\]** para sin hacer nada y muestra &lt;text&gt; si se ha especificado
 - **\-echo=&lt;text&gt;** eco de texto en la pantalla
 - **\-pause** fuerza, en al salida, una pausa esperando la pulsación de una tecla en caso de error \(sólo con driver GT alternativo\)
 - **\-exitstr** muestra el error resultante al salir como texto reconocible
 - **\-info** activa los mensajes informativos
 - **\-quiet\[\-\]** suprime todos los mensajes en pantalla


 - **\-bldf\[\-\]** hereda indicadores de Harbour: todos/no \(por defecto\)
 - **\-bldf=\[p\]\[c\]\[l\]** hereda todos los indicadores de \.prg/\.c/linker \(o ninguno\) desde la construcción de Harbour
 - **\-F&lt;framework&gt;** enlace con marco &lt;framework&gt; \(Sólo Darwin\)
 - **\-prgflag=&lt;f&gt;** pasa un indicador al compilador de Harbour
 - **\-cflag=&lt;f&gt;** pasa un indicador al compilador C
 - **\-resflag=&lt;f&gt;** pasa un indicador al compilador de recursos \(sólo para Windows\)
 - **\-ldflag=&lt;f&gt;** pasa un indicador al enlazador \(ejecutable\)
 - **\-dflag=&lt;f&gt;** pasa un indicador al enlazador \(biblioteca dinámica\)
 - **\-aflag=&lt;f&gt;** pasa un indicador al enlazador \(biblioteca estática\)
 - **\-iflag=&lt;f&gt;** pasa un indicador al comando de creación de bibliotecas de importación
 - **\-signflag=&lt;f&gt;** pasa un sólo indicador al comando de firma de código
 - **\-runflag=&lt;f&gt;** pasa un indicador al ejecutable de salida cuando la opción \-run es utilizada
 - **\-cflag\+=&lt;f&gt;** pasa un sólo indicador al compilador de C para reemplazar las opciones del compilador C añadida por hbmk2 mismo\. Usar con precaución\.
 - **\-ldflag\+=&lt;f&gt;** pasa una sola opción al enlazador \(ejecutable\) después de la lista de bibliotecas\. Usar con precaución\.
 - **\-dflag\+=&lt;f&gt;** pasa una sola opción al enlazador \(biblioteca dinámica\) después de la lista de bibliotecas\. Usar con precaución\.
 - **\-3rd=&lt;f&gt;** opciones/indicadores reservados para herramientas de terceros, siempre ignorados por el mismo hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** modifica las variables de entorno local\. &lt;e&gt; es el nombre de la variable de entorno a modificar\. &lt;o&gt; puede ser '=' para establecer/reemplazar, '\-' para borrar, '\+' para añadir al final de valor existente, '\#' para insertar al principio del valor existente\. &lt;v&gt; es el valor a establecer/añadir/insertar\.
 - **\-jobs=&lt;n&gt;** Inicia &lt;n&gt; tareas de compilación \(sólo para plataformas multiproceso\)
 - **\-head=&lt;m&gt;** analizando fuente de control de encabezado \(en construcción en modo incremental\)  
&lt;m&gt; puede ser: nativo \(compilador utilizado para extraer las dependencias\), completa \(por defecto, utiliza analizador de texto simple en el fichero entero\), dep, off
 - **\-rebuild** reconstrucción \(en modo incremental\)
 - **\-rebuildall** reconstruye con sub\-proyectos \(contrucción en modo incremental\)
 - **\-clean** compilación limpia \(en modo incremental\)
 - **\-workdir=&lt;dir&gt;** directorio de trabajo  
\(por defecto: \.hbmk/&lt;plataforma&gt;/&lt;compilador&gt; \[\*\] en modo incremental, si no, directorio temporal del SO\)


 - **\-hbcontainer** objetivo final virtual, no crea nada\. Es útil para crear un '\.hbp' con el único propósito de referenciar sub\-proyectos\.
 - **\-hbimplib** crea librería de importación \(sólo en Windows\)


 - **\-hbl\[=&lt;output&gt;\]** nombre\-de\-archivo \.hbl resultante\. macro %\{hb\_lng\} es aceptada en nombre\-de\-archivo\.
 - **\-lng=&lt;languages&gt;** lista de idiomas a ser reemplazados en %\{hb\_lng\} macros en archivos \.pot/\.po y nombres de archivos y salida \.hbl/\.po\. Lista separada por comas:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** crea/actualiza archivo \.po a partir del código fuente\. Lo combina con el anterior archivo \.po del mismo nombre\.
 - **\-minipo\[\-\]** añade \(o no\) el número de versión y referencia del archivo de origen al \.po \(por defecto: añadirlos\)
 - **\-rebuildpo** recrea archivo \.po, eliminando todas las entradas obsoletas en el mismo\.


 - **\-hbx=\[&lt;\.ch&gt;\]** Crea un archivo de cabecera de Harbour \(en formato '\.hbx'\) con todos los símbolos externos\. Un parámetro en blanco lo desactiva\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; es un nombre de archivo de cabecera\. &lt;\.hbc&gt; es un nombre de archivo \.hbc para ser incluido automáticamente en caso la cabecera se encuentra en cualquiera de las fuentes compiladas\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;n&gt; nombre de la dependencia de un paquete\. Se puede especificar varias veces\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;h&gt; es la cabecera clave \(\.h\) de la dependencia de un paquete\. Varios encabezados alternativos pueden ser especificados\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;f&gt; puede ser 'yes' o 'no', especifica si la dependencia es opcional\. Por defecto: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; es el nombre de la dependencia\. &lt;v&gt; es un valor que controla como se hace la detección\. Valores aceptados: no, yes, force, nolocal, local\. Por defecto: contenido de envvar HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; es el nombre de la dependencia\. Establecer &lt;r&gt; como directorio raíz para las rutas especificadas en la opción \-depincpath\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; es el nombre de la dependencia\. Añadir &lt;i&gt; a la lista de rutas de detección de encabezados\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; es el nombre de la dependencia\. Añadir &lt;i&gt; a la lista de rutas de detección de cabeceras, donde &lt;i&gt; apunta a un directorio local del proyecto, conteniendo una dependencia embebida \(conocido como 'alojado localmente'\)\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; es el nombre de la dependencia\. Añadir &lt;dll&gt; a la lista de las fuentes de bibliotecas de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; es el nombre de la dependencia\. Establecer nombre generado de biblioteca de importación a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; es el nombre de la dependencia\. Cierra la definición de la dependencias y realiza la detección, estableciendo todas las variables macro de los filtros predefinidos y las opciones de construcción relacionadas\. Es opcional; si se omite, la detección tendrá lugar después de procesar todas las opciones\.


 - **\-plugin=&lt;filename&gt;** añade complemento\. &lt;filename&gt; puede ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** pasa un archivo de entrada a los complementos
 - **\-pflag=&lt;f&gt;** pasa un sólo indicador a los complementos
  
Las siguientes opciones están disponibles en línea de comandos:  


 - **\-target=&lt;script&gt;** especifica un nuevo objetivo final\. &lt;script&gt; puede ser un fichero '\.prg' \(o sin extensión\) o un fichero '\.hbp'\. Tener en cuenta que los ficheros '\.hbp' son considerados automáticamente como objetivos finales por separado\.


 - **\-hbrun** ejecuta el objetivo final de la construcción
 - **\-hbraw** para después de ejecutar el compilador de Harbour
 - **\-hbcmp|\-clipper** para después de la creación de los archivos objeto  
crear un enlace/copia a hbmk2 para hbcmp/clipper resultará el mismo efecto
 - **\-hbcc** acepta indicadores propios de C  
puede crear un enlace/copia de hbmk2 a 'hbcc' para obtener el mismo efecto
 - **\-hblnk** acepta indicadores propios del enlazador
 - **\-autohbm\[\-\]** activa \(o desactiva\) el procesamiento de hbmk\.hbm en el directorio actual \(por defecto: yes\)
 - **\-hb10** habilita modo de compatibilidad 'Harbour 1\.0\.x'
 - **\-hb20** activa el modo de compatibilidad Harbour 2\.0\.x
 - **\-hb30** activa modo de compatibilidad con Harbour 3\.0\.x
 - **\-xhb** habilita el modo xHb
 - **\-hbc** activa modo puro C
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emula comportamiento de linkeditor compatible con clipper  
crear link o copiar hbmk2 para rtlink/blinker/exospace resultará el mismo efecto


 - **\-hbreg\[=global\]** registra Harbour Script \(\.hb\) con hbmk2 \(sólo en Windows\)
 - **\-hbunreg\[=global\]** anula el registro de hbmk2 del tipo de archivo de órdenes de Harbour \(\.hb\) \(sólo Windows\)


 - **\-find &lt;text&gt;** muestra un listado de las funciones conocidas de Harbour que contienen &lt;text&gt; en su nombre, junto a su paquete \(no es sensible a mayúsculas/minúsculas, acepta múltiples valores, puede contener caracteres comodín\)


 - **\-hbmake=&lt;file&gt;** convierte proyecto hbmake en un archivo \.hbp
 - **\-xbp=&lt;file&gt;** convierte proyecto \.xbp \(xbuild\) en un archivo \.hbp
 - **\-xhp=&lt;file&gt;** convierte un proyecto \.xhp \(xMate\) en un archivo \.hbp


 - **\-\-hbdirbin** envía el directorio de programas de Harbour a stdout\.
 - **\-\-hbdirdyn** envía el directorio de bibliotecas dinámicas de Harbour a stdout\.
 - **\-\-hbdirlib** envía el directorio de bibliotecas estáticas de Harbour a stdout\.
 - **\-\-hbdirinc** envía el directorio de archivos de cabecera de Harbour a stdout\.
 - **\-\-hbinfo\[=nested\]** envia información de la construcción de Harbour a stdout\. La salida esta en formato JSON\. Los directorios incluidos siempre contienen barras inclinadas\. Cada bloque JSON es seguido de un byte 0x0A\.


 - **\-plat=&lt;platform&gt;** selecciona la CPU de destino \(por defecto: 'automatic'\)
 - **\-cpu=&lt;cpu&gt;** selecciona la CPU de destino\. \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** sustituye la detección automática del compilar de C  
Valor especial:  
\- bld: usa la configuración original \(por defecto en \*nix\)
 - **\-build=&lt;name&gt;** utilizar un nombre de build especifico
 - **\-lang=&lt;lang&gt;** sustituye idioma por defecto\. &lt;lang&gt; es un código ISO de idioma\.
 - **\-width=&lt;n&gt;** establece el ancho de salida a &lt;n&gt; caracteres \(0=sin límite\)\.
 - **\-shl** muestra el nivel de sub\-proyecto en las líneas de salida
 - **\-viewhelp** abre la ayuda completa en un visor de texto
 - **\-longhelp** ayuda detallada
 - **\-longhelpmd** ayuda completa en formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** ayuda del compilador Harbour \(todas las opciones del compilador Harbour son aceptadas tal como son por hbmk2\)
 - **\-credits** créditos de autor del compilador Harbour
 - **\-build** información de la construcción del compilador Harbour
 - **\-version** muestra sólo la cabecera con la versión
  
Las opciones siguientes son las internas o para desarrolladores \(la compatibilidad no esta garantizada\):  


 - **\-debugtime** mide el tiempo que ha tardado la construcción
 - **\-debuginc** muestra detalles internos del modo de construcción incremental
 - **\-debugstub** muestra el contenido de todos los códigos fuentes generados internamente
 - **\-debugi18n** muestra detalles internos de la generación de los ficheros de traducción
 - **\-debugdepd** muestra detalles internos de la búsqueda de dependencia
 - **\-debugpars** muestra todos los parámetros de entrada en orden de procesamiento
 - **\-debugrte** genera un error en tiempo de ejecución


Puedes crear un enlace simbólico/copiar/renombrar hbmk2 a los siguientes nombres para cambiar el modo de ejecución por defecto:


 - **hbrun\*|\*hbrun** modo de ejecución de archivos de órdenes / intérprete de comandos interactivo
 - **hbrund|hbrun\*d** modo de ejecución de archivos de órdenes / intérprete de comandos interactivo en modo depuración
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


 - **\*\.hbp** archivo de proyecto\. Puede contener cualquier número de opciones de la linea de comandos, que son los esperados para crear un objetivo final\. La lineas que comienzan con el carácter "\#" son ignoradas, por otra parte es opcional incluir caracteres de nueva linea y las opciones deben de estar separadas por espacios, como en la linea de comandos\. Las opciones que contienen espacios deben encerrarse entre comillas dobles\. Cada referencia a un archivo '\.hbp' será ejecutado como un sub\-proyecto\.
 - **\*\.hbm** colección de opciones\. Puede ser utilizada para recoger opciones por defecto en un archivo e incluirlo dentro de un archivo de proyecto\. Utiliza el mismo formato que los archivos \.hbp\.
 - **\*\.hbc** colección de opciones que acompañan a los componentes \(conocidos como 'bibliotecas' o paquetes\)\. Usa una sintaxis diferente a la línea de comandos y a archivos '\.hbp'/'\.hbm'\. Las líneas que comienzan con "\#" son ignoradas\. Cada directiva debe ser ubicada en líneas separadas\.
 - **\*\.ch** si se pasa directamente como un archivo fuente, se utilizará como una cabecera estándar adicional
 - **hbmk\.hbc** archivo \.hbc estandar que es procesado automáticamente si está presente\. Posible\(s\) localizacion\(es\) \(en orden de preferencia\) \[\*\]: %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **hbmk\.hbm** archivo \.hbm situado en el directorio de trabajo actual, que es procesado automáticamente antes que otras opciones
 - **$hb\_pkg\_dynlib\.hbm** archivo especial \.hbm incrustado en hbmk2\. Se encarga de la creación de una biblioteca dinámica \(al estilo de las contribuciones de Harbour\)
 - **$hb\_pkg\_install\.hbm** fichero especial '\.hbm' incluido dentro de hbmk2\. Se encarga de los detalles de la instalación de los objetivos finales, y paquetes relacionados, a las localizaciones estándar \(al estilo de las construcciones de Harbour\)\.


 - **\*\.hb** Archivo de órdenes de Harbour
 - **\*\.hrb** binario portable de Harbour \(aka archivo de comandos pre\-compilado de Harbour\)
 - **hbstart\.hb** archivo de órdenes de inicio de Harbour para el intérprete de comandos de Harbour\. Se ejecuta automáticamente al comienzo de la ejecución del intérprete de comandos, si existe\. Localizaciones posibles \(en orden de precedencia\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **shell plugins** complementos '\.hb' y '\.hrb' para el intérprete de comandos interactivo de Harbour\. Pueden localizarse en \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puedes deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de linea\)\. Se guarda en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensiones para cargar en el interprete de comandos interactivo de Harbour\. Una extensión por línea, y se ignora todo lo que hay detrás del caracter '\#'\. Nombre de fichero alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\
  
Variables de macro:  


 - **$\{hb\_root\}** directorio de hbmk2
 - **$\{hb\_dir\}** directorio del fichero en el que es usado
 - **$\{hb\_dirname\}** directorio raiz del archivo que esta en proceso
 - **$\{hb\_name\}** nombre del archivo que se ha utilizado \(sin ruta ni extensión\)
 - **$\{hb\_self\}** nombre de archivo completo que esta siendo procesado
 - **$\{hb\_curdir\}** directorio de trabajo actual
 - **$\{hb\_tempdir\}** Directorio para archivos temporales del sistema operativo
 - **$\{hb\_targetname\}** nombre del proyecto \(sin directorio ni extensión\)\. Devuelve \.adhoc\. si no es un archivo de proyecto\.
 - **$\{hb\_targettype\}** tipo de proyecto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** plataforma seleccionada
 - **$\{hb\_comp\}** compilador de C seleccionado
 - **$\{hb\_comp\_ver\}** versión del compilador C
 - **$\{hb\_build\}** nombre del objetivo
 - **$\{hb\_cpu\}** CPU seleccionada
 - **$\{hb\_work\}** nombre de la carpeta de trabajo por defecto
 - **$\{hb\_workdynsub\}** subdirectorio de trabajo por defecto para bibliotecas dinámicas de destino
 - **$\{hb\_dynprefix\}** prefijo de la librería dinámica
 - **$\{hb\_dynsuffix\}** sufijo de la librería dinámica
 - **$\{hb\_dynext\}** extensión de las bibliotecas dinámicas
 - **$\{hb\_ver\}** Versión de Harbour en formato hexadecimal de tres bytes\. P\.ej\.: 030200
 - **$\{hb\_verstr\}** Versión de Harbour en un formato legible para humanos &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. Por ejemplo: 3\.2\.0dev
 - **$\{hb\_major\}** número mayor de la versión de Harbour
 - **$\{hb\_minor\}** número menor de la versión de Harbour
 - **$\{hb\_release\}** número de versión de lanzamiento de Harbour
 - **$\{hb\_status\}** Estado de la versión de Harbour
 - **$\{hb\_revision\}** Revisión de Harbour
 - **$\{hb\_host\_plat\}** Plataforma anfitrión de Harbour
 - **$\{hb\_host\_plat\_unix\}** devuelve '1' si la plataforma anfitriona es compatible \*nix\.
 - **$\{hb\_bin\}** directorio de los binarios de Harbour
 - **$\{hb\_lib\}** Directorio de las bibliotecas estáticas de Harbour
 - **$\{hb\_lib3rd\}** Directorio de las librerías estáticas de terceros de Harbour
 - **$\{hb\_dyn\}** Directorio de las bibliotecas de enlace dinámico de Harbour
 - **$\{hb\_inc\}** directorio de las cabeceras de Harbour
 - **$\{hb\_addons\}** directorio base de los programas adicionales de Harbour
 - **$\{hb\_first\}** nombre del fichero de código fuente que contiene la función de entrada \(sin el directorio ni la extensión\)
 - **$\{hb\_outputdir\}** directorio de salida
 - **$\{hb\_outputname\}** Nombre del archivo de salida \(sin extensión\)
 - **$\{hb\_level\}** nivel de recursión del sub\-proyecto
 - **$\{&lt;depname&gt;\}** devuelve el directorio de archivos de cabecera de la dependencia &lt;depname&gt;, o '1' si no se ha detectado
 - **$\{&lt;envvar&gt;\}** devuelve el valor de la variable de entorno &lt;envvar&gt;
  
Filtros \(puedes combinarlos y/o negarlos\):  


 - **\{&lt;platform&gt;\}** selecciona la plataforma\. Donde &lt;platform&gt; puede ser cualquiera de los valores aceptados por la opción '\-plat='\.
 - **\{&lt;compiler&gt;\}** selecciona el compilador de código fuente en C\. Donde &lt;compiler&gt; puede ser cualquiera de los valores aceptados por la opción '\-comp='\.
 - **\{&lt;cpu&gt;\}** CPU destino\. &lt;cpu&gt; puede ser una de: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** seleciona el tipo del objetivo final\. Donde &lt;targettype&gt; es cualquiera de los valores devueltos por la variable macro $\{hb\_targettype\}\.
 - **\{mt\}** el objetivo final de la construcción es multihilo \(ver opción \-mt\)
 - **\{st\}** el objetivo final es monohilo \(ver opción \-st\)
 - **\{gui\}** el objetivo final tiene un interfaz gráfico \(ver opción \-gui\)
 - **\{std\}** el objetivo es una consola de linea de comandos \(ver opción \-console\)
 - **\{debug\}** activa la depuración de código a nivel de C \(ver opción \-debug\)
 - **\{nodebug\}** La depuración a nivel C está desactivada \(ver la opción \-debug\-\)
 - **\{shared\}** construcción en modo compartido \(ver \-shared y opciones relacionadas\)
 - **\{static\}** construcción en modo estático \(ver \-static y opciones relacionadas\)
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
 - **\{allpocc\}** el compilador para el código fuente en C es pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** el compilador para el código fuente en C es icc\* \(icc, iccia64\)
 - **\{hb10\}** Modo de compatibilidad 'Harbour 1\.0\.x' \(ver opción \-hb10\)
 - **\{hb20\}** Modo de compatibilidad 'Harbour 2\.0\.x' \(ver opción \-hb20\)
 - **\{hb30\}** Modo de compatibilidad 'Harbour 3\.0\.x' \(ver opción \-hb30\)
 - **\{xhb\}** Modo 'xhb' \(ver opción \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** el filtro pasará si el nombre &lt;file&gt; o &lt;dir&gt; existe en el disco\.
 - **\{MACRO\}** el filtro se pasará si el valor de $\{MACRO\} no está vacio y no es igual a '0' o 'no' \(en mayúsculas o minúsculas\)
 - **\{MACRO='&lt;value&gt;'\}** el filtro pasará si el valor de $\{MACRO\} es igual a &lt;value&gt; \(no diferencia mayúsculas/minúsculas\)\.
 - **\{MACRO&gt;'&lt;value&gt;'\}** el filtro pasará si el valor de $\{MACRO\} es mayor a &lt;value&gt; \(no diferencia mayúsculas/minúsculas\)\.
 - **\{MACRO&lt;'&lt;value&gt;'\}** el filtro pasará si el valor de $\{MACRO\} es menor a &lt;value&gt; \(no diferencia mayúsculas/minúsculas\)\.


Constantes predefinidas en el código fuente:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** cuando un archivo de órdenes '\.hb' es compilado como un complemento de hbmk2
 - **\_\_HBEXTREQ\_\_** cuando un archivo fuente '\.hbx' está presente en un proyecto \(disponible en el código fuente de Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** cuando el paquete &lt;hbcname&gt;\.hbc se enlaza al objetivo final\. El valor se obtiene de la entrada 'version=' del archivo \.hbc, convertido a número decimal, '1' si no se especifica\. \(disponible en el código fuente de Harbour\)
 - **HBMK\_HAS\_&lt;depname&gt;** cuando la dependencia &lt;depname&gt; se detectó \(disponible en código fuente C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** cuando un archivo de código fuente es ejecutado con el intérprete de comandos
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Constantes predefinidas en ficheros de construcción \(disponibles después de '\-depfinish=&lt;depname&gt;'/'depfinish=&lt;depname&gt;'\):


 - **HBMK\_HAS\_&lt;depname&gt;** cuando la dependencia &lt;depname&gt; se detectó
 - **HBMK\_DIR\_&lt;depname&gt;** devuelve el directorio de los archivos de cabecera donde &lt;depname&gt; ha sido detectado, o vacio si no\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** cuando la dependencia &lt;depname&gt; se detectó en la localización configurada por la opción \-decincpathlocal=
  
Variables de entorno:  


 - **HBMK\_OPTIONS** acepta cualquier opción como si fuesen pasadas al comienzo de la línea de comandos
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
 - **HB\_COMPILER\_VER** sustituye la autodetección de la versión del compilador de C \(sólo en las familias de compiladores gcc y msvc\)\. Formato: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** sustituye el directorio del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_CCPREFIX** sustituye el prefijo del ejecutable del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_CCSUFFIX** sustituye el sufijo del ejecutable del compilador de C \(sólo para la familia de compiladores gcc\)
 - **HB\_INSTALL\_PREFIX** sustituye el directorio base de la instalación de Harbour
 - **HB\_INSTALL\_ADDONS** sustituye el directorio base de los complementos de Harbour


 - **HB\_EXTENSION** lista de extensiones para cargar en el intérprete de comandos de Harbour separados por espacio
  
directivas \.hbc \(tienen que ser escritas en líneas separadas\):  


 - **echo=&lt;msg&gt;** muestra &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** omite el procesado del resto del archivo \.hbc\. Muestra &lt;msg&gt;, si se especifica\.
 - **stop=\[&lt;msg&gt;\]** para la construcción\. Mostrar &lt;msg&gt;, si se especifica\.
 - **sources=** añade una lista de archivos separados por espacios como archivos de entrada
 - **headers=** añade una lista de archivos de cabecera \.ch separados por espacios como cabeceras estándar
 - **libs=** añade lista de bibliotecas separadas por espacios \(ver más en la opción \-l\)
 - **frameworks=** añade una lista de 'frameworks' separadas por espacios \(sólo en Darwin\)
 - **requests=** añade una lista de símbolos separados por espacios para forzar a enlazarlos al objetivo final
 - **syslibs=** añade lista de bibliotecas separadas por espacios como bibliotecas del sistema \(antes de las bibliotecas normales\)
 - **hbcs=** incrusta una lista de archivos \.hbc separados por espacios\. Se aceptan nombres sin extensión\. Estas referencias se procesan en el sitio\.
 - **autohbcs=** lista de valores separados por espacios como en la opción \-autohbc=
 - **libpaths=** lista de rutas adicionales a bibliotecas separadas por espacios
 - **incpaths=** añade una lista, separadas por espacio, de rutas adicionales a archivos de cabecera \(tanto para Harbour como para C\)
 - **instfiles=** lista de valores separados por espacios como en la opción \-instfile=
 - **instpaths=** lista de valores separados por espacios como en la opción \-instpath=
 - **prgflags=** lista de valores separados por espacios como en la opción \-prgflag=
 - **cflags=** lista de valores separados por espacios como en la opción \-cflag=
 - **resflags=** lista de valores separados por espacios como en la opción \-resflag=
 - **ldflags=** lista de valores separados por espacios como en la opción \-ldflag=
 - **ldflags\+=** lista de valores separados por espacios como en la opción \-ldflag\+=
 - **dflags=** lista de valores separados por espacios como en la opción \-dflag=
 - **dflags\+=** lista de valores separados por espacios como en la opción \-dflag\+=
 - **pflags=** lista de valores separados por espacios como en la opción \-pflag=
 - **psources=** lista de valores separados por espacios como en la opción \-pi=
 - **gui=&lt;bool&gt;** opción 'sí' = \-gui, 'no' = \-std
 - **mt=&lt;bool&gt;** opción 'sí' = \-mt, 'no' = \-st
 - **pic=&lt;bool&gt;** opción 'sí' = \-pic, 'no' = \-pic\-
 - **shared=&lt;bool&gt;** opción 'sí' = \-shared, 'no' = \-static
 - **shareddef=&lt;bool&gt;** similar a 'shared=', pero solo funciona si no fue establecido anteriormente el modo compartido/estático
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
 - **plugins=** lista de complementos de hbmk2 a cargar separados por espacios
 - **gt=&lt;name&gt;** el mismo que la opción \-gt&lt;name&gt;
 - **gtdef=&lt;name&gt;** establece el GT por defecto a usar
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
 - **repository=** lista de referencia a archivos fuentes del repositorio separadas por espacios


API del complemento:  
\('hbmk' es la variable de contexto recibida por la función de entrada del complemento\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Registra la extensión de archivo de entrada para ser pasada a un complemento \(por defecto, todos los archivos con extensión desconocida se pasan al compilador Harbour\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añade un archivo de entrada de Harbour al proyecto\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añade un archivo de entrada de C al proyecto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añade un archivo de entrada de C\+\+ al proyecto\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añade un archivo de entrda de recursos de Windows al proyecto\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Añade un archivo objeto binario al proyecto\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Añade un archivo para instalar, con un nombre de grupo \-instpath= opcional\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Envia el texto de salida a la salida estándar\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Envia el texto de salida a la salida estándar de errores\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Salida de texto a la salida estándar sin ningún formato\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Salida de texto a la salida estándar de errores sin ningún formato\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Evalúa la expresión macro hbmk2
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
El nombre de archivo tiene que estar entrecomillado para usarlo como parámetro de comando externo\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convierte el nombre del archivo al formato que necesita la plataforma/compilador de C\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Convierte el nombre de archivo para que tenga barras inclinadas como separadores de directorios\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Devuelve la ruta relativa al valor de \-workdir= para el directorio de trabajo actual\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Busca el archivo en &lt;xPath&gt; \(se admite una matriz o una cadena delimitada con separadores de ruta\) con una lista de extensiones alternativas &lt;aExtDef&gt; \(por defecto ejecutables binarios\)\. Devuelve un nombre de archivo si se encuentra, NIL si no\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Modifica el directorio y/o extensión del nombre del archivo
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Codifica el nombre de la función de acuerdo a las reglas del compilador Harbour para formar los nombres de función HB\_FUNC\(\) en el código C\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Elimina el cierre de las dobles comillas de la cadena\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convierte una lista de cadenas a una cadena\. Por defecto el separador es un espacio\.


Variables del complemento:  
\(elementos del parámetro de contexto 'hbmk', diferencia mayúsculas/minúsculas, solo lectura salvo que se haya establecido lo contrario\)


 - **"apiver"** versión del API como un número entero
 - **"cSTATE"** estado de retrollamada\. Puede ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** matriz de parámetros pasada al complemento vía opción \-pflag=/pi= o que tenga una extensión registrada vía hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** 'hash' de variables para uso del complemento\. Modificable, local para cada complemento\.
 - **"cPLAT"** valor de \-plat
 - **"cCOMP"** valor de \-comp
 - **"nCOMPVer"** muestra la variable de entorno HB\_COMPILER\_VER
 - **"cCPU"** valor de \-cpu
 - **"cBUILD"** valor de \-build=
 - **"cOUTPUTNAME"** valor de \-o
 - **"cTARGETNAME"** muestra la macro $\{hb\_targetname\}
 - **"cTARGETTYPE"** muestra la macro $\{hb\_targettype\}
 - **"lREBUILD"** estado de la opción \-rebuild
 - **"lCLEAN"** estado de la opción \-clean
 - **"lDEBUG"** estado de la opción \-debug
 - **"lMAP"** estado de la opción \-map
 - **"lSTRIP"** estado de la opción \-strip
 - **"lDONTEXEC"** estado de la opción \-traceonly
 - **"lIGNOREERROR"** estado de la opción \-ignore
 - **"lTRACE"** estado de la opción \-trace
 - **"lQUIET"** estado de la opción \-q
 - **"lINFO"** estado de la opción \-info
 - **"lBEEP"** estado de la opción \-beep
 - **"lRUN"** estado de la opción \-run
 - **"lINC"** estado de la opción \-inc
 - **"cCCPATH"** muestra la variable de entorno HB\_CCPATH
 - **"cCCPREFIX"** muestra la variable de entorno HB\_CCPREFIX
 - **"cCCSUFFIX"** muestra la variable de entorno HB\_CCSUFFIX
 - **"cCCEXT"** muestra la variable de entorno HB\_CCEXT
 - **"cWorkDir"** valor de \-workdir=
 - **"nExitCode"** Código de salida actual
  
API del intérprete de comandos disponible en los archivos de órdenes de Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambia el GT\. Por defecto \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Activa modo de compatibilidad 'Clipper' \(sin Unicode\)\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carga la cabecera de Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarga la cabecera de Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Muestra la lista de cabeceras de Harbour cargadas\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carga el paquete\. Similar a la directiva de preprocesado \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarga paquete\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de paquetes cargados\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) no está mapeada para el uso en archivo de órdenes\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) no está mapeada para el uso en archivo de órdenes\.


Ejemplos para comenzar con hbmk2:


 - **Ejecuta el intérprete de comandos interactivo \('dot' prompt\)**  
$ hbmk2 \.
 - **Ejecutar un archivo de órdenes de Harbour**  
$ hbmk2 myscript\.hb \[&lt;parameter\[s\]&gt;\]


Ejemplos para construir y ejecutar binarios portables de Harbour \(conocidos como archivos de órdenes precompilados de Harbour\):


 - **Para construir**  
$ hbmk2 \-gh myscript\.hb
 - **Para ejecutar el resultado de lo anterior**  
$ hbmk2 myscript\.hrb \[&lt;parameter\[s\]&gt;\]


Ejemplos para construir una aplicación Harbour:


 - **Para construir un simple '\.prg'**  
$ hbmk2 hello\.prg
 - **Construye una aplicación con varios archivos fuentes con extensión '\.prg' en modo incremental**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Construye una aplicación utilizando un fichero de proyecto**  
$ hbmk2 myapp\.hbp
 - **Construye una aplicación usando el modo incremental**  
$ hbmk2 myapp\.hbp \-inc
 - **Para construir una aplicación que utilice un paquete de contribuciones o de terceros que incorporen un archivo \.hbc**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Construye una aplicación que usa una biblioteca**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Construye una aplicación que usa un archivo de recursos de Windows**  
$ hbmk2 mymain\.prg myres\.rc
 - **Construye una aplicación que se enlaza con la bibliotecas dinámicas de Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Construye una aplicación con todos los archivos fuentes en '\.prg' y '\.c' que se encuentren en el subdirectorio 'source'**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Ejemplos para construir una biblioteca estática con Harbour:


 - **Para contruir la librería 'mylib' desde el código fuente**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Construye la biblioteca 'mylib' desde el código fuente usando el modo incremental**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Códigos de salida \("errorlevels"\):  


 - **0** sin errores
 - **1** plataforma desconocida
 - **2** compilador desconocido
 - **3** no se pudo detectar Harbour
 - **5** no se pudo crear fragmento de código
 - **6** fallo al compilar \(Harbour, compilador de C, compilador de recursos\)
 - **7** falló en el ensamblaje final \(enlazador o gestor de bibliotecas\)
 - **8** no soportado
 - **9** error al crear el directorio de trabajo
 - **19** ayuda
 - **10** dependencia no encontrada o desactivada
 - **20** inicio del complemento
 - **30** anidamiento demasiado profundo
 - **50** parada solicitada
 - **&lt;other&gt;** cuando se utilice la opción \-run, el código de salida será el que devuelva el ejecutable de destino
  
Notas:  


  - &lt;script&gt; puede ser:  
&lt;@script&gt; o &lt;script\.hbm&gt;: opciones de la línea de comandos en un archivo  
&lt;script\.hbp&gt;: opciones de la línea de comandos en un archivo, también marca un nuevo objetivo final si se especifica en la línea de comandos  
&lt;script\.hbc&gt;: archivo de configuración de paquetes de Harbour
  - Un nombre de archivo fuente sin extensión cargará el archivo \.hbp, si este existe en el directorio actual\. Si no, la extensión \.prg será usada\.
  - Múltiples parámetros son aceptados \-l, \-L, \-i y &lt;script&gt;\.
  - las opciones regulares de compilador Harbour también son aceptadas\.  
\(Verlos con la opción \-harbourhelp\)
  - archivo de opciones hbmk\.hbc en directorio de hbmk2 siempre es procesado si existe\. En plataformas \*nix este archivo es chequeado \(en este orden\) ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc antes de pasar a directorio de hbmk2\.
  - hbmk\.hbm hace script en el directorio actual siempre se procesa, si existe\.
  - Se recomienda usar barras inclinadas en los valores de opciones de directorios, pero tambien se aceptan igualmente barras invertidas\.
  - filtros para plataformas son aceptados en cada linea de archivo \.hbc y con varias opciones\.  
Formato de filtro: \{\[\!\]\[&lt;plataforma&gt;|&lt;compilador&gt;|&lt;cpu&gt;|&lt;palabra\-clave&gt;\]\}\. Filtros pueden ser combinados usando los operadores '&amp;', '|' y agrupados en parénteses\. Ej\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - La mayoría de la líneas de un fichero \.hbc \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) y sus correspondientes parámetros de línea de comandos aceptan variables de macro\. libpaths= también acepta %\{hb\_name\} que se transforma al nombre del fichero \.hbc que se busca\.
  - Tambien acepta Opciones de macros sustitución de comandos\. Incluya comando dentro de \`\`, y, si el comando contiene espacios, también entre comillas dobles\. F\.e\. "\-cflag==\`wx\-config \-cflags\`", o ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - Cuando varias opciones de selección del tipo de objetivo final \(\-hblib, \-hbdyn, etc\.\) son especificados, el primero será el elegido, el resto será ignorado silenciosamente\.
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
