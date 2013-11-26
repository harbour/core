Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-11\-26 05:07\)  
Copyright &copy; 1999\-2014, Viktor Szakáts  
<http://harbour\-project\.org/>  
Traducción \(gl\): JLalín  

Sintaxe:  
  
  hbmk2 \[opcións\] \[&lt;guión\(s\)&gt;\] &lt;fonte\(s\)\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descripción:  


  hbmk2 é unha ferramenta portable e integrada de construcción, facendo posible a creación de diferentes tipos de binarios \(executables, bibliotecas dinámicas, bibliotecas estáticas, binarios portables de Harbour\) a partires de múltiples tipos de fontes \(C, C\+\+, Objective\-C, Harbour, traduccións de gettext, recursos de Windows\)\. 'Integrado' significa que un sinxelo arquivo de proxecto hbmk2 pode controlar todos ou a maioría dos aspectos do proceso de construcción\. 'Portable' significa que que un sinxelo arquivo de proxecto hbmk2 pode controlar o proceso de construcción en todas as plataformas soportadas e con todos os compiladores C soportados\. Tamén axuda a cubrir a maioría de tarefas de construcción con sinxelos e pequenos arquivos de proxectos \(opcións\)\. hbmk2 tamén soporta proxectos \-independentes de Harbour\- C/C\+\+/Objective\-C puros\. Para conseguir os obxetivos hbmk2 autodetectará o compilador Harbour, o compilador de C e outras ferramentas necesarias e usaráas cando se necesiten\. hbmk2 permite extender os tipos de arquivos soportados usando complementos \(plugins\)\.  
Ademáis de construir executables, hbmk2 pode executar guións \(scripts\) de Harbour \(tanto en código fonte como precompilados\) e inclúe un intérprete interactivo de comandos\.
  
Opcións:  


 - **\-o&lt;outname&gt;** nome do arquivo de saída
 - **\-l&lt;libname&gt;** enlazar coa biblioteca &lt;libname&gt;\. &lt;libname&gt; non debería ter ruta, extensión nin o prefixo 'lib' \(a non ser que sexa parte do nome\)\. Non engadir bibliotecas estándar de Harbour, engadiranse cando se necesiten\. Cando &lt;libname&gt; comece co caracter '\-' a biblioteca será eliminada da lista de librerías ao enlazar\.
 - **\-L&lt;libpath&gt;** Ruta adicional para buscar bibliotecas
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** Ruta adicional para buscar cabeceiras
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
 - **\-pic\[\-\]** crear arquivo obxeto de posición independente \(sempre activado nos modos \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** crear binarios de Harbour compartidos con/sen referencias absolutas a os directorios de bibliotecas de Harbour \(predeterminado: 'fullshared' cando se instala Harbour en carpetas do sistema, 'fixshared' en outro caso\) \(opción fix/full só en \*nix\)
 - **\-nulrdd\[\-\]** enlazar con nulrdd
 - **\-debug\[\-\]** engadir/quitar información de depuración no compilador C\. Para depurar a nivel de Harbour use a opción \-b estándar
 - **\-optim\[\-\]** cambiar as optimizacións do compilador C \(predeterminado: on\)
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
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI Windows API usage for C/C\+\+ input files \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** Non usar as bibliotecas estáticas de Harbour ao enlazar
 - **\-nodefgt\[\-\]** Non enlazar GTs predeterminados \(eficaz en modo \-static\)
 - **\-nolibgrouping\[\-\]** desactivar agrupamento de bibliotecas para compiladores basados en gcc
 - **\-nomiscsyslib\[\-\]** non engadir lista extra de bibliotecas do sistema á lista predeterminada de bibliotecas
 - **\-traceonly** mostrar comandos para executar pero sen executalos
 - **\-warn=&lt;level&gt;** Establecer o nivel de aviso do compilador C  
  
&lt;level&gt; pode ser: max, yes, low, no, def \(predeterminado: yes\)
 - **\-safe\[\-\]** activa as opcións de seguridade no compilador de C/enlazador \(Predeterminado: activado en Windows, desactivado en outros sistemas\)
 - **\-compr=&lt;level&gt;** a compresión de executable/biblioteca dinámica \(necesita a ferramenta UPX\)  
  
&lt;level&gt; pode ser: yes, no, min, max
 - **\-run\[\-\]** executar/non executar o executable de saída
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. VCS specific information is added as \_HBMK\_VCS\_&lt;TYPE&gt;\_\*\_ constants, where supported\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-bldhead=&lt;file&gt;** generate \.ch header file with build information, like build sequence number and timestamp\. Generated header will define preprocessor constants \_HBMK\_BUILD\_ID\_ with sequence number \(incremented on each build\) and \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build
 - **\-icon=&lt;file&gt;** usar &lt;file&gt; como icono para a aplicación\. O arquivo &lt;file&gt; debe ser soportado na plataforma destino \(non soportado en algunhas plataformas/compiladores\)\. En Windows impleméntase xenerando e enlazando un arquivo de recursos\.
 - **\-manifest=&lt;file&gt;** Incrustar arquivo de manifesto &lt;file&gt; en executable/biblioteca dinámica \(só Windows\)
 - **\-sign=&lt;key&gt;** asinar executable con &lt;key&gt; \(Só Windows e Darwin\)\. En Windows úsase a ferramenta signtool\.exe \(pertence ao SDK de Windows\) ou posign\.exe \(pertence a Pelles C 7\), nesa orde, ambos autodetectados\.
 - **\-signpw=&lt;pw&gt;** Use &lt;pw&gt; como chave de acceso ao asinar executables \(só en Windows e Darwin\)
 - **\-signts=&lt;url&gt;** use &lt;url&gt; as trusted timestamp server\. Empty value resets it to the default: http://timestamp\.verisign\.com/scripts/timstamp\.dll
 - **\-instfile=&lt;g:file&gt;** engadir &lt;file&gt; á lista de arquivos para copiar indicados na opción \-instpath=\. &lt;g&gt; é un grupo opcional de copia \(distínguense maiúsculas/minúsculas\) e debe ter polo menos dous caracteres\. Cando non se indica &lt;file&gt; a lista de arquivos nese grupo será vaciada\.
 - **\-instpath=&lt;g:path&gt;** copia arquivo\(s\) de saída a &lt;path&gt;\. Se &lt;path&gt; é un diretorio, debería incluir un caracter separador de rutas ao final, neste caso os arquivos indicados na opción \-instfile tamén serán copiados\. Pode usarse varias veces\. &lt;g&gt; é un grupo opcional de copia e debe ter polo menos dous caracteres\. O arquivo de saída será copiado automáticamente ao grupo \(vacío\) predeterminado\. Existen grupos predefinidos: 'depimplib' para bibliotecas de importación e 'depimplibsrc' para os arquivos \(\.dll\) de bibliotecas de importación, ambos pertencen ás dependencias\.
 - **\-instforce\[\-\]** copiar arquivos xenerados na ruta de instalación aínda que estivesen actualizados
 - **\-depimplib\[\-\]** activa \(ou desactiva\) a xeneración de bibliotecas de importación para as bibliotecas indicadas nas opcións \-depimplibs= \(Predeterminado: yes\)
 - **\-stop\[=&lt;text&gt;\]** deter sen facer nada e mostrar &lt;text&gt; cando se especifica
 - **\-echo=&lt;text&gt;** Resoar textos na pantalla
 - **\-pause** forzar espera de pulsación de tecla ao terminar con error \(só con GTs alternativos\)
 - **\-exitstr** Mostrar resultado de error como texto lexible
 - **\-info** mostrar mensajes informativos
 - **\-quiet\[\-\]** suprimir todos los mensajes de pantalla


 - **\-bldf\[\-\]** herdar todo/ningunha opción \(predeterminada\) da construcción de Harbour
 - **\-bldf=\[p\]\[c\]\[l\]** herdar opcións \.prg/\.c/enlazador \(ou ningún\) da construcción de Harbour
 - **\-F&lt;framework&gt;** enlazar co marco de traballo &lt;framework&gt; \(Só Darwin\)
 - **\-prgflag=&lt;f&gt;** enviar opción ao compilador Harbour
 - **\-cflag=&lt;f&gt;** pasar parámetro al compilador C
 - **\-resflag=&lt;f&gt;** enviar parámetro ao compilador de recursos \(Só Windows\)
 - **\-ldflag=&lt;f&gt;** enviar opción ao enlazador \(executable\)
 - **\-dflag=&lt;f&gt;** enviar parámetro para o enlazador \(biblioteca dinámica\)
 - **\-aflag=&lt;f&gt;** enviar parámetro para o enlazador \(biblioteca estática\)
 - **\-iflag=&lt;f&gt;** Pasar a opción ao comando de creación de bibliotecas de importación
 - **\-signflag=&lt;f&gt;** pasar a opción para a ferramenta de asinado de código
 - **\-runflag=&lt;f&gt;** enviar parámetro ao executable de saída cando se usa a opción \-run
 - **\-cflag\+=&lt;f&gt;** enviar parámetros ao compilador C substituindo os parámetros engadidos por hbmk2\. Usar con precaución\.
 - **\-ldflag\+=&lt;f&gt;** enviar a opción ao enlazador \(executable\) despois da lista de bibliotecas\. Usar con precaución\.
 - **\-dflag\+=&lt;f&gt;** pasar a opción tal cuál ao enlazador \(biblioteca dinámica\), tra\-la lista de bibliotecas\. Usar con precaución\.
 - **\-3rd=&lt;f&gt;** opcións/parámetros reservados para ferramentas externas, ignoradas sempre por hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** modifica o ámbito local\. &lt;e&gt; é o nome de unha variable de ámbito para modificar\. &lt;o&gt; pode ser '=' para establecer, '\-' para eliminar, '\+' para engadir ao final, '\#' para insertar ao comezo\. &lt;v&gt; é o valor para establecer, engadir, insertar\.
 - **\-jobs=&lt;n&gt;** lanzar n fíos de compilación \(só en plataformas con multitarefa/multiproceso\)
 - **\-head=&lt;m&gt;** controla a análise dos fontes \(en modo de construcción incremental\)  
  
&lt;m&gt; pode ser: native \(usa o compilador para extraer dependencias\), full \(predeterminado, usa análise sinxelo de texto no arquivo\), dep, off
 - **\-rebuild** Reconstruir \(en modo construcción incremental\)
 - **\-rebuildall** Reconstruir con subproxectos \(en modo de construcción incremental\)
 - **\-clean** limpar \(en modo de construcción incremental\)
 - **\-workdir=&lt;dir&gt;** directorio de traballo  
  
\(predeterminado: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] en modo incremental, nos uotros casos o directorio temporal do sistema operativo\)


 - **\-hbcontainer** destino de construcción virtual, non crea nada\. Usado para crear un arquivo \.hbp co único propósito de referenciar subproxectos
 - **\-hbimplib** Crear biblioteca de importación \(só Windows\)


 - **\-hbl\[=&lt;output&gt;\]** arquivo \.hbl de saída\. A macro %\{hb\_lng\} é aceptada no nome do arquivo
 - **\-lng=&lt;languages&gt;** lista de idiomas para reemprazar nas macros %\{hb\_lng\} nos arquivos \.pot/\.po e arquivos de saída \.hbl/\.po\. Lista separada por comas:  
  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** crear/actualizar arquivo \.po a partir dos fontes\. Mesturar co arquivo \.po anterior co mesmo nome
 - **\-minipo\[\-\]** engadir \(ou non\) o número de versión de Harbour e referencia ao nome de arquivo ao arquivo \.po \(predeterminado: engadir ambas\)
 - **\-rebuildpo** recrear arquivo \.po, eliminando todas as entradas obsoletas


 - **\-hbx=\[&lt;\.ch&gt;\]** Crear cabeceira de Harbour \(en formato \.hbx\) con todos os símbolos externos\. Un parámetro vacío desactiva a opción\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; é un nome de arquivo de cabeceira\. &lt;\.hbc&gt; é un nome de arquivo incluido automáticamente cando a cabeceira é atopada en algún dos fontes compilados\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; é o nome da dependencia\. &lt;n&gt; é o nome da dependencia de un paquete\. Pode indicarse varias veces\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; é o nome da dependencia\. &lt;h&gt; é a cabeceira principal \(\.h\) do paquete da dependencia\. Poden indicarse múltiples cabeceiras alternativas\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; é o nome da dependencia\. &lt;f&gt; pose ser 'yes' ou 'no' e indica se a dependencia é opcional\. Predeterminado: 'no'
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; é o nome da dependencia\. &lt;v&gt; é un valor que controla como se fai a detección\. Os valores aceptados son: no, yes, force, nolocal, local\. Predeterminado: o contido da variable de ámbito HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; é o nome da dependencia\. Use &lt;r&gt; como directorio base para as rutas indicadas nas opcións \-depincpath\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;i&gt; á lista de rutas de busca de arquivos de cabeceira\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; é o nome da dependencia\. Engadir &lt;i&gt; á lista de directorios de cabeceiras, onde &lt;i&gt; apunta a un directorio local ao proxecto e contén unha dependencia incorporada \(entendido coma 'aloxada localmente'\)\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; é o nome da dependencia\. Engada &lt;dll&gt; á lista de arquivos de bibliotecas de importación\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; é o nome de dependencia\. Establecer o nome da biblioteca de importación xenerada a &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; é o nome da dependencia\. Pecha a definición da dependencia e fai a detección da dependencia, establecendo todas os filtros predefinidos nas macro variables e as opcións de construcción\. Opcionalmente, cando se omite, a detección terá lugar despois de procesar todas as opcións\.


 - **\-plugin=&lt;filename&gt;** engadir complemento\. &lt;filename&gt; pode ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** Pasar arquivos a os complementos
 - **\-pflag=&lt;f&gt;** pasar opción para os complementos
  
As seguintes opcións están dispoñibles na liña de comandos:  


 - **\-target=&lt;script&gt;** Indica un tipo novo de arquivo de saída\. &lt;script&gt; pode ser un arquivo \.prg \(ou sen extensión\) ou un arquivo \.hbp\. Cada arquivo \.hbp considérase automáticamente como tipos de arquivos de saída separados\.


 - **\-hbrun** Executar arquivo de saída
 - **\-hbraw** Deter despois de executar o compilador Harbour
 - **\-hbcmp|\-clipper** deter despois de crear os arquivos obxeto  
  
crear enlace/copia a hbmk2 de hbcmp/clipper para o mesmo efecto
 - **\-hbcc** aceptar opcións de C tal cuál  
  
crear enlace/copia de hbmk2 para hbcc para o mesmo resultado
 - **\-hblnk** aceptar parámetros para o enlazador
 - **\-autohbm\[\-\]** permitir \(ou impedir\) o procesamento de hbmk\.hbm no directorio actual \(Predeterminado: yes\)
 - **\-hb10** Habilitar modo compatibilidade Harbour 1\.0\.x
 - **\-hb20** Activar modo de compatibilidade con Harbour 2\.0\.x
 - **\-hb30** activar modo de compatibilidade Harbour 3\.0\.x
 - **\-xhb** activar o modo xhb
 - **\-hbc** activar modo C puro
 - **\-blinker** simular o comportamento de enlazador Clipper  
  
crear enlace/copia a hbmk2 para rtlink/blinker/exospace para o mesmo resultado
 - **\-exospace** see above
 - **\-rtlink** see above


 - **\-hbreg\[=global\]** registrar Harbour Script \(\.hb\) con hbmk2 \(Só Windows\)
 - **\-hbunreg\[=global\]** anular rexistro de Harbour Script \(\.hb\) de hbmk2 \(Só Windows\)


 - **\-find &lt;text&gt;** lista todas as funcións de Harbour que conteñen &lt;text&gt; no nome, ademáis do seu paquete \(non se distingue maiúsculas/minúsculas, acepta múltiples valores, pode conter caracteres comodín\)


 - **\-hbmake=&lt;file&gt;** converter arquivo de proxecto hbmake a arquivo \.hbp
 - **\-xbp=&lt;file&gt;** converter arquivo de proxecto \.xbp \(xbuild\) para \.hbp
 - **\-xhp=&lt;file&gt;** converter arquivo de proxecto \.xhp \(xMate\) para \.hbp


 - **\-\-hbdirbin** enviar directorio de binarios de Harbour á saída estándar 'stdout'
 - **\-\-hbdirdyn** mostrar o directorio da biblioteca dinámica de Harbour na saída estándar 'stdout'
 - **\-\-hbdirlib** mostrar o directorio da biblioteca estática de Harbour na saída estándar 'stdout'
 - **\-\-hbdirinc** enviar directorio de cabeceira de Harbour á saída estándar 'stdout'
 - **\-\-hbinfo\[=nested\]** mostrar saída do compilador na saída estándar 'stdout'\. O formato e JSON\. As rutas sempre conteñen barras invertidas\. Cada bloque JSON é seguido por un caracter 0x0A\.


 - **\-plat=&lt;platform&gt;** substituír a plataforma destino predeterminada \(predeterminado: automático\)
 - **\-cpu=&lt;cpu&gt;** substituír CPU predeterminado de destino \(predeterminado: automático\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** substituír autodetección do compilador C  
  
Valor especial:  
  
\- bld: usar as opcións de construcción orixinais \(predeterminado en \*nix\)
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
  
As seguintes opcións son internas/para uso de desenvolvedores \(non se garantiza a compatibilidade\):  


 - **\-debugtime** medir tempo empleado na construcción
 - **\-debuginc** mostrar os detalles da construcción incremental
 - **\-debugstub** mostrar o contido dos arquivos xenerados internamente
 - **\-debugi18n** mostrar os detalles da xeneración de arquivos de traducción
 - **\-debugdepd** mostrar os detalles da detección de dependencias
 - **\-debugpars** mostrar todos os parámetros na orde en que se procesan
 - **\-debugrte** xenerar un erro de execución


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


 - **\*\.hbp** arquivo de proxecto\. Pode conter calquera número de opcións que se necesiten para crear o arquivo de saída\. As liñas que comezan con caracter '\#' son ignoradas, o caracter de fin de liña/retorno é opcional e as opcións sepáranse con espazos, igual que nas opcións da liña de comandos\. As opcións que conteñan espazos deben ser postas entre comiñas dobles\. Cada arquivo \.hbp referenciado será executado como un subproxecto\.
 - **\*\.hbm** colección de opcións\. Pode usarse para agrupar opcións comúns no mesmo arquivo e incluílo nos arquivos de proxecto\. Usa o mesmo formato que os arquivos \.hbp\.
 - **\*\.hbc** colección de opcións que acompañan compoñentes \(bibliotecas, paquetes\)\. Usan unha sintaxe diferente da liña de comandos e arquivos \.hbp/\.hbm\. Liñas que comenzan co caracter '\#' son ignoradas, cada directiva debe estar nunha liña separada\.
 - **\*\.ch** cand se pasa directamente coma un arquivo fonte será usado como un arquivo adicional estándar de cabeceira
 - **hbmk\.hbc** arquivo \.hbc estándar que é procesado automáticamente se existe\. As localizacións posibles \(en orde de precedencia\) \[\*\]: %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **hbmk\.hbm** arquivo opcional \.hbm no directorio de traballo actual, que é automaticamente procesado antes de outras opcións
 - **$hb\_pkg\_dynlib\.hbm** arquivo especial \.hbm incorporado dentro de hbmk2\. Encárgase dos detalles de crear unha biblioteca dinámica \(ao estilo das contribucións de Harbour\)\.
 - **$hb\_pkg\_install\.hbm** arquivo especial \.hbm incrustado en hbmk2\. Encárgase dos detalles de instalar os arquivos de saída e paquetes relacionados en rutas estándar \(ao estilo das contribucións de Harbour\)\.


 - **\*\.hb** Guión de Harbour
 - **\*\.hrb** Binario portable de Harbour \(coñecido como guión de Harbour precompilado\)
 - **hbstart\.hb** guión de arranque para o intérprete interactivo\. Execútase automáticamente cando se inicia o intérprete, se existe\. Localizacións posibles \(en orde de precedencia\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbmk2&gt;
 - **shell plugins** complementos \.hb e \.hrb para o intérprete interactivo de Harbour\. Poden ubicarse en \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** garda o historial do intérprete interactivo de Harbour\. Pode omitirse o historial usando 'no' na primeira liña \(sen comiñas e con retorno de carro/nova liña\)\. Atópase en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensións para cargar no intérprete interactivo\. Unha extensión por liña, ignórase a partir do caracter '\#' \. Nome de arquivo alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\
  
Macro variables:  


 - **$\{hb\_root\}** directorio de hbmk2
 - **$\{hb\_dir\}** o directorio do nome do arquivo é usado en
 - **$\{hb\_dirname\}** directorio superior do nome do arquivo é usado en
 - **$\{hb\_name\}** nome do arquivo usado \(sen directorio nin extensión\)
 - **$\{hb\_self\}** nome completo é usado en
 - **$\{hb\_curdir\}** directorio de traballo actual
 - **$\{hb\_tempdir\}** Directorio do sistema operativo para arquivos temporais\.
 - **$\{hb\_targetname\}** nome do proxecto \(sen directorio nin extensión\)\. Retorna \.adhoc\. se non hay arquivo de proxecto\.
 - **$\{hb\_targettype\}** tipo do proxecto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
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
 - **$\{hb\_verstr\}** Versión de Harbour en formato lexible: &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. Por exemplo: 3\.2\.0dev
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
 - **$\{hb\_first\}** nome do arquivo fonte que contén a función de entrar \(sen directorio nin extensión\)
 - **$\{hb\_outputdir\}** directorio para a saída
 - **$\{hb\_outputname\}** Nome do arquivo de saída \(sen extensión\)
 - **$\{hb\_level\}** nivel de recursión para subproxectos
 - **$\{&lt;depname&gt;\}** retorna o directorio de cabeceira da dependencia &lt;depname&gt;, ou '1' cando non se pode detectar
 - **$\{&lt;envvar&gt;\}** Retorna o valor da variable de entorno &lt;envvar&gt;
  
Filtros \(poden combinarse e/ou negarse\):  


 - **\{&lt;platform&gt;\}** plataforma destino\. Donde &lt;platform&gt; pode ser calquera valor aceptado pola opción \-plat=\.
 - **\{&lt;compiler&gt;\}** compilador C de destino\. Donde &lt;compiler&gt; pode ser calquera valor aceptado pola opción \-comp=\.
 - **\{&lt;cpu&gt;\}** CPU destino\. Onde &lt;cpu&gt; pode ser: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** tipo de arquivo de saída\. Donde &lt;targettype&gt; e algún dos valores posibles da macro variable $\{hb\_targettype\}\.
 - **\{mt\}** Destino da construcción é de multifío/proceso \(ver opción \-mt\)
 - **\{st\}** Destino da construcción é de único fío/proceso \(ver opción \-st\)
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
 - **\{unix\}** a plataforma de destino é compatible con \*nix \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
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


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** cando un guión \.hb é compilado como un complemento hbmk2
 - **\_\_HBEXTREQ\_\_** cando un arquivo \.hbx está presente nun proxecto \(permitido nos fontes de Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** cando o paquete &lt;hbcname&gt;\.hbc está enlazado ao resultado da construcción\. O valor é contido da entrada version= no arquivo \.hbc, convertida a número decimal, que será '1' cando non se especifica\. \(Dispoñible nos fontes de Harbour\)
 - **HBMK\_HAS\_&lt;depname&gt;** cando a dependencia &lt;depname&gt; se detectou \(dispoñible nas fontes en C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** cando unha fonte Harbour se executa coma un guión do intérprete de comandos
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\) \(do not define them manually\):


 - **HBMK\_HAS\_&lt;depname&gt;** cando a dependencia &lt;depname&gt; foi detectada
 - **HBMK\_DIR\_&lt;depname&gt;** retorna o directorio de cabeceira donde &lt;depname&gt; se detectou, ou vacío cando non se atopa\.
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
 - **HB\_USER\_RESFLAGS** opcións para enviar ao compilador de recursos \(antes das opcións da liña de comandos\) \(Só Windows\)
 - **HB\_USER\_LDFLAGS** opcións para enviar ao enlazador \(executable\) \(antes das opcións da liña de comando\)
 - **HB\_USER\_DFLAGS** opcións para enviar ao enlazador \(biblioteca dinámica\) \(antes das opcións da liña de comando\)
 - **HB\_USER\_AFLAGS** opcións para enviar ao enlazador \(biblioteca estática\) \(antes das opcións da liña de comando\)
 - **HB\_COMPILER\_VER** substituír autodetección da versión do compilador C \(só para familia de compiladores gcc e msvc\)\. Formato: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** substituír o directorio do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_CCPREFIX** substituír o prefixo do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_CCSUFFIX** substituír o sufixo do executable do compilador de C \(só para a famila de compiladores gcc\)
 - **HB\_INSTALL\_PREFIX** Substituír o directorio base da instalación de Harbour
 - **HB\_INSTALL\_ADDONS** substituír o directorio base dos complementos de Harbour


 - **HB\_EXTENSION** lista separada por espazos de extensións para cargar no intérprete interactivo de Harbour
  
directivas \.hbc \(deben ser escritas en liñas separadas\):  


 - **echo=&lt;msg&gt;** Amosar &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** Omitir o procesamento do resto do arquivo \.hbc\. Mostrarase &lt;msg&gt; se se especificou\.
 - **stop=\[&lt;msg&gt;\]** Deter a construcción\. Mostra &lt;msg&gt; cando se especifica\.
 - **sources=** engadir lista separada por espazos de arquivos como arquivos de entrada
 - **headers=** engadir lista de cabeceiras \.ch separada por espazos como cabeceira estándar
 - **libs=** engada lista de bibliotecas separadas por espacios \(ver opción \-l\)
 - **frameworks=** engadir lista de entornos de traballo \(frameworks\) separados por espazos \(Só Darwin\)
 - **requests=** engadir lista separada por espazos de símbolos para forzar o enlazado co arquivo de saída
 - **syslibs=** engadir lista separada por espazos de bibliotecas como bibliotecas do sistema \(antes das bibliotecas habituáis\)
 - **hbcs=** incluir lista separada por espazos de arquivos \.hbc\. Tamén se aceptan nomes sen extensión\. Estas referencias procésanse no seu lugar\.
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
 - **gui=&lt;bool&gt;** opción 'yes' = \-gui, 'no' = \-std
 - **mt=&lt;bool&gt;** opción 'yes' = \-mt, 'no' = \-st
 - **pic=&lt;bool&gt;** opción 'yes' = \-pic, 'no' = \-pic\-
 - **shared=&lt;bool&gt;** opción 'yes' = \-shared, 'no' = \-static
 - **shareddef=&lt;bool&gt;** similar a shared= pero funciona só cando o modo shared/static non foi establecido antes
 - **fullstatic=&lt;bool&gt;** opción 'yes' = \-fullstatic, 'no' = \-static
 - **debug=&lt;bool&gt;** opción 'yes' = \-debug, 'no' = \-debug\-
 - **optim=** opción 'yes' = \-optim, 'no' = \-optim\-
 - **nulrdd=&lt;bool&gt;** opción 'yes' = \-nulrdd, 'no' = \-nulrdd\-
 - **nodefgt=&lt;bool&gt;** opción 'yes' = \-nodefgt, 'no' = \-nodefgt\-
 - **map=&lt;bool&gt;** opción 'yes' = \-map, 'no' = \-map\-
 - **hbcppmm=&lt;bool&gt;** opción 'yes' = \-hbcpmm, 'no' = \-hbcpmm\-
 - **implib=&lt;bool&gt;** opción 'yes' = \-implib, 'no' = \-implib\-
 - **winuni=&lt;bool&gt;** opción 'yes' = \-winuni, 'no' = \-winuni\-
 - **strip=&lt;bool&gt;** opción 'yes' = \-strip, 'no' = \-strip\-
 - **run=&lt;bool&gt;** opción 'yes' = \-run, 'no' = \-run\-
 - **inc=&lt;bool&gt;** opción 'yes' = \-inc, 'no' = \-inc\-
 - **safe=&lt;bool&gt;** opción 'yes' = \-safe, 'no' = \-safe\-
 - **cpp=** O mesmo que a opción \-cpp=
 - **warn=** O mesmo que a opción \-warn=
 - **compr=** O mesmo que a opción \-compr=
 - **head=** O mesmo que a opción \-head=
 - **plugins=** lista separada por espazos de complementos hbmk2 para cargar
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
 - **signts=** O mesmo que a opción \-signts=
 - **name=** Nome do paquete
 - **description=** Descripción do paquete
 - **version=&lt;x\.y\.z&gt;** número de versión do paquete, donde x,y,z son maiores ou iguais a cero e menores ou iguais a 255\. Predeterminado a 0\.0\.1 cando non se especifica\.
 - **keywords=** Lista de palabras chave separadas por espacios
 - **licences=** Lista de licencias separadas por espacios
 - **repository=** lista separada por espazos de referencias a repositorios de fontes


API para complementos:  
  
\('hbmk' é a variable de context recibida pola función de entrada do complemento\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Rexistrar a extensión do arquivo de entrada para enviar ao complemento \(todas as extensións descoñecidas pásanse ao compilador Harbour\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo de Harbour ao proxecto\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engadir un arquivo C ao proxecto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Engade un arquivo C\+\+ ao proxecto\.
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
 - **hbmk\_OutStdRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Enviar texto á saída estándar \(stdout\) sen ningún formato\.
 - **hbmk\_OutErrRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Enviar texto á saída de erro estándar \(stderr\) sen ningún formato\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Avaliar a macro expresión hbmk2\.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Citar/poñer entre comiñas o nome do arquivo para ser usado como parámetro para comandos externos\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Convertir nomes de arquivo ao formato requerido pola plataforma/compilador C\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Converter nome de arquivo para usar a barra invertida coma separadores de directorio\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Retorna a ruta relativa do valor de \-workdir= dende o directorio de traballo actual\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Atopar arquivo en &lt;xPath&gt; \(matriz ou literais delimitados por separador de directorios son válidos\) con lista de extensións alternativas &lt;aExtDef&gt; \(predeterminado para binarios executables\)\. Retorna o nome de arquivo cando se atopa e NIL cando non\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Cambiar directorio e/ou extensión no nome do arquivo\.
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Codificar nomes de funcións de acordo ás normas do compilador Harbour para xenerar funcións de tipo HB\_FUNC\(\) en código C\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Quitar redundantes/dobres comiñas en literais\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Convirte unha lista de literais en un literal\. O separador predeterminado é un espazo\.


Variables para os complementos: \(elementos hash do contexto de 'hbmk', sen distinción maiúsculas/minúsculas, só lectura se non se indica o contrario\)


 - **"apiver"** Versión do API como número enteiro
 - **"cSTATE"** aviso de estado\. Pode ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** matriz de parámetros pasados a complementos coas opcións \-pflag=/pi= ou que teñan unha extensión rexistrada con hbmk\_Register\_Input\_File\_Extension\(\)
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
  
Intérprete interactivo de Harbour dispoñible en guións:  


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
hb\_DirBase\(\) non mapeada para o guión\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) non mapeada para script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.


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
 - **Para construir unha aplicación que use un paquete de contribución ou que use un paquete de terceiros que inclúa un arquivo \.hbc**  
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
 - **6** failed in compilation phase
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


  - &lt;script&gt; pode ser:  
  
&lt;@script&gt; ou &lt;script\.hbm&gt;: arquivo de opcións de liña de comandos  
  
&lt;script\.hbp&gt;: arquivo de opcións de liña de comandos, también marca un novo destino de construcción cando se especifica na liña de comandos  
  
&lt;script\.hbc&gt;: arquivo de configuración do paquete
  - Cando o nome do arquivo fonte non ten extensión cargarase o arquivo \.hbp se o arquivo existe no directorio actual\. Se non existe usarase a extensión \.prg\.
  - Acéptanse parámetros \-l, \-L, \-i e &lt;script&gt; múltiples\.
  - As opcións habituáis do compilador Harbour acéptanse tal cuál\.  
  
\(ver opción \-harbourhelp\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/harbour, /etc/\.harbour, &lt;base&gt;/etc/\.harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - o guión de construcción hbmk\.hbm no directorio actual sempre é procesado se existe\.
  - Recoméndase o uso da barra invertida como separador de directorios pero a barra estándar tamén se acepta\.
  - Os filtros poden ser usados en cada liña do arquivo \.hbc e na maioría das opcións\.  
  
Tamén se poden combinar os filtros usando os operadores '&amp;' \(and\), '|' \(or\), negados co operador '\!' e agrupados con chaves\. Por exemplo: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - A maioría de liñas en arquivos \.hbc \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) e os correspondentes parámetros de liña de comandos aceptar macro variables\. libpaths= tamén acepta %\{hb\_name\} que será reemprazado polo nome do arquivo \.hbc buscado\.
  - As opcións que aceptan macro variables tamén admiten reemprazo de comandos\. Debe arrodear os comandos con \`\`, e cando o comando teña espazos debe arrodear con dobres comiñas\. A saída estándar do comando será usada coma o seu valor\. Por exemplo: "\-cflag=\`wx\-config \-\-cflags\`", ou ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - Cando se indican múltiples opcións para a xeneración \(\-hblib, \-hbdyn\), a primeira será a usada e o resto serán ignoradas\.
  - Bibliotecas e arquivos obxeto compilados con/para CA\-Cl\*pper non funcionarán en ningunha plataforma ou compilador soportados\.
  - Os valores predeterminados e o soporte de características pode variar para cada plataforma/compilador\.
  - Non se necesita GNU Make, ningunha ferramenta de tipo make específica do compilador C, nin MSYS \(en Windows\) para executar hbmk2\.
  - '\.' \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb ou \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
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

  
Autor:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
