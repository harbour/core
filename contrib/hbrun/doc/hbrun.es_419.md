Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 2007\-2013, Viktor Szakáts  
Copyright \(c\) 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Traducción \(es\_419\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintáxis:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;parameter\[s\]&gt;\]  
  
Descripción:  


  hbrun puede ejecutar scripts Harbour \(fuente y pre\-compilados\), y además presenta una consola interactiva de comandos\.
  
Las opciones de mas abajo están disponibles en la línea de comandos:  


 - **\-\-hb:debug** activa depuración de script


 - **\-help** esta ayuda
 - **\-viewhelp** ayuda extensa en visor de texto\.
 - **\-longhelp** ayuda detallada
 - **\-longhelpmd** ayuda extensa en formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Archivos:  


 - **\*\.hb** script Harbour
 - **\*\.hrb** Binario portable Harbour \(aka script precompilado Harbour\)
 - **hbstart\.hb** Script de inicio Harbour para la consola interactiva\. Es ejecutado automáticamente al iniciar la consola, si existe\. Ubicación\(es\) posible\(s\) \(en orden de precedencia\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbrun&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puede deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de línea\)\. Se guarda en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\


Constantes predefinidas en fuentes\.


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.
  
Variables de entorno  


 - **HB\_EXTENSION** lista separada por espacio de extensiones a cargar en la consola Harbour interactiva
  
API de consola disponible en scripts Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Habilita modo de compatibilidad Clipper \(no\-Unicode\)
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Cargar cabecera Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar cabecera Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Muestra lista de cabecera Harbour cargada\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carga paquete\. Similar a la directiva PP \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descargar paquete\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de paquetes cargados
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) no mapeado al script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) no mapeado al script\.
  
Notas:  


  - el archivo \.hb, \.prg, \.hrb o \.dbf pasado como primer parámetro será ejecutado como un script Harbour\. Si el nombre del archivo no contiene componentes de ruta, será buscado en el directorio de trabajo actual y en el PATH\. Si no se especifica una extensión, se buscarán las extensiones \.hb y \.hrb en ese orden\. Los archivos \.dbf se abrirán automáticamente en modo compartido y el intérprete de comandos de Harbour será iniciado\. Las extensiones no\-estandar se autodetectarán para archivos de tipo fuente y scripts precompilados\. Nota: para los scripts Harbour, la página de códigos \(codepage\) es establecida a UTF\-8 por defecto\. El archivo de cabecera principal 'hb\.ch' es incluido \(\#include\) automáticamente\. El formato de fecha por defecto es el estandar ISO: yyyy\-mm\-dd\. El GT por defecto es 'gtcgi', excepto que se detecten llamadas CUI de pantalla completa, en cuyo caso el GT 'gtwin' \[\*\] se selecciona automáticamente \(excepto para INIT PROCEDURESs\)\.
  - Puede usar las teclas &lt;Alt\+V&gt; en la consola interactiva Harbour para pegar texto del portapapeles\.
  - Valores marcados con \[\*\] pueden ser dependientes de la plataforma huésped o de la configuración\. Esta ayuda ha sido generada en la plataforma huésped 'win' \.
  
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
