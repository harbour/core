Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 2007\-2013, Viktor Szakáts  
Copyright \(c\) 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Traducción \(es\_ES\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintáxis:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;parameter\[s\]&gt;\]  
  
Descripción:  


  hbrun puede ejecutar archivos de órdenes \(tanto en código fuente como pre\-compilados\), y tambien ofrece un intérprete de línea de comandos\.
  
Las siguientes opciones están disponibles en línea de comandos:  


 - **\-\-hb:debug** activar la depuración de archivos de órdenes


 - **\-help** esta ayuda
 - **\-viewhelp** abrir ayuda completa en visor de texto
 - **\-longhelp** ayuda detallada
 - **\-longhelpmd** ayuda completa en formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Ficheros:  


 - **\*\.hb** Archivo de órdenes de Harbour
 - **\*\.hrb** binario portable de Harbour \(aka archivo de comandos pre\-compilado de Harbour\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbrun&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puedes deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de linea\)\. Se guarda en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensiones para cargar en el interprete de comandos interactivo de Harbour\. Una extensión por línea, y se ignora todo lo que hay detrás del caracter '\#'\. Nombre de fichero alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\


Constantes predefinidas en el código fuente:


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.
  
Variables de entorno:  


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
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
  
Notas:  


  - el archivo \.hb, \.prg, \.hrb o \.dbf pasado como primer parámetro será ejecutado como un archivo de órdenes\. Si el nombre del archivo no contiene componentes de una ruta, será buscado en el directorio actual y en el PATH\. Si no se especifica una extensión, se buscarán las extensiones \.hb y \.hrb  en ese orden\. Los archivos \.dbf se abrirán automáticamente en modo compartido y el intérprete de comandos de Harbour será iniciado\. Las extensiones no\-estandar se autodetectarán para archivos de tipo fuente y archivos de órdenes precompilados\. Nótese, que para los archivos de órdenes de Harbour, la página de códigos \(codepage\) establecida por defecto es la UTF\-8\. El archivo de cabecera principal 'hb\.ch' es incluido \(\#include\) automáticamente\. El formato de fecha por defecto es el estandar ISO: yyyy\-mm\-dd\. El GT por defecto es 'gtcgi', excepto que se detecten llamadas CUI de pantalla completa, en cuyo caso el GT 'gtwin' \[\*\]  se selecciona automáticamente \(excepto para INIT PROCEDURESs\)\.
  - Puede utilizar las teclas &lt;Alt\+V&gt; en el indicador de comandos interactivo de Harbour para pegar texto desde el portapapeles\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'win' host platform\.
  
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
