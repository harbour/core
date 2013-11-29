Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-11\-26 08:58\)  
Copyright &copy; 2007\-2014, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Traducción \(es\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintaxis:  
  
  hbrun &lt;archivo\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;opción&gt; \[&lt;parameter\[s\]&gt;\]  
  
Descripción:  


  hbrun puede ejecutar archivos de órdenes \(tanto en código fuente como pre\-compilados\), y tambien ofrece un intérprete de línea de comandos\.
  
Las siguientes opciones están disponibles en la línea de comandos:  


 - **\-\-hb:debug** activa la depuración de archivos de órdenes


 - **\-help** esta ayuda
 - **\-viewhelp** abre la ayuda completa en un visor de texto
 - **\-longhelp** ayuda completa
 - **\-longhelpmd** ayuda completa en formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Ficheros:  


 - **\*\.hb** archivo de órdenes de Harbour
 - **\*\.hrb** binario portable de Harbour \(aka archivo de comandos pre\-compilado de Harbour\)
 - **hbstart\.hb** archivo de órdenes de inicio de Harbour para el intérprete de comandos de Harbour\. Se ejecuta automáticamente al comienzo de la ejecución del intérprete de comandos, si existe\. Localizaciones posibles \(en orden de precedencia\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio de hbrun&gt;
 - **shell plugins** complementos '\.hb' y '\.hrb' para el intérprete de comandos interactivo de Harbour\. Pueden localizarse en \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puedes deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de linea\)\. Se guarda en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensiones para cargar en el interprete de comandos interactivo de Harbour\. Una extensión por línea, y se ignora todo lo que hay detrás del caracter '\#'\. Nombre de fichero alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** cuando un archivo de código fuente es ejecutado con el intérprete de comandos
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Variables de entorno:  


 - **HB\_EXTENSION** lista de extensiones para cargar en el intérprete de comandos de Harbour separados por espacio
  
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
Carga el paquete en memoria\. Similar a la directiva de preprocesado \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarga el paquete de la memoria\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de paquetes cargados\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) no está mapeada para el uso en archivo de órdenes\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) no está mapeada para el uso en archivo de órdenes\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Notas:  


  - \.hb, \.prg, \.hrb o \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Puede utilizar las teclas &lt;Alt\+V&gt; en el indicador de comandos interactivo de Harbour para pegar texto desde el portapapeles\.
  - Los valores marcados con \[\*\] pueden depender de la plataforma anfitriona y/o la configuración\. Esta ayuda se generó en una plataforma anfitriona 'win'\.
  
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
    Creative Commons Attribution\-ShareAlike 4\.0 International:  
    https://creativecommons\.org/licenses/by\-sa/4\.0/  

  
Autor:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
