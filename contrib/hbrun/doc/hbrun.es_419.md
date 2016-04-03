Harbour Shell / Script Runner 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 2007\-2016, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<https://github\.com/vszakats/harbour\-core/>  
Traducción \(es\_419\): Guillermo Varona Silupú &lt;gvaronas@gmail\.com&gt;  

Sintáxis:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;parameter\[s\]&gt;\]  
  
Descripción:  


  hbrun puede ejecutar scripts Harbour \(fuente y pre\-compilados\), y además presenta una consola interactiva de comandos\.
  
Las opciones de mas abajo están disponibles en la línea de comandos:  


 - **\-\-hb:debug** activa depuración de script


 - **\-help** esta ayuda
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
  
Archivos:  


 - **\*\.hb** script Harbour
 - **\*\.hrb** Binario portable Harbour \(aka script precompilado Harbour\)
 - **hbstart\.hb** Script de inicio Harbour para la consola interactiva\. Es ejecutado automáticamente al iniciar la consola, si existe\. Ubicación\(es\) posible\(s\) \(en orden de precedencia\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;directorio hbrun&gt;/\.\./etc/harbour, &lt;directorio hbrun&gt;/\.\./etc, &lt;directorio hbrun&gt;
 - **shell plugins** plugins \.hb y \.hrb para la consola interactiva Harbour\. Pueden residir en \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** guarda el historial de comandos del intérprete de comandos de Harbour\. Puede deshabilitar el historial haciendo que la primera linea sea 'no' \(sin comillas y con salto de línea\)\. Se guarda en \[\*\]: $HOME/\.harbour/
 - **hb\_extension** lista de extensiones para cargar en el interprete de comandos interactivo de Harbour\. Una extensión por línea, y se ignora todo lo que hay detrás del caracter '\#'\. Nombre de fichero alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: $HOME/\.harbour/


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** cuando un archivo fuente Harbour es ejecutado como un script de consola
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Variables de entorno  


 - **HB\_EXTENSION** lista separada por espacio de extensiones a cargar en la consola Harbour interactiva
  
API de consola disponible en scripts Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Intercambia GT\. Por defecto \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
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
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Notas:  


  - \.hb, \.prg, \.hrb o \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Valores marcados con \[\*\] pueden ser dependientes de la plataforma huésped o de la configuración\. Esta ayuda ha sido generada en la plataforma huésped 'darwin' \.
  
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
their website at https://www\.gnu\.org/\)\.  
  
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
