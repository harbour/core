Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-06\-25 17:16\)  
Copyright &copy; 2007\-2013, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Traducción \(gl\): JLalín  

Sintaxe:  
  
  hbrun &lt;arquivo\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;opción&gt; \[&lt;parameter\[s\]&gt;\]  
  
Descripción:  


  hbrun é quen de executar guións de Harbour \(arquivos fontes ou precompilados\), e dispón de un intérprete interactivo\.
  
As seguintes opcións están dispoñibles na liña de comandos:  


 - **\-\-hb:debug** activar depuración de guións


 - **\-help** esta axuda
 - **\-viewhelp** axuda extendida no visor de texto
 - **\-longhelp** axuda extendida
 - **\-longhelpmd** axuda extendia en formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Arquivos:  


 - **\*\.hb** Guión de Harbour
 - **\*\.hrb** Binario portable de Harbour \(coñecido como guión de Harbour precompilado\)
 - **hbstart\.hb** guión de arranque para o intérprete interactivo\. Execútase automáticamente cando se inicia o intérprete, se existe\. Localizacións posibles \(en orde de precedencia\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;directorio hbrun&gt;
 - **shell plugins** complementos \.hb e \.hrb para o intérprete interactivo de Harbour\. Poden ubicarse en \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** garda o historial do intérprete interactivo de Harbour\. Pode omitirse o historial usando 'no' na primeira liña \(sen comiñas e con retorno de carro/nova liña\)\. Atópase en \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensións para cargar no intérprete interactivo\. Unha extensión por liña, ignórase a partir do caracter '\#' \. Nome de arquivo alternativo en MS\-DOS: hb\_ext\.ini\. Reside en \[\*\]: %APPDATA%\\\.harbour\\


Constantes predefinidas nos fontes:


 - **\_\_HBSCRIPT\_\_HBSHELL** cando unha fonte Harbour se executa coma un guión do intérprete de comandos
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Variables de ámbito:  


 - **HB\_EXTENSION** lista separada por espazos de extensións para cargar no intérprete interactivo de Harbour
  
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
  
Notas:  


  - \.hb, \.prg, \.hrb ou \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Pose usar &lt;Alt\+V&gt; no modo de intérprete interactivo de Harbour para pegar texto dende o portapapéis\.
  - Os valores marcados con \[\*\] poden depender da plataforma anfitriona e/ou da configuración\. Esta axuda foi xenerada na plataforma 'win'\.
  
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


 - Viktor Szakáts \(harbour syenar\.net\) 
