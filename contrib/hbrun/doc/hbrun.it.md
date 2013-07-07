Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-07\-05 12:12\)  
Copyright &copy; 2007\-2013, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Traduzione \(it\): \(inserisci qui il tuo nome\)  

Sintassi:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;parametro\[i\]&gt;\]  
  
Descrizione:  


  hbrun is able to run Harbour scripts \(both source and precompiled\), and it also features an interactive shell prompt\.
  
Le opzioni riportate di seguito sono disponibili da riga di comando:  


 - **\-\-hb:debug** abilita il debug dello script


 - **\-help** questo aiuto
 - **\-viewhelp** Help esteso nel visualizzatore di testo
 - **\-longhelp** aiuto esteso
 - **\-longhelpmd** Help esteso in formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Files:  


 - **\*\.hb** Script di Harbour
 - **\*\.hrb** Harbour binario portabile \(conosciuto anche come script Harbour precompilato\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;cartella hbrun&gt;
 - **shell plugins** plugins \.hb e \.hrb per la shell interattiva di Harbour\. Possono essere situati in \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** memorizza la cronologia dei comandi per la shell interattiva di Harbour\. E' possibile disattivare la cronologia creando la prima linea 'no' \(senza virgolette e ritorno a capo\)\. E' contenuta in \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: %APPDATA%\\\.harbour\\


Costanti predefinite nei sorgenti:


 - **\_\_HBSCRIPT\_\_HBSHELL** quando un file sorgente Harbour è eseguito come uno script di shell
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, ecc\.\.\.
  
Variabili d'ambiente:  


 - **HB\_EXTENSION** lista di estensioni separate da spazio da caricare nella shell interattiva di Harbour
  
Shell API disponibile negli Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambia GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Abilita la modialità di compatibilità Clipper \(non\-Unicode\)
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica l'intestazione Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scarica l'intestazione Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Visualizza l'elenco di intestazione di Harbour caricato\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica un pacchetto\. Simile alla direttiva PP \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scaricare pacchetto\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista dei pacchetti caricati
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
  
Note:  


  - \.hb, \.prg, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Puoi usare i tasti &lt;Alt\+V&gt; nella shell interativa di Harbour per incollare il testo dalla clipboard\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'win' host platform\.
  
Licenza:  


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

  
Autore:  


 - Viktor Szakáts \(harbour syenar\.net\) 
