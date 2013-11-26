Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-11\-26 05:07\)  
Copyright &copy; 2007\-2014, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Tradução \(pt\_BR\): Sami Laham &lt;sami@laham\.com\.br&gt; / Vailton Renato &lt;vailtom@gmail\.com&gt;  

Sintaxe:  
  
  hbrun &lt;arquivo \[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;opção&gt; \[&lt;parâmetro\[s\]&gt;\]  
  
Descrição:  


  hbrun está habilitado a rodar scripts Harbour \(ambos fonte e pré\-compilado\), e dispõe também de um "prompt shell" interativo\.
  
Opções abaixo estão disponíveis em linha de comando:  


 - **\-\-hb:debug** ativar debugação de script


 - **\-help** esta ajuda
 - **\-viewhelp** help completo em formato "text viewer"
 - **\-longhelp** ajuda detalhada
 - **\-longhelpmd** help completo em formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Arquivos:  


 - **\*\.hb** script Harbour
 - **\*\.hrb** Harbour binario portável \(Também conhecido como Harbour script pré\-compilado\)
 - **hbstart\.hb** arquivo de inicialização de script para shell interativo Harbour\. Se presente ele é executado automaticamente na inicialização do shell\. Localizações possíveis \(em ordem de precedência\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbrun diretório&gt;
 - **shell plugins** \.hb e \.hrb plugins para shell interativo Harbour\. Eles pode residir em \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** armazena o histórico de comando do shell interativo Harbour shell\. Voce pode desabilitar o histórico fazendo a primeira linha 'no' \(sem aspas e com nova linha\)\. Localizado em \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensões a ser carregada no shell interativo Harbour\. Uma extensão por linha, a parte alem do caracter '\#' será ignorada\. Nome alternativo em MS\-DOS: hb\_ext\.ini\. localizado em \[\*\]: %APPDATA%\\\.harbour\\


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** quando um programa fonte Harbour está rodando como "shell script"
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Variáveis ​​de ambiente:  


 - **HB\_EXTENSION** lista de extensões para carga no shell interativo do Harbour separada por espaço
  
Shell API disponível nos scripts em Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Mudar GT\. Padrão \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
habilitar modo de compatibilidade Clipper \(non\-Unicode\)
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carregar cabeçalho "header" Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarregar cabeçalho "header" Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Mostra a lista de cabeçalhos Harbour carregados\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
carregar pacote\. Similar para diretivas de \#request PP\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarregar pacote\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de pacotes carregados\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) não mapeado para script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) não mapeado para script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Notas:  


  - \.hb, \.prg, \.hrb ou \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Voce pode usar &lt;Alt\+V&gt; no "shell do Harbour" para colar um texto do clipboard\.
  - Valores marcados com \[\*\] pode ser plataforma hospedagem e/ou configuração dependente\. Esta ajuda foi gerada em 'win' plataforma de hospedagem\.
  
Licença:  


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
