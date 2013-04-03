Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-04\-03 03:33\)  
Copyright \(c\) 2007\-2013, Viktor Szakáts  
Copyright \(c\) 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Translation \(pt\_BR\): Vailton Renato &lt;vailtom@gmail\.com&gt;  

Sintaxe:  
  
  hbrun &lt;arquivo \[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;opção&gt; \[&lt;parâmetro\[s\]&gt;\]  
  
Descrições:  


  hbrun está habilitado a rodar scripts Harbour \(ambos fonte e pré\-compilado\), e dispõe também de um "prompt shell" interativo\.
  
Opções abaixo estão disponíveis em linha de comando:  


 - **\-\-hb:debug** ativar debugação de script


 - **\-help** esta ajuda
 - **\-viewhelp** help completo em formato "text viewer"
 - **\-longhelp** ajuda detalhada
 - **\-longhelpmd** help completo em formato [Markdown](http://daringfireball.net/projects/markdown/)
  
Arquivos:  


 - **\*\.hb** script Harbour
 - **\*\.hrb** Harbour binario portável \(Também conhecida como Harbour script pré\-compilado\)
 - **hbstart\.hb** script de inicialização Harbour para shell interativo Harbour\. Se presente ele é executado automaticamente na inicialização do shell\. Localizações possíveis \(em ordem de precedência\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbrun diretório&gt;
 - **shell plugins** \.hb e \.hrb plugins para shell interativo Harbour\. Eles pode residir em \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** armazena o historico de comando do shell interativo Harbour shell\. Voce pode desabilitar o historico fazendo a primeira linha 'no' \(sem aspas e com nova linha\)\. Localizado em \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensões a ser carregada no shell interativo Harbour\. Uma extensão por linha, a parte alem do caracter '\#' será ignorada\. Nome alternativo em MS\-DOS: hb\_ext\.ini\. localizado em \[\*\]: %APPDATA%\\\.harbour\\


Constantes pré\-definidas nos fontes:


 - **\_\_HBSCRIPT\_\_HBSHELL** quando um programa fonte Harbour está rudando como "shell script"
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.
  
Variáveis ​​de ambiente:  


 - **HB\_EXTENSION** lista separada por espaço de extensões para carga no shell interativo do Harbour
  
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
pacote carregado\. Similar para diretivas de \#request PP\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Descarregar pacote\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista de pacotes carregados\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) não mapeado para script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) não mapeado para script\.
  
Notas:  


  - \.hb, \.prg, \.hrb ou \.dbf arquivo passado como primeiro parâmetro irá rodar como Script Harbour\. Se o nome do arquivo não contiver componentes do "path", ele será procurado no diretório de trabalho atual e no "PATH"\.Se não é dada extensão, \.hb e \.hrb serão pesquisados nessa ordem\. arquivos \.dbf serão abertos no modo compartilhado "shared" e o "shell" interativo Harbour será lançado\. Extensões não padronizadas serão detectadas para fontes e e tipos de script pré\-compilados\. Nota, para Scripts Harbour, a pagina de códigos "codepage" será em UTF\-8 por padrão\. O nucleo padrão de cabeçalhos 'hb\.ch' será automaticamente incluido\. O formato da data será "aaaa\-mm\-dd" padrão "ISO"\. O Gt padrão é 'gtcgi', a menos que as chamadasCUI de tela cheia seja detectadas, quando 'gtwin' \[\*\] será automaticamente selecionado \(exeto para "INIT PROCEDUREs"\)\.
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
