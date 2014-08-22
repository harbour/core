Harbour Shell / Script Runner 3\.4\.0dev \(5890c33\) \(2014\-07\-21 01:51\)  
Copyright &copy; 2007\-2014, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<https://github\.com/vszakats/harbour\-core/>  
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
 - **\-longhelpmd** help completo em formato [Markdown](https://daringfireball.net/projects/markdown/)
  
Arquivos:  


 - **\*\.hb** script Harbour
 - **\*\.hrb** Harbour binario portável \(Também conhecido como Harbour script pré\-compilado\)
 - **hbstart\.hb** arquivo de inicialização de script para shell interativo Harbour\. Se presente ele é executado automaticamente na inicialização do shell\. Localizações possíveis \(em ordem de precedência\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbrun diretório&gt;
 - **shell plugins** \.hb e \.hrb plugins para shell interativo Harbour\. Eles pode residir em \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** armazena o histórico de comando do shell interativo Harbour shell\. Voce pode desabilitar o histórico fazendo a primeira linha 'no' \(sem aspas e com nova linha\)\. Localizado em \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensões a ser carregada no shell interativo Harbour\. Uma extensão por linha, a parte alem do caracter '\#' será ignorada\. Nome alternativo em MS\-DOS: hb\_ext\.ini\. localizado em \[\*\]: %APPDATA%\\\.harbour\\


Constantes pré\-definidas nos fontes \(não defini\-las manualmente\):


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
Nome do script em execução\.
  
Notas:  


  - Arquivo \.hb, \.prg, \.hrb ou \.dbf passado como primeiro parâmetro será executado como script Harbour\. Se o nome do arquivo não contém componentes do caminho "path", ele será procurado no diretório de trabalho atual e em PATH\. Se não for dada a extensão, as extensões \.hb e \.hrb serão pesquisados nessa\. Aquivo \.dbf será aberto automaticamente no modo compartilhado e o shell interativo Harbour lançado\. Extensões fora do padrão será detectado automaticamente para a fonte e tipos de script pré\-compilados\.Note, para Harbour scripts a "codepage" é colocada em UTF\-8 por padrão\.O cabeção principal 'hb\.ch' será automaticamente incluido " \#included " no prompt shell interativo\. O formato de data padráo é o ISO yyyy\-mm\-dd\. SET EXACT é colocado para ON\. O GT padrá é 'gtcgi', a não ser que sejam detectados chamadas CUI em tela cheia, quando 'gtwin' \[\*\] é selecionado automaticamente \(exceto para INIT PROCEDUREs\)\.
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
    Creative Commons Attribution\-ShareAlike 4\.0 International:  
    https://creativecommons\.org/licenses/by\-sa/4\.0/  

  
Autor:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
