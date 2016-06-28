Harbour Make \(hbmk2\) 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 1999\-2016, Viktor Szakáts  
<https://github\.com/vszakats/harbour\-core/>  
Tradução \(pt\_BR\): Sami Laham &lt;sami@laham\.com\.br&gt; / Vailton Renato &lt;vailtom@gmail\.com&gt;  

Sintaxe:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.hbc|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descrição:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will auto\-detect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Opções:  


 - **\-o&lt;outname&gt;** nome do arquivo de saída
 - **\-l&lt;libname&gt;** linkar com a biblioteca &lt;libname&gt;\. &lt;libname&gt; deve estar sem o "path", sem a extensão e sem o prefixo "lib" \(a não ser que faça a parte do nome\)\. Não adicione bibliotecas do núcleo Harbour, elas serão inseridas automaticamente quando necessário\. Se &lt;libname&gt; começar com um carácter '\-', a biblioteca será removida da lista de bibliotecas na hora de linkar\.
 - **\-L&lt;libpath&gt;** "paths" adicionais para pesquisa por bibliotecas
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** "paths" adicionais para pesquisa de arquivos de cabeçalho "headers"
 - **\-static|\-shared** linkar com biliotecas estáticas/compartilhadas
 - **\-gt&lt;name&gt;** linkar com GT&lt;name&gt; controlador de terminal gráfico "GT ", a linkagem pode ser repetida com mais GTs\. O primeiro será o GT padrão na execução do programa\.
 - **\-inc\[\-\]** habilita/desabilitar o modo de compilação incremental \(padrão: desbilitado\)
 - **\-hbexe** criar um executável \(padrão\)
 - **\-hblib** criar biblioteca estática
 - **\-hbdyn** criar biblioteca dinâmica \(não linkada com a Harbour VM\)
 - **\-hbdynvm** criar biblioteca dinâmica \(ligada com a Harbour VM\)
 - **\-strip\[\-\]** strip \(or don't\) debugging \(and other extra\) information from target binary\. They are included by default by certain C compilers, f\.e\.: gcc\*, clang, mingw\*, djgpp\.


 - **\-mt|\-st** linkar com suporte multi ou single\-thread na Harbour VM
 - **\-gui|\-std|\-cli** criar GUI/console/executável linha de comando
 - **\-main=&lt;mainfunc&gt;** sobrepor o nome inicial das função/procedures
 - **\-request=&lt;func&gt;** forçar a linkagem da função/procedure
 - **\-fullstatic** linkar com todas as bibliotecas estáticas
 - **\-pic\[\-\]** criar posição independente do código objeto \(sempre habilitado no modo \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** criar binário que faça uso da biblioteca compartilhada do Harbour com referência absoluta \(ou não\) \(padrão: 'fullshared' quando Harbour é instalado no local do sistema, caso contrário será 'fixshared'\) \(fix/full é somente para \*nix\)
 - **\-nulrdd\[\-\]** linkar com o nulrdd
 - **\-debug\[\-\]** adicionar/excluir informações de debug do compilador C\. Para compilar com Harbour utilize a opção \-b como de costume\.
 - **\-optim\[\-\]** alterna as optimizações do compilador C \(ativado por padrão\)
 - **\-cpp\[\-\]** forçar compilação em modo C\+\+/C
 - **\-cpp=&lt;value&gt;** seleciona modo C\+\+\.Os Valores permitidos são: def, yes, no
 - **\-c=&lt;value&gt;** select C standard\. Allowed values are: iso90, iso99, iso11, gnu90, gnu99, gnu11
 - **\-cpp=&lt;value&gt;** select C\+\+ mode or standard\. Allowed values are: def, yes, no, iso98, iso11, iso14, gnu98, gnu11, gnu14
 - **\-map\[\-\]** criar \(ou não\) o arquivo map
 - **\-implib\[\-\]** create \(or not\) an import library \(in \-hbdyn/\-hbexe mode\)\. The name will have a suffix added\.
 - **\-implib=&lt;output&gt;** criar bibliotéca importação \(no modo \-hbdyn/\-hbexe\) nomear a &lt;output&gt; \(padrão: mesma saída\)
 - **\-ln=&lt;link&gt;** criar um link simbólico apontando para &lt;output&gt; \(&lt;link&gt; é considerado em relação ao &lt;output&gt;\)
 - **\-trace\[\-\]** exibir os comandos executados
 - **\-beep\[\-\]** ativa \(ou desativa\) um beep simples em caso de sucesso ou um beep duplo em caso de erro
 - **\-ignore\[\-\]** ignore erros quando executar as ferramentas de compilação \(padrão: off\)
 - **\-hbcppmm\[\-\]** sobrepor o as funções padrão C\+\+ de gerenciamento de memoria pelas do Harbour
 - **\-winuni\[\-\]** seleciona entre UNICODE \(WIDE\) and ANSI no uso da API Windows para arquivos de entrada C/C\+\+ \(padrão: ANSI\) \(Somente Windows\. Para WinCE ele é sempre colocado em UNICODE\)
 - **\-nohblib\[\-\]** não usar biblioteca estática do núcleo do Harbour quando linkar
 - **\-nodefgt\[\-\]** Não link GTs padrão \(o modo \-static é mais eficaz\)
 - **\-nolibgrouping\[\-\]** desativar agrupamento de LIBs em compiladores baseados no gcc\.
 - **\-nomiscsyslib\[\-\]** não adicione bibliotecas extras do sistema à lista padrão de bibliotecas
 - **\-traceonly** exibir os comandos à serem executados, mas não execute\-os
 - **\-warn=&lt;level&gt;** define o nível de aviso do compilador C  
&lt;level&gt; pode ser: max, yes, low, no, def \(padrão: yes\)
 - **\-harden\[\-\]** habilita a opção hardening no compilador/Linkador C \(padrão: habilitado para Windows, desabilitado para outros sistemas\)
 - **\-vcsts\[\-\]** set timestamp of output file\(s\) to the last repository commit \(Supported with: Git\)
 - **\-compr=&lt;level&gt;** comprime executável/bibliotéca dinâmica \(precisa da ferramenta UPX\)  
&lt;level&gt; pode ser: yes, no, min, high, max
 - **\-run\[\-\]** executar/não executar o aplicativo gerado\.
 - **\-vcshead=&lt;file&gt;** gerar arquivo de cabeçalho \.ch com informações do repositório local\. Git, SVN, Mercurial, Bazaar, Fossil, CVS e Monotone são suportados atualmente\. O cabeçalho gerado irá definir constantes pré\-processador \_HBMK\_VCS\_TYPE\_ com o nome de VCS detectado \_HBMK\_VCS\_ID\_ como o unico ID do repositorio local\. Informações específicas de VCS são adicionado como constante \_HBMK\_VCS\_&lt;TYPE&gt;\_\*\_, onde são suportadas\. Se nenhum sistema de VCS for detectado, um número seqüencial será lançado automaticamente em cada construção\.
 - **\-bldhead=&lt;file&gt;** generate \.ch header file with build information, like build sequence number and timestamp\. Generated header will define preprocessor constants \_HBMK\_BUILD\_ID\_ and \_HBMK\_BUILD\_ID\_NUM\_ with sequence number \(incremented on each build\), \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build and \_HBMK\_BUILD\_RANDSTR\_32\_ with a random string of 32 bytes in hexadecimal format
 - **\-haltrev\[\-\]** do not increase revision numbers in \-bldhead= \(\_HBMK\_BUILD\_ID\_\) and \-vcshead= \(\_HBMK\_VCS\_ID\_\) options \(default: do increase\)
 - **\-icon=&lt;file&gt;** colocar o arquivo &lt;file&gt; como ícone de aplicação\. O arquivo &lt;file&gt; deve estar em um formato suportado na plataforma alvo \(não suportado por algumas plataformas/compiladores\)\. No Windows, isso é implementado gerando e linkando um arquivo de recursos\.
 - **\-manifest=&lt;file&gt;** incorporar arquivo manifest &lt;file&gt; no executável / lib dinâmica \(somente para Windows\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both auto\-detected\.
 - **\-signpw=&lt;pw&gt;** usar &lt;pw&gt; como senha ao assinar executável \(somente Windows e Darwin\)
 - **\-signts=&lt;\[std:\]url&gt;** usar &lt;url&gt; como servidor data e hora "timestamp" confiável\. Opcional &lt;std&gt; pode especificar o padrão como 'rfc3161' ou 'authenticode' \(sem aspas\)\. O padrão é 'rfc3161'\. Valor vazio redefine para o padrão: http://timestamp\.globalsign\.com/scripts/timstamp\.dll
 - **\-instfile=&lt;g:file&gt;** adicionar &lt;file&gt; para a lista de arquivos a serem copiados para caminho especificado pela opção \-instpath\. &lt;g&gt; é um grupo de cópias opcional \(case sensitive\), deve ser de pelo menos dois caracteres\.No caso de você não especificar &lt;file&gt;, a lista de arquivos naquele grupo será esvaziado\.
 - **\-instpath=&lt;g:path&gt;** cópia de arquivo alvo \(s\) para &lt;path&gt;\. Se &lt;path&gt; é um diretório, ele deve terminar com o separador de path, neste caso, os arquivos especificados pela opção \-instfile também serão copiados\. pode ser especificado várias vezes\. &lt;g&gt; é um grupo de cópias opcional, ele deve ser de pelo menos dois caracteres\. A construção alvo será automaticamente copiada para o grupo padrão \(vazio\)\. Que existem após grupos internos &lt;g&gt;: 'depimplib' para fonte de bibliotecas de importação e 'depimplibsrc' para fonte de bibliotecas de importação de arquivos \(\.dll\), ambos pertencentes as dependências\.
 - **\-instforce\[\-\]** copie os arquivo\(s\) para o destino do caminho de instalação mesmo que já atualizados
 - **\-depimplib\[\-\]** habilitar \(ou desabilitar\) a importação da biblioteca da fontes de bibliotecas especificadas em \-depimplibs= opções \(padrão: yes\)
 - **\-stop\[=&lt;text&gt;\]** parar sem fazer nada e mostrar &lt;texto&gt; se especificado
 - **\-echo=&lt;text&gt;** ecoa texto na tela
 - **\-skip** skip processing the rest of the project file \(filters not supported\)
 - **\-pause** obrigar à espera de uma tecla na saída em caso de erro \(somente com driver GT alternativo\)
 - **\-exitstr** Exibir erros na forma de texto amigável "human readable"
 - **\-info** ativar mensagens informativas
 - **\-quiet\[\-\]** suprimir todas as mensagens


 - **\-bldf\[\-\]** herdar flags do Harbour: todos/nenhum \(padrão\)
 - **\-bldf=\[p\]\[c\]\[l\]** herdar todos os flags \.prg/\.c/linker \(ou nenhum\) provindos do Harbour
 - **\-F&lt;framework&gt;** linkar com &lt;framework&gt; framework \(somente Darwin\)
 - **\-prgflag=&lt;f&gt;** passa um "flag" para o compilador Harbour
 - **\-cflag=&lt;f&gt;** passa um "flag" para o compilador C
 - **\-resflag=&lt;f&gt;** passa um "flag" para o compilador de recursos \(apenas Windows\)
 - **\-ldflag=&lt;f&gt;** passa um "flag" para o linkeditor \(executável\)
 - **\-dflag=&lt;f&gt;** passa um "flag" para o linkeditor \(biblioteca dinânica\)
 - **\-aflag=&lt;f&gt;** passa um "flag" para o linkeditor \(bibliotéca estática\)
 - **\-iflag=&lt;f&gt;** passa um "flag" para criar o comando de importação de bibliotecas
 - **\-signflag=&lt;f&gt;** passa um "flag" para criar o comando "code sign"
 - **\-runflag=&lt;f&gt;** passa um "flag" para o executável gerado quando \-run for utilizado
 - **\-cflag\+=&lt;f&gt;** passa um "flag" para o compilador C substituindo os "flags" adicionados por ele mesmo hbmk2\. Use com cuidado\.
 - **\-ldflag\+=&lt;f&gt;** passa uma opção "raw" para linkar \(executável\), após a lista da biblioteca\. Use com cuidado\.
 - **\-dflag\+=&lt;f&gt;** passa uma opção "raw" para linkar \(biblioteca dinâmica\), após a lista da biblioteca\. Use com cuidado\.
 - **\-3rd=&lt;f&gt;** "options/flags" reservado para ferramentas de terceiros, sempre ignorado por hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alterar as variáveis locais de ambiente\. &lt;e&gt;é o nome da variável a ser alterada\. &lt;o&gt; pode ser '=' para definir/sobrepor, '\-' para apagar, '\+' to adicionar ao fim do valor existente, '\#' para inserir o valor inicialização da variável\. &lt;v&gt; é o valor a ser definido/adicionardo/inserido "set/append/insert"\.
 - **\-jobs=&lt;n&gt;** start n compilation threads \(multiprocess platforms only\) \(default: number of processors available or 1 if not detectable/applicable; on this system: 2\)
 - **\-head=&lt;m&gt;** controle de análise de fonte header \(em modo de construção incremental\)  
&lt;m&gt; Pode ser: native \(usa o compilador para extrair dependências\), full \(padrão, usa o analisador de texto simples em todo o arquivo\), dep, off
 - **\-rebuild** recriar \(em modo incremental\)
 - **\-rebuildall** recriar com os sub\-projetos \(em modo incremental\)
 - **\-clean** compilação limpa \(em modo de compilação incremental\)
 - **\-workdir=&lt;dir&gt;** diretório de trabalho  
\(padrão: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] no modo incremental, outra forma diretório temporário do sistema operacional "OS temp directory"\)


 - **\-hbcontainer** compilação virtual, ela não cria nada\. Útil para a criação de um \.hbp Com o único propósito de fazer referência a sub\-projectos
 - **\-hbimplib** criar Bibliotecas de importação \(só para Windows\)


 - **\-hbl\[=&lt;output&gt;\]** nome\-de\-arquivo \.hbl resultante\. A macro %\{hb\_lng\} é aceita no nome\-de\-arquivo\.
 - **\-lng=&lt;languages&gt;** lista de idiomas à serem substituidos nas macros %\{hb\_lng\} nos arquivos \.pot/\.po e nos nomes de arquivos de saída \.hbl/\.po\. Lista separada por vírgula:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** criar/atualizar arquivo \.po à partir dos fontes\. Se um arquivo \.po com o mesmo nome existir, o arquivo será mesclado\.
 - **\-minipo\[\-\]** do \(not\) add source file reference to \.po \(default: add them\)
 - **\-rebuildpo** recria o arquivo \.po removendo assim todas as entradas obsoletas no mesmo


 - **\-hbx=&lt;n\[\.hbx&gt;\]&gt;** criar cabeçalho Harbour \(no formato \.hbx\) com todos os símbolos externos\. Sem parâmetro irá desabilitar isso\.  
A extensão padrão é \.hbx\. Se colocar, &lt;n&gt; será automaticamente adicionada a lista de arquivos de entrada Harbour e serão incorporadas ao projeto\. Portanto, a parte do nome de &lt;n&gt; não deve ser o mesmo que qualquer outro arquivo de entrada presente no projeto\.
 - **\-hbx\[\-\]** atualiza \(ou não\) os arquivos \.hbx especificados na opção \-hbx= \(padrão: atualiza\)
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt;é um nome de arquivo header \. &lt;\.hbc&gt; é um nome de arquivo \.hbc que será automaticamente incluído caso o header seja encontrado em algum dos fonte compilados\. \(EXPERIMENTAL\)


 - **\-depurlbase=&lt;d:u&gt;** &lt;d&gt; é o nome da dependência\. &lt;u&gt; é a URL do projeto\. Pode ser especificado varias vezes\.
 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; é o nome da dependência\. &lt;n&gt; é o nome do pacote da depêndencia\. Pode ser especificado varias vezes\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; é o nome da dependência\. &lt;h&gt; é a key header \(\.h\) do pacote de dependência\. Múltiplas alternativas de headers podem ser especificadas\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; é o nome da dependência\. &lt;f&gt; pode ser 'yes' or 'no', especifica se a dependência é opcional\.  
Padrão: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; é o nome da dependência\. Definir &lt;r&gt; como diretório raiz para os caminhos especificados nas opções \-depincpath \.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; é o nome da dependêcia\. Adicione &lt;i&gt; para detectar o cabeçalho na lista de caminhos\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; é o nome da dependêcia\. Adicione &lt;i&gt; para detectar o cabeçalho na lista de caminhos, onde &lt;i&gt; aponta para o diretório local do projeto e contendo um incorporado \(Também conhecida como 'locally hosted' dependência\)\.
 - **\-depimplibs=&lt;d:dll\[:lib\]&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;dll&gt; to the import library source list\. Optionally override the name of the generated implib to become &lt;lib&gt;\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; é o nome da dependência\. Coloque no nome da biblioteca de importação para &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; é o nome da dependência\. Fecha as definição de dependência e faça a detecção de dependência real, configure todos os filtros pré\-definidos, macro variáveis e adeque as opções de construção\. Opcional, se for omitido, a detecção será realizada após o processamento de todas as opções\.


 - **\-plugin=&lt;filename&gt;** adicionar plugin\. &lt;filename&gt; pode ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** passar arquivo de entrada paraplugins
 - **\-pflag=&lt;f&gt;** passa um "flag" para plugins
  
Opções abaixo estão disponíveis em linha de comando:  


 - **\-target=&lt;script&gt;** especificar um destino da nova compilação\. &lt;script&gt; pode ser \.prg \(sem extensão\) ou um arquivo \.hbp \. Note\-se que arquivos \.hbp são automaticamente considerados como alvos separados de compilação\.


 - **\-hbrun** executar programa gerado\.
 - **\-hbraw** interromper após executar o compilador Harbour
 - **\-hbcmp|\-clipper** interromper após criar os arquivos objetos  
criar um link ou copiar o hbmk2 para hbcmp/clipper resultará no mesmo efeito
 - **\-hbcc** aceita "flags" específicos C\.  
criar link/copia hbmk2 para hbcc para o mesmo efeito
 - **\-hblnk** aceita parâmetros específicos do linkeditor\.
 - **\-autohbm\[\-\]** habilita \(ou desabilita\) o processamento de hbmk\.hbm no diretório atual \(padrão: yes\)
 - **\-hb10** habilita modo de compatibilidade 'Harbour 1\.0\.x'
 - **\-hb20** habilita modo de compatibilidade 'Harbour 2\.0\.x'
 - **\-hb30** habilita modo de compatibilidade 'Harbour 3\.0\.x'
 - **\-hb32** enable Harbour 3\.2\.0dev compatibility mode
 - **\-xhb** habilitar modo xHb
 - **\-hbc** ativa modo C puro
 - **\-blinker** emulate Cl\*pper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect
 - **\-exospace** veja acima
 - **\-rtlink** veja acima


 - **\-hbreg\[=global\]** registre Harbour Script \(\.hb\) com hbmk2 \(somente para Windows\)
 - **\-hbunreg\[=global\]** Harbour Script \(\.hb\) não registrado hbmk2 \(somente para Windows\)


 - **\-find &lt;text&gt;** listar todas as funções conhecidas do Harbour que contenham &lt;text&gt; em seu nome, juntamente com o seu pacote \(tanto faz maiúsculas ou minúsculas, aceita vários valores, pode conter caracteres curinga\)
 - **\-doc &lt;text&gt;** show documentation for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-docjson &lt;text&gt;** output documentation in JSON format for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-fixcase &lt;file\[s\]&gt;** fix casing of Harbour function names to their 'official' format\. Core functions and functions belonging to all active contribs/addons with an \.hbx file will be processed\.
 - **\-sanitize &lt;file\[s\]&gt;** convert filenames to lowercase, EOLs to platform native and remove EOF character, if present\.


 - **\-hbmake=&lt;file&gt;** converte um projeto do hbmake para um arquivo \.hbp
 - **\-xbp=&lt;file&gt;** converte um projeto \.xbp \(xbuild\) para um arquivo \.hbp
 - **\-xhp=&lt;file&gt;** converte um projeto \.xhp \(xMate\) para um arquivo \.hbp


 - **\-\-hbdirbin** saída do diretório de binarios Harbour para stdout
 - **\-\-hbdirdyn** saída do diretório de bibliotéca dinâmica Harbour para stdout
 - **\-\-hbdirlib** saída do diretório de bibliotécas estáticas Harbour para stdout
 - **\-\-hbdirinc** saída do diretório header Harbour para stdout
 - **\-\-hbinfo\[=nested\]** redireciona as informações 'Harbour build' para stdout\. Saida é em formato JSON\. Os 'paths'  
inclusos sempre contém as "barras direita" como separador de diretório\. Cada bloco de JSON é seguido por um byte 0x0A\.


 - **\-plat=&lt;platform&gt;** sobrepor a plataforma padrão \(padrão: automatic\)
 - **\-cpu=&lt;cpu&gt;** sobrepor o destino da CPU padrão \(padrão: automatic\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** override C compiler auto\-detection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** especificar um nome de aplicativo "build"
 - **\-lang=&lt;lang&gt;** Sobrepor a linguagem padrão\. &lt;lang&gt; está em formato de código ISO\.
 - **\-width=&lt;n&gt;** ajuste a largura de saída para &lt;n&gt; caracteres \(0=ilimitado\)\.
 - **\-shl** exibir niveis de sub\-projeto nas linhas de saída
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
 - **\-harbourhelp** Ajuda do compilador Harbour \(todas as opções do compilador Harboursão aceitos como é por hbmk2\)
 - **\-credits** créditos compilador Harbour
 - **\-build** Compilador Harbour \(build\)
 - **\-version** exibir somente o cabeçalho com a versão do hbmk
  
As opções abaixo são para desenvolvedores e internas \(compatibilidade não garantida\):  


 - **\-debugtime** medir o tempo gasto na contrução\.
 - **\-debuginc** exibir o conteúdo da compilação incremental
 - **\-debugstub** exibir o conteúdo de todos os arquivos fontes gerados internamente
 - **\-debugi18n** exibir o conteúdo da geração do arquivo de tradução
 - **\-debugdepd** exibir o conteúdo da detecção de dependência
 - **\-debugpars** exibir todos os parâmetros de entrada na ordem de processamento
 - **\-debugrte** gerar um erro de tempo de execução "run\-time error"


Você pode criar um link simbólico/copiar/renomear hbmk2 para os seguintes nomes para alterar o modo padrão de operação:


 - **hbrun\*|\*hbrun** rodar em modo script / "shell" interativo
 - **hbrund|hbrun\*d** rodar em modo script /"shell" interativo com debugador
 - **harbour** modo \-hbraw \(emular compilador específico Harbour "raw"\)
 - **clipper** mode \-hbcmp \(emulate Cl\*pper compiler\)
 - **rtlink** mode \-rtlink \(emulate Cl\*pper linker\)
 - **exospace** mode \-rtlink \(emulate Cl\*pper linker\)
 - **blinker** mode \-rtlink \(emulate Cl\*pper linker\)
 - **\*10** opção \-hb10
 - **\*20** opção \-hb20
 - **\*30** opção \-hb30
 - **\*32** opção \-hb32
 - **x\*** opção \-xhb
 - **hbcmp\*|\*hbcmp** modo \-hbcmp \(emular compilador Harbour produzindo um objeto binario "binary object"\)
 - **hbcc\*|\*hbcc** modo \-hbcc \(emular compilador C\)
 - **hblnk\*|\*hblnk** modo \-hblnk \(emular linkeditor C\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Arquivos:  


 - **\*\.hbp** arquivo de projeto\. Pode conter qualquer número de opções de linha de comando, que são esperados para criar uma saída\. As linhas que começam com caracter '\#' são ignoradas, caso contrário, nova linha é opcional e as opções são separados por espaço, assim como na linha de comando\. Você deve colocar as opções que contenham espaço entre aspas duplas\. Cada referência de arquivo \.hbp será executado como um sub\-projeto\.
 - **\*\.hbm** coleção de opções\. Pode ser usado coletando\-as para dentro de um arquivo e inclui\-lo dentro de arquivo de projeto\.Usar o mesmo formato dos arquivos \.hbp\.
 - **\*\.hbc** conjunto de opções que acompanham os componentes \(também conhecidos como 'libs', pacotes\)\. Usam diferentes sintaxes de linha de comado "command\-line" e arquivos \.hbp/\.hbm\. As linhas que começam com o carácter '\#' serão ignoradas, cada diretiva deve ser colocada em linhas separadas\.
 - **\*\.ch** se passado diretamente como um arquivo fonte, ele será utilizada como padrão cabeçalho adicional
 - **hbmk\.hbc** por padrão, se presente o arquivo \.hbc é o que será processado automaticamente\. Localização \(ões\) possíveis \(em ordem de prioridade\) \[\*\]: $HOME/\.harbour, /etc/harbour, &lt;hbmk2 diretório&gt;/\.\./etc/harbour, &lt;hbmk2 diretório&gt;/\.\./etc, &lt;hbmk2 diretório&gt;
 - **hbmk\.hbm** o arquivo \.hbm opcional residente no diretório de trabalho atual será processado automaticamente antes das outras opções
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** script Harbour
 - **\*\.hrb** Harbour binario portável \(Também conhecido como Harbour script pré\-compilado\)
 - **hbstart\.hb** arquivo de inicialização de script para shell interativo Harbour\. Se presente ele é executado automaticamente na inicialização do shell\. Localizações possíveis \(em ordem de precedência\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;hbmk2 diretório&gt;/\.\./etc/harbour, &lt;hbmk2 diretório&gt;/\.\./etc, &lt;hbmk2 diretório&gt;
 - **shell plugins** \.hb e \.hrb plugins para shell interativo Harbour\. Eles pode residir em \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** armazena o histórico de comando do shell interativo Harbour shell\. Voce pode desabilitar o histórico fazendo a primeira linha 'no' \(sem aspas e com nova linha\)\. Localizado em \[\*\]: $HOME/\.harbour/
 - **hb\_extension** lista de extensões a ser carregada no shell interativo Harbour\. Uma extensão por linha, a parte alem do caracter '\#' será ignorada\. Nome alternativo em MS\-DOS: hb\_ext\.ini\. localizado em \[\*\]: $HOME/\.harbour/
  
Variáveis macro:  


 - **$\{hb\_root\}** diretório de hbmk2
 - **$\{hb\_dir\}** diretório de nome de arquivo é usado em
 - **$\{hb\_dirname\}** nome do diretório raiz dos arquivos em uso
 - **$\{hb\_name\}** é usado para nomear o arquivo \(sem diretório e extensão\)
 - **$\{hb\_self\}** nome completodo arquivo que está em uso
 - **$\{hb\_curdir\}** diretório de trabalho atual
 - **$\{hb\_tempdir\}** Diretório do Sistema Operacional para arquivos temporários
 - **$\{hb\_targetname\}** nome do projeto \(sem a diretório e extensão\)\. Retorna \.adhoc\. se não existir arquivo de projeto\.
 - **$\{hb\_targettype\}** tipo do projeto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** plataforma selecionada
 - **$\{hb\_comp\}** compilador C selecionado
 - **$\{hb\_comp\_ver\}** versão do compilador C
 - **$\{hb\_build\}** nome do aplicativo "build"
 - **$\{hb\_cpu\}** CPU selecionada
 - **$\{hb\_work\}** nome padrão do diretório base de trabalho
 - **$\{hb\_workdynsub\}** subdiretório padrão de trabalho para bibliotecas dinâmicas de destino
 - **$\{hb\_dynprefix\}** prefixo de biblioteca dinâmica
 - **$\{hb\_dynsuffix\}** sufixo de biblioteca dinâmica
 - **$\{hb\_dynext\}** extensão de biblioteca dinâmica
 - **$\{hb\_ver\}** Versão do Harbour no formato de tres bytes em hexadecimal\. Exemplo: 030400
 - **$\{hb\_verstr\}** Versão do Harbour no formato legível por humanos &lt;maior&gt;\.&lt;menor&gt;\.&lt;release&gt;&lt;status&gt;\. Ex\.: 3\.4\.0dev
 - **$\{hb\_major\}** número da versão principal Harbour
 - **$\{hb\_minor\}** número da versão secundária Harbour
 - **$\{hb\_release\}** número da versão Harbour liberada
 - **$\{hb\_status\}** status da versão Harbour
 - **$\{hb\_ver\_id\}** Identificador da versão Harbour
 - **$\{hb\_revision\}** revisão Harbour
 - **$\{hb\_host\_plat\}** plataforma de hospedagem Harbour
 - **$\{hb\_host\_plat\_unix\}** retorna '1' se o Harbour estiver hospedado numa plataforma compatível com \*nix
 - **$\{hb\_bin\}** diretório de binarios Harbour
 - **$\{hb\_lib\}** diretório de bibliotécas estáticas Harbour
 - **$\{hb\_lib3rd\}** diretório Harbour de bibliotécas estáticas de terceiros
 - **$\{hb\_dyn\}** diretório de bibliotécas dinâmicas Harbour
 - **$\{hb\_inc\}** diretório Harbour para header
 - **$\{hb\_addons\}** diretório base para complementos "add\-ons" Harbour\.
 - **$\{hb\_first\}** nome do arquivo do fonte que detém a função de entrada \(sem diretório e extensão\)
 - **$\{hb\_outputdir\}** diretório da saída
 - **$\{hb\_outputname\}** nome de saída \(sem a extensão\)
 - **$\{hb\_level\}** nível de recursão do sub\-projeto
 - **$\{&lt;depname&gt;\}** retorna o diretório header da dependência &lt;depname&gt;, ou ´1´se ela não foi detectada\.
 - **$\{&lt;envvar&gt;\}** retorna os valores das variáveis de ambiente &lt;envvar&gt;
  
Filtros \(você pode combinar e/ou negá\-los\):  


 - **\{&lt;platform&gt;\}** platforma alvo\. Onde &lt;platform&gt; pode ser qualquer valor aceito pela opção \-plat=\.
 - **\{&lt;compiler&gt;\}** Compilador alvo C \. Onde &lt;compiler&gt; pode ser qualquer valor aceito pela opção \-comp=\.
 - **\{&lt;cpu&gt;\}** CPU alvo\. Onde &lt;cpu&gt; pode ser qualquer uma dessas: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** tipo de construção alvo\. Onde &lt;targettype&gt; é qualquer um dos valores retornados por macro variável $\{hb\_targettype\}\.
 - **\{&lt;package\-manager&gt;\}** gerenciador de pacote\.Onde&lt;package\-manager&gt; pode ser qualquer um de: deb, rpm, portage, homebrew, rudix, macports, fink, pkg, cygwin
 - **\{mt\}** construção alvo é "multi\-threaded" \(veja opção \-mt\)
 - **\{st\}** construção o alvo é "single\-threaded" \(veja opção \-st\)
 - **\{gui\}** GUI alvo \(veja opção \-gui\)
 - **\{std\}** alvo console \(veja opção \-console\)
 - **\{debug\}** Debugador nivel C está habilitado \(veja a opção \-debug\)
 - **\{nodebug\}** Debugador nivel C está disabilitado \(veja a opção \-debug\)
 - **\{shared\}** construção compartilhada \(veja \-shared e opções relativas\)
 - **\{static\}** construção compartilhada \(veja \-static e opções relativas\)
 - **\{lngcpp\}** Forçado modo C\+\+ \(veja opção \-cpp\)
 - **\{lngc\}** forçado modo C \(veja opção \-cpp\-\)
 - **\{winuni\}** Modo Windows UNICODE \(WIDE\) \(veja opção \-winuni\)
 - **\{winansi\}** modo Windows ANSI \(veja opção \-winuni\-\)
 - **\{unix\}** plataforma alvo é compatível com \*nix \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** plataforma alvo é compátivel com Windows \(win, wce\)
 - **\{allgcc\}** o compilador C alvo pertence a família gcc\.\(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** compilador alvo C é mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** compilador C alvo é msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** compilador C alvo é bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** compilador alvo C é pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** compilador alvo C é icc\* \(icc, iccia64\)
 - **\{hb10\}** habilita modo de compatibilidade 'Harbour 1\.0\.x' \(veja opção \-hb10\)
 - **\{hb20\}** habilita modo de compatibilidade 'Harbour 2\.0\.x' \(veja opção \-hb20\)
 - **\{hb30\}** habilita modo de compatibilidade 'Harbour 3\.0\.x' \(veja opção \-hb30\)
 - **\{hb32\}** Harbour 3\.2\.0dev compatibility mode \(see \-hb32 option\)
 - **\{xhb\}** modo xhb \(veja opção \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** passará pelo filtro se o nome do arquivo &lt;file&gt; ou &lt;dir&gt; diretório existir no disco\.
 - **\{MACRO\}** passará pelo filtro se o valor $\{MACRO\} não for vazio e não for igual a zero '0' ou 'no' \(insensível a maiúscula e minúscula "case insensitive"\)
 - **\{MACRO='&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} for igual a &lt;value&gt; \(insensível a maiúscula e minúscula "case insensitive"\)
 - **\{MACRO&gt;'&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} for maio que o &lt;value&gt; \(insensível a maiúscula e minúscula "case insensitive"\)
 - **\{MACRO&lt;'&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} menor que &lt;value&gt; \(insensível a maiúscula e minúscula "case insensitive"\)


Constantes pré\-definidas nos fontes \(não defini\-las manualmente\):


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** quando um script \.hb é compilado como hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** quando um arquivo \.hbx está presente em um projeto \(disponível nos fontes do Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** quando o pacote &lt;hbcname&gt;\.hbc é linkado a construção alvo\. O valor "version=" é igual ao da versão do arquivo \.hbc, convertido para numero decimal que é '1', se não especificado\. \(disponível nos fontes do Harbour\)
 - **HBMK\_HAS\_&lt;depname&gt;** quando dependência &lt;depname&gt; foi detectada \(disponível nos fontes C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** quando um programa fonte Harbour está rodando como "shell script"
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.


Constantes predefinidas em arquivos de construção \(eles estão disponíveis depois '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\) \(não defini\-las manualmente\):


 - **HBMK\_HAS\_&lt;depname&gt;** quando &lt;depname&gt; dependência foi detectada
 - **HBMK\_DIR\_&lt;depname&gt;** retorna o diretório header onde &lt;depname&gt; foi detectada, ou vazio se não\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** quando dependência &lt;depname&gt; foi detectada em um local configurado pela opção \-depincpathlocal
  
Variáveis ​​de ambiente:  


 - **HBMK\_OPTIONS** aceita todas as opções como se estivessem passado no início da linha de comando
 - **HB\_PLATFORM** aceita os mesmos valores que a opção \-plat=
 - **HB\_COMPILER** aceita os mesmos valores que a opção \-comp=
 - **HB\_CPU** aceita os mesmos valores que a opção \-cpu=
 - **HB\_BUILD\_NAME** aceita os mesmos valores que a opção \-build=
 - **HB\_LANG** aceita os mesmos valores que a opção \-lang=
 - **HB\_USER\_LIBS** aceita os mesmos valores \(separados por espaços\) como opção \-l
 - **HB\_USER\_LIBPATHS** aceita os mesmos valores \(separados por espaços\) como opção \-L
 - **HB\_USER\_PRGFLAGS** opções a serem passados ​​para o compilador Harbour \(antes da linha de comando\)
 - **HB\_USER\_CFLAGS** opções a serem passados ​​para o compilador C \(antes da linha de comando\)
 - **HB\_USER\_RESFLAGS** opções a serem passadas ​​para o compilador de recursos \(antes da linha de comando\) \(somente Windows\)
 - **HB\_USER\_LDFLAGS** opções a serem passados ​​para linkar o \(executável\) \(antes das opções da linha de comando\)
 - **HB\_USER\_DFLAGS** opções a serem passados ​​para o linkar \(biblioteca dinâmica\) \(antes das opções da linha de comando\)
 - **HB\_USER\_AFLAGS** opções a serem passados ​​para o linkar \(biblioteca estática\) \(antes das opções da linha de comando\)
 - **HB\_CCPATH** sobrepor o diretório de execução do compilador C \(apenas para família de compilador gcc\)
 - **HB\_CCPREFIX** sobrepor o prefixo do executável do compilador C \(apenas para família de compilador gcc\)
 - **HB\_CCSUFFIX** sobrepor o sufixo executável do compilador C \(apenas para família de compilador gcc\)
 - **HB\_INSTALL\_PREFIX** sobrepor o diretório base de instalação Harbour
 - **HB\_INSTALL\_ADDONS** sobrepor o diretório base de instalação aducionais Harbour


 - **HB\_EXTENSION** lista de extensões para carga no shell interativo do Harbour separada por espaço
  
Diretivas \.hbc \(devem ser escritas em linhas separadas\):  


 - **echo=&lt;msg&gt;** Exibir &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** pular o processamento do resto do arquivo \.hbc\. Mostrar &lt;msg&gt;, se especificado\.
 - **stop=\[&lt;msg&gt;\]** parar a construção\. Mostrar &lt;msg&gt;, se especificado\.
 - **sources=** adicionar lista de arquivos de entrada separada por espaços
 - **headers=** adicionar lista de arquivos "\.ch" tipo "headers" separada por espaços como padrão de header
 - **libs=** adicionar lista de bibliotecas separada por espaços \(veja mais opções em \-l\)
 - **frameworks=** adicionar lista de "frameworks" separada por espaços \(somente para Darwin\)
 - **requests=** adicionar lista de símbolos separada por espaços para forçar a linkagem do programa
 - **syslibs=** adicionar lista de bibliotecas separada por espaços como bibliotecas do sistema \(antes bibliotecas regulares\)
 - **hbcs=** incorporar lista separada por espaços de arquivos \.hbc\. São aceitos nomes sem a extensão\. Estas referências são processados ​​no local\.
 - **autohbcs=** lista separada por espaço de opções de valores de entrada \-autohbc=
 - **libpaths=** lista dos "paths" adicionais de bibliotecas separada por espaços
 - **incpaths=** adicionar lista dos locais adicionais dos "header" separada por espaços \(para ambos Harbour e C\)
 - **instfiles=** lista separada por espaço de opções de valores de entrada \-instfile=
 - **instpaths=** lista separada por espaço de opções de valores de entrada \-instpath=
 - **prgflags=** lista separada por espaço de opções de valores de entrada \-prgflag=
 - **cflags=** lista separada por espaço de opções de valores de entrada \-cflag=
 - **resflags=** lista separada por espaço de opções de valores de entrada \-resflag=
 - **ldflags=** lista separada por espaço de opções de valores de entrada \-ldflag=
 - **ldflags\+=** lista separada por espaço de opções de valores de entrada \-ldflag\+=
 - **dflags=** lista separada por espaço de opções de valores de entrada \-dflag=
 - **dflags\+=** lista separada por espaço de opções de valores de entrada \-dflag\+=
 - **pflags=** lista separada por espaço de opções de valores de entrada \-pflag=
 - **psources=** lista separada por espaço de opções de valores de entrada \-pi=
 - **gui=&lt;bool&gt;** opções 'yes' = \-gui, 'no' = \-std
 - **mt=&lt;bool&gt;** opções 'yes' = \-mt, 'no' = \-st
 - **pic=&lt;bool&gt;** opções 'yes' = \-pic, 'no' = \-pic\-
 - **shared=&lt;bool&gt;** opções 'yes' = \-shared, 'no' = \-static
 - **shareddef=&lt;bool&gt;** semelhante ao shared=, mas funciona apenas se o modo shared/static não foi definido antes
 - **fullstatic=&lt;bool&gt;** opções 'yes' = \-fullstatic, 'no' = \-static
 - **debug=&lt;bool&gt;** opções 'yes' = \-debug, 'no' = \-debug\-
 - **optim=** opções 'yes' = \-optim, 'no' = \-optim\-
 - **nulrdd=&lt;bool&gt;** opções 'yes' = \-nulrdd, 'no' = \-nulrdd\-
 - **nodefgt=&lt;bool&gt;** opções 'yes' = \-nodefgt, 'no' = \-nodefgt\-
 - **map=&lt;bool&gt;** opções 'yes' = \-map, 'no' = \-map\-
 - **hbcppmm=&lt;bool&gt;** opções 'yes' = \-hbcpmm, 'no' = \-hbcpmm\-
 - **implib=&lt;bool&gt;** opções 'yes' = \-implib, 'no' = \-implib\-
 - **winuni=&lt;bool&gt;** opções 'yes' = \-winuni, 'no' = \-winuni\-
 - **strip=&lt;bool&gt;** opções 'yes' = \-strip, 'no' = \-strip\-
 - **run=&lt;bool&gt;** opções 'yes' = \-run, 'no' = \-run\-
 - **inc=&lt;bool&gt;** opções 'yes' = \-inc, 'no' = \-inc\-
 - **harden=&lt;bool&gt;** opções 'yes' = \-harden, 'no' = \-harden\-
 - **cpp=** o mesmo para \-cpp= opções
 - **warn=** o mesmo para \-warn= opções
 - **compr=** o mesmo para \-compr= opções
 - **head=** o mesmo para \-head= opções
 - **plugins=** lista separada por espaço de hbmk2 plugins para carga
 - **gt=&lt;name&gt;** o mesmo para \-gt&lt;name&gt; opções
 - **gtdef=&lt;name&gt;** definir o GT padrão a ser usado
 - **env=** o mesmo para \-env: opções
 - **depurlbase=** o mesmo para \-depurlbase= opções
 - **deppkgname=** o mesmo para \-deppkgname= opções
 - **depkeyhead=** o mesmo para \-depkeyhead= opções
 - **depoptional=** o mesmo para \-depoptional= opções
 - **depcontrol=** o mesmo para \-depcontrol= opções
 - **depincroot=** o mesmo para \-depincroot= opções
 - **depincpath=** o mesmo para \-depincpath= opções
 - **depincpathlocal=** o mesmo para \-depincpathlocal= opções
 - **depimplibs=** o mesmo para \-depimplibs= opções
 - **depimplibd=** o mesmo para \-depimplibd= opções
 - **depfinish=** o mesmo para \-depfinish= opções
 - **signts=** o mesmo para \-signts= opções
 - **name=** Nome do Pacote
 - **description=** descrição do pacote
 - **version=&lt;x\.y\.z&gt;** número da versão do pacote, onde x,y,z &gt;= 0 &lt;= 255\. O padrão é 0\.0\.1, se não especificado\.
 - **keywords=** lista separada por espaço de palavras\-chave
 - **licences=** lista separada por espaço de licenças
 - **repository=** lista dos repositório de referências dos fontes separada por espaço


Plugin API:  
\('hbmk' é a variável de contexto recebida pela função de entrada do plugin\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unrecognized file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Adicionar um arquivo de entrada Harbour ao projeto\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Adicionar um arquivo de entrada C ao projeto\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Adicionar um arquivo de entrada C\+\+ ao projeto\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Adicionar um arquivo de entrada "Windows resource" ao projeto\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Adicionar um arquivo objeto binário ao projeto\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
adicione um arquivo para ser instalado, com o opcional \-instpath= nome do grupo\.
 - **hbmk\_AddOption\_PRG\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Adicionar uma opção do compilador Harbour\.
 - **hbmk\_AddOption\_C\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Adicionar uma opção do compilador C\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Texto de saída para stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Texto de saída para stderr\.
 - **hbmk\_OutStdRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Texto de saída para stdout sem formatação\.
 - **hbmk\_OutErrRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Texto de saída para stderr sem formatação\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Avaliar expressão macro hbmk2 \.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
utilizar "Escape/quote" no nome de arquivo para usá\-lo como parâmetro de comando externo\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Converte o nome do arquivo para o formato requerido pelo plataforma/compilador C
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Converter nome do arquivo para ter barra separadora de diretório\.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
retorna o caminho relativo de \-workdir= valor do diretório atual de trabalho\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Encontrar o arquivo na &lt;xPath&gt; \(matriz ou string delimitada pelo separado de path são aceitas\) com a lista de &lt;aExtDef&gt; extensões alternativas \(padrão para executavéis binários\)\. Retorna o nome do arquivo se encontrado caso contrario retorna NIL\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
mudar diretório e/ou extensão do nome do arquivo
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
codificar os nomes de função de acordo com o compilador Harbour formatando os nomes das funçoes em código C "HB\_FUNC\(\)" de acordo as regras\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Remover aspas duplas de uma string\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Converte matriz de strings para uma string\. O separador padão é um espaço simples\.


Váriavés Plugin:  
\('hbmk' itens de contexto de hash, case\-sensitive, apenas leitura, a menos que indicado o contrário\)


 - **"apiver"** versão da API como um número inteiro
 - **"cSTATE"** estado do retorno\. Pode ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** passar uma matriz de parâmetros para os plugins através das opções \-pflag=/pi= ou tendo registrado as extensões via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** variáveis "hash de plugin" ​​personalizadas\. Gravável, local para cada plugin
 - **"cPLAT"** \-plat valor
 - **"cCOMP"** \-comp valor
 - **"nCOMPVer"** detected compiler version in &lt;MMmm&gt; format
 - **"cCPU"** \-cpu valor
 - **"cBUILD"** \-build= valor
 - **"cOUTPUTNAME"** \-o valor
 - **"cTARGETNAME"** veja $\{hb\_targetname\} macro
 - **"cTARGETTYPE"** veja $\{hb\_targettype\} macro
 - **"lREBUILD"** \-rebuild opções de status
 - **"lCLEAN"** \-clean opções de status
 - **"lDEBUG"** \-debug opções de status
 - **"lMAP"** \-map opções de status
 - **"lSTRIP"** \-strip opções de status
 - **"lDONTEXEC"** \-traceonly opções de status
 - **"lIGNOREERROR"** \-ignore opções de status
 - **"lTRACE"** \-trace opções de status
 - **"lQUIET"** \-q opções de status
 - **"lINFO"** \-info opções de status
 - **"lBEEP"** \-beep opções de status
 - **"lRUN"** \-run opções de status
 - **"lINC"** \-inc opções de status
 - **"cCCPATH"** veja HB\_CCPATH envvar
 - **"cCCPREFIX"** veja HB\_CCPREFIX envvar
 - **"cCCSUFFIX"** veja HB\_CCSUFFIX envvar
 - **"cCCEXT"** veja HB\_CCEXT envvar
 - **"cWorkDir"** \-workdir= valor
 - **"nExitCode"** Código de saída atual
  
Shell API disponível nos scripts em Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Mudar GT\. Padrão \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
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


Exemplos para iniciar com hbmk2:


 - **Para rodar o "shell" interativo \(interpretador de comandos\)**  
$ hbmk2 \.
 - **Para executar um script Harbour**  
$ hbmk2 myscript\.hb \[&lt;parâmetro\[s\]&gt;\]


Exemplo para gerar e rodar Harbour binario portável \(Também conhecida como Harbour script pré\-compilado\)


 - **Para gerar**  
$ hbmk2 \-gh myscript\.hb
 - **Para executar resultado acima**  
$ hbmk2 myscript\.hrb \[&lt;parâmetro\[s\]&gt;\]


Exemplos para gerar uma aplicação Harbour:


 - **Para criar um simples \.prg**  
$ hbmk2 hello\.prg
 - **para gerar multiplos fontes \.prg dentro de uma aplicação em modo incremental**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Para gerar uma aplicação usando um arquivo de projeto**  
$ hbmk2 myapp\.hbp
 - **Para gerar uma aplicação usando o modo incremental**  
$ hbmk2 myapp\.hbp \-inc
 - **Para gerar uma aplicação que usa os pacotes da contribuinte ou pacotes de terceiros \(aplicações adicionais\) que venha incorporado um arquivo \.hbc**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Para gerar uma aplicação que utiliza uma biblioteca específica "raw"**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Para gerar uma aplicação que utiliza recursos do Windows**  
$ hbmk2 mymain\.prg myres\.rc
 - **Para gerar uma aplicação sem linkar bibliotecas dinâmicas Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Para gerar uma aplicação de todos os fontes \.prg e \.c residentes no subduretório**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Exemplos para gerar uma biblioteca estática Harbour:


 - **Para gerar a biblioteca 'mylib' dos fontes**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Para gerar a biblioteca 'mylib' dos fontes usando o modo incremental**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Códigos de saída \("errorlevels"\):  


 - **0** sem erros
 - **1** unrecognized platform
 - **2** unrecognized compiler
 - **3** falha Harbour não detectado
 - **5** criação stub falhou
 - **6** falha em fase de compilação
 - **7** falha na montagem final \(linkeditor ou gerenciador de bibliotecas\)
 - **8** não suportado
 - **9** Falhou na criação do diretório de trabalho
 - **19** ajuda
 - **10** dependência não encontrada ou desativada
 - **20** inicialização de plugin
 - **30** aninhamento muito profundo
 - **50** parada solícitada
 - **&lt;outros&gt;** quando a opção \-run for usada, o código de saida será o código devolvido pelo executável de destino
  
Notas:  


  - &lt;script&gt; pode ser:  
&lt;@script&gt; ou &lt;script\.hbm&gt;: arquivo com opções de linha de comando  
&lt;script\.hbp&gt;: arquivo com opções de linha de comando, marca também uma nova compilação alvo se especificado na linha de comando  
&lt;script\.hbc&gt;: arquivo de configuração de pacote
  - se existir um nome de arquivo projeto \.hbp no diretório atual este será carregado como fonte de origem\. Caso contrário, o arquivo o com extensão \.prg extension será usado\.
  - Multiplos parâmetros \-l, \-L, \-i e &lt;script&gt; são aceitos\.
  - Opções usadas com o compilador Harbour também são aceitas\.  
\(veja com a opção \-harbourhelp\)
  - O arquivo de configuração hbmk\.hbc no diretório do hbmk2 sempre será processado caso exista\. Em plataformas \*nix este arquivo é sempre procurado nas pastas ~/harbour, /etc/\.harbour, &lt;base&gt;/etc/\.harbour, &lt;base&gt;/etc \(nesta ordem\) antes do diretório hbmk2\.
  - O script hbmk\.hbm no diretório atual será sempre processado se existir\.
  - Usar a opção de barras como separador de diretório é recomendado\.mas barras invertidas também são igualmente aceitas\.
  - Os filtros são aceitos para cada uma das linhas de um arquivo \.hbc e possuem diversas opções\.  
Filtros podem ser combinados usando os operadores '&amp;' \(e\), '|' \(ou\), negados pelo operador y '\!' e agrupados por parenteses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - A maioria das linhas de um arquivo \.hbc \(libs =, HBCS =, prgflags =, cflags =, ldflags =, libpaths =, instfiles =, instpaths =, echo =\) e os parâmetros correspondentes de linha de comando aceitarão variáveis ​​macro\. libpaths = também aceita%\{hb\_name\} que se transforama no nome do arquivo\. hbc sob pesquisa\.
  - Também aceita opções de macros substituição de comando\. Neste caso basta inserir o comando dentro de \`\`, e, se o comando contiver espaço, também adicione aspas duplas\. Ex: "\-cflag=\`wx\-config \-\-cflags\`", ou ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - Quando vários tipos de opções de seleção \(\-hblib, \-hbdyn, etc\.\) são especificados na compilação, o primeiro será o mais significativo o resto será ignorado silenciosamente\.
  - Bibliotecas e arquivos de objetos construídos com/para CA\-Cl\*pper não irá funcionar com qualquer plataforma /compilador suportada\.
  - Padrões e especificações suportadas podem variar de acordo com plataforma/compilador\.
  - Para executar o hbmk2 não é necessário qualquer ferrramenta "make" do compilador C, GNU Make e MSYS \(no Windows\)\.
  - '\.' \(ponto\) passado como primeiro parâmetro entrará no shell interativo Harbour\.


  - \.hb, \.hrb ou \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Valores marcados com \[\*\] pode ser plataforma hospedagem e/ou configuração dependente\. Esta ajuda foi gerada em 'darwin' plataforma de hospedagem\.


Valores suportados para &lt;compiler&gt; conforme a &lt;platform&gt; disponível:


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
