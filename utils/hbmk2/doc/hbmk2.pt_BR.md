Harbour Make \(hbmk2\) 3\.2\.0dev \(r2013\-04\-03 03:33\)  
Copyright \(c\) 1999\-2013, Viktor Szakáts  
<http://harbour\-project\.org/>  
Translation \(pt\_BR\): Vailton Renato &lt;vailtom@gmail\.com&gt;  

Sintaxe:  
  
  hbmk2 \[opções\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Descrições:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will autodetect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Opções  


 - **\-o&lt;outname&gt;** nome de arquivo de saída
 - **\-l&lt;libname&gt;** linkar com a biblioteca &lt;libname&gt;\. &lt;libname&gt; deve estar sem path, extensão e prefixo 'lib' \(menos a parte do nome\)\. Não adicione bibliotecas do núcleo Harbour, elas são automaticamente inseridas quando necessário\. Se &lt;libname&gt; inicializar com um caracter '\-', a biblioteca será removida da lista de bibiotecas na hora de linkar\.
 - **\-L&lt;libpath&gt;** path adicional para pesquisar por bibliotecas
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** paths adicionais para pesquisa de arquivos headers
 - **\-static|\-shared** linkar com biliotecas estáticas/compartilhadas
 - **\-gt&lt;name&gt;** linkar com GT&lt;name&gt; GT driver, a linkagem pode ser repetido com mais GTs\. O primeiro será o GT padrão na execuçao do programa
 - **\-inc\[\-\]** habilita/desabilitar o modo de compilação incremental \(padrão: desbilitado\)
 - **\-hbexe** criar um executável \(padrão\)
 - **\-hblib** criar biblioteca estática
 - **\-hbdyn** criar biblioteca dinâmica \(não ligada com a Harbour VM\)
 - **\-hbdynvm** criar biblioteca dinâmica \(ligada com a Harbour VM\)


 - **\-mt|\-st** linkar com suporte multi ou single\-thread na HVM
 - **\-gui|\-std** criar um executável GUI/console
 - **\-main=&lt;mainfunc&gt;** sobrepor o nome inicial das função/procedures
 - **\-request=&lt;func&gt;** forçar a linkagem da função/procedure
 - **\-fullstatic** linkar com todas as bibliotecas estáticas
 - **\-pic\[\-\]** criar posição independente do código objeto \(sempre habilitado no modo \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** criar binário que faça uso da biblioteca compartilhada do Harbour com referência absoluta \(ou não\) \(padrão: 'fullshared' quando Harbour é instalado no local do sistema, caso contrário será 'fixshared'\) \(fix/full é somente para \*nix\)
 - **\-nulrdd\[\-\]** linkar com o nulrdd
 - **\-debug\[\-\]** adicionar/excluir informações de debug do compilador C\. Para compilar com Harbour utilize a opção \-b como de costume\.
 - **\-optim\[\-\]** alterna as optimizações do compilador C \(ativado por padrão\)
 - **\-cpp\[\-\]** forçar compilação em modo C\+\+/C
 - **\-cpp=&lt;value&gt;** selecione modo C\+\+\.Os Valores permitidos são: def, yes, no
 - **\-map\[\-\]** criar \(ou não\) o arquivo map
 - **\-implib\[\-\]** Criar \(ou não\) uma biblioteca importação \(no modo \-hbdyn/\-hbexe\)\. O nome terá um sufixo adicionado\.
 - **\-implib=&lt;output&gt;** Criar biblioteca importação \(no modo \-hbdyn/\-hbexe\) nomeado para &lt;output&gt; \(padrão: com o mesmo\)
 - **\-ln=&lt;link&gt;** criar um link simbólico apontando para &lt;output&gt; \(&lt;link&gt; é considerado em relação ao &lt;output&gt;\)
 - **\-strip\[\-\]** strip \(ou não\) arquivos binários "strip reduz o tamanho do binário gerado"
 - **\-trace\[\-\]** exibir os comandos executados
 - **\-beep\[\-\]** ativa \(ou desativa\) um beep simples em caso de sucesso ou um beep duplo em caso de erro
 - **\-ignore\[\-\]** ignore erros quando executar as ferramentas de compilação \(padrão: off\)
 - **\-hbcppmm\[\-\]** sobrepor o as funções padrão C\+\+ de gerenciamento de memoria pelas do Harbour
 - **\-winuni\[\-\]** selecione o modo de compilação entre UNICODE \(WIDE\) e ANSI \(padrão: ANSI\) \(somente Windows\. Para WinCE está sempre configurado para UNICODE\)
 - **\-nohblib\[\-\]** não usar bibliotecas estáticas do núcleo do Harbour quando linkar
 - **\-nodefgt\[\-\]** Não link GTs padrão \(o modo \-static é mais eficaz\)
 - **\-nolibgrouping\[\-\]** desativar agrupamento de LIBs em compiladores baseados no gcc\.
 - **\-nomiscsyslib\[\-\]** não adicione bibliotecas extras do sistema à lista padrão de bibliotecas
 - **\-traceonly** exibir os comandos à serem executados, mas não execute\-os
 - **\-warn=&lt;level&gt;** define o nível de aviso do compilador C  
&lt;level&gt; pode ser: max, yes, low, no, def \(padrão: yes\)
 - **\-safe\[\-\]** habilitar as opções de segurança no compilador/linker C \(padrão: habilitado no Windows, desabilitado nos outros sistemas\)
 - **\-compr=&lt;level&gt;** comprimir executável/lib dinamica \(precisa programa UPX\)  
&lt;level&gt; pode ser: yes, no, min, max
 - **\-run\[\-\]** executar/não executar o aplicativo gerado\.
 - **\-vcshead=&lt;file&gt;** gerar arquivo de cabeçalho "\. ch" com informações do repositório local\. Git, SVN, Mercurial, Bazaar, Fossil, CVS e Monotone são suportados atualmente\. O cabeçalho gerado irá definir a constante \_HBMK\_VCS\_TYPE\_ no pré\-processador com o nome detectados de VCS e \_HBMK\_VCS\_ID\_ com o ID único do repositório local\. Se nenhum sistema VCS é detectado, um número seqüencial será lançado automaticamente em cada construção\.  
VCS \- sistema de controle de versão\.
 - **\-tshead=&lt;file&gt;** gerar cabeçalho \.ch com informação de data/hora\. Cabeçalho gerado conterá as macros \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ com a data/hora de criação do arquivo\.
 - **\-icon=&lt;file&gt;** colocar o arquivo &lt;file&gt; como ícone de aplicação\. O arquivo &lt;file&gt; deve estar em um formato suportado na plataforma alvo \(não suportado por algumas plataformas/compiladores\)\. No Windows, isso é implementado gerando e linkando um arquivo de recursos\.
 - **\-manifest=&lt;file&gt;** incorporar arquivo manifest &lt;file&gt; no executável / lib dinâmica \(somente para Windows\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both autodetected\.
 - **\-signpw=&lt;pw&gt;** usar &lt;pw&gt; como senha ao assinar executável \(somente Windows e Darwin\)
 - **\-instfile=&lt;g:file&gt;** adicionar &lt;file&gt; para a lista de arquivos a serem copiados para caminho especificado pela opção \-instpath\. &lt;g&gt; é um grupo de cópias opcional \(case sensitive\), deve ser de pelo menos dois caracteres\.No caso de você não especificar &lt;file&gt;, a lista de arquivos naquele grupo será esvaziado\.
 - **\-instpath=&lt;g:path&gt;** copy target file\(s\) to &lt;path&gt;\. if &lt;path&gt; is a directory, it should end with path separator, in this case files specified by \-instfile option will also be copied\. can be specified multiple times\. &lt;g&gt; is an optional copy group, it must be at least two characters long\. Build target will be automatically copied to default \(empty\) copy group\. There exist following built\-in &lt;g&gt; groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source \(\.dll\) files, both belonging to dependencies\.
 - **\-instforce\[\-\]** copie os arquivo\(s\) para o destino do caminho de instalação mesmo que já atualizados
 - **\-depimplib\[\-\]** habilitar \(ou desabilitar\) a importação da biblioteca da fontes de bibliotecas especificadas em \-depimplibs= opções \(padrão: yes\)
 - **\-stop\[=&lt;text&gt;\]** interromper sem executar nada
 - **\-echo=&lt;text&gt;** ecoa texto na tela
 - **\-pause** forçar pause por uma tela em caso de erro \(somente com driver GT alternativo\)
 - **\-exitstr** Exibir erros na forma de texto amigável "human readable"
 - **\-info** ativar mensagens informativas
 - **\-quiet\[\-\]** suprimir todas as mensagens


 - **\-bldf\[\-\]** herdar flags do Harbour: todos/nenhum \(padrão\)
 - **\-bldf=\[p\]\[c\]\[l\]** herdar todos os flags \.prg/\.c/linker \(ou nenhum\) provindos do Harbour
 - **\-F&lt;framework&gt;** linkar com &lt;framework&gt; framework \(somente Darwin\)
 - **\-prgflag=&lt;f&gt;** especifica flags para o Harbour
 - **\-cflag=&lt;f&gt;** especifica flags para o compilador C
 - **\-resflag=&lt;f&gt;** especifica flags para o compilador de recursos \(apenas windows\)
 - **\-ldflag=&lt;f&gt;** especifica flags para o linkeditor \(executável\)
 - **\-dflag=&lt;f&gt;** informar flags para o linkeditor \(biblioteca dinânica\)
 - **\-aflag=&lt;f&gt;** passa flag para o linkeditor \(lib estática\)
 - **\-iflag=&lt;f&gt;** passar um unico "flag" para criar o comando de impotação de bibliotecas
 - **\-signflag=&lt;f&gt;** passar um único "flag" para criar o comando "code sign"
 - **\-runflag=&lt;f&gt;** argumentos à serem passados ao executável gerado quando \-run for utilizado
 - **\-cflag\+=&lt;f&gt;** passa umúnico flag para o compilador C substituindo os flags adiconados porele mesmo hbmk2\. Use com cuidado\.
 - **\-ldflag\+=&lt;f&gt;** passar uma unica opção "raw" para linkar \(executável\), após a lista da biblioteca\. Use com cuidado\.
 - **\-dflag\+=&lt;f&gt;** passar uma unica opção "raw" para linkar \(biblioteca dinâmica\), após a lista da biblioteca\. Use com cuidado\.
 - **\-3rd=&lt;f&gt;** "options/flags" reservado para ferramentas de terceiros, sempre ignorado por hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** alterar as variáveis locais de ambiente\. &lt;e&gt;é o nome da variável a ser alterada\. &lt;o&gt; pode ser '=' para definir/sobrepor, '\-' para apagar, '\+' to adicionar ao fim do valor existente, '\#' para inserir o valor inicialização da variável\. &lt;v&gt; é o valor a ser definido/adicionardo/inserido "set/append/insert"\.
 - **\-jobs=&lt;n&gt;** dispara &lt;n&gt; threads de compilação \(apenas plataformas multiprocessadas\)
 - **\-head=&lt;m&gt;** controle de análise de fonte header \(em modo de construção incremental\)  
&lt;m&gt; Pode ser: native \(usa o compilador para extrair dependências\), full \(padrão, usa o analisador de texto simples em todo o arquivo\), dep, off
 - **\-rebuild** recriar \(em modo incremental\)
 - **\-rebuildall** recriar com os sub\-projetos \(em modo incremental\)
 - **\-clean** compilação limpa \(em modo de compilação incremental\)
 - **\-workdir=&lt;dir&gt;** diretório de trabalho  
\(padrão: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] no modo incremental, outra forma diretório temporário do sistema operacional "OS temp directory"\)


 - **\-hbcontainer** virtual build target, it does not create anything\. Useful for creating an \.hbp with the sole purpose of referencing sub\-projects
 - **\-hbimplib** Criar Bibliotecas de importação \(só para Windows\)


 - **\-hbl\[=&lt;output&gt;\]** nome\-de\-arquivo \.hbl resultante\. A macro %\{hb\_lng\} é aceita no nome\-de\-arquivo\.
 - **\-lng=&lt;languages&gt;** lista de idiomas à serem substituidos nas macros %\{hb\_lng\} nos arquivos \.pot/\.po e nos nomes de arquivos de saída \.hbl/\.po\. Lista separada por vírgula:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** criar/atualizar arquivo \.po à partir dos fontes\. Se um arquivo \.po com o mesmo nome existir, o arquivo será mesclado\.
 - **\-minipo\[\-\]** adicionar \(ou não\) a referência do número da versão do Harbour e o arquivo de origem ao \.po \(ativo por padrão\)
 - **\-rebuildpo** recria o arquivo \.po removendo assim todas as entradas obsoletas no mesmo


 - **\-hbx=\[&lt;\.ch&gt;\]** Criar header Harbour \(no formato \.hbx\) com todos os símbolos externos\. Parâmetros vazios irão desativá\-lo\.
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt;é um nome de arquivo header \. &lt;\.hbc&gt; é um nome de arquivo \.hbc que será automaticamente incluído caso o header seja encontrado em algum dos fonte compilados\. \(EXPERIMENTAL\)


 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; é o nome da dependência\. &lt;n&gt; é o nome do pacote da depêndencia\. Pode ser especificado varias vezes\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; é o nome da dependência\. &lt;h&gt; é a key header \(\.h\) do pacote de dependência\. Múltiplas alternativas de headers podem ser especificadas\.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; é o nome da dependência\. &lt;f&gt; pode ser 'yes' or 'no', especifica se a dependência é opcional\.  
Padrão: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; is the name of the dependency\. &lt;v&gt; is a value that controls how detection is done\. Accepted values: no, yes, force, nolocal, local\. Default: content of environment variable HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; é o nome da dependência\. Definir &lt;r&gt; como diretório raiz para os caminhos especificados nas opções \-depincpath \.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; é o nome da dependêcia\. Adicione &lt;i&gt; para detectar o cabeçalho na lista de caminhos\.
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; é o nome da dependêcia\. Adicione &lt;i&gt; para detectar o cabeçalho na lista de caminhos, onde &lt;i&gt; aponta para o diretorio local do projeto e contendo um incorporado \(Também conhecida como 'locally hosted' dependência\)\.
 - **\-depimplibs=&lt;d:dll&gt;** &lt;d&gt; é o nome da dependência\. Adicione &lt;dll&gt; para importar a biblioteca na lista de fontes\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; é o nome da dependência\. Coloque no nome da biblioteca de importação para &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; é o nome da dependência\. Fecha as definição de dependência e faça a detecção de dependência real, configure todos os filtros pré\-definidos, macro variáveis e adeque as opções de construção\. Opcional, se for omitido, a detecção será realizada após o processamento de todas as opções\.


 - **\-plugin=&lt;filename&gt;** adicionar plugin\. &lt;filename&gt; pode ser: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** passar arquivo de entrada paraplugins
 - **\-pflag=&lt;f&gt;** passar um unico "flag" paraplugins
  
Opções abaixo estão disponíveis em linha de comando:  


 - **\-target=&lt;script&gt;** specify a new build target\. &lt;script&gt; can be \.prg \(or no extension\) or \.hbp file\. Note that \.hbp files are automatically considered as separate build targets\.


 - **\-hbrun** executar programa gerado\.
 - **\-hbraw** interromper após executar o compilador Harbour
 - **\-hbcmp|\-clipper** interromper após criar os arquivos objetos  
criar um link ou copiar o hbmk2 para hbcmp/clipper resultará no mesmo efeito
 - **\-hbcc** aceita raw C flags  
cria link/copia hbmk2 para hbcc para o mesmo efeito
 - **\-hblnk** parâmetros específicos do linkeditor\.
 - **\-autohbm\[\-\]** habilita \(ou desabilita\) o processamento de hbmk\.hbm no diretório atual \(padrãot: yes\)
 - **\-hb10** habilita modo de compatibilidade 'Harbour 1\.0\.x'
 - **\-hb20** habilita modo de compatibilidade 'Harbour 2\.0\.x'
 - **\-hb30** habilita modo de compatibilidade 'Harbour 3\.0\.x'
 - **\-xhb** habilitar modo xHb
 - **\-hbc** ativa modo C puro
 - \-rtlink 
 - \-blinker 
 - **\-exospace** emula o comportamento de um linkeditor compatível com clipper  
criar um link ou copiar o hbmk2 para rtlink/blinker/exospace resultará no mesmo efeito


 - **\-hbreg\[=global\]** registre Harbour Script \(\.hb\) com hbmk2 \(somente para Windows\)
 - **\-hbunreg\[=global\]** Harbour Script \(\.hb\) não registrado hbmk2 \(somente para Windows\)


 - **\-find &lt;text&gt;** lista todas as funções Harbour que contém &lt;text&gt; noseu nome, juntamente com seu pacote \(case insensitive, aceita multiplos valores, pode conter caracteres coringa "wildcard"\)


 - **\-hbmake=&lt;file&gt;** converte um projeto do hbmake em um arquivo \.hbp
 - **\-xbp=&lt;file&gt;** converte um projeto \.xbp \(xbuild\) em um arquivo \.hbp
 - **\-xhp=&lt;file&gt;** converte um projeto \.xhp \(xMate\) em um arquivo \.hbp


 - **\-\-hbdirbin** saída do diretório de binarios Harbour para stdout
 - **\-\-hbdirdyn** saída do diretório de bibliotéca dinâmica Harbour para stdout
 - **\-\-hbdirlib** saída do diretório de bibliotécas estáticas Harbour para stdout
 - **\-\-hbdirinc** saída do diretório Harbour header para stdout
 - **\-\-hbinfo\[=nested\]** output Harbour build information to stdout\. Output is in JSON format\. The included paths always contain forward slashes\. Each JSON block is followed by an 0x0A byte\.


 - **\-plat=&lt;platform&gt;** sobrepor a plataforma padrão \(padrão: automatic\)
 - **\-cpu=&lt;cpu&gt;** sobrepor o destino da CPU padrão \(padrão: automatic\) \(EXPERIMENTAL\)
 - **\-comp=&lt;compiler&gt;** sobrepor auto\-detecção do compilador C  
VAlores especiais:  
\- bld: use as definições da versão original \(padrão em \*nix\)
 - **\-build=&lt;name&gt;** especificar um nome de 'build'
 - **\-lang=&lt;lang&gt;** Sobrepor a linguagem padrão\. &lt;lang&gt; está em formato de código ISO\.
 - **\-width=&lt;n&gt;** ajuste a largura de saída para &lt;n&gt; caracteres \(0=ilimitado\)\.
 - **\-shl** exibir niveis de sub\-projeto nas linhas de saída
 - **\-viewhelp** help completo em formato "text viewer"
 - **\-longhelp** ajuda detalhada
 - **\-longhelpmd** help completo em formato [Markdown](http://daringfireball.net/projects/markdown/)
 - **\-harbourhelp** Ajuda do compilador Harbour \(todas as opções do compilador Harbour serão aceitas como hbmk2\)
 - **\-credits** créditos compilador Harbour
 - **\-build** Compilador Harbour \(build\)
 - **\-version** exibir somente o cabeçalho com a versão do hbmk
  
Opções abaixo são " internal/developer ones" \(compatibilidade não garantida\):  


 - **\-debugtime** medir o tempo gasto na contrução\.
 - **\-debuginc** exibir as partes internas da compilação incremental
 - **\-debugstub** exibir o conteúdo de todos os arquivos fontes gerados internamente
 - **\-debugi18n** exibir o conteúdo da geração de arquivo de tradução
 - **\-debugdepd** exibir a dependência interna da detecção
 - **\-debugpars** exibir todos os parâmetros de entrada na ordem de processamento
 - **\-debugrte** gerar um "run\-time error" erro de tempo de execução


É possível "sym\-link/copy/rename" hbmk2 para os seguintes nomes para alterar o modo padrão de operação:


 - **hbrun\*|\*hbrun** rodando em modo script / "shell" interativo
 - **hbrund|hbrun\*d** rodando em modo script / debugador em "shell" interativo
 - **harbour** modo \-hbraw \(emular \- raw \- compilador Harbour\)
 - **clipper** modo \-hbcmp \(emular compilador Clipper\)
 - **rtlink** modo \-rtlink \(emular linkeditor Clipper\)
 - **exospace** modo \-rtlink \(emular linkeditor Clipper\)
 - **blinker** modo \-rtlink \(emular linkeditor Clipper\)
 - **\*10** opções \-hb10
 - **\*20** opções \-hb20
 - **\*30** opções \-hb30
 - **x\*** opções \-xhb
 - **hbcmp\*|\*hbcmp** modo \-hbcmp \(emular compilador Harbour produzindo um objeto binario "binary object"\)
 - **hbcc\*|\*hbcc** modo \-hbcc \(emular compilador C\)
 - **hblnk\*|\*hblnk** modo \-hblnk \(emular linkeditor C\)
 - **hbexe\*|\*hbexe** modo \-hbexe
 - **hblib\*|\*hblib** modo \-hblib
 - **hbdyn\*|\*hbdyn** modo \-hbdyn
  
Arquivos:  


 - **\*\.hbp** arquivo de projeto\. Pode conter qualquer número de opções de linha de comando, que são esperados para criar uma saída\. As linhas que começam com caracter '\#' são ignoradas, caso contrário, nova linha é opcional e as opções são separados por espaço, assim como na linha de comando\. Você deve colocar a opção contendo espaço entre aspas duplas\. Cada referência de arquivo \.hbp será executado como um sub\-projeto\.
 - **\*\.hbm** coleção de opções\. Elas podem ser usadas dentro de um arquivo e incluídas dentro de arquivo de projeto\.Usar o mesmo formata dos arquivos \.hbp\.
 - **\*\.hbc** conjunto de opções que acompanham os componentes \(também conhecidos como 'libs', pacotes\)\. Usam diferentes sintaxes de linha de comado "command\-line" e arquivos \.hbp/\.hbm\. As linhas começam com o caracter '\#' serão ignoradas, cada diretiva deve ser colocada em linhas separadas\.
 - **\*\.ch** se passado diretamente como um arquivo de origem, ele será utilizada como padrão cabeçalho adicional
 - **hbmk\.hbc** por padrão, se presente o arquivo \.hbc é o que será processado automaticamente\. Localização \(ões\) possíveis \(em ordem de prioridade\) \[\*\]: %APPDATA%\\\.harbour, &lt;hbmk2 diretório&gt;
 - **hbmk\.hbm** o arquivo opcional \.hbm residente no diretório de trabalho atual será processado automaticamente antes das outras opções
 - **$hb\_pkg\_dynlib\.hbm** arquivo especial \.hbm incorporado dentro hbmk2\. Ele gerencia os detalhes da criação de uma biblioteca dinâmica \(no estilo 'Harbour contribs'\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file embedded inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** script Harbour
 - **\*\.hrb** Harbour binario portável \(Também conhecida como Harbour script pré\-compilado\)
 - **hbstart\.hb** script de inicialização Harbour para shell interativo Harbour\. Se presente ele é executado automaticamente na inicialização do shell\. Localizações possíveis \(em ordem de precedência\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbmk2 diretório&gt;
 - **shell plugins** \.hb e \.hrb plugins para shell interativo Harbour\. Eles pode residir em \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** armazena o historico de comando do shell interativo Harbour shell\. Voce pode desabilitar o historico fazendo a primeira linha 'no' \(sem aspas e com nova linha\)\. Localizado em \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** lista de extensões a ser carregada no shell interativo Harbour\. Uma extensão por linha, a parte alem do caracter '\#' será ignorada\. Nome alternativo em MS\-DOS: hb\_ext\.ini\. localizado em \[\*\]: %APPDATA%\\\.harbour\\
  
Variáveis macro:  


 - **$\{hb\_root\}** directório de hbmk2
 - **$\{hb\_dir\}** diretório de nome de arquivo é usado em
 - **$\{hb\_dirname\}** diretório raiz do nome de arquivo é usado em
 - **$\{hb\_name\}** nomeie o nome de arquivo ele é usado \(sem diretório e extensão\)
 - **$\{hb\_self\}** nome completo que é usado em
 - **$\{hb\_curdir\}** diretório de trabalho atual
 - **$\{hb\_tempdir\}** Diretório do Sistema Operacional para arquivos temporários
 - **$\{hb\_targetname\}** nome do projeto \(sem a extensão de diretório\)\. Retorna \.adhoc\. se não existir arquivo de projeto\.
 - **$\{hb\_targettype\}** tipo do projeto \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** plataforma selecionada
 - **$\{hb\_comp\}** compilado C selecionado
 - **$\{hb\_comp\_ver\}** versão do compilador C
 - **$\{hb\_build\}** nome da construção "build"
 - **$\{hb\_cpu\}** CPU selecionada
 - **$\{hb\_work\}** nome padrão do diretorio de trabalho
 - **$\{hb\_workdynsub\}** subdiretório padrão de trabalho para bibliotecas dinâmicas
 - **$\{hb\_dynprefix\}** prefixo de biblioteca dinâmica
 - **$\{hb\_dynsuffix\}** sufixo de biblioteca dinâmica
 - **$\{hb\_dynext\}** extensão de biblioteca dinâmica
 - **$\{hb\_ver\}** Versão do Harbour no formato de tres bytes em hexadecimal\. Por exemplo: 030200
 - **$\{hb\_verstr\}** Versão do Harbour no formato legível por humanos &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. Ex\.: 3\.2\.0dev
 - **$\{hb\_major\}** número da versão principal Harbour
 - **$\{hb\_minor\}** úmero da versão secundária Harbour
 - **$\{hb\_release\}** número da versão Harbour
 - **$\{hb\_status\}** status da versão Harbour
 - **$\{hb\_revision\}** revisão Harbour
 - **$\{hb\_host\_plat\}** plataforma de hospedagem Harbour
 - **$\{hb\_host\_plat\_unix\}** retorna '1' se o Harbour estiver hospedado numa plataforma compatível com \*nix
 - **$\{hb\_bin\}** diretório de binarios Harbour
 - **$\{hb\_lib\}** diretório de bibliotécas estáticas Harbour
 - **$\{hb\_lib3rd\}** diretório Harbour de bibliotécas estáticas de terceiros
 - **$\{hb\_dyn\}** diretório de bibliotécas dinâmicas Harbour
 - **$\{hb\_inc\}** diretório Harbour para header
 - **$\{hb\_addons\}** diretório base para complementos "add\-ons" Harbour\.
 - **$\{hb\_first\}** nome do arquivo de origem que detém a função de entrada \(sem diretório e extensão\)
 - **$\{hb\_outputdir\}** diretório de saída
 - **$\{hb\_outputname\}** nome de saída \(sem a extensão\)
 - **$\{hb\_level\}** nível de recursão do sub\-projeto
 - **$\{&lt;depname&gt;\}** retorna o diretório header da dependência &lt;depname&gt;, ou ´1´se ela não foi detectada\.
 - **$\{&lt;envvar&gt;\}** returna os valores das variáveis de ambiente &lt;envvar&gt;
  
Filtros \(você pode combinar e / ou negá\-los\):  


 - **\{&lt;platform&gt;\}** platforma alvo\. Onde &lt;platform&gt; pode ser qualquer valor aceito pela opção \-plat=\.
 - **\{&lt;compiler&gt;\}** Compilador alvo C \. Onde &lt;compiler&gt; pode ser qualquer valor aceito pela opção \-comp=\.
 - **\{&lt;cpu&gt;\}** CPU alvo\. Onde &lt;cpu&gt; pode ser qualquer uma dessas: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** tipo de construção alvo\. Onde &lt;targettype&gt; é qualquer um dos valores retornados por macro variável $\{hb\_targettype\}\.
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
 - **\{xhb\}** modo xhb \(veja opção \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** passará pelo filtro se o nome do arquivo &lt;file&gt; ou &lt;dir&gt; diretório existir no disco\.
 - **\{MACRO\}** passará pelo filtro se o valor $\{MACRO\} não for vazio e não for igual a zero '0' ou 'no' \(maiúsculas e minúsculas "case insensitive"\)
 - **\{MACRO='&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} for igual a &lt;value&gt; \(maiúsculas e minúsculas "case insensitive"\)
 - **\{MACRO&gt;'&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} for maio que o &lt;value&gt; \(maiúsculas e minúsculas "case insensitive"\)
 - **\{MACRO&lt;'&lt;value&gt;'\}** passará pelo filtro se o valor $\{MACRO\} menor que &lt;value&gt; \(maiúsculas e minúsculas "case insensitive"\)


Constantes pré\-definidas nos fontes:


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** quando um script \.hb é compilado como hbmk2 plugin
 - **\_\_HBEXTREQ\_\_** quando um arquivo \.hbx está presente em um projeto \(disponível nos fontes do Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** quando o pacote &lt;hbcname&gt;\.hbc está linkado a ao alvo\. O valor "version=" é igual ao da versão do arquivo \.hbc, convertido para numero decimal que é '1', se não especificado\. \(disponível nos fontes do Harbour\)
 - **HBMK\_HAS\_&lt;depname&gt;** quando dependência &lt;depname&gt; foi detectada \(disponível nos fontes C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** quando um programa fonte Harbour está rudando como "shell script"
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.


Constantes predefinidas em arquivos de construção \(eles estão disponíveis depois '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\):


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
 - **HB\_USER\_LDFLAGS** opções a serem passados ​​para o linkar o \(executável\) \(antes das opções da linha de comando\)
 - **HB\_USER\_DFLAGS** opções a serem passados ​​para o linkar \(biblioteca dinâmica\) \(antes das opções da linha de comando\)
 - **HB\_USER\_AFLAGS** opções a serem passados ​​para o linkar \(biblioteca estática\) \(antes das opções da linha de comando\)
 - **HB\_COMPILER\_VER** sobrepor a versão de autodetecção do compilador C \(somente para compiladores da familia gcc e msvc\)\. Formato: &lt;15&gt;&lt;00&gt;\[\.&lt;00&gt;\] = &lt;major&gt;&lt;minor&gt;\[\.&lt;revision&gt;\]
 - **HB\_CCPATH** sobrepor o diretório de execução do compilador C \(apenas para família de compilador gcc\)
 - **HB\_CCPREFIX** sobrepor o prefixo do executável do compilador C \(apenas para família de compilador gcc\)
 - **HB\_CCSUFFIX** sobrepor o sufixo executável do compilador C \(apenas para família de compilador gcc\)
 - **HB\_INSTALL\_PREFIX** sobrepor o diretório base de instalação Harbour
 - **HB\_INSTALL\_ADDONS** sobrepor o diretório base de instalação aducionais Harbour


 - **HB\_EXTENSION** lista separada por espaço de extensões para carga no shell interativo do Harbour
  
diretivas \.hbc \(devem ser escritas em linhas separadas\):  


 - **echo=&lt;msg&gt;** Exibir &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** pular o processamento do resto do arquivo hbc\.\. Mostrar &lt;msg&gt;, se especificado\.
 - **stop=\[&lt;msg&gt;\]** parar a geração\. Mostrar &lt;msg&gt;, se especificado\.
 - **sources=** adicionar lista separada por espaços de arquivos de entrada
 - **headers=** adicionar lista separada por espaços de arquivos "\.ch" tipo "headers"
 - **libs=** adicionar lista separada por espaços de bibliotecas \(veja mais opções em \-l\)
 - **frameworks=** adicionar lista separada por espaços de "frameworks" \(somente para Darwin\)
 - **requests=** adicionar lista separada por espaços de símbolos para forçar a linkagem do programa
 - **syslibs=** adicionar lista separada por espaços de bibliotecas como bibliotecas do sistema \(antes bibliotecas regulares\)
 - **hbcs=** incorporat lista separada por espaçoe de arquivos \.hbc\. São aceitos nomes se a extensão\. Essas referências são processados ​​no local\.
 - **autohbcs=** lista separada por espaço de opções de valores de entrada \-autohbc=
 - **libpaths=** adicionar lista separada por espaços dos locais de bibliotecas adicionais
 - **incpaths=** adicionar lista separada por espaços dos locais adicionais dos "header" \(para ambos Harbour e C\)
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
 - **shareddef=&lt;bool&gt;** semelhante ao shared=, mas funciona apenas se o modo shared/static não foi definido antes de
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
 - **safe=&lt;bool&gt;** opções 'yes' = \-safe, 'no' = \-safe\-
 - **cpp=** mesmo que \-cpp= opções
 - **warn=** mesmo que \-warn= opções
 - **compr=** mesmo que \-compr= opções
 - **head=** mesmo que \-head= opções
 - **plugins=** lista separada por espaço de hbmk2 plugins para carga
 - **gt=&lt;name&gt;** mesmo que \-gt&lt;name&gt; opções
 - **gtdef=&lt;name&gt;** definir o GT padrão a ser usado
 - **env=** mesmo que \-env: opções
 - **deppkgname=** mesmo que \-deppkgname= opções
 - **depkeyhead=** mesmo que \-depkeyhead= opções
 - **depoptional=** mesmo que \-depoptional= opções
 - **depcontrol=** mesmo que \-depcontrol= opções
 - **depincroot=** mesmo que \-depincroot= opções
 - **depincpath=** mesmo que \-depincpath= opções
 - **depincpathlocal=** mesmo que \-depincpathlocal= opções
 - **depimplibs=** mesmo que \-depimplibs= opções
 - **depimplibd=** mesmo que \-depimplibd= opções
 - **name=** Nome do Pacote
 - **description=** descrição do pacote
 - **version=&lt;x\.y\.z&gt;** número da versão do pacote, onde x,y,z &gt;= 0 &lt;= 255\. O padrão é 0\.0\.1, se não especificado\.
 - **keywords=** lista separada por espaço de palavras\-chave
 - **licences=** lista separada por espaço de licenças
 - **repository=** lista separada por espaço dos repositório de referências dos fontes


Plugin API:  
\('hbmk' is the context variable received by the plugin entry function\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Registre extensão de arquivo de entrada a ser passado para plugin \(por padrão todas as extensões desconhecidas são passadas para o compilador Harbour\)\.
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
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Texto de saída para stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Texto de saída para stderr\.
 - **hbmk\_OutStdRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
Texto de saída para stdout sem formatação\.
 - **hbmk\_OutErrRaw\( hbmk, \.\.\. \) \-&gt; NIL**  
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
mudar diretório e/ou extenção do nome do arquivo
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
codificar os nomes de função de acordo com o compilador Harbour formatando os nomes das funçoes em código C "HB\_FUNC\(\)" de acordo as regras\.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Remover aspas duplas de uma string\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Converte matriz de strings para uma string\. O separador padão é um espaço simples\.


Plugin variables:  
\('hbmk' context hash items, case\-sensitive, read\-only unless marked otherwise\)


 - **"apiver"** versão da API como um número inteiro
 - **"cSTATE"** estado do retorno\. Pode ser: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** passar uma matriz de parâmetros para os plugins através das opções \-pflag=/pi= ou tendo registrado as extensões via hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** variavés "hash de plugin" ​​personalizadas\. Gravável, local para cada plugin
 - **"cPLAT"** \-plat valor
 - **"cCOMP"** \-comp valor
 - **"nCOMPVer"** veja HB\_COMPILER\_VER envvar
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


Exemplos ta iniciar com hbmk2:


 - **Para rodar o "shell" interativo \(interpretador de comandos\)**  
$ hbmk2 \.
 - **Para executar un script Harbour**  
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
 - **para construir uma aplicação que usa os pacotes da contribuinte ou pacotes de terceiros que vem com um arquivo \.hbc**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Para gerar uma aplicação que utiliza uma biblioteca raw**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Para gerar uma aplicação que utiliza recursos do Windows**  
$ hbmk2 mymain\.prg myres\.rc
 - **Para gerar uma aplicação sem linkar bibliotecas dinâmicas Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Para gerar uma aplicação de todos os fontes \.prg e \.c residentes no subduretório**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Exemplos para gerar uma biblioteca estática Harbour:


 - **para gerar a biblioteca 'mylib' dos fontes**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **para gerar a biblioteca 'mylib' dos fontes usando o modo incremental**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Códigos de saída \("errorlevels"\):  


 - **0** sem erros
 - **1** plataforma desconhecida
 - **2** compilador desconhecido
 - **3** falhou na detecção Harbour
 - **5** criação stub falhou
 - **6** Falha na compilação \(Harbour, compilador C, compilador Recursos "RC"\)
 - **7** Falha na montagem final \(linker ou gerenciador de bibliotecas\)
 - **8** não suportado
 - **9** Falhou na criação do diretório de trabalho
 - **19** ajuda
 - **10** dependência em falta ou desativada
 - **20** inicialização de plugin
 - **30** aninhamento muito profundo
 - **50** solicitação de parada
 - **&lt;outros&gt;** quando a opção \-run for usada, o código de saida será o código devolvido pelo executável de destino
  
Notas:  


  - &lt;script&gt; can be:  
  &lt;@script&gt; or &lt;script\.hbm&gt;: command\-line options in file  
  &lt;script\.hbp&gt;: command\-line options in file, it also marks a new build target if specified on the command\-line  
  &lt;script\.hbc&gt;: package configuration file
  - se existir um nome de arquivo projeto \.hbp no diretório atual este será carregado como fonte de origem\. Caso contrário, o arquivo o com extensão \.prg extension será usado\.
  - Multiplos parâmetros \-l, \-L, \-i e &lt;script&gt; são aceitos\.
  - Opções usadas com o compilador Harbour também são aceitas\.
  - O arquivo de configuração hbmk\.hbc no diretório do hbmk2 sempre ser processado caso exista\. Em plataformas \*nix este arquivo é sempre procurado nas pastas ~/\.harbour, /etc/harbour, &lt;base&gt;/etc/harbour, &lt;base&gt;/etc \(exatamente nesta ordem\) antes da pasta que contém o hbmk2\.
  - O script hbmk\.hbm no diretório atual será sempre processado se existir\.
  - Usar a opção de barras como separador de diretório é recomendado\.mas barras invertidas também são igualmente aceitas\.
  - Os filtros para plataformas são aceitos para cada uma das linhas de um arquivo \.hbc e possuem diversas opções\.  
Formato de um filtro: \{\[\!\]\[&lt;arquitetura&gt;|&lt;compilador&gt;|&lt;cpu&gt;|&lt;palavra\-chave&gt;\]\}\. Os filtros podem ser combinados usando os operadores '&amp;', '|' e agrupados por parênteses\. Ex\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - A maioria das linhas \.hbc \(libs =, HBCS =, prgflags =, cflags =, ldflags =, libpaths =, instfiles =, instpaths =, echo =\) e os parâmetros correspondentes de linha de comando aceitarão variáveis ​​macro\. libpaths = também aceita%\{hb\_name\} que traduz o nome do arquivo\. hbc sob pesquisa\.
  - Opções aceitando macros também suportam linhas de comando\. Neste caso basta rodeiar o comando dentro de \`\`, e, se o comando contiver espaço, também adicione aspas duplas\. i\.e\. "\-cflag=\`wx\-config \-\-cflags\`", ou ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - When multiple build target type selection options \(\-hblib, \-hbdyn, etc\.\) are specified, the first one will be significant, the rest will be silently ignored\.
  - Bibliotecas e arquivos de objetos construídos com/para CA\-Cl\*pper não irá funcionar com qualquer plataforma /compilador suportada\.
  - Padrões e especificações suportadas podem variar de acordo com plataforma/compilador\.
  - não necessita de qualquer ferrramenta make do compilador C, GNU Make e MSYS \(no Windows\) para rodar hbmk2\.
  - \. \(ponto\) passado como primeiro parâmetro entrará no shell interativo Harbour\.


  - \.hb, \.hrb ou \.dbf arquivo passado como primeiro parâmetro irá rodar como Script Harbour\. Se o nome do arquivo não contiver componentes do "path", ele será procurado no diretório de trabalho atual e no "PATH"\.Se não é dada extensão, \.hb e \.hrb serão pesquisados nessa ordem\. arquivos \.dbf serão abertos no modo compartilhado "shared" e o "shell" interativo Harbour será lançado\. Extensões não padronizadas serão detectadas para fontes e e tipos de script pré\-compilados\. Nota, para Scripts Harbour, a pagina de códigos "codepage" será em UTF\-8 por padrão\. O nucleo padrão de cabeçalhos 'hb\.ch' será automaticamente incluido\. O formato da data será "aaaa\-mm\-dd" padrão "ISO"\. O Gt padrão é 'gtcgi', a menos que as chamadasCUI de tela cheia seja detectadas, quando 'gtwin' \[\*\] será automaticamente selecionado \(exeto para "INIT PROCEDUREs"\)\.
  - Voce pode usar &lt;Alt\+V&gt; no "shell do Harbour" para colar um texto do clipboard\.
  - Valores marcados com \[\*\] pode ser plataforma hospedagem e/ou configuração dependente\. Esta ajuda foi gerada em 'win' plataforma de hospedagem\.


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
