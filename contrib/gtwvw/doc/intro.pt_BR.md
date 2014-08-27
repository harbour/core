# INTRODUÇĂO

Alguns agradecimentos săo indispensáveis: Harbour Claro, sem
essa fantástica ferramenta todo esse trabalho năo existiria (os
agradecimentos, na verdade, săo para todas as pessoas que
desenvolvem/contribuem). GTWVW O autor da GTWVW (Budyanto Dj.)
e seus "ancestrais". Marcos Antonio Gambeta Ele escreveu um guia de
programaçăo para a GTWVT e disponibilizou como freeware. A leitura
desse manual me ajudou a resolver diversas dúvidas quanto aos
parâmetros das funções, retorno de valores, etc. Forum de notícias
Harbour Leitura obrigatória. Forum Clipper on line Muita gente boa
disposta a dividir seus conhecimentos. MS SDK HELP FILES Leitura
indispensável para entender realmente como e porquę de alguns
parâmetros e características da GTWVW. Julio C. Cantillo Molina Seu
trabalho na WVWTOOLS abriu meus olhos para as reais possibilidades da
GTWVW. A realizaçăo desse trabalho de documentaçăo das funções da GTWVW
foi feito com muita atençăo, mas nem por isso, está imune a falhas,
erros de interpretaçăo e gramaticais. É a versăo 0.01 alpha para
exemplos, trechos de códigos. Criei um grupo no Yahoo características
técnicas, relacionado com a GTWVW. portuguęs, cabe aos usuários da
GTWVW fornecer correções,

<https://br.groups.yahoo.com/neo/groups/gtwvw/info> para discussăo
das resoluçăo de problemas, trocas de experięncias e tudo mais

Manoel Angeiras(angeiras@gmail.com, angeiras@yahoo.com) Versăo
2007 Janeiro


# SOBRE A GTWVW

GTWVW é um driver de terminal para Harbour com algumas bibliotecas em
tempo de execuçăo, permitindo ao programador mesclar texto e elementos
GUI, em uma aplicaçăo multi-janela. GTWVW é exclusivamente desenhado
para a plataforma win32. Usando a GTWVW o programador pode usar todas
as funções padrăo da GT, normalmente chamadas indiretamente pelo
Harbour, como:

   - ?, ?? (QOut(), QQOut())
   - @ ... SAY ... (DevPos(), DevOut())
   - Scroll()
   - SetPos()
   - \_GET\_()
   - ReadModal()
   - Inkey()
   - AChoice()
   - Alert()
   - etc.

Todos os comandos e funções tem o mesmo comportamento que teriam em
outras GTs (por exemplo, no modo console). Podemos citar algumas
características especiais da GTWVW:

- Permite ao programador maximizar, etc). realizar operações sobre
  janelas (abrir, fechar, minimizar,
- Pode mesclar elementos de texto e GUI em uma mesma janela.
- Controle nativos do windows (statusbar, toolbars, scrollbars,
  pushbuttons, checkboxes). Veremos essas e outras características
  com detalhes mais adiante.


# SOBRE JANELAS

## Algumas convenções básicas:
- As janelas săo numeradas de 0..n, sendo 0 a janela principal,
  e n a janela atual.
- A janela principal é automaticamente aberta durante a
  inicializaçăo do programa.
- Todas as janelas săo automaticamente fechadas quando o programa
  termina.
- A janela-pai da janela n que será aberta é a janela atual
  (tipicamente a janela n-1).
- A grande maioria das funções tem como primeiro parâmetro o número
  da janela atual, mas a GTWVW năo utiliza esse parâmetro. Ao invés
  disso, a GTWVW "acha" a janela mais atual e utiliza-a. Mesmo
  assim, temos que considerar o número da janela para a ordem
  correta dos parâmetros. Por exemplo, a funçăo para excluir um
  combobox é definida da seguinte forma wvw_cbDestroy( nWinNum,
  nCBId ) onde nWinNum é o número da janela e nCBId o identificador
  do combobox. Para a GTWVW o parâmetro nWinNum é ignorado, entăo
  tanto faz chamarmos a funçăo assim wvw_cbDestroy( nWinNum, nCBId )
  ou assim wvw_cbDestroy( , nCBId )

## COORDENADAS

Existem dois modelos de coordenadas da tela, que o usuário pode
selecionar/mudar a qualquer momento:

### Standard Mode
- Nesse modo as coordenadas săo relativas a janela atual.
- A janela atual é sempre setada, no início do programa ou em cada
  operaçăo de abertura e fechamento de janela.
- Todas as saídas/entradas da tela săo orientadas para a janela
  principal. Dessa forma as funções MaxRow() e MaxCol() retornarăo
  os limites da janela atual.

### Maincoord Mode
- Nesse modo as coordenadas săo relativas a janela principal (como
  no Clipper). - Todas as saídas/entradas trabalham baseadas na
  janela atual. Internamente, existe um processo que verifica em
  qual janela deve ser feita a operaçăo de saída/entrada, dependendo
  da linha/coluna. Após cada operaçăo, a janela atual é sempre
  resetada para a janela principal. Dessa forma MaxRow() e MaxCol()
  sempre retornarăo as coordenadas máximas da janela principal, năo
  importando o número de janelas abertas.
- Esse modo foi projetado para ser a forma mais rápida e eficiente
  para portar aplicativos do Clipper para Harbour.


# SOBRE MINIMIZAR, MAXIMIZAR E REDESENHAR TELAS

Algumas funções da GTWVW năo săo automaticamente redesenhadas quando as
janelas săo minimizadas ou sobrepostas por outros elementos. Nossa
aplicaçăo deve "lembrar" quais săo os elementos que devem ser
redesenhados e a GTWVW nos ajuda com isso.

Existe uma funçăo, WVW_PAINT(), definida pela nossa aplicaçăo, que
é chamada pela GTWVW, para que possamos redesenhar os nossos elementos
gráficos ou qualquer outra coisa que quisermos.

Algumas das funções da GTWVW que precisam do suporte para redesenho săo:
   wvw_DrawBoxGet()
   wvw_DrawBoxRaised()
   wvw_DrawBoxRecessed()
   wvw_DrawBoxGroup()
   wvw_DrawImage()
   wvw_DrawLabel()

Observe que o redesenho năo é feito de forma imediata pela GTWVW.
O intervalo para redesenho pode ser definido pela nossa aplicaçăo
através da funçăo wvw_SetPaintRefresh(). Se setarmos o intervalo para
redesenho para zero, a GTWVW chamará a funçăo WVW_PAINT() cada vez que
foi requisitado o redesenho, pelo windows (exceto se uma chamada prévia
ainda năo foi retornada). Se o intervalo para redesenho for setado para
maior que zero (valores válidos maiores que 50), entăo a funçăo
WVW_PAINT() será chamada após esse intervalo, em milisegundos, apenas
se ainda persistir uma açăo de redesenho pendente. O intervalo default
para o redesenho é de 100.


# SOBRE O CURSOR

Existem dois estilos para o cursor: Horizontal (como em aplicações
MS-DOS) Vertical (mais comum em aplicações Windows) O programador
pode wvw_SetVertCaret(). selecionar qual o estilo que deseja, através
da funçăo

O novo estilo do cursor será aplicado para todas as janelas (atualmente
o cursor é apenas mostrado na janela atual). O estilo default é o
horizontal.


# SOBRE O ESPAÇAMENTO DE LINHAS

O programador pode escolher se haverá espaço entre linhas. Isso pode
desejável, entre outras razões, porque os elementos GUI podem
sobrescrever a linha acima ou abaixo.

Cada janela pode ter seu próprio espaçamento, que pode ser configurado
através da funçăo wvw_SetDefLineSpacing().

O espaçamento default é de zero.


# SOBRE FONTES

Nas diversas funções da GTWVW sobre as fontes, existem diversos
parâmetros para controlar o tipo, largura, altura e outros fatores da
fonte. Vamos ver aqui os detalhes de cada parâmetro da funções sobre
fontes. Por exemplo, na funçăo wvw_CreateFont(), tem a seguinte lista
de parâmetros:
   cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
   nCharset, nQuality, nEscapement e o significado para cada um é:

- `cFontFace` Uma string que especifica o nome da fonte. O tamanho dessa
  string năo deve exceder 32 caracteres.
- `nHeight` A largura da fonte.
- `nWidth` O tamanho da fonte.
- `nWeight` Especifica o "peso" da fonte, variando de 0 até 1000. Por
  exemplo, um "peso" de 400 geralmente determina uma fonte normal,
  já 700, negrito. Existem 15 (quinze) modelos de "peso" de fonte,
  que podem ser encontradas no arquivo `wingdi.ch` (FW_DONTCARE, FW_THIN,
  etc.).
- `lItalic` Identifica se a fonte será itálica ou năo.
- `lUnderline` Identifica uma fonte sublinhada ou năo.
- `lStrikeOut` Identifica uma fonte strikeout (fonte com linha traçada
  no meio das letras).
- `nCharSet` Especifica o cojnunto de caracteres a ser usado. Por exemplo,
  ANSI_CHARSET, DEFAULT_CHARSET, OEM_CHARSET, etc. (os modelo estăo no
  arquivo `wingdi.ch`).
- `nQuality` Identifica a qualidade de saída da fonte. Essa característica
  define como a GDI deve interpretar a forma lógica da fonte, com a sua
  forma de apresentaçăo física. Existem tręs valores possíveis para
  a qualidade da fonte: DEFAULT_QUALITY DRAFT_QUALITY PROOF_QUALITY
- `nEscapement` Especifica o ângulo, em décimos de graus, entre o vetor
  de fuga e o eixo-x do dispositivo. O vetor de fuga é paralelo com a
  linha base do texto. O default é 0. A aparęncia da fonte năo importa.
  Aparęncia intermediária de qualidade da fonte. A qualidade da fonte é
  a melhor forma possível.


# FUNÇÕES CALLBACK

Existem algumas funções que devem ser definidas na nossa aplicaçăo, que
săo na verdade, chamadas diretamente pela GTWVW. Algumas das principais
funções săo:

- WVW_PAINT( nWinNum )

  Essa funçăo é chamada sempre que o Windows recebe uma mensagem
  WM_PAINT (para redesenho da tela). Na verdade o intervalo para
  chamada de WVW_PAINT() pode ser configurado, através da funçăo
  wvw_SetPaintRefresh().

- WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )

  Essa funçăo é chamada a cada intervalo de tempo, que pode ser definido
  através da funçăo wvw_SetTimer().

- WVW_SIZE( nWindow, hWnd, message, wParam, lParam ) Chamada sempre que

  a janela é minimizada, maximizada ou restaurada. Em conjunto com essa
  funçăo, deve ser definida a funçăo wvw_Size_Ready(), que indica se o
  processamento de WVW_Size() deve ser realizado ou năo.


# EXEMPLOS

No site do grupo tem uma pequena aplicaçăo de exemplo do uso da GTWVW,
algumas extensões, exemplos de tela, aquivo manifest, etc.
