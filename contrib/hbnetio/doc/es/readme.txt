/* Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
   Translation: Guillermo Varona Silupú */

HBNETIO es la implementación alternativa de RDD (Database Driver
Reemplazable) IO API para Harbour con soporte adicional para RPC
(Remote Process Call - Llamada a procesos remotos). Contiene código
del cliente y del servidor.
Es compatible con la conexión de compresión de flujo utilizando
la compresión ZLIB y cifrado utilizando el algoritmo Blowfish.
Después de su registro en el lado del cliente todos los archivos
RDD nativos utilizados por Harbour con el nombre que comienza con
"net:" se redirigen al servidor hbnetio.

Funciones del lado del cliente:
===============================
   netio_Connect( [<cServer>], [<nPort>], [<nTimeOut>], ;
                  [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <lConnected>
      Registra HBNETIO como alternativa API RDD IO redirigiendo todos
      los archivos cuyo nombre empieza con "net:" hacia el servidor
      HBNETIO, por defecto, establece dirección del servidor, puerto
      y los parámetros de conexión y trata de establecer la conexión.
      Cuando se ejecuta por primera vez, establece parámetros de
      conexión por defecto para todos los hilos. Cada hilo puede
      sobrescribir estos ajustes predeterminados con sus propias
      llamadas locales a la función netio_Connect().
      Cada llamada exitosa a netio_Connect() aumenta el contador de
      referencia para la conexión dada, netio_Disconnect() lo disminuye.
      La conexión se cierra cuando el contador llega a 0. Esto significa
      que cada llamada a netio_Connect() necesita una llamada
      correspondiente a netio_Disconnect(). Las conexiones son
      reconocidas por la dirección IP del servidor y el número de puerto,
      que son compartidos entre los hilos. Por eso, cuando más de un
      hilo llama a netio_Connect() se crea una sola conexión. También
      significa que netio_Disconnect() no tiene que ser llamado por
      el mismo hilo que llama a netio_Connect().
      A la salida de aplicación todas las conexiones se cierran
      automáticamente.
      Es posible abrir muchas conexiones diferentes y mantenerlas
      abiertas.
      En las operaciones de IO RDD y llamadas RPC es posible especificar
      la dirección del servidor como parte del nombre del archivo
      o procedimiento / función, es decir,
         USE net:example.org:2942:path/to/file
         netio_ProcExec( "example.org:2942:procname" )
      o el uso de rutas UNC:
         USE net://example.org:2942/path/to/file
         netio_ProcExec( "//example.org:2942/procname" )
      También es posible especificar la contrasena. La cadena de
      conexión está en formato:
         <server>[:<port>[:<passwd>]]:<filepath|funcname>
      o:
         //<server>:<port>:<passwd>:<filepath|funcname>
      o:
         //<server>[:<port>]/<filepath|funcname>
      Las barras invertidas '\' también son compatibles y se puede
      utilizar en lugar de '/'.
      Contrasena siempre termina en ":" y toda la cadena de conexión
      termina en Chr(0) asi que no es posible utilizar estos dos caracteres
      como parte de la contrasena. De todos modos cuando se requieren
      contrasenas, se recomienda abrir la conexión con netio_Connect()
      y luego especificar sólo servidor y puerto, si el servidor no es
      lo suficientemente única para elegir las conexiones existentes.
      Si no se da servidor entonces se elige conexión predeterminada.


   netio_GetConnection( [<cServer>], [<nPort>], [<nTimeOut>], ;
                        [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <pConnection> | NIL

      Obtiene puntero con conexión HBNETIO. Se puede utilizar para
      acelerar llamadas a RPC y funciones de transmisión cuando
      <pConnection> se pasa como parámetro a estas funciones.


   netio_Disconnect( [<cServer>], [<nPort>] ) -> <lOK>
      Cierra la conección creada por netio_Connect()


   netio_Decode( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
                 [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] )
         -> <lDecoded>
      Decodifica los parámetros de conexión desde <cFullName> y los
      valores de la configuración predeterminada.
      Retorna .T. si <cFullName> contiene la configuración de conexión.
      <cFullName> no debe contener el prefijo "net:".


   netio_TimeOut( <pConnection> [, <nTimeOut>] ) -> [<nTimeOut>]
      Obtener / Establecer tiempo de espera del cliente para los
      mensajes


   netio_ProcExists( [<pConnection>,] <cProcName> ) -> <lExists>
      Compruebe si existe la función o procedimiento en el lado del
      servidor.


   netio_ProcExec( [<pConnection>,] <cProcName> [, <params,...>] ) -> <lSent>
      Ejecuta Función o procedimiento en el lado del servidor que
      no espera la confirmación del servidor.

   netio_ProcExecW( [<pConnection>,] <cProcName> [, <params,...>] )
         -> <lExecuted>
      Ejecuta función o procedimiento en el lado del servidor y espera
      la confirmación del servidor.

   netio_FuncExec( [<pConnection>,] <cFuncName> [, <params,...>] )
         -> <xFuncRetVal>
      Ejecuta función en el lado del servidor y espera el valor de
      retorno de la función enviada por el servidor.

   netio_OpenDataStream( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
         -> <nStreamID>
   netio_OpenItemStream( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
         -> <nStreamID>
      Abre un flujo / canal de comunicación que permite enviar datos
      de forma asíncrona del servidor al cliente.
      Ésto se ejecuta en el lado del servidor:
          <cStreamFuncName> (<pConnSock>, <nStreamID> [, <params, ...>])
      y luego se comprueba el valor devuelto por la función anterior.
      Si es igual a <nStreamID> entonces el flujo de la comunicación
      es abierta y <nStreamID> se devuelve al cliente.
      La función devuelve un nuevo ID del flujo o -1 si el flujo
      de la comunicación no se puede realizar.
      <cStreamFuncName> puede contener información acerca de los
      parámetros de conexión al igual que <cProcName> en las
      funciones NETIO_PROC*().

   netio_CloseStream( <nStreamID>,
                      [<pConnection>] | [[<cServer>], [<nPort>]] ) -> <lOK>
      Cierra el flujo / canal de comunicación.

   netio_GetData( <nStreamID>,
                  [<pConnection>] | [[<cServer>], [<nPort>]] )
         -> <aData> | <cData> | NIL
      recupera los datos enviados desde el servidor por el flujo
      de comunicación.
      Si el flujo fue abierto por netio_OpenDataStream(), los datos
      se devuelve como cadena.
      Si el flujo fue abierto por netio_OpenItemStream(), los datos
      se devuelve como un arreglo de items.



Funciones del lado del Servidor:
================================
   netio_Listen( [<nPort>], [<cIfAddr>], [<cRootDir>], [<lRPC>] )
            -> <pListenSocket> | NIL
   netio_Accept( <pListenSocket>, [<nTimeOut>],
                 [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
            -> <pConnectionSocket> | NIL
   netio_Compress( <pConnectionSocket>,
                   [<cPass>], [<nCompressionLevel>], [<nStrategy>] ) -> NIL
   netio_VerifyClient( <pConnectionSocket> ) -> <lAccepted>
   netio_Server( <pConnectionSocket> ) -> NIL
   netio_RPC( <pListenSocket> | <pConnectionSocket> [, <lEnable>] ) -> <lPrev>
   netio_RPCFilter( <pConnectionSocket>,
                    <sFuncSym> | <hValue> | NIL ) -> NIL
   netio_ServerStop( <pListenSocket> | <pConnectionSocket> [, <lStop>] ) -> NIL
   netio_ServerTimeOut( <pConnectionSocket> [, <nTimeOut>] ) -> [<nTimeOut>]
   netio_MTServer( [<nPort>], [<cIfAddr>], [<cRootDir>],
                   [<xRPC> | <sFuncSym> | <hValue>],
                   [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
                   [<sSrvFunc>] )
            -> <pListenSocket>

   netio_SrvStatus( <pConnectionSocket>
                    [, <nStreamID> | <nSrvInfo>, @<xData>] ) -> <nStatus>
   netio_SrvSendItem( <pConnectionSocket>, <nStreamID>, <xData> ) -> <lSent>
   netio_SrvSendData( <pConnectionSocket>, <nStreamID>, <cData> ) -> <lSent>
