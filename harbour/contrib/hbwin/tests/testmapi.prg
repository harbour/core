function main( cSubject, cBody, lMailConf, lFromUser, aSender, aDest, aFiles )

   local nRet := HB_MAPISendMail( cSubject,                        ; // subject
                                  cBody,                           ; // menssage
                                  nil,                             ; // type of message
                                  DtoS( Date() ) + " " + Time(),   ; // send date
                                  "",                              ; // conversation ID
                                  lMailConf,                       ; // acknowledgment
                                  lFromUser,                       ; // user intervention
                                  aSender,                         ; // sender
                                  aDest,                           ; // destinators
                                  aFiles                           ; // attach
                                )

return nRet == 0
