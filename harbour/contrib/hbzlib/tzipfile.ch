#xtranslate CREATE  ZIPFILE <oZip> File <cFile> [METHOD <nMeth> ] [BLOCK <bBlock>] ;
        [<over: OVERWRITE>] [<comp:COMPRESS>] => ;
        <oZip> :=tZipFile():New(<cFile>,<nMeth>,<{bBlock}>,<.comp.>,<.over.>)
#xtranslate ADD FILE <cFile> to <oZip> => <oZip>:AddFile(<cFile>)
#xtranslate ACTIVATE ZIP <oZip> => <oZip>:DoCompress()
#xtranslate ACTIVATE UNZIP <oZip> => <oZip>:DoUnzip()
#xtranslate GET FILELIST <oZip> => <oZip>:Getlist()
#xtranslate SHOW FILELIST <oZip> => <oZip>:ShowList()

