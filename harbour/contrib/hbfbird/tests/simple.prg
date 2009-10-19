/*
 * $Id$
 */

#include "common.ch"

Function main()
    Local oServer, oQuery, oRow, i, x, aTables, aStruct, aKey

    Local cServer := '192.168.1.33:D:\firebird\test\test.gdb' 
    Local cUser := 'sysdba'
    Local cPass := 'masterkey'
    Local nDialect := 1
    Local cQuery
    
    if File('test.gdb')
        FErase('test.gdb')
    end
    
    ? FBCreateDB('test.gdb', cuser, cpass, 1024, 'ASCII', nDialect )
               
    ? "Connecting..."
        
    oServer := TFBServer():New(cServer, cUser, cPass, nDialect)

    if oServer:NetErr()
        ? oServer:Error()
        quit
    end

    ? 'Tables...'
    
    For x := 1 to 1
        aTables := oServer:ListTables()
    
        For i := 1 to Len(aTables)
            ? aTables[i]
        next
    Next        
        
    ? 'Using implicit transaction...'
    
    if oServer:TableExists('TEST')
        oServer:Execute('DROP TABLE Test')
        oServer:Execute('DROP DOMAIN boolean_field')
    end        

    ? 'Creating domain for boolean fields...'
    oServer:Execute('create domain boolean_field as smallint default 0 not null check (value in (0,1))')
    
    oServer:StartTransaction()
    ? 'Creating test table...'
    cQuery := 'CREATE TABLE test('
    cQuery += '     Code SmallInt not null primary key, '
    cQuery += '     dept Integer, '
    cQuery += '     Name Varchar(40), '
    cQuery += '     Sales boolean_field, '
    cQuery += '     Tax Float, '
    cQuery += '     Salary Double Precision, '
    cQuery += '     Budget Numeric(12,2), '
    cQuery += '     Discount Decimal(5,2), '
    cQuery += '     Creation Date, '
    cQuery += '     Description blob sub_type 1 segment size 40 ) '

    oServer:Execute(cQuery)
    
    if oServer:neterr()
        ? oServer:Error()
    end

    oServer:Commit()
    
    oServer:Query('SELECT code, dept, name, sales, salary, creation FROM test')    
    wait

    
    ? 'Structure of test table'
    aStruct := oServer:TableStruct('test')
    
    For i := 1 to Len(aStruct)
        ? 
        For x := 1 to Len(aStruct[i]) 
            ?? aStruct[i,x]
        Next            
    next
    
    ? 'Inserting, declared transaction control '
    oServer:StartTransaction()
    
    For i := 1 to 100
        cQuery := 'INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) '
        cQuery += 'VALUES( ' + str(i) + ', 2, "TEST", 1, 5, 3000, 1500.2, 7.5, "12-22-2003", "Short Description about what ? ")'
        
        oServer:Execute(cQuery)  
        
        if oServer:neterr()
            ? oServer:error()
        end              
    Next                    
        
    oServer:Commit()

    oQuery := oServer:Query('SELECT code, name, description, sales FROM test')
    
    aStruct := oQuery:Struct()
    
    For i := 1 to Len(aStruct)
        ? aStruct[i,1], aStruct[i,2], aStruct[i,3], aStruct[i,4]
    Next
        
    aKey := oQuery:GetKeyField()
    
    ? "Fields: ", oQuery:Fcount(), "Primary Key: ", aKey[1]

    oRow := oQuery:Blank()
    
    ? oRow:FCount(), ;
      oRow:Fieldpos('code'), ;
      oRow:Fieldget(1), ;
      oRow:Fieldname(1), ;
      oRow:Fieldtype(1), ;
      oRow:Fielddec(1), ;
      oRow:Fieldlen(1), ;
      len(oRow:Getkeyfield())

    oRow:Fieldput(1, 150)
    oRow:Fieldput(2, 'MY TEST')
    
    ? oRow:Fieldget(1), oRow:Fieldget(2)
    
    ? oServer:Append(oRow)
    
    ? oServer:Delete(oQuery:blank(), 'code = 200')
    
    ? oServer:Execute('error caused intentionaly')    

    DO WHILE ! oQuery:Eof()
        oQuery:Skip()
        ? oQuery:Fieldget(oQuery:Fieldpos('code')), ;
          oQuery:Fieldget(4), ;
          oQuery:Fieldget(2), ;
          oQuery:Fieldname(1),;
          oQuery:Fieldtype(1), ;
          oQuery:Fielddec(1), ;
          oQuery:Fieldlen(1),;
          oQuery:Fieldget(3)
    
        if oQuery:Recno() == 50
            oRow := oQuery:getrow()
            
            oRow:Fieldput(2, 'My Second test')
            ? 'Update: ', oServer:Update(oRow)
        end      
        
        if oQuery:Recno() == 60
            oRow := oQuery:getrow()
            ? 'Delete: ', oServer:Delete(oRow)
        end
    END    

    ? 'Delete: ', oServer:Delete(oQuery:Blank(), 'code = 70')

    oQuery:Refresh()
    
    DO WHILE oQuery:Fetch()
        oRow := oQuery:getrow()
        
        ? oRow:Fieldget(oRow:Fieldpos('code')), ;
          oRow:Fieldget(4), ;
          oRow:Fieldget(2), ;
          oRow:Fieldname(1),;
          oRow:Fieldtype(1), ;
          oRow:Fielddec(1), ;
          oRow:Fieldlen(1),;
          oRow:Fieldget(3)
    END    

    oQuery:Destroy()
    
    oServer:Destroy()
    
    
    ? "Closing..."
        
    return nil
    

    
