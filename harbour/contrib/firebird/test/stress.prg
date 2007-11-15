/*
 * $Id: atrepl.c 7583 2007-07-06 21:17:36Z druzus $
 */

/* VERY IMPORTANT: Don't use this querys as sample, they are used for stress tests !!! */

Function Main()
    Local oServer, oQuery, oRow, i, x

    Local cServer := '192.168.1.33:D:\firebird\test\test2.gdb' 
    Local cUser := 'sysdba'
    Local cPass := 'masterkey'
    Local nDialect := 1
    Local cQuery

    CLEAR SCREEN
    
    if ! File('test2.gdb') .and. .F.
      ? FBCreateDB('test2.gdb', cuser, cpass, 1024, 'WIN1251', nDialect )
    end      
               
    ? "Connecting..."
        
    oServer := TFBServer():New(cServer, cUser, cPass, nDialect)

    if oServer:NetErr()
        ? oServer:Error()
        quit
    end

    if oServer:TableExists('test')
        ? oServer:Execute('DROP TABLE Test')
        ? oServer:Execute('DROP DOMAIN boolean_field')
    end        
    
    ? 'Creating domain for boolean fields...'

    ? oServer:Execute('create domain boolean_field as smallint default 0 not null check (value in (0,1))')
    
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

    ? oServer:Execute(cQuery)

quit

    oQuery := oServer:Query('SELECT code, dept, name, sales, salary, creation FROM test')
    
    oServer:StartTransaction()
     
    For i := 1 to 10000
        @ 15,0 say 'Inserting values....' + str(i)
        
        oRow := oQuery:Blank()
        
        oRow:Fieldput(1, i)
        oRow:Fieldput(2, i+1)
        oRow:Fieldput(3, 'DEPARTMENT NAME ' + strzero(i) )
        oRow:Fieldput(4, (mod(i,10) == 0) )
        oRow:Fieldput(5, 3000 + i )
        oRow:fieldput(6, Date() )
        
        oServer:Append(oRow)
        
        if mod(i,100) == 0
            oServer:Commit()
            oServer:StartTransaction()
        end
    Next
    
    For i := 5000 to 7000
        @ 16,0 say 'Deleting values....' + str(i)

        oRow := oQuery:Blank()
        oServer:Delete(oRow, 'code = ' + str(i))
        
        if mod(i,100) == 0
            oServer:Commit()
            oServer:StartTransaction()
        end
    Next
    
    For i := 2000 to 3000
        @ 17,0 say 'Updating values....' + str(i)

        oRow := oQuery:Blank()
        oRow:Fieldput(5, 4000+i)
        oServer:update(oRow, 'code = ' + str(i))
        
        if mod(i,100) == 0
            oServer:Commit()
            oServer:StartTransaction()
        end
    Next

    oQuery := oServer:Query('SELECT sum(salary) sum_salary FROM test WHERE code between 1 and 4000')
    
    if ! oQuery:Neterr()
        oQuery:Fetch()
        @ 18,0 say 'Sum values....' + Str(oQuery:Fieldget(1))
        oQuery:Destroy()
    end


    x := 0
    For i := 1 to 4000
        oQuery := oServer:Query('SELECT * FROM test WHERE code = ' + str(i))
        
        if ! oQuery:Neterr()
            oQuery:Fetch()
            oRow := oQuery:getrow()

            oQuery:destroy()
            x += oRow:fieldget(oRow:fieldpos('salary'))
            
            @ 19,0 say 'Sum values....' + str(x)
        end            
    Next   
    
    
    oServer:Destroy()
    
    ? "Closing..."
return nil
