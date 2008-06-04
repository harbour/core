/*
 *
 * $Id$
 *
 */

#include "common.ch"

Function main()
    Local oServer, oQuery, oRow, i, x, aTables, aStruct, aKey

    Local cHost := 'localhost'
    Local cDatabase := 'test'
    Local cUser := 'user'
    Local cPass := 'pass'
    Local cQuery
    
    oServer := TPQServer():New(cHost, cDatabase, cUser, cPass)

    if oServer:NetErr()
        ? oServer:Error()
        quit
    end
    
    oServer:SetVerbosity(2)
    oServer:traceon('lixo.log')

    ? 'Tables...'
    
    For x := 1 to 1
        aTables := oServer:ListTables()
    
        For i := 1 to Len(aTables)
            ? aTables[i]
        next
    Next        
        
    if oServer:TableExists('TEST')
        ? oQuery := oServer:Execute('DROP TABLE Test')
        
        oQuery:Destroy()
    end        

    ? 'Creating test table...'
    cQuery := 'CREATE TABLE test('
    cQuery += '     Code integer not null primary key, '
    cQuery += '     dept Integer, '
    cQuery += '     Name Varchar(40), '
    cQuery += '     Sales boolean, '
    cQuery += '     Tax Float4, '
    cQuery += '     Salary Double Precision, '
    cQuery += '     Budget Numeric(12,2), '
    cQuery += '     Discount Numeric (5,2), '
    cQuery += '     Creation Date, '
    cQuery += '     Description text ) '
    
    oQuery := oServer:Query(cQuery)
    
    if oQuery:neterr()
        ? oQuery:Error()
    end

    oQuery:Destroy()
    
    ? 'Structure of test table'
    aStruct := oServer:TableStruct('test')

    For i := 1 to Len(aStruct)
        ? 
        For x := 1 to Len(aStruct[i]) 
            ?? aStruct[i,x], " "
        Next            
    next    
    
    ? 'Inserting, declared transaction control '
    oServer:StartTransaction()
    
    For i := 1 to 10
        cQuery := "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) "
        cQuery += "VALUES( " + str(i) + ", 2, 'TEST', 'y', 5, 3000, 1500.2, 7.5, '12-22-2003', 'Short Description about what ? ')"
        
        oQuery := oServer:Query(cQuery)  
        
        if oQuery:neterr()
            ? oQuery:error()
        end              
        
        oQuery:destroy()
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
      oRow:Fieldpos('sales'), ;
      oRow:Fieldget(1), ;
      oRow:Fieldname(2), ;
      oRow:Fieldtype(1), ;
      oRow:Fielddec(1), ;
      oRow:Fieldlen(1)

    oRow:Fieldput(1, 150)
    oRow:Fieldput(2, 'MY TEST')
    
    ? oRow:Fieldget(1), oRow:Fieldget(2)
        
    ? oRow:aRow[1], oRow:aRow[2], oRow:aOld[1], oRow:aOld[2]
    
    ? oQuery:Append(oRow)
    
    ? oQuery:error()
    
    DO WHILE ! oQuery:Eof()
        ? oQuery:Recno(),;
          oQuery:Fieldpos('code'),;
          oQuery:Fieldget(oQuery:Fieldpos('code')), ;
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
            ? 'Update: ', oQuery:Update(oRow)
        end      
        
        if oQuery:Recno() == 60
            oRow := oQuery:getrow()
            ? 'Delete: ', oQuery:Delete(oRow)
        end
        
        oQuery:Skip()

    END    

    oQuery:Refresh()
    
    For i := 1 to oQuery:Lastrec()
        oRow := oQuery:getrow(i)
        
        ? i, oRow:Fieldget(oRow:Fieldpos('code')), ;
          oRow:Fieldget(4), ;
          oRow:Fieldget(2), ;
          oRow:Fieldname(1),;
          oRow:Fieldtype(1), ;
          oRow:Fielddec(1), ;
          oRow:Fieldlen(1),;
          oRow:Fieldget(i, 3)
                    
    END    

    oQuery:Destroy()
    
    oServer:Destroy()
        
    ? "Closing..."
        
    return nil
    

    
