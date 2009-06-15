
function getXmlHttp() 
{ 
   var obj=null;

   if( window.XMLHttpRequest )  
   {
      obj = new XMLHttpRequest();
   } 
   else if( window.ActiveXObject )  
   {
      obj = new ActiveXObject("Microsoft.XMLHTTP");
   }
   if ( obj == null ) 
   {
      alert("Browser does not support HTTP Request");
   } 
   return obj;
}

function ubrcall(id,param)
{
   var tbl = document.getElementById(id);
   var r = getXmlHttp();
   r.open("GET", "?ajax=" + id + "&" + param, true);
   r.onreadystatechange=function () 
   {
      if( r.readyState == 4 ) 
      {
         if( r.status == 200 )
         {
            tbl.innerHTML = r.responseText;
         }
         r = null;
      }
   }
   r.send(null);
}

