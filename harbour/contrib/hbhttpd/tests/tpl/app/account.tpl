{{extend _main}}
<a href="/app/shopping">Shopping</a> | <a href="/app/cart">Cart</a> | <a href="/app/logout">Logout</a>
<hr>
<H1>My account</H1>
{{if errtext}}<span style="color:red; font-weight:bold; border:2px solid red; padding:5px; background-color:#FCC;">{{= errtext}}</span><p>{{endif}}
<table>
<tr><td><b>User name</b></td><td>{{= user}}</td></tr>
<tr><td><b>Name</b></td><td>{{= name}}</td></tr>
</table>
<p>
<a href="/app/account/edit">Edit</a>
