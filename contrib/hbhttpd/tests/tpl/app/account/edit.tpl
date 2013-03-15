{{extend _main}}
<a href="/app/shopping">Shopping</a> | <a href="/app/cart">Cart</a> | <a href="/app/account">My account</a> | <a href="/app/logout">Logout</a>
<hr>
<H1>My account</H1>
{{if errtext}}<span style="color:red; font-weight:bold; border:2px solid red; padding:5px; background-color:#FCC;">{{= errtext}}</span><p>{{endif}}
<form method="post">
<table>
<tr><td>User name</td><td>{{= user}}</td></tr>
<tr><td>Name</td><td><input name="name" value="{{= name}}"></td></tr>
<tr><td>Password</td><td><input type="password" name="password1" value=""></td></tr>
<tr><td>Repeat password</td><td><input type="password" name="password2" value=""></td></tr>
<tr><td>&nbsp;</td><td><input type="submit" name="save" value="Save"></td></tr>
</table>
</form>