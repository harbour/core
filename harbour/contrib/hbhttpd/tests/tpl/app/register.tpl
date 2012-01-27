{{extend _main}}
<a href="/app/login">Login</a>
<hr>
<H1>Create new account</H1>
{{if errtext}}<span style="color:red; font-weight:bold; border:2px solid red; padding:5px; background-color:#FCC;">{{= errtext}}</span><p>{{endif}}
<form method="post">
<table>
<tr><td>User name</td><td><input name="user" value="{{= user}}"></td></tr>
<tr><td>Name</td><td><input name="name" value="{{= name}}"></td></tr>
<tr><td>Password</td><td><input type="password" name="password1" value=""></td></tr>
<tr><td>Repeat password</td><td><input type="password" name="password2" value=""></td></tr>
<tr><td>&nbsp;</td><td><input type="submit" name="register" value="Create new account"></td></tr>
</table>
</form>