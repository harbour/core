{{extend _main}}
<H1>Login</H1>
{{if errtext}}<span style="color:red; font-weight:bold; border:2px solid red; padding:5px; background-color:#FCC;">{{= errtext}}</span><p>{{endif}}
<form method="POST">
<table style="layoutgrid">
<tr><td>User</td><td><input type="text" name="user"></td></tr>
<tr><td>Password</td><td><input type="password" name="password"></td></tr>
<tr><td></td><td><input type="submit" name="login" value="Login"></td></tr>
</table>
</form>
<p>
<a href="/app/register">Create new account</a>
