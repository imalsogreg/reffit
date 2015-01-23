<!-- TODO: fix.  this seems to trigger BEFORE login <p><loginError/></p> -->

<bind tag="postAction">/login</bind>
<bind tag="submitText">Login</bind>

<apply template="userlogin"/>

<div style="margin: 0px auto; width:40%">
<p style="text-align:right">
   Don't have an account yet? <a href="/new_user">Join us!</a>
</p>
</div>
