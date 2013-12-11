<nav>
<ul>
  <li><a href="/">Home</a></li>
  <ifLoggedIn>
     <li><a><loggedInUser/></a></li>
     <li><a href="/logout">Logout</a></li>
  </ifLoggedIn>
  <ifLoggedOut>
     <li><a href="login">Login</a></li>
  </ifLoggedOut>
</ul>
</nav>
