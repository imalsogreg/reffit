<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
  <!-- Brand and toggle get grouped for better mobile display -->
  <div class="navbar-header">
    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar-collapse-1">
      <span class="sr-only">Toggle navigation</span>
      <span class="icon-bar"></span> <!-- why these spans? -->
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
    </button>
    <a class="navbar-brand" href="/">Reffit (Alpha)</a>
  </div>
  
  <div class="collapse navbar-collapse" id="navbar-collapse-1" style="padding:0px 50px 10px 50px">
    <ul class="nav navbar-nav">
      
    </ul>
    <form class="navbar-form navbar-left" role="search" action="/search/" method="GET">
      <div class="form-group">
	<input type="text" class="form-control" placeholder="Search" name="q"/>
      </div>
      <button type="submit" class="btn btn-default">Submit</button>
    </form>
    <ul class="nav navbar-nav navbar-right">
      <ifLoggedIn>
	<li><a href="/new_article" class="${newArticleIsActive}">
	    <button type="button" class="btn btn-default btn-xs"><span class="glyphicon glyphicon-plus"></span> Add Article</button>
	</a></li>
      </ifLoggedIn>
      <li><a href="/about" class="${aboutIsActive}">About</a></li>
      <ifLoggedOut>
	<li><a href="/new_user" class="${newUserIsActive}">Sign Up
	</a></li>
	<li><a href="/login" class="${loginIsActive}">Log In
	</a></li>
      </ifLoggedOut>
      <ifLoggedIn>
	<li><a href="/user/${loggedInUser}" class="${userIsActive}">
	    <loggedInUser/> (<userRep/>)
	</a></li>
	<li><a href="/logout">Logout</a></li>
      </ifLoggedIn>
    </ul>
  </div>
</nav>
