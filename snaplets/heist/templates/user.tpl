<apply template="base">

  <div class="user-info">
    
    <div class="user-top-name">
      <userName/>
    </div>

    <div class="user-top">
      <ifLoggedIn> 
	<a href="/toggle_following/${userName}">
	  <button class="follow">
	    <span>
	      <followButtonText/>
	    </span>
	  </button>
	</a>
      <h1>

    </div>

    <ul class="nav nav-tabs">
      <li><a href="user_activity"  data-toggle="tab">Activity</a></li>
      <li><a href="user_pinboard"  data-toggle="tab">
	  Pinboard <span class="badge"><nPinboard/></span>
      </a></li>
      <li><a href="user_following" data-toggle="tab">
	  Following <span class="badge"><nFollowing/></span>
      </a></li>
      <li><a href="user_followers" data-toggle="tab">
	  Followers <span class="badge"><nFollowers/></span>
      </a></li>
    </ul>
    
    <div class="tab-pane active" id="Activity">
      <activities>
	<userEvent/>
      </activities>
    </div>
    
    <div class="tab-pane" id="Pinboard">
      <pinboard>
	<apply template="paperBlock"/>
      </pinboard>
    </div>
    
    <div class="tab-pane" id="Following">
      <following>
	<apply template="userBlock"/>
      </following>
    </div>
    
    <div class="tab-pane" id="Followers">
      <followers>
	<apply template="userBlock"/>
      </followers>
    </div>
    
    
  </div>
  
</apply>
