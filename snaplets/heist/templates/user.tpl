<apply template="base">

  <div class="user-info">
    
    <div class="user-top-name">
      <h1><userName/> 
	
	<followButton>
	  <span class="user-top">
	    <ifLoggedIn> 
	      <a href="/${followBtnLink}">
		<button class="follow btn btn-success">
		  <span>
		    <followBtnText/>
		  </span>
		</button>
	      </a>
	    </ifLoggedIn>
	  </span>
	</followButton>
	
      </h1>
    </div>

    <ul class="nav nav-tabs">
      <li><a href="#activity"  data-toggle="tab">Activity</a></li>
      <li><a href="#pinboard"  data-toggle="tab">
	  Pinboard <span class="badge"><nPinboard/></span>
      </a></li>
      <li><a href="#following" data-toggle="tab">
	  Following <span class="badge"><nFollowing/></span>
      </a></li>
      <li><a href="#followers" data-toggle="tab">
	  Followers <span class="badge"><nFollowers/></span>
      </a></li>
    </ul>
    
    <div class="tab-content">
    <div class="tab-pane active" id="activity">
      <activities>
	<ul>
	  <li>Event 1</li>
	  <li>Event 2</li>
	</ul>
	<userEvent/>
      </activities>
    </div>
    
    <div class="tab-pane" id="pinboard">
      <pinboard>
	<apply template="_paper_roll"/>
      </pinboard>
    </div>
    
    <div class="tab-pane" id="following">
      <following>
	<apply template="userBlock"/>
      </following>
    </div>
    
    <div class="tab-pane" id="followers">
      <followers>
	<apply template="userBlock"/>
      </followers>
    </div>
    
    </div>
  </div>
  
</apply>
