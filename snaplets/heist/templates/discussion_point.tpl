<div class="discussion-point" style="border:0px;margin:0px;margin-left:20px;margin-bottom:20px;margin-top:20px;">

  <div class="discussionPointPart" style="background-color:rgba(40,60,200,0.05);border:0px black;border-style:solid;padding:5px">
    <div>
      <dpText/>
    </div>
    <hr/>
    <span>
      <a href="${authorLink}">
	<dpAuthor/>
      </a> 
      <dpTime/> 
      <ifLoggedIn>
	<apply template="new_discussion_link">Reply
	</apply> 
      </ifLoggedIn>
    </span>

  </div>

  <div class="discussionChildResponseForm">
    <apply template="_discussion_form"></apply>
  </div>

  <div class="sub-discussions" style="border:0px solid;border-left:4px solid;border-color:rgb(150,190,220);">
    
    <subDiscussions>
      <subDiscussion/>
    </subDiscussions>
    
  </div>
</div>
