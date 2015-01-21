<div class="discussion-point" style="border:0px;margin:0px;margin-left:20px;margin-bottom:20px;margin-top:20px;">

  <div class="discussionPointPart" style="background-color:rgba(40,60,200,0.1);border:1px black;border-style:solid;padding:5px">
    <p>
      <dpText/>
      <span style="float:right">
	<a href="${authorLink}">
	  <dpAuthor/>
	</a> 
	<dpTime/> 
	<ifLoggedIn>
	  <apply template="new_discussion_link">Reply
	  </apply> 
	</ifLoggedIn>
      </span>
    </p>
  </div>

  <div class="replyFormDiv" style="margin:5px;margin-left:30px;display:none;" discussionid="${discussionId}">
    <textarea rows="4" cols="60" name="dpText" class="proseBox"/>
    <br/>
    <select name="posterIdSelect" class="posterIdSelect">
      <option value="${userName}"><userName/></option>
      <option value="">Anonymous</option>
    </select>
    <input type="submit"/>
    <br/>
  </div>

  <div class="sub-discussions" style="border:0px solid;border-left:4px solid;border-color:rgb(150,190,220);">
    
    <subDiscussions>
      <subDiscussion/>
    </subDiscussions>
    
  </div>
</div>
