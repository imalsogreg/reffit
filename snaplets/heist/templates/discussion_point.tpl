<div class="discussion-point" style="border:0px;margin:0px;margin-left:20px;margin-bottom:20px;">

  <div class="discussionPointPart" style="background-color:rgba(40,60,200,0.1);">
  <p><dpText/></p>
  <p style="text-align:right"><a href="${authorLink}"><dpAuthor/></a> <dpTime/> <ifLoggedIn><apply template="new_discussion_link">Reply</apply> </ifLoggedIn></p>
  </div>


  <div class="replyFormDiv" style="display:none" discussionid="${discussionId}">
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
