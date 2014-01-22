<div class="discussion-point" style="border:4px solid;border-color:green;">
  <p><dpText/></p>
  <p style="text-align:right"><dpAuthor/> <dpTime/> <apply template="new_discussion_link">REPLY</apply></p>
  
  <div class="replyFormDiv" style="display:none" discussionid="${discussionId}">
  <input type="textarea" rows="2" cols="60" name="dpText" class="proseBox"/>
  <select name="posterId">
    <option value="${userName}"><userName/></option>
    <option value="">Anonymous</option>
  </select>
  <br/>
  <input type="submit"/>
  </div>

  <div class="sub-discussions" style="margin-left:20px">
    
    <subDiscussions>
      <subDiscussion/>
    </subDiscussions>
    
  </div>
</div>
