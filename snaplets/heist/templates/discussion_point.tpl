<div class="discussion-point" style="border:0px;border-bottom:1px solid;border-color:black;background-color:rgba(40,60,200,0.1);margin:0px;margin-left:20px;margin-bottom:20px;">
  <p><dpText/></p>
  <p style="text-align:right"><dpAuthor/> <dpTime/> <apply template="new_discussion_link">REPLY</apply></p>
  
  <div class="replyFormDiv" style="display:none" discussionid="${discussionId}">
  <textarea rows="4" cols="60" name="dpText" class="proseBox"/>
  <br/>
  <select name="posterIdSelect" class="posterIdSelect">
    <option value="${userName}"><userName/></option>
    <option value="">Anonymous</option>
  </select>
  <br/>
  <input type="submit"/>
  </div>

  <div class="sub-discussions" >
    
    <subDiscussions>
      <subDiscussion/>
    </subDiscussions>
    
  </div>
</div>
