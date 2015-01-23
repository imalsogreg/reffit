<form method="POST" class="discussionForm" data-id="${itemid}">

  <ifLoggedIn>

    <div class="replyFormDiv">      
      <textarea name="dpText" rows="4" cols="60" class="proseBox" value="TEXT AREA"/>
      <br/>
      <select name="posterIdSelect" class="posterIdSelect">
	<option value="${userName}"><userName/></option>
	<option value="">Anonymous</option>
      </select>
      <input type="submit"/>
    </div>
  </ifLoggedIn>
    
  <br/>
  <input type="text" name="docId" value="${docid}" style="display:none"/>
  <input type="text" name="commentId" value="${commentid}" style="display:none"/>
  <input type="text" name="parentId" class="parentid" value="${parentid}" style="display:none" />

</form>
