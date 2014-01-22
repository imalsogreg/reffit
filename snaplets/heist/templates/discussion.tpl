<apply template="base">

  <div class="discussionHeader">
    Discussion <discussionReNode/>
  </div>

  <form method="POST">

    <ifLoggedIn>
      <p><span class="dummySpanToMakeDepthFromParentMatchJQueryScriptExpectation">
	  <apply template="new_discussion_link">Add Point</apply>
      </span></p>
      <div class="replyFormDiv" discussionid="">
	
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
    <input type="text" name="parentId" class="parentid" value="" style="display:none"/>
    <input type="text" name="posterId" class="posterIdFinal" value="${userName}" style="display:none"/>

    <discussionNodes>
      <discussionNode/>
    </discussionNodes>

  </form>

  <script>
    $(document).ready(function() {
     var allProseBox = $(".proseBox");
     var posterIdFinal = $(".posterIdFinal");
     $(".raiseDiscussionInput").click(function(){
       var replyFormDiv = $( ".replyFormDiv" );
       var proseBox     = $( ".proseBox"     );
       var parentIdBox  = $( ".parentid"     );
       replyFormDiv.hide();
       proseBox.val("");
       $(this).parent().parent().parent().children(".replyFormDiv").show();
       var pId = $(this).parent().parent().parent().children(".replyFormDiv").attr("discussionid");
       parentIdBox.val( pId );
     });
     $(".posterIdSelect").click(function(){
      posterIdFinal.val( $(this).val() );
      $(".posterIdSelect").val( $(this).val() );
     });
    });
  </script>

</apply>
