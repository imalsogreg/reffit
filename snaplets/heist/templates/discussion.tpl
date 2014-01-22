<apply template="base">

  <div class="discussionHeader">
    Discussion <discussionReNode/>
  </div>

  <form method="POST">

    <p><apply template="new_discussion_link">Add Point</apply></p>

    <div class="replyFormDiv" discussionid="">
      <input type="textarea" name="dpText" class="proseBox" value="TEXT AREA"/>
      <input type="submit"/>
    </div>
    <input type="text" name="docId" value="${docid}"/>
    <input type="text" name="commentId" value="${commentid}"/>
    <input type="text" name="parentId" class="parentid" value=""/>

    <discussionNodes>
      <discussionNode/>
    </discussionNodes>

  </form>

  <script>
    $(document).ready(function() {
     var allProseBox = $(".proseBox");
     $(".raiseDiscussionInput").click(function(){
       var replyFormDiv = $( ".replyFormDiv" );
       var proseBox     = $( ".proseBox"     );
       var parentIdBox  = $( ".parentid"     );
       replyFormDiv.hide();
       proseBox.val("");
       $(this).parent().parent().children(".replyFormDiv").show();
       var pId = $(this).parent().parent().children(".replyFormDiv").attr("discussionid");
       parentIdBox.val( pId );
     });
    });
  </script>

</apply>
