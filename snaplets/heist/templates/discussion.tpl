<apply template="base">

  <div class="discussionHeader">
    Discussion <discussionReNode/>
  </div>

  <form method="POST">
    <input type="textarea" name="dpText"/>
<!--    <input type="text" id="paperid"   value="${paperid}"/>
    <input type="text" id="commentid" value="${commentid}"/> -->
    <input type="text" name="docId" value="10"/>
    <input type="submit"/>
  </form>

  <discussionNodes>
    <discussionNode/>
  </discussionNodes>

</apply>
