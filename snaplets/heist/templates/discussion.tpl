<apply template="base">

  <div class="discussionHeader">
    Discussion of <discussionType/>: <discussionReNode/>
  </div>

  <ifLoggedIn>
    <apply template="new_discussion_link">Add Point
    </apply>
  </ifLoggedIn>

  <div class="discussionTopResponseForm">
    <apply template="_discussion_form"></apply>
  </div>

  <discussionNodes>
    <discussionNode/>
  </discussionNodes>

  <script>
    $(document).ready(function() {
     $(".discussionForm").hide();
     $(".raiseDiscussionInput").click(function(){
       var dpointid = this.getAttribute('data-id');
       console.log( dpointid );
       $(".discussionForm").hide();
       var mySelector = "form[data-id='" + dpointid + "']"
       $(mySelector).show();
     });
    });
  </script>

</apply>
