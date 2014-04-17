<div class="prose_div">

<div class="up_down_vote_count">
  <table>
    <tr> <td> 
	<a href="${upBtnUrl}">
	  <div class="arrow-up ${upBtnHighlight}"/> 
	</a>
    </td> </tr>
    <tr> <td> 
	<span style="color:green"><upCount/></span>/<span style="color:red"><downCount/></span>
    </td> </tr>
    <tr> <td> 
	<a href="${downBtnUrl}">
	  <div class="arrow-down ${downBtnHighlight}"/> 
	</a>
    </td> </tr>
  </table>
</div>

<div class="prose-text">
  <p><proseText/></p>
</div>

<reBlock>
  <p>
    Re: <span class="label label-default"><critiqueDim/></span>     
  </p>
</reBlock>



<div class="prose-poster">
  <p>  
    <a href="${prosePosterDestination}">
      <prosePoster/>
    </a> <proseTimeSince/>
    <editBlock>
      <button class="btn btn-default btn-xs">
      <a href="${editURL}">Edit <span class="glyphicon glyphicon-pencil"></span></a>
      </button>
    </editBlock>
    <button class="btn btn-default btn-xs">
      <a href="${discussionUrl}">Discuss (<nDiscussionPoints/>)
	<span class="glyphicon glyphicon-comment"></span>
      </a>
    </button>
  </p>

</div>


</div>
