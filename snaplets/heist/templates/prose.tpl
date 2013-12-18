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

<div class="prose-poster">
  <p>
    <a href="${prosePosterDestination}">
      <prosePoster/>
    </a>
  </p>
  <p>
    <apply-content/> <!-- This brings in the Re: dimension tag -->
  </p>
</div>


</div>
