<apply template="base">

  <dfForm class="form-group form-horizontal">

    <div class="form-group">
      <dfLabel ref="poster" class="col-sm-2 control-label">Post as: </dfLabel>
      <div class="col-sm-8">
	<dfInputSelect ref="poster" class="form-control" value="${poster}" ${posterDisabled}/>
      </div>
    </div>

    <reBlock>  
      <!-- Put this part in a splice so that it can be chopped out -->
      <!-- by the handler for new summaries (summaries don't have  -->
      <!-- a dimension                                             -->
      <div class="form-group">
	<dfLabel ref="dimension" class="col-sm-2 control-label">About: </dfLabel>
	<div class="col-sm-8">
	  <dfInputSelect ref="dimension" class="form-control" value="${value}"/>
	</div>
      </div>
    </reBlock>
    
    <div class="form-group">
      <dfLabel ref="prose" class="col-sm-2 control-label">Comment: </dfLabel>
      <div class="col-sm-8">
	<dfInputTextArea ref="prose" rows="10" cols="50" class="form-control textarea">
	  <prose/>This is a test!
	</dfInputTextArea>
      </div>
    </div>
    
    <div class="form-group">
      <div class="col-sm-offset-2 col-sm-10">
	<dfInputSubmit value="Enter"/>
      </div>
    </div>

  </dfForm>

  <!--Hack because can't otherwise get default text into dfInputTextArea -->
  <p class="textLoad" hidden="true"><prose/></p>
  <script> 
    $(document).ready(function () {
      var defaultText = $('.textLoad').text();
      $('.textarea').text(defaultText);
    });
  </script>

</apply>
