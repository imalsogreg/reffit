<apply template="base">

  <dfForm class="form-group form-horizontal">

    <div class="form-group">
      <dfLabel ref="poster" class="col-sm-2 control-label">Post as: </dfLabel>
      <div class="col-sm-8">
	<dfInputSelect ref="poster" class="form-control"/>
      </div>
    </div>

    <reBlock>  
      <!-- Put this part in a splice so that it can be chopped out -->
      <!-- by the handler for new summaries (summaries don't have  -->
      <!-- a dimension                                             -->
      <div class="form-group">
	<dfLabel ref="dimension" class="col-sm-2 control-label">About: </dfLabel>
	<div class="col-sm-8">
	  <dfInputSelect ref="dimension" class="form-control"/>
	</div>
      </div>
    </reBlock>

    <!-- Pass the catch-logout time delay to jquery through a splice -->
    <div class="timeout_div">
      <timeout_secs/>
    </div>

    <script>
      $(document).ready(function(){
      $('.timeout_alert').hide();
      var timeoutSecs = $('.timeout_div').text();
      $('.timeout_alert').slideUp( 300 ).delay( timeoutSecs*1000 ).fadeIn(400);
      $('.submit-button').prop('disabled',true).delay( timeoutSecs*1000 );
      });
    </script>

    <div class="alert alert-info timeout_alert">
      <p>Sorry!  It seems you have been logged out since visiting the last page.  Please copy your comment so you don't lose your work, and log back in.</p>
    </div>

    <div class="form-group">
      <dfLabel ref="prose" class="col-sm-2 control-label">Comment: </dfLabel>
      <div class="col-sm-8">
	<dfInputTextArea ref="prose" rows="10" cols="50" class="form-control"/>
      </div>
    </div>
    
    <div class="form-group">
      <div class="col-sm-offset-2 col-sm-10 submit-button">
	<dfInputSubmit value="Enter"/>
      </div>
    </div>

  </dfForm>

</apply>
