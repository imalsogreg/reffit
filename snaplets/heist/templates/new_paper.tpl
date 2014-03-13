<div id="new_paper">

  <div class="form-group">
    
	<div class="row form-horizontal">
	    <label ref="doi" for="doi" class="col-sm-2 control-label">DOI</label>

	    <div class="col-sm-8">
	      <input type="text" size="20" class="form-control doiText" placeholder="10.1037/0003-066X.49.12.997"/>
	    </div>

	    <div class="col-sm-2">
	      <button class="btn btn-default form-control doiButton">Apply</button>
	    </div>
	</div>
	<br/>

	<div class="alert alert-danger doiError" style="display:none;">
	  <p>There was a problem with the DOI search.</p>
	</div>
	<div class="alert alert-success doiSuccess" style="display:none;">
	  <p>Found your paper</p>
	</div>

	<dfForm class="form-horizontal" role="form-group">
	  <dfChildErrorList ref=""/>
	  <div class="form-group">
            <label ref="poster" for="poster" class="col-sm-2 control-label">Post as</label>
            <div class="col-sm-10">
              <dfInputSelect ref="poster" class="form-control"/>
            </div>
	  </div>
	  <div class="form-group">
            <label ref="title" for="title" class="col-sm-2 control-label">Title</label>
            <div class="col-sm-10">
              <dfInputText ref="title" size="50" class="form-control" placeholder="The Earth is Round (p < .05)"/>
            </div>
	  </div>
	  <div class="form-group">
            <label ref="authors" for="authors" class="col-sm-2 control-label">Authors</label>
            <div class="col-sm-10">
              <dfInputText ref="authors" size="50" class="form-control" placeholder="Jacob Cohen"/>
            </div>
	  </div>
	  <div class="form-group">
            <label ref="link" for="link" class="col-sm-2 control-label">Link to Article</label>
            <div class="col-sm-10">
              <dfInputText ref="link" size="50" class="form-control" placeholder="http://www.unt.edu/rss/class/mike/5030/articles/Cohen1994.pdf‎"/>
            </div>
	  </div>
	  <div class="form-group">
            <label ref="docClass" for="docClass" class="col-sm-2 control-label">Article Type</label>
            <div class="col-sm-10">
              <dfInputSelect ref="docClass" class="form-control"/>
            </div>
	  </div>
	  
	  <!-- Hide this form group.  JS of field tags selector will invisibly write to it. -->
	  <div class="form-group" style="display:none;">
            <label ref="docTags" for="docTags" class="col-sm-2 control-label">Field Tags</label>
            <div class="col-sm-10">
              <dfInputText ref="docTags" size="80" class="form-control" id="fieldTagsHidden"/>
            </div>
	  </div>
	  
	  <div class="form-group">
	    <label class="col-sm-2 control-label">Field Tags</label>
	    <div class="col-sm-8">
	      <input  class="form-control disabledInput" id="fieldTagsView" disabled/>
	    </div>
	    <div class="col-sm-2">
	      <button class="btn btn-default btn-xs form-control btn-clear" href="#">Clear</button>
	    </div>	    
	  </div>
	  
	  <div class="form-group">
	    <div class="col-sm-offset-2 col-sm-8">
	      <div class="tree">
		<tagsButton/>
	      </div>	      
	    </div>
	    
            <div class="col-sm-2">
              <dfInputSubmit value="Enter" class="btn btn-default"/>
            </div>
	  </div>
	</dfForm>
	
  </div>

  <script>

  $(document).ready(function(){

    var r = $(".doiButton").click(function(){
      var doi = $('.doiText').val();
      window.location.href = "/new_article/" + encodeURIComponent(doi);
    });

  });


  </script>


</div>
