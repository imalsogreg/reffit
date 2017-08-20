<div id="new_paper">

  <div class="form-group">
    <h3 style="font-family:helvetica">Find Paper by DOI</h3>
	<div class="row form-horizontal">
	    <label ref="doi" for="doi" class="col-sm-2 control-label">DOI</label>

	    <div class="col-sm-8">
	      <input type="text" size="20" class="form-control doiText"/>
	    </div>

	    <div class="col-sm-2">
	      <button type="button" onClick="getDoiInfo()"
         class="btn btn-default form-control doiButton">
           Apply
         </button>
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
              <dfInputSelect ref="poster" class="form-control" value="${poster}"/>
            </div>
	  </div>

	  <hr/>

	  <div class="form-group control-title">
            <label ref="title" for="title" class="col-sm-2 control-label">Title</label>
            <div class="col-sm-10">
              <dfInputText id="title" ref="title" size="50" class="form-control" value="${title}"/>
            </div>
	  </div>
	  <div class="form-group control-authors">
            <label ref="authors" for="authors" class="col-sm-2 control-label">Authors</label>
            <div class="col-sm-10">
              <dfInputText id="authors" ref="authors" size="50" class="form-control" value="${authors}"/>
            </div>
	  </div>
	  <div class="form-group control-link">
            <label ref="link" for="link" class="col-sm-2 control-label">Link to Article</label>
            <div class="col-sm-10">
              <dfInputText id="link" ref="link" size="50" class="form-control" value="${url}"/>
            </div>
	  </div>
	  <div class="form-group">
            <label ref="docClass" for="docClass" class="col-sm-2 control-label">Article Type</label>
            <div class="col-sm-10">
              <dfInputSelect ref="docClass" class="form-control" value="${docclass}"/>
            </div>
	  </div>
	  
	  <!-- Hide this form group.  JS of field tags selector will invisibly write to it. -->
	  <div class="form-group" style="display:none;">
            <label ref="docTags" for="docTags" class="col-sm-2 control-label">Field Tags</label>
            <div class="col-sm-10">
              <dfInputText ref="docTags" size="80" class="form-control" id="fieldTagsHidden" value="${doctags}"/>
            </div>
	  </div>

	  <hr style="color:black;"/>
	  
	  <div class="form-group">
	    <label class="col-sm-2 control-label">Field Tags</label>
	    <div class="col-sm-8">
	      <input  class="form-control disabledInput" id="fieldTagsView" disabled value="${doctagsvisible}"/>
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



</div>
