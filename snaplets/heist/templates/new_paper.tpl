<div id="new_paper">

  <div class="form-group">
    
    <h2>Submit a Paper</h2>


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
	  <div class="form-group">
            <label ref="docTags" for="docTags" class="col-sm-2 control-label">Field Tags</label>
            <div class="col-sm-10">
              <dfInputText ref="docTags" size="80" class="form-control" placeholder="Math.Statistics, Philosophy.PhilosophyOfScience"/>
            </div>
	  </div>
	  <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
              <dfInputSubmit value="Enter" class="btn btn-default"/>
            </div>
	  </div>
	</dfForm>
 

 

 
	<a class="testButton">Test</a> <input type="text" size="50" class="testText"/>
	

    
  </div>

  <button class="toggleFieldTags btn btn-default">Available Field Tags</button>
  <p class="field_tags"><fieldTags/></p>
  
  <script>

  function opensearch(data) { 
    if (data && typeof data === 'object'){
      console.log( "data is ok." );
      $(".doiSuccess").show();
      return 0;
    };
    $(".doiError").show();
  };

  $(document).ready(function(){

    $(".toggleFieldTags").click(function(){
      $(".field_tags").toggle();
    });

    $(".testButton").click(function(){
      $(".testText").val("Hi");
    });

    var r = $(".doiButton").click(function(){

      $.ajax({

        crossDomain: true,


        dataType: 'jsonp',
        url: "http://nurture.nature.com/cgi-bin/opensearch?db=crossref&out=jsonp&q=" + "10.1159/000072442",


        success: function(data){
           console.log( "Success" );
        },
        failure: function(data){
           console.log( "Failure" );
        }

      });

    });

  });


  </script>


</div>
