<div id="new_paper">

  <div class="form-group">
    
    <h2>Submit a Paper</h2>
    
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
    
  </div>
  
  <script>
  $(document).ready(function(){
    $(".toggleFieldTags").click(function(){
      $(".field_tags").toggle();
    });
  });
  </script>

  <button class="btn btn-default">Available Field Tags</button>
  <p class="field_tags"><fieldTags/></p>

</div>
