<div id="new_paper">

  <div class="form-group">
    
    <h2>Submit a Paper</h2>
    
    <dfForm class="new_paper_form" role="form-group">
      
      <dfChildErrorList ref=""/>
      
      <label ref="poster" for="poster">Post as: </label>
      <dfInputSelect ref="poster" class="form-control"/>
      <br>
      
      <label ref="title">Title: </label>
      <dfInputText ref="title" size="50" class="form-control"/>
      <br>
      
      <dfLabel ref="authors">Authors: </dfLabel>
      <dfInputText ref="authors" size="50"/>
      <br>
      
      <dfLabel ref="link">Link to Article: </dfLabel>
      <dfInputText ref="link" size="50"/>
      <br>
      
      <dfLabel ref="docClass">Article Type:</dfLabel>
      <dfInputSelect ref="docClass"/>
      <br>
      
      <dfLabel ref="docTags">Field Tags:</dfLabel>
      <dfInputText ref="docTags" size="80"/>
      <br>
      
      <dfInputSubmit value="Enter"/>
      
    </dfForm>
    
  </div>
  
  <script>
  $(document).ready(function(){
    $(".toggleFieldTags").click(function(){
      $(".field_tags").toggle();
    });
  });
  </script>

  <button class="toggleFieldTags">Available Field Tags</button>
  <p class="field_tags"><fieldTags/></p>

</div>
