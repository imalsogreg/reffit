<div class="panel panel-success">
  <div class="panel-heading"><h4 class="panel-title">Filter settings</h4></div>
  <div class="panel-body">
    
    <ifLoggedIn>
      <form method="GET">
	<ul>
          <fieldTags>
	    <fieldTag>
              
              <div class="row form-horizontal">
		
                <div class="col-sm-2">
	          <input type="checkbox" name="filterTag.${fieldTagFullText}" class="form-control"><fieldTagLabel/>
                </div>
		
                <div class="col-sm-8">
                  <span class="form-control"><fieldTagText/></span>
                </div>
		
                <div class="col-sm-2">
	          <a href="/delete_usertag/${fieldTagFullText}">
		    <button type="button" class="close form-control">&times;</button>
	          </a>
                </div>
		
	      </div>
	      
	    </fieldTag>
          </fieldTags>
	</ul>
        
	<div class="row form-horizontal">
          
	  
          <script>
            $(document).ready(function(){
            $('.addTagBtn').click(function(){
            var r = $('#fieldTagsHidden').val();
            window.location.href = "/add_usertag/" + encodeURIComponent(r);
            return false;
            });
            });
	    
          </script>
	  
	  <!-- The old way.
	       <input type="text" class="col-sm-offset-2 col-sm-6 control-label addTagText"/>
	       <div class="col-sm-4"><a class="addTagBtn">
		   <button class="btn btn-default form-control">Add</button></a>
	       </div>  -->
	  
	  <!-- Dirty hack coming.  tree.js code assumes what we do w/ a tag tree is
	       to populate a new paper's fieldTagsHidden and fieldTagsView fields.
	       Here, I'm going to make fields by the same name, so that tree.js will
	       fill them w/ field labels on tree clicks. -->
	  
	  <div class="form-group">
	    <div class="col-sm-8">
	      <input type="text" class="col-sm-5 control-label" id="fieldTagsView"/>
	    </div>
	    <div class="col-sm-4"><a class="addTagBtn">
		<button class="btn btn-default form-control">Apply</button></a>
	    </div>
            
	  </div>
	  
	  <div class="form-group" style="display:none;">
	    <input type="text" id="fieldTagsHidden"/>
	  </div>
	  
	  <div class="form-group">
	    <div class="col-sm-12">
	      <div class="tree">
		<tagsButton/>
	      </div>	     
	    </div>
	  </div>
	  
	</div>
        
	<select class="form-control" name="sortBy">
          <option>New</option>
          <option>Hot</option>
          <option>Popular</option>
          <option>Controversial</option>
	</select>
	
	<button type="submit" class="btn btn-default">Sort</button>
      </form>
      
      
    </ifLoggedIn>
    

    <ifLoggedOut>
      <form class="form-horizontal">

	<div class="form-group">
	  <div class="col-sm-12">
	    <div class="tree">
	      <tagsButton/>
	    </div>	     
	  </div>
	</div>
	  
	<div class="form-group" style="display:none;">
	<input type="text" class="form-control" id="fieldTagsHidden" value=""/>
	</div>

	<div class="form-group col-sm-offset-2 col-sm-12">
	  <input type="text" name="filterTag" id="fieldTagsView" value="" class="form-control" />
	</div>

	<select class="form-control col-sm-12" name="sortBy">
          <option>New</option>
          <option>Hot</option>
          <option>Popular</option>
          <option>Controversial</option>
	</select>
	
	<button type="submit" class="btn btn-default">Sort</button>


      </form>
    </ifLoggedOut>

    
    
  </div>
  
</div>
