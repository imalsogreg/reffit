<div class="panel panel-success">
  <div class="panel-heading"><h4 class="panel-title">Filter settings</h4></div>
  <div class="panel-body">
    <form method="GET">
      
      <ifLoggedIn>
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
          
	  <input type="text" class="col-sm-offset-2 col-sm-6 control-label addTagText"/>
          
          <script>
          $(document).ready(function(){
            $('.addTagBtn').click(function(){
              var r = $('.addTagText').val();
              window.location.href = "/add_usertag/" + encodeURIComponent(r);
              return false;
            });
          });

          </script>

	  <div class="col-sm-4"><a class="addTagBtn">
	      <button class="btn btn-default form-control">Add</button></a>
	  </div>
          
	</div>
        
 
  
      </ifLoggedIn>
      
      <ifLoggedOut>
        <input type="text" name="filterTag" placeholder="Biology.Neuroscience" class="form-control">
      </ifLoggedOut>
      
      <select class="form-control" name="sortBy">
        <option>New</option>
        <option>Hot</option>
        <option>Popular</option>
        <option>Controversial</option>
      </select>
      
      <button type="submit" class="btn btn-default">Sort</button>
      
    </form>
    
  </div>
  
</div>
