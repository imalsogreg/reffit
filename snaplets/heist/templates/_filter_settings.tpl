<div class="panel panel-success">
  <div class="panel-heading"><h4 class="panel-title">Filter settings</h4></div>

  <div class="panel-body">
    <form method="GET">
      
      <fieldTags>
	<ul>
	  <fieldTag>
	    <li>
	      <input type="checkbox" ref="filterTag"><fieldTagLabel/>
	      <a href="/${userName}/deleteFieldPath/${fieldTagText}">
		<button type="button" class="close">&times;</button>
	      </a>
	    </li>
	  </fieldTag>
	</ul>
      </fieldTags>
      
      <select class="form-control">
	<option>New</option>
	<option>Hot</option>
	<option>Popular</option>
	<option>Controversial</option>
      </select>
      
      <button type="submit" class="btn btn-default">Sort</button>
      
    </form>
  
  </div>

</div>
