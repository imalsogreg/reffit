<link rel="stylesheet" type="text/css" href="/static/media/css/article_view.css"/>

<nav class="navbar navbar-default" role="navigation">
 <div class="navbar-header">
   <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="navbar-collapse-2">
     <span class="sr-only">Toggle Article Navigation</span>
     <span class="icon-bar"></span>
     <span class="icon-bar"></span>
     <span class="inoc-bar"></span>
   </button>
   <a class="navbar-brand"><docType/></a>
 </div>
 
 <div class="collapse navbar-collapse" id="navbar-collapse-2">
   <ul class="nav navbar-nav">
     <li class="${overviewIsActive}" id="overviewButton"><a>Overview</a></li>
     <li class="${summariesIsActive}" id="summariesButton">
	 <a>Summaries <span class="badge"><nSummaries/></span></a></li>
     <li class="${praiseIsActive}" id="praiseButton">
	 <a>Praise <span class="badge"><nPraise/></span></a></li>
     <li class="${criticismsIsActive}" id="criticismsButton">
	 <a>Criticisms <span class="badge"><nCriticisms/></span></a></li>
   </ul>
 </div> <!-- navbar-collapse -->
</nav>

<div class="article_content">

  <script>
    $(document).ready( function(){

      $("#overviewButton").click( function() {
        $("#summaries-div").attr('class','col-md-4');
        $("#praise-div").attr('class','col-md-4');
        $("#criticisms-div").attr('class','col-md-4');
       });

      $("#summariesButton").click( function() {
        $("#summaries-div").attr('class','');
        $("#praise-div").attr('class','hide');
        $("#criticisms-div").attr('class','hide');
      });

      $("#praiseButton").click( function () {
        $("#summaries-div").attr('class','hide');
        $("#praise-div").attr('class','');
        $("#criticisms-div").attr('class','hide');
      });

      $("#criticismsButton").click( function () {
        $("#summaries-div").attr('class','hide');
        $("#praise-div").attr('class','hide');
        $("#criticisms-div").attr('class','');
      });

    });
</script>

    <div class="article_view_header">
      Article Top (Title)
    </div>

  <div class="row">
    
    <div class="col-sm-4" id="summaries-div">
      <div class="article_summaries">
	<h3>Summaries 
          <a href="/new_summary/${docId}">
            <button type="button" class="btn btn-primary btn-xs">New</button>
        </a></h3>
	<articleSummaries>
	  <apply template="summary"/>
	</articleSummaries>
      </div>
    </div>

    <div class="col-sm-4" id="praise-div">
      <div class="article_praise">
	<h3>Praise  <a href="/new_praise/${docId}"><button type="button" class="btn btn-success btn-xs">New</button></a></h3>
	<articlePraise>
	  <apply template="articlePraise"/>
	</articlePraise>
      </div>
    </div>

    <div class="col-sm-4" id="criticisms-div">
      <div class="article_criticisms">
	<h3>Criticism <a href="/new_criticism/${docId}"><button type="button" class="btn btn-warning btn-xs">New</button></a></h3>
	<articleCriticisms>
	  <apply template="articleCriticisms"/>
	</articleCriticisms>
      </div>
    </div>

  </div>

</div>
