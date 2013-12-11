<link rel="stylesheet" type="text/css" href="/static/media/css/article_view.css"/>

<script>
$(document).ready( function(){
 $("#article_summaries_button").click( function() {
    $("#article_praise").hide();
    $("#article_criticisms").hide();
    $("#article_summaries").show();
  });

});
</script>

<div class="article_view">

<div class="article_view_header">
</div>

<div class="article_view_nav">

 <div id="article_summaries_button">
SummariesB
    <articleSummarySummary/>
 </div>

 <div id="article_praise_button">
PraiseB
   <articlePraiseSummary/>
 </div>

 <div id="article_criticism_button">
CriticismsB
   <articleCriticismSummary/>
 </div>

</div>

<div id="article_summaries">
Summaries
 <articleSummaries/>
</div>

<div id="article_praise">
Praise
 <articlePraise/>
</div>

<div id="article_criticism">
Criticism
 <articleCriticisms/>
</div>

</div>