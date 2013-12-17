<div id="new_paper">

<h2>Submit a Paper</h2>

<dfForm class="new_paper_form">

<dfChildErrorList ref=""/>

<dfLabel ref="poster">Post as: </dfLabel>
<dfInputSelect ref="poster"/>
<br>

<dfLabel ref="title">Title: </dfLabel>
<dfInputText ref="title" size="50"/>
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
