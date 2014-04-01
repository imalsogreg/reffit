<apply template="base">

  <style>
    #content{
     background-color:#cccccc;
     background-attachment:fixed;
     background-image: url("static/media/img/plasma.png");
    }

    .tools-spacer-center{
    width:20px;
    background-color:green;
    }
    .tools-spacer-sides{
    width:30px;
    background-color:red;
    }

    #tools-div{
    }
    #tools-div .tools-col{
    margin:0px;
    padding:0px;
    float:left;
    }

    .githubSprite{
    width:200px;
    height:200px;
    display:block;
    background-image: url("static/media/img/githubPic.png");
    }
    .githubSprite:hover{
    background-image: url("static/media/img/githubPicWhite.png");
    }

    .haskellSprite{
    width:234px;
    height:200px;
    display:block;
    background-image: url("static/media/img/haskellPic.png");
    }
    .haskellSprite:hover{
    background-image: url("static/media/img/haskellPicWhite.png");
    }

    .snapSprite{
    width:200px;
    height:200px;
    display:block;
    background-image: url("static/media/img/snapPic.png");
    }
    .snapSprite:hover{
    background-image: url("static/media/img/snapPicWhite.png");
    }

  </style>

  <!-- Right side nav -->
  <div>
    
  </div>

  <!-- Reffit brand banner -->
  <div>
    <!--    <apply template="big-banner"/> -->
    <h1>Big banner here</h1>
  </div>

  <!-- Goals -->
  <div>
    <div class="splash-list">

      <div class="splash-list-item">
	<div class="splash-list-icon">
	</div>
	<div class="splash-list-content">
	  <h2>Goal 1</h2>
	  <p>Sub-text 1 is a sentence about the goal</p>
	</div>
      </div>

      <div class="splash-list-item">
	<div class="splash-list-icon">
	</div>
	<div class="splash-list-content">
	  <h2>Goal 2</h2>
	  <p>Sub-text 2 is a sentence about the goal</p>
	</div>
      </div>

      <div class="splash-list-item">
	<div class="splash-list-icon">
	</div>
	<div class="splash-list-content">
	  <h2>Goal 3</h2>
	  <p>Sub-text 3 is a sentence about the goal</p>
	</div>
      </div>

    </div>
  </div>

  <!-- Community  -->
  <div>
    
  </div>

  <!-- Privacy -->
  <div>
    
  </div>

  <!-- Tools -->
  <div id="tools-div">
    <div class="row">
      <div class="tools-col tools-spacer-sides">a</div>
      <div class="tools-col">
	Col 1
	<div class="githubSprite"/>
      </div>
      <div class="tools-col tools-spacer-center"></div>
      <div class="tools-col">
	Col 2
	<div class="haskellSprite"/>
      </div>
      <div class="tools-col tools-spacer-center"></div>
      <div class="tools-col">
	Col 3
	<div class="snapSprite"/>
      </div>
      <div class="tools-col tools-spacer-sides">b</div>
    </div>
  </div>

  <!-- Wall of links -->
  <div>
    <!-- e.g. - about, careers, blog, contact, etc -->
  </div>

</apply>
