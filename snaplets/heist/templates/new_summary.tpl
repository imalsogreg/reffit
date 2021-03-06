<apply template="base">


    <dfForm class="form-group form-horizontal">
      
      <dfChildErrorList ref=""/>
      
      <div class="form-group">
        <dfLabel ref="poster" class="col-sm-2 control-label">Post as: </dfLabel>
        <div class="col-sm-8">
          <dfInputSelect ref="poster" class="form-control"/>
        </div>
      </div>

      <div class="form-group">
        <dfLabel ref="prose" class="col-sm-2 control-label">Comment: </dfLabel>
        <div class="col-sm-8">
          <dfInputTextArea ref="prose" rows="10" cols="50" class="form-control"/>
        </div>

      </div>
      
      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-10">
          <dfInputSubmit value="Submit"/>
        </div>
      </div>

    </dfForm>


</apply>

