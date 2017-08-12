<apply template="base">

  <div style="margin:0 auto; width: 75%;">
    <h3>Password reset</h3>
    <form  method="POST" action="/reset/${token}" class="form-horizontal">

      <div class="form-group">
        <label class="control-label col-sm-4">Email</label>
        <div class="col-sm-8">
          <input type="text" name="email" size="20" class="form-control"/>
        </div>
      </div>
    

      <div class="form-group">
        <label class="control-label col-sm-4">New Password</label>
        <div class="col-sm-8">
          <input type="password" name="password" size="20" class="form-control"/>
        </div>
      </div>

      <div class="form-group">
        <label class="control-label col-sm-4">Repeat Password</label>
        <div class="col-sm-8">
          <input type="password" name="password2" size="20" class="form-control"/>
        </div>
      </div>
    
      <div class="form-group">   
        <div class="col-sm-offset-4 col-sm-4">
          <button type="submit" value="Sumbit" class="form-control">Submit</button>
        </div>
      </div>

    </form>
  </div>

</apply>