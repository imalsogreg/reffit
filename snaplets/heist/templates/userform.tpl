<div style="margin: 0 auto; width: 50%">

<form method="post" action="${postAction}" class="form-horizontal">

  <div class="form-group">
    <label class="control-label col-sm-4">Login</label>
    <div class="col-sm-8">
      <input type="text" name="login" size="20" class="form-control" />
    </div>
  </div>
  
  <div class="form-group">
    <label class="control-label col-sm-4">Email</label>
    <div class="col-sm-8">
      <input type="text" name="email" size="20" class="form-control"/>
    </div>
  </div>

  <div class="form-group">
    <label class="control-label col-sm-4">Password</label>
    <div class="col-sm-8">
      <input type="password" name="password" size="20" class="form-control" />
    </div>
  </div>

  <div class="form-group">
    <div class="col-sm-offset-4 col-sm-4">
      <input type="submit" value="${submitText}" class="form-control" />
    </div>
  </div>

</form>

</div>
