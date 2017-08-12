<apply template="base">

  <script>
    function isEmail(email){
      var regex = /^([a-zA-Z0-9_.+-])+\@(([a-zA-Z0-9-])+\.)+([a-zA-Z0-9]{2,4})+$/;
      return regex.test(email);
    }
    function submitRequest(){
       var email = $("#email").val();
       var info  = $("#email-info");
       if (isEmail(email)) {
         info.html('Sending...');
         $.ajax({url: "/reset",
              method: "POST",
              data: {email: email},
              success: function( result ) {
                 info.html("Success! Check your email.");
                 console.log(result);
              },
              error: function (resp, status, err) {
                 info.html("Something went wrong");
              }
            });
       } else {
         info.html("Please enter a valid email address");
       };
       };
  </script>

  <div style="margin:0 auto; width:75%;">

    <form method="POST" action="/reset" class="form-horizontal">

      <div class="alert alert-info" role="alert" id="email-info">
          Enter the email address you used to sign up. If it&apos;
          in the system, we&apos;ll email you a password reset link.
      </div>
      <div class="form-group">
        <label class="control-label col-sm-2">Email</label>
        <div class="col-sm-8">
          <input type="text" name="email" id="email" size="20" class="form-control"/>
        </div>
        <div class="col-sm-2">
          <button type="button" value="s2" onClick="submitRequest()"
                  class="form-control">Submit</button>
        </div>
      </div>

    </form>

  </div>

</apply>
