function getDoiInfo() {

    var doi = $('.doiText').val();

    $.ajax({url: encodeURI("/doi/" + doi)
            ,method: "GET"
            ,success: function(result){
                var r = JSON.parse(result);
                $('#title').val(r.titleHint);
                $('#authors').val(r.authorsHint);
                $('#link').val(r.linkHint);
            }
            ,error: function (resp, status, err){
                console.log(resp);
            }
           });

}
