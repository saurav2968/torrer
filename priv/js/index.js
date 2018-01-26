
// called when submit button clicked for a file
function new_request(){
    console.log("New request");
    var file  = document.getElementById("torrent_file")
    if(file.value){
        console.log("Processing file: " + file.value);
        // make a POST request on REST
        $.ajax({
          url:"/rest/new",
          type:"POST",
          data:file.value,
          contentType:"application/json; charset=utf-8",
          success: function(){
            console.log("Success in posting");
            window.location = "/";
          }
        });
    }
    else{
        alert("Please choose a file!");
    }
}

