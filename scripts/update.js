var fs = require("fs");
var path = require("path");

var files = fs.readdirSync("tests");
for (var i = 0; i < files.length; i++) {
  var file = files[i];
  var ext = path.extname(file);
  if (ext == '.c') {
    var name = path.basename(file, ext);
    console.log('./main.byte bash tests/' + name + '.c > tests/bash/' + name + '.sh');
    console.log('./main.byte bat tests/' + name + '.c > tests/batch/' + name + '.bat');
    console.log('bash tests/bash/' + name + '.sh > tests/output/' + name + '.txt');
  }
}
