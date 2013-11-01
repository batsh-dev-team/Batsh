var fs = require("fs");
var path = require("path");

var files = fs.readdirSync("tests");
for (var i = 0; i < files.length; i++) {
  var file = files[i];
  var ext = path.extname(file);
  if (ext == '.js') {
    var name = path.basename(file, ext);
    console.log('./main.byte bash tests/' + name + '.js > tests/bash/' + name + '.sh');
    console.log('./main.byte winbat tests/' + name + '.js > tests/batch/' + name + '.bat');
    console.log('bash tests/bash/' + name + '.sh > tests/output/' + name + '.txt');
  }
}
