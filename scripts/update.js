var fs = require("fs");
var path = require("path");

var files = fs.readdirSync("tests");
for (var i = 0; i < files.length; i++) {
  var file = files[i];
  var ext = path.extname(file);
  if (ext == '.batsh') {
    var name = path.basename(file, ext);
    console.log('./batsh bash tests/' + name + '.batsh > tests/bash/' + name + '.sh');
    console.log('./batsh winbat tests/' + name + '.batsh > tests/batch/' + name + '.bat');
    console.log('bash tests/bash/' + name + '.sh > tests/output/' + name + '.txt');
  }
}
