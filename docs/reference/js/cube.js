var lastX;
var lastY;
var rotation = {x: 0, y: 0, angle: 0};
var matrix3d = rotate(0,0,0);
var newMatrix;
$(document).ready(function() {
  $('body').on('mousedown', function(event) {
    $('body').on('mouseup', function() {
      $('body').off('mousemove');
      matrix3d = newMatrix;
    });

    lastX=event.pageX;
    lastY=event.pageY;

    $('body').on('mousemove', function (event) {

      var r = {x: event.pageX - lastX, y: event.pageY - lastY};
      r.angle = Math.sqrt(r.x*r.x + r.y*r.y);

      // var newRotation = {
      //   x: rotation.x + r.x,
      //   y: rotation.y + r.y,
      //   angle: Math.sqrt(Math.pow(rotation.x+r.x,2) + Math.pow(rotation.y+r.y,2))
      // }
      // newRotation.x = newRotation.x / Math.sqrt(Math.pow(newRotation.x,2)+Math.pow(newRotation.y,2));
      // newRotation.y = newRotation.y / Math.sqrt(Math.pow(newRotation.x,2)+Math.pow(newRotation.y,2));
      //rotation = newRotation;
      $('.cube').css({transform: "rotate3d("+(-r.y)+","+r.x+",0,"+r.angle+"deg)"});
      var str = $('.cube').css('transform');
      str = str.substring(8,str.length);
      newMatrix = multiply(matrix3d, stringToMatrix(str));
      $('.cube').css('transform',"matrix3d" + matrixToString(newMatrix));
    });
  });
});

function rotate(a,b,g) {
  return [
          [cos(a)*cos(b), cos(a)*sin(b)*sin(g)-sin(a)*cos(g), cos(a)*sin(b)*cos(g)+sin(a)*sin(g), 0],
          [sin(a)*cos(b), sin(a)*sin(b)*sin(g)+cos(a)*cos(g), sin(a)*sin(b)*cos(g)-cos(a)*sin(g), 0],
          [-sin(b), cos(b)*sin(g), cos(b)*cos(g), 0],
          [0, 0, 0, 1]
        ];
}

function matrixToString(matrix) {
  var s = "(";
  for(i=0; i<matrix.length; i++) {
    for(j=0; j<matrix[i].length; j++) {
      s+=matrix[i][j];
      if(i<matrix.length-1 || j<matrix.length-1) s+=", ";
    }
  }
  return s+")";
}

function stringToMatrix(s) {
  array=s.substring(1,s.length-1).split(", ");
  return [array.slice(0,4), array.slice(4,8), array.slice(8,12), array.slice(12,16)];
}

function radians(degrees) {
  return degrees * (Math.PI/180);
}

function cos(a) {
  return Math.cos(radians(a));
}

function sin(a) {
  return Math.sin(radians(a));
}




function multiply(a, b) {
  var aNumRows = a.length, aNumCols = a[0].length,
      bNumRows = b.length, bNumCols = b[0].length,
      m = new Array(aNumRows);  // initialize array of rows
  for (var r = 0; r < aNumRows; ++r) {
    m[r] = new Array(bNumCols); // initialize the current row
    for (var c = 0; c < bNumCols; ++c) {
      m[r][c] = 0;             // initialize the current cell
      for (var i = 0; i < aNumCols; ++i) {
        m[r][c] += a[r][i] * b[i][c];
      }
    }
  }
  return m;
}
