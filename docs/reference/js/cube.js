var lastX;
var lastY;
var matrix3d = rotate(0,0,0);
var newMatrix;
$(document).ready(function() {
  $('body').on('mousedown', function(event) {
    $('body').on('mouseup', function() {
      $('body').off('mousemove');
      m = $('.cube').css('transform');
      if(m.match(/matrix3d/) == null)
        matrix3d = rotate(0,0,0);
      else
        matrix3d = stringToMatrix(m.substring(8,m.length));
    });

    lastX=event.pageX;
    lastY=event.pageY;

    $('body').on('mousemove', function (event) {

      var r = {x: -(event.pageY - lastY), y: event.pageX - lastX};
      r.angle = Math.sqrt(r.x*r.x + r.y*r.y);
      rotate3d = multiply(matrix3d, [[r.x],[r.y],[0],[r.angle]]);
      var str = 'matrix3d' + matrixToString(matrix3d)
            + ' rotate3d(' + rotate3d[0][0] + ', ' + rotate3d[1][0] + ', ' + rotate3d[2][0] + ', ' + rotate3d[3][0] + 'deg)';
      $('.cube').css('transform',str);
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
      if(i<matrix.length-1 || j<matrix[i].length-1) s+=", ";
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
