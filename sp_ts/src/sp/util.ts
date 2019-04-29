export class Util {

  static hexSize : integer = 32;

  constructor() {}

  static hex_to_pixel(q : integer, r : integer) {
      var x = Util.hexSize * 0.75 * q;
      var y = Util.hexSize * (r + 0.5 * (q & 1));

      return {x: x, y: y};
  }

  static distance(srcX : integer, srcY : integer, dstX : integer, dstY : integer) {
    var srcCube = Util.odd_q_to_cube(srcX, srcY);
    var dstCube = Util.odd_q_to_cube(dstX, dstY);

    return (Math.abs(srcCube.x - dstCube.x) +
            Math.abs(srcCube.y - dstCube.y) +
            Math.abs(srcCube.z - dstCube.z)) / 2;
  }
  
  static odd_q_to_cube(Q : integer, R : integer) {
    var X = Q;
    var Z = R - (Q - (Q & 1)) / 2;
    var Y = (-1 * X) - Z;

    return {x: X, y: Y, z: Z};         
  }

  static cube_to_odd_q(X, Y, Z) {
    var Q = X;
    var R = parseInt(Z + (X - (X & 1)) / 2);

    return {q: Q, r: R};
  }

  static getNeighbours(Q, R) {
    var conversion = [ [1, -1, 0], [1, 0, -1], [0, 1, -1], [-1, 1, 0], [-1, 0, 1], [0, -1, 1] ];
    var cube = Util.odd_q_to_cube(Q, R);
    var i; 
    var neighbours = [];

    for(i = 0; i < conversion.length; i++) {
        var offset = conversion[i];
        var odd_q = Util.cube_to_odd_q(cube.x + offset[0], cube.y + offset[1], cube.z + offset[2]);

        if(i == 0) 
            odd_q["d"] = "se";
        else if(i == 1)
            odd_q["d"] = "ne";
        else if(i == 2)
            odd_q["d"] = "n";
        else if(i == 3)
            odd_q["d"] = "nw";
        else if(i == 4)
            odd_q["d"] = "sw";
        else if(i == 5)
            odd_q["d"] = "s";

        neighbours.push(odd_q)
    }

    return neighbours; 
  }
}