function Gauss(a, b) {
  this.A = A;
  this.b = b;
}

function Matrix(m, n){
  assert(m>0);
  assert(n>0);

  this.m = m;
  this.n = n;
  this.rowIds = range(0,m-1);
  this.colIds = range(0,n-1);
  this.mat = _newMatrix(m,n);
  this.fill(0.0);

  function _newMatrix(m, n){
    var mat = new Array(m);
    for(var i=0; i<m; i++){
      mat[i] = new Array(n);
    }
    return mat;
  }
}

Matrix.prototype = {
  at: function(i,j) {
    return this.mat[i][j];
  },
  set: function(i,j,v) {
    this.mat[i][j] = v;
  },
  forEach: function(f_ij){
    for(var i=0; i<this.m; i++){
      for(var j=0; j<this.n; j++){
        this.set(i,j,f_ij(i,j));
      }
    }
    return 0;
  },
  fill: function(v){
    this.forEach(function(i,j){return v;});
    return v;
  },
  toString: function(){
    var str ="";
    for(var i=0; i<this.m; i++){
      for(var j=0; j<this.n; j++){
        str = str + " " + this.at(i,j);
      }
      str = str + "\n"
    }
    return str;
  },
  stdout: function(){
    console.log(this.toString());
  },
  stdout2: function(str){
    console.log(str);
    console.log(this.toString());
  }
}

function set_multi(matA, matB, result){
  assert(matA.n == matB.m);
  assert(matA.m == result.m);
  assert(matB.n == result.n);

  matA.rowIds.forEach(function(i){matB.colIds.forEach(function(j){
    var v = sum(matA.colIds.map(function(k){return matA.at(i,k) * matB.at(k,j);}));
    result.set(i,j, v);
  })})
}

function multi(matA, matB){
  assert(matA.n == matB.m);

  var result = new Matrix(matA.m, matB.n);
  set_multi(matA, matB, result);
  return result;
}

function trans(mat){
  var result = new Matrix(mat.n, mat.m);
  result.forEach(function(i,j){return mat.at(j,i);})
  return result;
}

function list2matrix(m,n,vs) {
  var mat = new Matrix(m, n);
  mat.forEach(function(i,j){
    var k = i*mat.n + j;
    if(k < vs.length){return vs[k];}
    return 0.0;
  })
  return mat;
}


function range(start, end){
  assert(end >= start);

  var n = end - start + 1;
  var arr = new Array(n);
  for(var i=0; i<n; i++){
    arr[i]=start+i;
  }
  return arr;
}

function sum(xs){
  return xs.reduce(function(x, y){return x+y;});
}

function assert(condition) {
    if (!condition) {
        throw message || "Assertion failed";
    }
}

// test
var a20 = list2matrix(3,3,
  [2,1,1,
   4,1,0,
   -2,2,1]);
var b20 = list2matrix(3,1,
   [1,-2,7]);

var a58 = list2matrix(3,4,
  [1, 3, 3, 2,
   2, 6, 9, 5,
   -1, -3, 3, 0]);
var b58 = list2matrix(3,1, [1,2,-1]);

a58.stdout("A58");
b58.stdout("b58");


console.log(range(0,9))
multi(trans(a58), a58).stdout();
