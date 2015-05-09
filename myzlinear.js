function isZero(x){
  return Math.abs(x) < 0.0000001;
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
        f_ij(i,j);
      }
    }
    return 0;
  },
  forEach_set: function(f_ij){
    for(var i=0; i<this.m; i++){
      for(var j=0; j<this.n; j++){
        this.set(i,j,f_ij(i,j));
      }
    }
    return 0;
  },
  fill: function(v){
    this.forEach_set(function(i,j){return v;});
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

function multi_set(matA, matB, result){
  assert(matA.n == matB.m);
  assert(matA.m == result.m);
  assert(matB.n == result.n);

  result.forEach_set(function(i,j){
    return sum(matA.colIds.map(function(k){return matA.at(i,k) * matB.at(k,j);}));
  })
}

function multi(matA, matB){
  assert(matA.n == matB.m);

  var result = new Matrix(matA.m, matB.n);
  multi_set(matA, matB, result);
  return result;
}

function trans(mat){
  var result = new Matrix(mat.n, mat.m);
  result.forEach_set(function(i,j){return mat.at(j,i);})
  return result;
}


function copy_set(matA, result) {
  assert(matA.m == result.m);
  assert(matA.n == result.n);

  result.forEach_set(function(i,j){return matA.at(i,j);});
}

function copy(matA){
  var result = new Matrix(matA.m, matA.n);
  copy_set(matA, result);
  return result;
}

function bind_set(A, B, result){
  assert(A.m == B.m);
  assert(A.m == result.m);
  assert( (A.n + B.n) == result.n);

  A.forEach(function(i,j){result.set(i,j,A.at(i,j));});
  B.forEach(function(i,j){result.set(i,(j+A.n),B.at(i,j));})
}

function bind(A,B){
  assert(A.m == B.m);

  var result = new Matrix(A.m, (A.n + B.n));
  bind_set(A, B, result);
  return result;
}

function list2matrix(m,n,vs) {
  var mat = new Matrix(m, n);
  mat.forEach_set(function(i,j){
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

function isSameDim(matA, matB){
  return (matA.m == matB.m) && (matA.n == matB.n);
}

function assert(condition) {
    if (!condition) {
        throw message || "Assertion failed";
    }
}


function Gauss(m,n) {
  this.A = new Matrix(m,n);
  this.b = new Matrix(m,1);
  this.Ab = new Matrix(m, n+1);
  this.m = m;
  this.n = n;

  this.p = range(0, m);
  this.flgBaseVars = new Array(n);
  for(var j=0; j<n; j++) this.flgBaseVars[j] = false;

  this.x = new Matrix(n, 1);
  this.x.fill(0.0);

  this.xs = null;
}

Gauss.prototype ={
  set: function(A, b) {
    assert(isSameDim(this.A, A));
    assert(isSameDim(this.b, b));

    copy_set(A, this.A);
    copy_set(b, this.b);
    bind_set(A, b, this.Ab);
  },
  solve: function(){
    forward(this.Ab, this.p, this.flgBaseVars);
    if(this.isSolvable()){
      backward(this.Ab, this.p, this.flgBaseVars, this.x);
    }
  },
  solveXs: function(){
    if(numOfFreeVars(this.flgBaseVars) <1) return;
    this.xs = new Matrix(this.n, numOfFreeVars(this.flgBaseVars));
    backwardXs(this.Ab, this.p, this.flgBaseVars, this.xs);
  },
  isSolvable: function(){
    for(var i = numOfBaseVars(this.flgBaseVars); i<this.m; i++){
      if(! isZero(this.Ab.at(this.p[i],this.n))) return false;
    }
    return true;
  },

  stdout: function(){
    console.log("solvable: "+this.isSolvable());
    console.log(this.flgBaseVars);
    this.A.stdout();
    this.b.stdout();
    this.Ab.stdout();
    if(this.isSolvable()){
      this.x.stdout();
      if(this.xs != null) this.xs.stdout();
    }
  }
}

function makeGauss(A, b){
  assert(A.m == b.m);

  var g = new Gauss(A.m, A.n);
  g.set(A, b);
  return g;
}

function forward(Ab, p, flgBaseVars){
  var m = Ab.m;
  var n = Ab.n - 1;


  var pj = 0;
  for(var pi=0; pi<m; pi++){
    for(;pj<n; pj++){
      replaceRow(pi,pj);
      if(! isZero(Ab.at(p[pi],pj))) break;
    }
    if(pj >= n) break;

    var pivot = Ab.at(p[pi],pj);
    flgBaseVars[pj] = true;
    for(var i=pi+1; i<m; i++){
      var dino = Ab.at(p[i],pj)/pivot;
      for(var j=pj; j<n+1; j++){
        Ab.set(p[i], j, Ab.at(p[i],j)-dino*Ab.at(p[pi],j));
      }
    }
  }

  function replaceRow(ti, tj){
    var max_i = ti;
    var max_v = Math.abs(Ab.at(p[ti], tj));
    for(var i=ti+1; i<m; i++){
      var v = Math.abs(Ab.at(p[i],tj));
      if( v > max_v){
        max_i = i;
        max_v = v;
      }
    }
    swap_p(ti, max_i);
  }
  function swap_p(fi, ti){
    var tmp;
    tmp = p[ti];
    p[ti] = p[fi];
    p[fi] = tmp;
  }

}

function numOfBaseVars(flgVars){
  return flgVars.filter(function(x){return x;}).length;
}
function numOfFreeVars(flgVars){
  return flgVars.length - numOfBaseVars(flgVars);
}

function backward(Ab,p,flgVars,x){
  var m = Ab.m;
  var n = Ab.n-1;

  var j = n;
  for(var i=numOfBaseVars(flgVars)-1; i>=0; i--){
    j = next_j();
    var tmp = 0.0;
    for(var k=j+1; k<n; k++){
      tmp = tmp + Ab.at(p[i],k) * x.at(k,0);
    }
    x.set(j,0,(Ab.at(p[i],n) - tmp)/Ab.at(p[i],j));
  }

  function next_j(){
    j--;
    for(;j>=0;j--){
      if(flgVars[j]) return j;
    }
    return 0;
  }

}

function freeVars(flgVars){
  var vs = new Array(0);
  for(var j=0; j<flgVars.length; j++){
    if(! flgVars[j]) vs[vs.length] = j;
  }
  return vs;
}

function backwardXs(Ab, p, flgVars, Xs){
  var m = Ab.m;
  var n = Ab.n-1;
  var fvars = freeVars(flgVars);
  var tmp_x = new Matrix(n,1);

  for(var i=0; i<m; i++){
    Ab.set(i,n,0.0);
  }

  for(var v=0; v<fvars.length; v++){
    tmp_x.fill(0.0);
    tmp_x.set(fvars[v],0, 1.0);
    backward(Ab, p, flgVars, tmp_x);
    for(var j=0; j<n; j++){
      Xs.set(j,v, tmp_x.at(j,0));
    }
  }

}
// test
var A20 = list2matrix(3,3,
  [2,1,1,
   4,1,0,
   -2,2,1]);
var b20 = list2matrix(3,1,
   [1,-2,7]);

var A58 = list2matrix(3,4,
  [1, 3, 3, 2,
   2, 6, 9, 5,
   -1, -3, 3, 0]);
var b58 = list2matrix(3,1, [1,3,1]);



function fillRandom(mat){
  mat.forEach_set(function(i,j){return Math.random();});
}
function diff(x, y){
  assert(isSameDim(x,y));

  var tmp = 0.0;
  x.forEach(function(i,j){ tmp = tmp + (Math.pow(x.at(i,j)-y.at(i,j),2.0));});
  return Math.sqrt(tmp);
}
function gauss_check_mn(m,n){
  var A = new Matrix(m,n);
  var ans_b = new Matrix(m,1);
  var ans_x = new Matrix(n,1);

  fillRandom(A);
  fillRandom(ans_x);
  multi_set(A,ans_x, ans_b);

  var g = makeGauss(A, ans_b);
  g.solve();
  g.solveXs();

  console.log("m:"+m+", n:"+n+", diffX:"+diff(ans_b, multi(A, g.x)));
}

function gauss_check(n){
  for(var i=1; i<n; i++){
    for(var j=1; j<n; j++){
      gauss_check_mn(i, j);
    }
  }
}
gauss_check(30);
