//'use strict'
function Spline(aX, aF, aN) {
    var sp = {};
    sp.x = copy(aX);   // 0 - N-1
    sp.f = copy(aF);
    sp.n = aN;
    sp.left_b = 0.0;
    sp.right_b = 0.0;

    sp.h = new Matrix(aN, 1);   // 0 - N-2
    sp.f1 = new Matrix(aN, 1);  // 0 - N-2
    sp.f2 = new Matrix(aN, 1); // 1 - N-2

    for(var i=0; i < sp.n-1; i++) sp.h.set(i, 0, sp.x.at(i+1, 0) - sp.x.at(i, 0));

    var H = new Matrix(sp.n-2, sp.n-2);
    //var y = new Matrix(sp.n-2, 1);
    var b = new Matrix(sp.n-2, 1);

    for(var i=0; i<sp.n-2; i++)  H.set(i, i,  2 * ( sp.h.at(i,0) + sp.h.at(i+1, 0)));
    for(var i=0; i<sp.n-3; i++)  H.set(i, i+1,  sp.h.at(i+1, 0));
    for(var i=1; i<sp.n-2; i++)  H.set(i, i-1,  sp.h.at(i-1, 0));

    for(var i=0; i<sp.n-2; i++)
    b.set(i, 0, 6.0 * ( (sp.f.at(i+2, 0) - sp.f.at(i+1, 0))/sp.h.at(i+1, 0) - (sp.f.at(i+1,0) - sp.f.at(i,0))/sp.h.at(i, 0) ) );

    var g = makeGauss(H, b);
    g.solve();
    //copy_set(g.x, y);

    sp.f2.set(0, 0, sp.left_b);
    for(var i=1; i<sp.n-1; i++) sp.f2.set(i, 0, g.x.at(i-1, 0));
    sp.f2.set(sp.n-1, 0, sp.right_b);

    //sp.f2.set(0, 0, (sp.f2.at(1, 0)+sp.f2.at(sp.n-2, 0))/2.0);
    //sp.f2.set(sp.n-1, 0, sp.f2.at(0,0));

    for(var i=0; i<sp.n-1; i++) {
        sp.f1.set(i, 0,   (sp.f.at(i+1, 0) - sp.f.at(i, 0))/ sp.h.at(i, 0) - sp.f2.at(i+1, 0) * sp.h.at(i, 0) / 6.0 - sp.f2.at(i, 0) * sp.h.at(i, 0) / 3.0 );
    }

    function _searchInterval(_x) {
        if(_x < sp.x.at(0, 0)) return 0;

        for(var i=0; i<sp.n-1; i++) {
            if( (sp.x.at(i, 0) <= _x) && (_x <= sp.x.at(i+1, 0)) ) {
                return i;
            }
        }
        return sp.n-2;
    }

    sp.lval = function(_x) {
        var i = _searchInterval(_x);
        return (sp.f.at(i+1, 0) - sp.f.at(i, 0))/(sp.x.at(i+1, 0)-sp.x.at(i,0)) * (_x-sp.x.at(i, 0)) + sp.f.at(i, 0);
    };

    sp.val = function(_x) {
        var i = _searchInterval(_x);
        return sp.f.at(i, 0)
        + sp.f1.at(i, 0) * (_x - sp.x.at(i, 0))
        + sp.f2.at(i, 0) * Math.pow( (_x - sp.x.at(i, 0)), 2.0 )/2.0
        + (sp.f2.at(i+1, 0) - sp.f2.at(i, 0))
        * Math.pow( (_x - sp.x.at(i, 0)), 3.0)/(6.0 * sp.h.at(i, 0));
    };

    return sp;
}


var n = 11;

var x = new Matrix(n, 1);
var y = new Matrix(n, 1);

function test_f(_x) {
    //return Math.sin(- _x * _x);
    return Math.sin(- _x);
}
for(var i=0; i<n; i++) {
    x.set(i, 0,  10.0/(n-1) * (2*i*i) - 5.0);
    y.set(i, 0,  test_f(x.at(i, 0)));
}


var lsp = Spline(x, y, n);
//Spline lsp = new Spline(x, y, n, 0.5, 0.5);

var N = 100;
for(var i=0; i<=N; i++) {
    var testx = 14.0 /N * i - 7;
    console.log(testx + ", "+ (lsp.val(testx)));
}

lsp.f.stdout();
