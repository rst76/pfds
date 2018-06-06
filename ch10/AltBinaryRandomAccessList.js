function cons(x, ds) {
    dd = [];
    if (ds.length === 0) {
        dd = [x];
    } else if (ds[0] === null) {
        dd = ds.slice(1);
        dd.unshift(x);
    } else {
        dd = cons([x, ds[0]], ds.slice(1));
        dd.unshift(null);
    }
    return dd;
}

function uncons(ds) {
    if (ds.length === 1) {
        return [ds[0],[]];
    } else if (ds[0] !== null) {
        dd = ds.slice(1);
        dd.unshift(null);
        return [ds[0], dd];
    } else {
        u = uncons(ds.slice(1));
        u[1].unshift(u[0][1]);
        return [u[0][0], u[1]];
    }
}

function head(xs) {return uncons(xs)[0]}

function tail(xs) {return uncons(xs)[1]}

function lookup(i, xs) {
    if (xs[0] !== null) {
        if (i === 0) {
            return xs[0];
        } else {
            xx = xs.slice(1);
            xx.unshift(null);
            return lookup(i - 1, xx);
        }
    } else {
        xy = lookup(Math.floor(i / 2), xs.slice(1));
        return xy[i % 2];
    }
}

function fupdate(f, i, xs) {
    if (xs[0] !== null) {
        if (i === 0) {
            xx = xs.slice(1);
            xx.unshift(f(xs[0]));
            return xx;
        } else {
            xx = xs.slice(1);
            xx.unshift(null);
            return cons(xs[0], fupdate(f, i - 1, xx));
        }
    } else {
        ff = function(x) {
            x[i % 2] = f(x[i % 2]);
            return x;
        };
        xx = fupdate(ff, Math.floor(i / 2), xs.slice(1));
        xx.unshift(null);
        return xx;
    }
}

function update(i, y, xs) {
    return fupdate(function(x) {return y}, i, xs);
}

function show(a) {
    if (Array.isArray(a)) {
        return "[" + a.map(show).join(",") + "]";
    } else {
        return  "" + a;
    }
}

list = [];
for (i = 0; i < 16; i++) {
    list = cons(i, list);
    console.log(show(list));
}
for (i = 0; i < 16; i++) {
    console.log(lookup(i, list));
    update(i, 0, list);
    console.log(show(list));
}
for (i = 0; i < 16; i++) {
    list = tail(list);
    console.log(show(list));
}
