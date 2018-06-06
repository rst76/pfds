function cons(x, ds) {
    dd = [];
    if (ds.length === 0) {
        dd = [[x]];
    } else if (ds[0].length < 3) {
        dd = ds;
        dd[0].unshift(x);
    } else {
        dd = cons([ds[0][1], ds[0][2]], ds.slice(1));
        dd.unshift([x, ds[0][0]]);
    }
    return dd;
}

function uncons(ds) {
    if (ds.length === 1 && ds[0].length === 1) {
        return [ds[0][0],[]];
    } else if (ds[0].length > 1) {
        dd = ds.slice(1);
        dd.unshift(ds[0].slice(1));
        return [ds[0][0], dd];
    } else {
        u = uncons(ds.slice(1));
        u[1].unshift(u[0]);
        return [ds[0][0], u[1]];
    }
}

function head(xs) {return uncons(xs)[0]}

function tail(xs) {return uncons(xs)[1]}

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
    list = tail(list);
    console.log(show(list));
}
