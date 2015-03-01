var nodes = {};
var edges = {};
var selected;
var last;

var directed;
var context;

var radius = 30;

var _cnid = 0;
var _ceid = 0;

function init(context_, directed_) {
    directed = directed_;
    context = context_;
}

function draw_arrow(fromx, fromy, tox, toy) {
      var headlen = 15;
      var angle = Math.atan2(toy-fromy,tox-fromx);
      context.moveTo(fromx, fromy);
      context.lineTo(tox, toy);
      context.lineTo(tox-headlen*Math.cos(angle-Math.PI/6),toy-headlen*Math.sin(angle-Math.PI/6));
      context.moveTo(tox, toy);
      context.lineTo(tox-headlen*Math.cos(angle+Math.PI/6),toy-headlen*Math.sin(angle+Math.PI/6));
}

function draw_line(fromx, fromy, tox, toy) {
    context.moveTo(fromx, fromy);
    context.lineTo(tox, toy);
}

function draw_edges(t) {
    context.beginPath();
    context.strokeStyle = "black";
    context.lineWidth = 3;
    for (var i in edges) {
        var edge = edges[i];
        var from = nodes[edge.from];
        var to = nodes[edge.to];

        var dx1 = to.x - from.x;
        var dy1 = to.y - from.y;

        var d = Math.sqrt(dx1 * dx1 + dy1 * dy1);

        var dx2 = dx1 * radius / d;
        var dy2 = dy1 * radius / d;

        var fx = from.x;
        var fy = from.y;
        var tx = to.x - dx2;
        var ty = to.y - dy2;
        if (i == last) {
            tx = fx + (tx - fx) * t;
            ty = fy + (ty - fy) * t;
        }
        if (directed) {
            draw_arrow(fx, fy, tx, ty);
        } else {
            draw_line(fx, fy, tx, ty);
        }

        // weight
        context.fillStyle = "black";
        context.font="20px Ubuntu";
        context.textBaseline = "middle";
        var mx = (fx + tx) / 2;
        var my = (fy + ty) / 2;
        var a = my - from.y;
        var b = mx - from.x;
        var c = d / 2;
        var h = 15;
        var dxm = a * h / c;
        var dym = b * h / c;
        context.fillText(edge.weight, mx + dxm, my - dym);
    }
    context.stroke();
}

function draw_nodes(t) {
    for (var i in nodes) {
        var node = nodes[i];
        context.fillStyle = "white";
        context.beginPath();

        var angle = -2 * Math.PI;
        var r = radius;

        if (i == last) {
            r = t * r;
        }
        context.arc(node.x, node.y, r, 0, angle, true);
        context.lineWidth = 6;
        if (i == selected) {
            context.strokeStyle = "blue";
        } else {
            context.strokeStyle = "black";
        }
        context.stroke();
        context.fill();

        // node weight
        context.fillStyle = "black";
        context.font="22px Ubuntu";
        context.textBaseline = "middle";
        var text_width = context.measureText(node.weight).width;
        context.fillText(node.weight, node.x - (text_width / 2), node.y);

        // node id
        context.fillStyle = "black";
        context.font="18px Ubuntu";
        context.textBaseline = "top";
        var text_width = context.measureText(node.weight).width;
        context.fillText(node.idx, node.x + radius, node.y - radius);
    }
}

function draw(t) {
    context.beginPath();
    context.clearRect(0, 0, width, height);
    context.fill();

    draw_edges(t);
    draw_nodes(t);
}

function appendNode(x, y, weight) {
    var idx = "n" + _cnid;
    nodes[idx] = {idx: idx, x: x, y: y, weight: weight};
    last = idx;
    _cnid += 1;
}

function appendEdge(from, to, weight) {
    var idx = "e" + _ceid;
    edges[idx] = {idx: idx, from: from, to: to, weight: weight};
    last = idx;
    _ceid += 1;
}

function setWeight(id, weight) {
    nodes[id].weight = weight;
}

function select(id) {
    selected = id;
}

function unselect() {
    selected = undefined;
}

function nodeOnPosition(x, y) {
    for (var i in nodes) {
        var node = nodes[i];
        if (hitNodeTest(node, x, y)) {
            return i;
        }
    }
}

function hitNodeTest(node, x, y) {
    var dx = x - node.x;
    var dy = y - node.y;
    return (dx * dx + dy * dy < radius * radius);
}
