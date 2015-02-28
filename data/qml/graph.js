var nodes = {};
var edges = {};
var selected;
var last;

var directed;
var context;

var radius = 30;

var _cid = 0;

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

function draw(t) {
    context.beginPath();
    context.clearRect(0, 0, width, height);
    context.fill();

    context.strokeStyle = "black";
    context.fillStyle = "white";

    // draw edges
    context.beginPath();
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
    }
    context.stroke();

    // draw nodes
    for (var i in nodes) {
        var node = nodes[i];
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
    }
}

function appendNode(x, y, weight) {
    var idx = _cid;
    nodes[idx] = {idx: idx, x: x, y: y, weight: weight};
    last = idx;
    _cid += 1;
}

function appendEdge(from, to) {
    var idx = _cid;
    edges[idx] = {idx: idx, from: from, to: to};
    last = idx;
    _cid += 1;
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
        if (hitTest(nodes[i], x, y)) {
            return i;
        }
    }
}

function hitTest(node, x, y) {
    var dx = x - node.x;
    var dy = y - node.y;
    return (dx * dx + dy * dy < radius * radius);
}
