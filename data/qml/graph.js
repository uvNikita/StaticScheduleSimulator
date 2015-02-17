var nodes = [];
var edges = [];
var selected;
var last;

var radius = 20;

function draw(context, t) {
    context.beginPath();
    context.clearRect(0, 0, width, height);
    context.fill();

    context.strokeStyle = "black";
    context.fillStyle = "white";

    // draw edges
    for (var i = 0; i < edges.length; i++) {
        var edge = edges[i];
        var from = nodes[edge.from]
        var to = nodes[edge.to]
        context.beginPath();
        context.moveTo(from.x, from.y);
        if (i == edges.length - 1 && last == "edge") {
            context.lineTo(from.x + (to.x - from.x) * t, from.y + (to.y - from.y) * t) - from.y;
        } else {
            context.lineTo(to.x, to.y);
        }
        context.lineWidth = 3;
        context.stroke();
    }

    // draw nodes
    for (var i = 0; i < nodes.length; i++) {
        var node = nodes[i]
        context.beginPath();

        var angle = -2 * Math.PI;
        var r = radius;

        if (i == nodes.length - 1 && last == "node") {
            r = t * r;
        }
        context.arc(node.x, node.y, r, 0, angle, true);
        context.lineWidth = 3;
        if (i == selected) {
            context.strokeStyle = "blue";
        } else {
            context.strokeStyle = "black";
        }
        context.stroke();
        context.fill();
    }
}

function appendNode(x, y) {
    nodes.push({x: x, y: y});
    last = "node";
}

function appendEdge(from, to) {
    edges.push({from: from, to: to});
    last = "edge";
}

function select(id) {
    selected = id;
}

function unselect() {
    selected = undefined;
}

function nodeOnPosition(x, y) {
    for (var i = 0; i < nodes.length; i++) {
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
