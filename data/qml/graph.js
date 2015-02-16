var nodes = [];

var radius = 20;

function draw(context, t) {
    context.beginPath();
    context.clearRect(0, 0, width, height);
    context.fill();

    for (var i = 0; i < nodes.length; i++) {
        var node = nodes[i]
        context.beginPath();

        var angle = -2 * Math.PI;
        var r = radius;

        if (i == nodes.length - 1) {
            r = t * r;
        }
        context.arc(node.x, node.y, r, 0, angle, true);
        context.stroke();
    }
}

function append(x, y) {
    nodes.push({x: x, y: y});
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
