var nodes = [];

function draw(context, t) {
    context.beginPath();
    context.clearRect(0, 0, width, height);
    context.fill();

    for (var i = 0; i < nodes.length; i++) {
        var node = nodes[i]
        context.beginPath();

        var angle = -2 * Math.PI;
        var radius = 20;

        if (i == nodes.length - 1) {
            radius = t * radius;
        }
        context.arc(node.x, node.y, radius, 0, angle, true);
        context.stroke();
    }
}

function append(x, y) {
    nodes.push({x: x, y: y});
}
