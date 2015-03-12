var nodes = {};
var edges = {};
var selectedNode;
var selectedEdge;
var last;

var directed;
var context;

var radius = 30;

var _cid = 0;

function init(context_, directed_) {
    directed = directed_;
    context = context_;
}

function getGraph() {
    var nodes_ls = [];
    var edges_ls = [];
    for (var i in nodes) {
        nodes_ls.push(nodes[i]);
    }
    for (var i in edges) {
        edges_ls.push(edges[i]);
    }
    return {"nodes": nodes_ls, "edges": edges_ls};
}

function loadGraph(graph) {
    unselectEdge();
    unselectNode();
    last = undefined;
    nodes = {};
    edges = {};
    var nodes_ls = graph.nodes;
    var edges_ls = graph.edges;
    for (var i = 0; i < nodes_ls.length; i++) {
        var node = nodes_ls[i];
        nodes[node.idx] = node;
    }
    for (var i = 0; i < edges_ls.length; i++) {
        var edge = edges_ls[i];
        edges[edge.idx] = edge;
    }
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
    context.strokeStyle = "black";
    context.lineWidth = 3;
    for (var i in edges) {
        context.beginPath();
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

        if (i == selectedEdge) {
            context.strokeStyle = "blue";
        } else {
            context.strokeStyle = "black";
        }

        if (directed) {
            draw_arrow(fx, fy, tx, ty);
        } else {
            draw_line(fx, fy, tx, ty);
        }
        context.stroke();

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
        if (dxm < 0) {
            dxm = -dxm;
            dym = -dym;
        }
        context.fillText(edge.weight, mx + dxm, my - dym);
        context.stroke();
    }
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
        if (i == selectedNode) {
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

function genId() {
    var idx = 0;
    while (true) {
        if (!(idx in nodes) && !(idx in edges))
            return idx;
        else
            idx += 1;
    }
}

function appendNode(x, y, weight) {
    var idx = genId();
    nodes[idx] = {
        idx: idx,
        x: x,
        y: y,
        weight: Number(weight)
    };
    last = idx;
}

function appendEdge(from, to, weight) {
    var idx = genId();
    edges[idx] = {
        idx: idx,
        from: Number(from),
        to: Number(to),
        weight: Number(weight)
    };
    last = idx;
}

function setNodeWeight(id, weight) {
    nodes[id].weight = weight;
}

function setEdgeWeight(id, weight) {
    edges[id].weight = weight;
}

function selectNode(idx) {
    selectedNode = idx;
    selectedEdge = undefined;
}

function unselectNode() {
    selectedNode = undefined;
}

function selectEdge(idx) {
    selectedEdge = idx;
    selectedNode = undefined;
}

function unselectEdge() {
    selectedEdge = undefined;
}

function nodeOnPosition(x, y) {
    for (var i in nodes) {
        var node = nodes[i];
        if (hitNodeTest(node, x, y)) {
            return i;
        }
    }
}

function deleteNode(idx) {
    delete nodes[idx];
    var edges_to_delete = [];
    for (var eid in edges) {
        var edge = edges[eid];
        if (edge.from == idx || edge.to == idx) {
            edges_to_delete.push(eid);
        }
    }
    for (var i = 0; i < edges_to_delete.length; i++) {
        delete edges[edges_to_delete[i]];
    }
}

function deleteEdge(idx) {
    delete edges[idx];
}

function edgeOnPosition(x, y) {
    for (var i in edges) {
        var edge = edges[i];
        if (hitEdgeTest(edge, x, y)) {
            return i;
        }
    }
}

function hitEdgeTest(edge, x, y) {
    var select_radius = 30;
    var from = nodes[edge.from];
    var to = nodes[edge.to];
    var mx = (from.x + to.x) / 2;
    var my = (from.y + to.y) / 2;

    var dx = x - mx;
    var dy = y - my;
    return (dx * dx + dy * dy < select_radius * select_radius);
}

function hitNodeTest(node, x, y) {
    var dx = x - node.x;
    var dy = y - node.y;
    return (dx * dx + dy * dy < radius * radius);
}
