var size = 20;
var indent_left = 20;
var indent_up = 1;

function draw(ctx, data) {
    ctx.beginPath();
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    ctx.fill();
    if (data !== null) {
        draw_grid();
        var shift = 1;
        for (var node_id in data) {
            var node_flow = data[node_id];
            shift = draw_node_flow(node_id, node_flow, shift);
        }
    }

    function draw_node_flow(node_id, node_flow, shift) {
        var rows = node_flow["links"].length + 1
        var height = rows * size;
        var new_shift = shift + height;

        ctx.beginPath();
        ctx.font="20px Ubuntu";
        ctx.textBaseline = "middle";
        var node_id_x = indent_left - 10;
        var node_id_y = shift + height / 2;
        ctx.fillText(node_id, node_id_x, node_id_y);
        ctx.stroke();

        var cpu_flow = node_flow["cpu"];
        var curr_row = 0;
        draw_flow(cpu_flow, curr_row);

        var link_flows = node_flow["links"];
        for (var link_id = 0; link_id < link_flows.length; link_id++) {
            var link_flow = link_flows[link_id];
            curr_row++;
            draw_flow(link_flow, curr_row);
        };

        ctx.beginPath();
        ctx.lineWidth = 1;
        ctx.moveTo(indent_left, new_shift);
        ctx.lineTo(ctx.canvas.width - indent_left, new_shift);
        ctx.stroke();

        return new_shift;
    }

    function draw_task(start_x, start_y, task) {
        ctx.lineWidth = 1;
        ctx.beginPath();
        var calc_time = task["to"] - task["from"];
        var width = calc_time * size;
        var height = size;
        ctx.rect(start_x, start_y, width, size);
        ctx.stroke();

        ctx.beginPath();
        ctx.font="15px Ubuntu";
        ctx.textBaseline = "bottom";
        var text = task["val"];
        var text_width = ctx.measureText(text).width;
        // console.log(text_width);
        var text_x = start_x + width / 2 - text_width / 2;
        var text_y = start_y + height;
        ctx.fillText(text, text_x, text_y); 
        // console.log("x: ", text_x, "y: ", text_y, "text: ", text);
        ctx.stroke();
    }

    function draw_flow(flow, row) {
        // console.log("asdas ", flow, row);
        for (var i = flow.length - 1; i >= 0; i--) {
            var task = flow[i];
            var start_x = indent_left + size * task["from"];
            var start_y = shift + size * row;
            draw_task(start_x, start_y, task);
        }
    }

    function draw_grid() {
        ctx.lineWidth = 0.2;
        var num_cols = Math.floor((ctx.canvas.width - indent_left) / size);
        var num_rows = Math.floor((ctx.canvas.height - indent_up) / size);
        for (var i = 0; i < num_cols; i++) {
            ctx.beginPath();
            ctx.moveTo(indent_left + i * size, indent_up);
            ctx.lineTo(indent_left + i * size, ctx.canvas.height - indent_up);
            ctx.stroke();
        };
        for (var i = 0; i < num_rows; i++) {
            ctx.beginPath();
            ctx.moveTo(indent_left, indent_up + i * size);
            ctx.lineTo(ctx.canvas.width - indent_left, indent_up + i * size);
            ctx.stroke();
        };
    }
}

