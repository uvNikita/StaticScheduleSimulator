import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import "graph.js" as Graph

Rectangle {
    anchors.fill: parent

    Canvas {
        id: canvas
        anchors.fill: parent
        property double animationProgress: 0.0

        onPaint: {
            Graph.draw(canvas.getContext("2d"), animationProgress);
        }

        onAnimationProgressChanged: {
            requestPaint();
        }
    }

    MouseArea {
        property bool dragging: false;
        property int dragId: -1;
        property double dragHoldX;
        property double dragHoldY;

        anchors.fill: parent
        onPressed: {
            var nodeId = Graph.nodeOnPosition(mouseX, mouseY);
            if (nodeId !== undefined) {
                dragId = nodeId;
                var node = Graph.nodes[dragId];
                dragHoldX = mouseX - node.x;
                dragHoldY = mouseY - node.y;
            }
        }
        onPositionChanged: {
            if (dragId != -1) {
                dragging = true;
                Graph.nodes[dragId].x = mouseX - dragHoldX;
                Graph.nodes[dragId].y = mouseY - dragHoldY;
                canvas.requestPaint();
            }
        }
        onReleased: {
            if (!dragging) {
                var nodeId = Graph.nodeOnPosition(mouseX, mouseY);
                if (nodeId !== undefined) {
                    if (Graph.selected !== undefined) {
                        Graph.appendEdge(Graph.selected, nodeId);
                        Graph.unselect();
                        canvas.requestPaint();
                    } else {
                        Graph.select(nodeId);
                    }
                } else {
                    Graph.appendNode(mouseX, mouseY);
                    animateChart.start();
                }
            }
            dragging = false;
            dragId = -1;
        }
    }

    NumberAnimation {
        id: animateChart
        target: canvas
        properties: "animationProgress"
        from: 0.0
        to: 1.0
        duration: 100
    }
}
