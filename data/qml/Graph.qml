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
        property int dragId;
        property double dragHoldX;
        property double dragHoldY;

        anchors.fill: parent
        onClicked: {
            if (Graph.nodeOnPosition(mouseX, mouseY) === undefined) {
                Graph.append(mouseX, mouseY);
                animateChart.start();
            }
        }
        onPressed: {
            var nodeId = Graph.nodeOnPosition(mouseX, mouseY);
            if (nodeId !== undefined) {
                dragId = nodeId;
                dragging = true;
                var node = Graph.nodes[dragId];
                dragHoldX = mouseX - node.x;
                dragHoldY = mouseY - node.y;
            }
        }
        onPositionChanged: {
            if (dragging) {
                Graph.nodes[dragId].x = mouseX - dragHoldX;
                Graph.nodes[dragId].y = mouseY - dragHoldY;
                canvas.requestPaint();
            }
        }
        onReleased: {
            dragging = false;
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
