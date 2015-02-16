import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0

Rectangle {
    anchors.fill: parent
    property var nodes: []

    Canvas {
        id: canvas
        anchors.fill: parent
        property double animationProgress: 0.0

        onPaint: {
            function draw(t) {
                var context = getContext("2d");

                context.beginPath();
                context.clearRect(0, 0, width, height);
                context.fill();

                for (var i = 0; i < parent.nodes.length; i++) {
                    var node = parent.nodes[i]
                    context.beginPath();

                    var angle = -2 * Math.PI
                    var radius = 20

                    if (i == parent.nodes.length - 1) {
                        radius = t * radius
                    }
                    console.log(angle)
                    context.arc(node.x, node.y, radius, 0, angle, true)
                    context.stroke();
                }
            }
            draw(animationProgress)
        }

        onAnimationProgressChanged: {
            requestPaint();
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            parent.nodes.push({x: mouseX, y: mouseY});
            animateChart.start()
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
