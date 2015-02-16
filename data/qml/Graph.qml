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
        anchors.fill: parent
        onClicked: {
            Graph.append(mouseX, mouseY);
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
