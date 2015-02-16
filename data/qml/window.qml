import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.0

ApplicationWindow {
    id: window
    visible: true
    title: "Static Scheduler Simulator"

    Rectangle {
        anchors.fill: parent
        Layout.minimumWidth: 400
        Layout.minimumHeight: 400
        property var nodes: []

        Canvas {
            id: canvas
            anchors.fill: parent
            onPaint: {
                var context = getContext("2d");

                context.beginPath();
                context.clearRect(0, 0, width, height);
                context.fill();

                for (var i = 0; i < parent.nodes.length; i++) {
                    var node = parent.nodes[i]
                    context.beginPath();
                    context.arc(node.x, node.y, 10, 0, 2*Math.PI, true)
                    context.stroke();
                }
            }
        }

        MouseArea {
            anchors.fill: parent
            onClicked: {
                parent.nodes.push({x: mouseX, y: mouseY});
                canvas.requestPaint();
            }
        }
    }

    statusBar: StatusBar {
        RowLayout {
            anchors.fill: parent
            Label {
                text: "Status bar"
            }
        }
    }
}
