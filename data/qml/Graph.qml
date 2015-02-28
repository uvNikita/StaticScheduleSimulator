import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1
import Ubuntu.Components.Popups 0.1

import "graph.js" as Graph

Page {
    property bool directed: false;

    Keys.onPressed: {
        switch(event.key) {
            case Qt.Key_Return:
                if (Graph.selected !== undefined) {
                    var dialog = PopupUtils.open(nodeDialogComponent);
                    dialog.focus();
                }
                break;
        }
    }

    Component {
        id: nodeDialogComponent
        Dialog {
            id: nodeDialog
            title: "Weight"
            text: "Enter weight of the node"

            TextField {
                id: nodeWeight
                placeholderText: "1"
                validator: IntValidator { bottom: 1; top: 99; }
                onAccepted: {
                    var nodeId = Graph.selected;
                    var weight = parseInt(text);
                    Graph.setWeight(nodeId, weight);
                    Graph.unselect();
                    PopupUtils.close(nodeDialog);
                    canvas.requestPaint();
                }
            }

            function focus() {
                nodeWeight.forceActiveFocus();
            }
            Button {
                text: "Cancel"
                onClicked: {
                    PopupUtils.close(nodeDialog)
                    Graph.unselect();
                    canvas.requestPaint();
                }
            }
        }
    }

    Canvas {
        id: canvas
        anchors.fill: parent
        property double animationProgress: 0.0

        onPaint: {
            Graph.init(canvas.getContext("2d"), directed);
            Graph.draw(animationProgress);
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
                        if (nodeId == Graph.selected) {
                            Graph.unselect();
                            //statusbar.state = "DEFAULT";
                            canvas.requestPaint();
                        } else {
                            Graph.appendEdge(Graph.selected, nodeId);
                            Graph.unselect();
                            animateChart.start();

                            //statusbar.text = "Done";
                            //statusbar.pulseOk();
                        }
                    } else {
                        Graph.select(nodeId);
                        //statusbar.text = "Now select second node...";
                    }
                    canvas.requestPaint();
                } else {
                    Graph.appendNode(mouseX, mouseY, 1);
                    animateChart.start();
                }
            }
            dragging = false;
            dragId = -1;
        }
    }

    UbuntuNumberAnimation {
        id: animateChart
        target: canvas
        properties: "animationProgress"
        from: 0.0
        to: 1.0
        duration: UbuntuAnimation.SnapDuration
    }
}
