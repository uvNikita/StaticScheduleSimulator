import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1
import Ubuntu.Components.Popups 0.1

import "graph.js" as Graph

Page {
    property bool directed: false;

    tools: ToolbarItems {
        ToolbarButton {
            action: Action {
                text: "validate"
                onTriggered: {
                    var gr = JSON.stringify(Graph.getGraph());
                    console.log(gr);
                    validateTask(gr);
                }
            }
        }
    }

    Keys.onPressed: {
        switch(event.key) {
            case Qt.Key_Return:
                if (Graph.selectedNode !== undefined) {
                    var dialog = PopupUtils.open(nodeDialogComponent);
                    dialog.focus();
                } else if (Graph.selectedEdge !== undefined) {
                    var dialog = PopupUtils.open(edgeDialogComponent);
                    dialog.focus();
                }
                break;
            case Qt.Key_Delete:
                if (Graph.selectedNode !== undefined) {
                    Graph.deleteNode(Graph.selectedNode);
                    Graph.unselectNode();
                    canvas.requestPaint();
                } else if (Graph.selectedEdge !== undefined) {
                    Graph.deleteEdge(Graph.selectedEdge);
                    Graph.unselectEdge();
                    canvas.requestPaint();
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
                    var nodeId = Graph.selectedNode;
                    var weight = parseInt(text);
                    Graph.setNodeWeight(nodeId, weight);
                    Graph.unselectNode();
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
                    Graph.unselectNode();
                    canvas.requestPaint();
                }
            }
        }
    }

    Component {
        id: edgeDialogComponent
        Dialog {
            id: edgeDialog
            title: "Weight"
            text: "Enter weight of the edge"

            TextField {
                id: edgeWeight
                placeholderText: "1"
                validator: IntValidator { bottom: 1; top: 99; }
                onAccepted: {
                    var edgeId = Graph.selectedEdge;
                    var weight = parseInt(text);
                    Graph.setEdgeWeight(edgeId, weight);
                    Graph.unselectEdge();
                    PopupUtils.close(edgeDialog);
                    canvas.requestPaint();
                }
            }

            function focus() {
                edgeWeight.forceActiveFocus();
            }
            Button {
                text: "Cancel"
                onClicked: {
                    PopupUtils.close(edgeDialog)
                    Graph.unselectEdge();
                    canvas.requestPaint();
                }
            }
        }
    }

    Label {
        text: taskGraphResult
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
                var edgeId = Graph.edgeOnPosition(mouseX, mouseY);
                if (nodeId !== undefined) {
                    if (Graph.selectedNode !== undefined) {
                        if (nodeId == Graph.selectedNode) {
                            Graph.unselectNode();
                            canvas.requestPaint();
                        } else {
                            Graph.appendEdge(Graph.selectedNode, nodeId, 1);
                            Graph.unselectNode();
                            animateChart.start();
                        }
                    } else {
                        Graph.selectNode(nodeId);
                    }
                } else if (edgeId !== undefined) {
                    if (edgeId == Graph.selectedEdge) {
                        Graph.unselectEdge();
                        canvas.requestPaint();
                    } else {
                        Graph.selectEdge(edgeId);
                    }
                } else {
                    Graph.appendNode(mouseX, mouseY, 1);
                    Graph.unselectEdge();
                    animateChart.start();
                }
                canvas.requestPaint();
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
