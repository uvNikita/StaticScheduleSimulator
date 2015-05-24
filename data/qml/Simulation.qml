import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1

import "diagram.js" as Diagram

Page {
    property var system;
    property var task;

    tools: ToolbarItems {
        ToolbarButton {
            action: Action {
                id: simulateAction
                iconName: "media-playback-start"
                enabled: true
                onTriggered: {
                    enabled = false;
                    var system_graph = JSON.stringify(system.getGraph());
                    var task_graph = JSON.stringify(task.getGraph());
                    simulate(task_graph, system_graph);
                }
            }
        }
    }

    Component.onCompleted: {
        onSimulationFinished.connect(function() {
            simulateAction.enabled = true;
            canvas.requestPaint();
        });
    }

    ColumnLayout {
        anchors.fill: parent
        RowLayout {
            Label {
                property var res: JSON.parse(taskValidationResult)
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width / 2
                horizontalAlignment: Text.AlignHCenter
                fontSize: "large"
                text: "Task: " + res.errors
                onResChanged: {
                    if (res.status == "ok") {
                        color = "green";
                    } else if (res.status == "error") {
                        color = "red";
                    } else {
                        color = "black";
                    }
                }
            }
            Label {
                property var res: JSON.parse(systemValidationResult)
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width / 2
                horizontalAlignment: Text.AlignHCenter
                fontSize: "large"
                text: "System: " + res.errors
                onResChanged: {
                    if (res.status == "ok") {
                        color = "green";
                    } else if (res.status == "error") {
                        color = "red";
                    } else {
                        color = "black";
                    }
                }
            }
            // Label {
            //     Layout.fillWidth: true
            //     Layout.preferredWidth: parent.width / 2
            //     horizontalAlignment: Text.AlignHCenter
            //     fontSize: "large"
            //     text: "System: " + JSON.parse(systemValidationResult).errors
            // }
        }
        Canvas {
            id: canvas
            Layout.fillHeight: true
            // anchors.fill: parent
            Layout.fillWidth: true
            onPaint: {
                Diagram.draw(canvas.getContext("2d"), JSON.parse(simulationResult));
            }
        }
    }
}
