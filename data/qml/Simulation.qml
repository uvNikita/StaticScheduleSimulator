import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1
import Ubuntu.Components.Popups 0.1

import "diagram.js" as Diagram

Page {
    property var system;
    property var task;

    head.actions: [
            Action {
                id: simulateAction
                iconName: "media-playback-start"
                enabled: true
                onTriggered: {
                    enabled = false;
                    PopupUtils.open(simulationConfig.dialog);
                }
            }
        ]

    SimulationConfigDialog {
        id: simulationConfig
        success_callback: function (config) {
            var system_graph = JSON.stringify(system.getGraph());
            var task_graph = JSON.stringify(task.getGraph());
            var config = JSON.stringify(config);
            simulate(task_graph, system_graph, config);
        }
        cancel_callback: function () {
            simulateAction.enabled = true;
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
        }
        Canvas {
            id: canvas
            Layout.fillHeight: true
            Layout.fillWidth: true
            onPaint: {
                Diagram.draw(canvas.getContext("2d"), JSON.parse(simulationResult));
            }
        }
    }
}
