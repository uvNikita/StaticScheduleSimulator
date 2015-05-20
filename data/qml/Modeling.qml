import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1

Page {
    property var system;
    property var task;

    tools: ToolbarItems {
        ToolbarButton {
            action: Action {
                id: modelateAction
                iconName: "media-playback-start"
                enabled: true
                onTriggered: {
                    enabled = false;
                    var system_graph = JSON.stringify(system.getGraph());
                    var task_graph = JSON.stringify(task.getGraph());
                    modelate(task_graph, system_graph);
                }
            }
        }
    }

    Component.onCompleted: {
        onModelationFinished.connect(function() {
            console.log("DONE");
            console.log(simulationResult);
            modelateAction.enabled = true;
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
            Layout.fillHeight: true
            Layout.fillWidth: true
        }
    }
}
