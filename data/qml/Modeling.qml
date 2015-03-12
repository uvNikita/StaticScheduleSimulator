import QtQuick 2.0
import QtWebKit 3.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1

Page {
    property var system;
    property var task;

    tools: ToolbarItems {
        ToolbarButton {
            text: "asdf"
            action: Action {
                iconName: "media-playback-start"
                onTriggered: {
                    var system_graph = JSON.stringify(system.getGraph());
                    validateSystem(system_graph);
                    var task_graph = JSON.stringify(task.getGraph());
                    validateTask(task_graph);
                    console.log(system_graph);
                    console.log(task_graph);
                }
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent
        RowLayout {
            Label {
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width / 2
                horizontalAlignment: Text.AlignHCenter
                fontSize: "large"
                text: taskGraphResult
            }
            Label {
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width / 2
                horizontalAlignment: Text.AlignHCenter
                fontSize: "large"
                text: systemGraphResult
            }
        }
        Canvas {
            Layout.fillHeight: true
            Layout.fillWidth: true
        }
    }
}
