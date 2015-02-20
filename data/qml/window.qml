import QtQuick.Controls 1.2
import QtQuick.Layouts 1.0

ApplicationWindow {
    id: window
    visible: true
    title: "Static Scheduler Simulator"

    TabView {
        id: tabView
        anchors.fill: parent
        Layout.minimumWidth: 600
        Layout.minimumHeight: 400
        Layout.preferredWidth: 1000
        Layout.preferredHeight: 700
        Tab {
            title: "Task"
            Graph { }
        }
        Tab {
            title: "System"
            Graph { }
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
