import QtQuick.Controls 1.2
import QtQuick.Layouts 1.0

ApplicationWindow {
    id: window
    visible: true
    title: "Static Scheduler Simulator"

    TabView {
        id: tabView
        anchors.fill: parent
        Layout.minimumWidth: 400
        Layout.minimumHeight: 400
        Tab {
            title: "Task graph"
            Graph { }
        }
        Tab {
            title: "System graph"
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
