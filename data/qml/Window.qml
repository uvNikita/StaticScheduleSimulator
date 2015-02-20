import QtQuick 2.0
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
            Graph { directed: true }
        }
        Tab {
            title: "System"
            Graph { directed: false }
        }
    }

    statusBar: AppStatusBar { id: statusbar }

}
