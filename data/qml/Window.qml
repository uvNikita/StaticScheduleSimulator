import QtQuick 2.0
import QtQuick.Controls 1.1
import Ubuntu.Components 1.1

// ApplicationWindow {
Item {
    visible: true
    width: units.gu(128)
    height: units.gu(84)
    MainView {
        useDeprecatedToolbar: false
        width: parent.width
        height: parent.height

        Keys.onPressed: {
            if(event.modifiers && Qt.ControlModifier) {
                switch(event.key) {
                    case Qt.Key_Tab:
                        var cid = tabView.selectedTabIndex;
                        tabView.selectedTabIndex = cid === tabView.count - 1 ? 0 : cid + 1;
                        event.accepted = true;
                        break;
                }
            }
        }

        Tabs {
            id: tabView
            Tab {
                title: "Task"
                page: Graph { id: task; directed: true }
            }
            Tab {
                title: "System"
                page: Graph { id: system; directed: false }
            }
            Tab {
                title: "Simulation"
                page: Simulation { task: task; system: system}
            }
        }
    }
}
