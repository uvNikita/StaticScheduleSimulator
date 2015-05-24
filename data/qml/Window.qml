import QtQuick 2.0
import Ubuntu.Components 1.1

MainView {
    useDeprecatedToolbar: false
    width: units.gu(128)
    height: units.gu(85)

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
            title: "Modeling"
            page: Modeling { task: task; system: system}
        }
    }
}
