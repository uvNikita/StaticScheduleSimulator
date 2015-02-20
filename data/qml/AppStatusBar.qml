import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.0

StatusBar {
    property alias text: label.text

    state: "DEFAULT"
    states: [
        State {
            name: "DEFAULT"
            PropertyChanges { target: label; color: "black" }
        },
        State {
            name: "ERROR"
            PropertyChanges { target: label; color: "red" }
        }
    ]

    function pulseOk() {
        pulseAnimation.start();
    }

    RowLayout {
        Label {
            id: label
            text: "Done"
            color: "black"
        }
    }
    SequentialAnimation {
        id: pulseAnimation
        ColorAnimation { target: label; property: "color"; to: "green"; duration: 300 }
        ColorAnimation { target: label; property: "color"; to: "black"; duration: 300 }
    }
}
