import QtQuick 2.0
import QtQuick.Dialogs 1.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1
import Ubuntu.Components.Popups 0.1
import Ubuntu.Components.ListItems 1.0 as ListItem

Item {
    property var callback;
    property var dialog: component
    Component {
        id: component
        Dialog {
            id: simulationDialog
            title: "Simulation config"
            Column {
                spacing: units.gu(3)
                ListItem.ItemSelector {
                    id: simulationSelector
                    text: "Simulation"
                    model: ["Simple", "Optimized"]
                }
                ListItem.ItemSelector {
                    id: queueSelector
                    text: "Queue generator"
                    model: ["Critical path", "Difference", "Random"]
                }
                ListItem.ItemSelector {
                    id: connectionSelector
                    text: "Connection"
                    model: ["Full Duplex", "Half Duplex"]
                }
                RowLayout {
                    Button {
                        text: "Ok"
                        color: UbuntuColors.green
                        onClicked: {
                            var simulation;
                            switch (simulationSelector.selectedIndex) {
                                case 0:
                                    simulation = "simple";
                                    break;
                                case 1:
                                    simulation = "optimized";
                                    break;
                            }

                            var queue;
                            switch (queueSelector.selectedIndex) {
                                case 0:
                                    queue = {"type": "crit_path"};
                                    break;
                                case 1:
                                    queue = {"type": "diff"};
                                    break;
                                case 2:
                                    queue = {"type": "random", "seed": Math.floor(Math.random() * 10)};
                                    break;
                            }
                            var connection;
                            switch (connectionSelector.selectedIndex) {
                                case 0:
                                    connection = "fullduplex";
                                    break;
                                case 1:
                                    connection = "halfduplex";
                                    break;
                            }
                            var config = {
                                "simulation": simulation,
                                "queue": queue,
                                "connection": connection
                            }
                            callback(config);
                            PopupUtils.close(simulationDialog);
                        }
                    }
                    Button {
                        Layout.fillWidth: true
                        text: "Cancel"
                        onClicked: {
                            PopupUtils.close(simulationDialog);
                        }
                    }
                }
            }
        }
    }
}