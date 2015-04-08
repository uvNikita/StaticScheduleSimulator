import QtQuick 2.0
import QtQuick.Dialogs 1.0
import QtQuick.Layouts 1.0
import Ubuntu.Components 1.1
import Ubuntu.Components.Popups 0.1

Item {
    property var callback;
    property var dialog: component
    Component {
        id: component
        Dialog {
            id: generateDialog
            title: "Generate new graph"

            ColumnLayout {
                RowLayout {
                    TextField {
                        Layout.fillWidth: true
                        id: nodeWeightMin
                        text: "1"
                        placeholderText: "Node min"
                        validator: IntValidator { bottom: 1; top: 99; }
                    }

                    TextField {
                        Layout.fillWidth: true
                        id: nodeWeightMax
                        text: "20"
                        placeholderText: "Node max"
                        validator: IntValidator { bottom: 1; top: 99; }
                    }
                }
                RowLayout {
                    TextField {
                        Layout.fillWidth: true
                        id: edgeWeightMin
                        text: "1"
                        placeholderText: "Edge min"
                        validator: IntValidator { bottom: 1; top: 99; }
                    }

                    TextField {
                        Layout.fillWidth: true
                        id: edgeWeightMax
                        text: "20"
                        placeholderText: "Edge max"
                        validator: IntValidator { bottom: 1; top: 99; }
                    }

                }

                TextField {
                    Layout.fillWidth: true
                    id: nodesCount
                    text: "10"
                    placeholderText: "Nodes count"
                    validator: IntValidator { bottom: 1; top: 99; }
                }

                TextField {
                    Layout.fillWidth: true
                    id: correlation
                    text: "0.6"
                    placeholderText: "Correlation"
                    validator: DoubleValidator { bottom: 0.0; top: 2.0; notation: DoubleValidator.StandardNotation}
                }

                RowLayout {
                    Button {
                        text: "Ok"
                        color: UbuntuColors.green
                        onClicked: {
                            var nwmin = parseInt(nodeWeightMin.text);
                            var nwmax = parseInt(nodeWeightMax.text);
                            var ewmin = parseInt(edgeWeightMin.text);
                            var ewmax = parseInt(edgeWeightMax.text);
                            var count = parseInt(nodesCount.text);
                            var corr = parseFloat(correlation.text);
                            if (!(nwmin > 0.0 &&
                                  nwmin < nwmax &&
                                  ewmin > 0.0 &&
                                  ewmin < ewmax &&
                                  count > 0 &&
                                  corr > 0 &&
                                  corr < 1.0)) {
                                    return;
                            }

                            var graph = generateTask(nwmin, nwmax, ewmin, ewmax, count, corr);
                            callback(JSON.parse(graph));
                            PopupUtils.close(generateDialog);
                        }
                    }
                    Button {
                        Layout.fillWidth: true
                        text: "Cancel"
                        onClicked: {
                            PopupUtils.close(generateDialog);
                        }
                    }
                }
            }
        }
    }
}