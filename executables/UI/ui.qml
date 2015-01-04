// bldr: qml -apptype widget %

import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

ApplicationWindow {
    id: window
    visible: true;
    width: 398;
    height: 133;

    Rectangle {
        id: entryBar;
        color: "transparent"
        height: 30;
        width: parent.width;
        anchors.top: parent.top;
        anchors.topMargin: 3;
        anchors.right: parent.right;
        anchors.rightMargin: 4;
        anchors.left: parent.left;
        anchors.leftMargin: 4;

        Label {
            id: langPairState;
            font.pointSize: 8;
            text: langPair;
            font.family: "Sans";
            anchors.verticalCenter: input.verticalCenter;
        }

        TextField {
            id: input;
            focus: true;
            onAccepted: (function (){
                    handleInput(input.text);
                    input.selectAll();
                })();
            font.pointSize: 8;
            height: 22;
            anchors.left: langPairState.right;
            anchors.right: parent.right;
            anchors.leftMargin: 8;
        }

        BusyIndicator {
            running: isLoading;
            anchors.right: input.right;
            anchors.verticalCenter: input.verticalCenter;
            height: input.height * 0.78;
        }
    }

    Rectangle {
        color: isError ? "#33FF0000" : "#00000000";
        width: parent.width;
        anchors.top: entryBar.bottom;
        anchors.bottom: parent.bottom;

        Text {
            id: output;
            anchors.fill: parent;
            anchors.margins: 4;
            wrapMode: Text.WrapAnywhere;
            font.pointSize: 8;
            font.family: "Sans";
            text: result;
        }
    }
}
