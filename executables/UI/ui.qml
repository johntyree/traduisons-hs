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
        color: "#E1E1E1"
        height: 30;
        width: parent.width;
        id: entryBar;
        anchors.top: parent.top;
        anchors.topMargin: 3;
        anchors.right: parent.right;
        anchors.rightMargin: 4;
        anchors.left: parent.left;
        anchors.leftMargin: 4;

        Label {
            id: langPairState;
            font.pointSize: 8;
            // text: langPair;
            text: "en | fi:";
            font.family: "Sans";
            anchors.verticalCenter: input.verticalCenter;
        }

        TextField {
            id: input;
            focus: true;
            onAccepted: factorial(input.text);
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

    Text {
        id: output;
        anchors.top: entryBar.bottom;
        anchors.bottom: window.bottom;
        wrapMode: Text.WrapAnywhere;
        font.pointSize: 8;
        font.family: "Sans";
        text: result;
    }
}
