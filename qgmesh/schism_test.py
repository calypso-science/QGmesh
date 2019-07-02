from PyQt5.QtCore import Qt,QSettings,QProcess,QProcessEnvironment,pyqtSignal,QObject
from PyQt5.QtGui import QDoubleValidator,QTextCursor
from PyQt5 import QtWidgets
from qgis.core import QgsProject
import shlex
import os,sys


class EmittingStream(QObject):

    textWritten = pyqtSignal(str)

    def __init__(self,parent=None):
        super(EmittingStream, self).__init__(parent)

    def write(self, text):
        self.textWritten.emit(str(text))
    def flush(self):
        pass

class RunSCHISMDialog(QtWidgets.QDialog) :

    def __init__(self, parent=None) :

        super(RunSCHISMDialog, self).__init__(parent)
        self.setWindowTitle("Running SCHISM test")
        layout = QtWidgets.QVBoxLayout()
        self.root='/home/remy/Software/QGmesh/test/test_schism'
        self.textWidget = QtWidgets.QPlainTextEdit()
        self.textWidget.setReadOnly(True)
        layout.addWidget(self.textWidget)
        hlayout = QtWidgets.QHBoxLayout()
        layout.addLayout(hlayout)
        hlayout.addStretch(1)
       
        self.closeBtn = QtWidgets.QPushButton("Close")
        self.closeBtn.clicked.connect(self.close)
        hlayout.addWidget(self.closeBtn)
        self.killBtn = QtWidgets.QPushButton("Kill")
        self.killBtn.clicked.connect(self.killp)
        hlayout.addWidget(self.killBtn)
        self.resize(600, 600)
        self.setLayout(layout)

    def killp(self) :
        self.p.kill()
        self.log("Killed", "red")
        self.killed = True

    def onStdOut(self) :
        while self.p.canReadLine() :
            txt = str(self.p.readLine().data(),"utf8")
            if txt.startswith("Error   : ") or txt.startswith("Fatal   : "):
                self.log(txt[10:], "red")
            elif txt.startswith("Warning : ") :
                self.log(txt[10:], "orange")
            elif txt.startswith("Info    : Running") :
                self.log(txt[10:], "black")
            elif txt.startswith("Info    : ") :
                self.log(txt[10:])
            else :
                self.log(txt)

    def log(self, msg, color = None):
        scroll = self.textWidget.verticalScrollBar()
        scrollToEnd = scroll.value() == scroll.maximum()
        if color is not None :
            self.textWidget.appendHtml("<b> <font color=%s>" % color + msg + "</b></font>")
        else :
            self.textWidget.appendHtml(msg)
        if scrollToEnd :
            scroll.setValue(scroll.maximum())

    def onFinished(self, state) :
        if not self.killed :
            if state != 0 :
                self.log("The grid has problem.", "red")
                f = open(os.path.join(self.root,'fort.11'), "r")
                for xx in f:
                    self.log(xx, "red")
                f.close()
            else :
                self.log("The grid is good.", "green")
        self.closeBtn.show()
        self.closeBtn.setFocus()
        self.killBtn.hide()

    def onError(self, state):
        if self.killed :
            return
        if state == QProcess.FailedToStart :
            self.log("Cannot start SCHISM executable : " + self.args[0], "red")
        elif state == QProcess.Crashed :
            self.log("SCHISM crashed.", "red")
        else :
            self.log("Unkown SCHISM error.", "red")

    def exec_(self, args) :
        self.p = QProcess()
        self.p.setProcessChannelMode(QProcess.MergedChannels)
        self.p.readyReadStandardOutput.connect(self.onStdOut)
        self.p.error.connect(self.onError)
        self.p.finished.connect(self.onFinished)
        self.textWidget.clear()
        self.args = args
        self.closeBtn.hide()
        self.killed = False
        self.killBtn.show()
        self.killBtn.setFocus()
        self.show()
        env = QProcessEnvironment.systemEnvironment()
        env.remove("TERM")
        self.p.setProcessEnvironment(env)
        self.p.start(args[0], args[1:])
        super(RunSCHISMDialog, self).exec_()