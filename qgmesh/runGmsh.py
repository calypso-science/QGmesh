# author  : Jonathan Lambrechts jonathan.lambrechts@uclouvain.be
# licence : GPLv2 (see LICENSE.md)

from PyQt5.QtCore import Qt,QSettings,QProcess,QProcessEnvironment
from PyQt5.QtGui import QDoubleValidator
from PyQt5 import QtWidgets
from qgis.core import QgsProject
import shlex
import os



class RunGmshDialog(QtWidgets.QDialog) :

    def __init__(self) :
        super(RunGmshDialog, self).__init__()
        self.setWindowTitle("Running Gmsh")
        layout = QtWidgets.QVBoxLayout()
        self.textWidget = QtWidgets.QPlainTextEdit()
        self.textWidget.setReadOnly(True)
        layout.addWidget(self.textWidget)
        hlayout = QtWidgets.QHBoxLayout()
        layout.addLayout(hlayout)
        hlayout.addStretch(1)
        self.closeBtn = QtWidgets.QPushButton("Close")
        hlayout.addWidget(self.closeBtn)
        self.closeBtn.show()
        self.closeBtn.clicked.connect(self.close)
        self.killBtn = QtWidgets.QPushButton("Kill")
        self.killBtn.clicked.connect(self.killp)
        hlayout.addWidget(self.killBtn)
        self.resize(600, 600)
        self.setLayout(layout)

    def killp(self) :
        self.p.kill()
        self.closeBtn.show()
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
                self.log("An error occured.", "red")
            else :
                self.log("Gmsh finished.", "green")
                self.loadMshBtn.show()
        self.closeBtn.show()
        self.closeBtn.setFocus()
        self.killBtn.hide()

    def onError(self, state):
        if self.killed :
            return
        if state == QProcess.FailedToStart :
            self.log("Cannot start gmsh executable : " + self.args[0], "red")
        elif state == QProcess.Crashed :
            self.log("Gmsh crashed.", "red")
        else :
            self.log("Unkown gmsh error.", "red")

    def exec_(self, geofile) :
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
        import pygmsh
		mesh = pygmsh.generate_mesh(geo.geo)
        self.p.start(args[0], args[1:])
        super(RunGmshDialog, self).exec_()



