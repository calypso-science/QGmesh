# author  : Jonathan Lambrechts jonathan.lambrechts@uclouvain.be
# licence : GPLv2 (see LICENSE.md)

from PyQt5.QtCore import Qt,QSettings,QProcess,QProcessEnvironment,pyqtSignal,QObject
from PyQt5.QtGui import QDoubleValidator,QTextCursor
from PyQt5 import QtWidgets
from qgis.core import QgsProject
import shlex
import os,sys
import pygmsh

class EmittingStream(QObject):

    textWritten = pyqtSignal(str)

    def __init__(self,parent=None):
        super(EmittingStream, self).__init__(parent)

    def write(self, text):
        self.textWritten.emit(str(text))
    def flush(self):
        pass
class RunGmshDialog(QtWidgets.QDialog) :

    def __init__(self,geo,parent=None) :
        super(RunGmshDialog, self).__init__(parent)
        self.setWindowTitle("Running Gmsh")
        layout = QtWidgets.QVBoxLayout()
        self.textWidget = QtWidgets.QPlainTextEdit()
        self.textWidget.setReadOnly(True)
        layout.addWidget(self.textWidget)
        hlayout = QtWidgets.QHBoxLayout()
        layout.addLayout(hlayout)
        hlayout.addStretch(1)
        self.runBtn = QtWidgets.QPushButton("Run")
        hlayout.addWidget(self.runBtn)
        self.runBtn.clicked.connect(self.meshit)
        self.closeBtn = QtWidgets.QPushButton("Close")
        hlayout.addWidget(self.closeBtn)
        self.closeBtn.show()
        self.closeBtn.clicked.connect(self.close)
        self.killBtn = QtWidgets.QPushButton("Kill")
        self.killBtn.clicked.connect(self.killp)
        self.geo=geo.geo
        self.msh=None




        hlayout.addWidget(self.killBtn)
        self.resize(600, 600)
        self.setLayout(layout)
        #sys.stdout = EmittingStream(textWritten=self.normalOutputWritten)
        self.killed = False
    def __del__(self):
    # Restore sys.stdout
        sys.stdout = sys.__stdout__

    def killp(self) :
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

        self.closeBtn.show()
        self.closeBtn.setFocus()
        self.killBtn.hide()



    def normalOutputWritten(self, text):
        """Append text to the QTextEdit."""
        # Maybe QTextEdit.append() works as well, but this is how I do it:
        cursor = self.textWidget.textCursor()
        cursor.movePosition(QTextCursor.End)
        cursor.insertText(text)
        self.textWidget.setTextCursor(cursor)
        self.textWidget.ensureCursorVisible()

    def meshit(self):
        self.log('Creating the mesh...',"green")
        cursor = self.textWidget.textCursor()
        cursor.movePosition(QTextCursor.End)
        self.show()
        try:

            self.msh=pygmsh.generate_mesh(self.geo,extra_gmsh_arguments=['-2','-algo','front2d','-epslc1d','1e-3'])
            self.closeBtn.show()
            self.onFinished(0)
            self.runBtn.hide()

        except:
            self.onFinished(1)

        sys.stdout = sys.__stdout__





    def exec_(self) :
        

        self.closeBtn.hide()
        self.killBtn.show()
        self.killBtn.setFocus()

        
        self.textWidget.clear()
        cursor = self.textWidget.textCursor()
        cursor.movePosition(QTextCursor.End)
        self.show()




        super(RunGmshDialog, self).exec_()
        return self.msh


        
        
