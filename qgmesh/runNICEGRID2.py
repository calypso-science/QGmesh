from PyQt5.QtCore import Qt,QSettings,QProcess,QProcessEnvironment,pyqtSignal,QObject
from PyQt5.QtGui import QDoubleValidator,QTextCursor
from PyQt5 import QtWidgets
from qgis.core import QgsProject
import shlex
import os,sys
from .tools import *



class EmittingStream(QObject):

    textWritten = pyqtSignal(str)

    def __init__(self,parent=None):
        super(EmittingStream, self).__init__(parent)

    def write(self, text):
        self.textWritten.emit(str(text))
    def flush(self):
        pass

class RunSCHISMDialog(QtWidgets.QDialog) :

    def __init__(self,mesh,shapefiles=None, parent=None,tmp='/tmp') :

        super(RunSCHISMDialog, self).__init__(parent)
        self.setWindowTitle("Cleaning trimesh grid")
        self.mesh=mesh
        layout = QtWidgets.QVBoxLayout()

      
        self.ShapefileSelector = QtWidgets.QListWidget()
        self.ShapefileSelector.addItems(shapefiles)
        TitleLayout("Select node shapefile to edit", self.ShapefileSelector, layout)
        layout.addWidget(self.ShapefileSelector)
        

        self.algoSelector = QtWidgets.QComboBox(self)
        self.algoSelector.addItem("do not change boundary", "0")
        self.algoSelector.addItem("correct boundary as well", "1")
        self.algoSelector.addItem("correct and adjust boundary", "2")
        TitleLayout("Boundary options", self.algoSelector, layout)

        
        self.textWidget = QtWidgets.QPlainTextEdit()
        self.textWidget.setReadOnly(True)
        layout.addWidget(self.textWidget)

       
        hlayout = QtWidgets.QHBoxLayout()
        layout.addLayout(hlayout)
        hlayout.addStretch(1)
        self.runBtn = QtWidgets.QPushButton("Clean")
        self.runBtn.clicked.connect(self.clean)
        self.runBtn.show()
        hlayout.addWidget(self.runBtn)

        self.closeBtn = QtWidgets.QPushButton("Close")
        self.closeBtn.clicked.connect(self.close)
        hlayout.addWidget(self.closeBtn)
        self.killBtn = QtWidgets.QPushButton("Kill")
        self.killBtn.clicked.connect(self.killp)
        hlayout.addWidget(self.killBtn)
        self.resize(600, 600)
        self.setLayout(layout)

        self.tempdir=tmp

    def killp(self) :
        self.p.kill()
        self.log("Killed", "red")
        self.killed = True
        self.closeBtn.show()

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
            else :
                self.log("The grid is clean.", "green")
        self.closeBtn.show()
        self.closeBtn.setFocus()
        self.killBtn.hide()
        if os.path.isfile(os.path.join(os.path.dirname(__file__),'nicegrid.flag')):
            os.system('rm %s' % os.path.join(os.path.dirname(__file__),'nicegrid.flag'))

    def onError(self, state):
        if self.killed :
            return
        if state == QProcess.FailedToStart :
            self.log("Cannot start nicegrid2: ", "red")
        elif state == QProcess.Crashed :
            self.log("nicegrid2 crashed.", "red")
        else :
            self.log("Unkown nicegrid2 error.", "red")

    def clean(self):
        nicegrid2=os.path.join(os.path.dirname(__file__),'nicegrid2')
        fname=os.path.join( self.tempdir,'gridin.gr3')
        fout=os.path.join( self.tempdir,'gridout.gr3')
        algo=self.algoSelector.currentIndex()
        file3=self.ShapefileSelector.currentItem().text()
        if file3 !="None":
            X=self.mesh.x
            Y=self.mesh.y
            Z=self.mesh.x*0
            proj = QgsProject.instance()
            for child in proj.layerTreeRoot().findLayers():
                if child.name()==file3:
                    layer = proj.mapLayer(child.layerId())
                    for node in layer.getFeatures():
                        nodeID = node.attribute(0)-1
                        Z[nodeID]=1

            fileWrite = open(os.path.join(os.path.dirname(__file__),'nicegrid.flag'), 'w')
            # Add a header
            fileWrite.write('nicegrid.flag create with QGmesh\n')
            fileWrite.write('%i\n' % (len(X)))
            for node in range(0,len(X)):
                fileWrite.write('%i\t%f\t%f\t%f\t\n'% (node+1,X[node],Y[node],Z[node]))
            fileWrite.close()

        export_function=load_IO()
        export_function['schismIO']['export'](self.mesh,fname)

        self.p.start(nicegrid2,[fname,fout,str(algo)])

    def load_file(self):
        qfd = QFileDialog()
        path = ""

        import_function=load_IO()
        fil=''
        for model in import_function:
            fil+=import_function[model]['extension']()+';'


        fname = QFileDialog.getOpenFileName(qfd, 'Import Mesh file', path, fil)
        fname=fname[0]
        if fname=='':
            return

        filename, file_extension = os.path.splitext(fname)
        if file_extension=='':
            return

    def exec_(self) :
        self.p = QProcess()
        self.p.setProcessChannelMode(QProcess.MergedChannels)
        self.p.readyReadStandardOutput.connect(self.onStdOut)
        self.p.error.connect(self.onError)
        self.p.finished.connect(self.onFinished)
        self.textWidget.clear()
        self.closeBtn.hide()
        self.killed = False
        self.killBtn.show()
        self.killBtn.setFocus()
        self.show()
        env = QProcessEnvironment.systemEnvironment()
        env.remove("TERM")
        self.p.setProcessEnvironment(env)
        super(RunSCHISMDialog, self).exec_()