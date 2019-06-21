# author  : Jonathan Lambrechts jonathan.lambrechts@uclouvain.be
# licence : GPLv2 (see LICENSE.md)

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QApplication,QVBoxLayout,QHBoxLayout,QPushButton,QDialog,QLabel,QLineEdit,QFileDialog,QProgressDialog,QInputDialog,QWidget
from PyQt5.QtGui import QIcon
from qgis.gui import QgsProjectionSelectionTreeWidget
from qgis.core import *
import sys
import numpy as np
from PyQt5 import QtWidgets



class raster_calculator(QWidget):

    def __init__(self,rasters,funct_name):
        super(raster_calculator).__init__()

        layout = QtWidgets.QVBoxLayout()
        self.rasterSelector = QtWidgets.QListWidget()
        self.rasterSelector.addItems(rasters)
        TitleLayout("Raster", self.rasterSelector, layout)

        if funct_name=='wavelength':
            self.title = 'Wave length'
            self.setWindowTitle(self.title)
            self.initWL(layout)
    
    def initWL(self,layout):
       
        self.minimum_depth=QtWidgets.QLineEdit()
        self.minimum_depth.setText("5")
        TitleLayout("minimum depth", self.minimum_depth, layout)
        self.period=QtWidgets.QLineEdit()
        self.period.setText("20")
        TitleLayout("Wave period", self.period, layout)

        self.gravity=QtWidgets.QLineEdit()
        self.gravity.setText("9.81")
        TitleLayout("Gravity", self.gravity, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.wavelength_calc, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()


    def wavelength_calc(self):
        d0=float(self.minimum_depth.text())
        g=float(self.gravity.text())
        t=float(self.period.text())
        raster=self.rasterSelector.currentItem().text()
        self.close()
        
        # minimum_depth=5.
        # g=9.81
        # T=20
        # wid = QInputDialog.getDouble(self, "Convert to wavelength","minimum depth:", 5.00, 0, 100, 1)
        # return wid
        # #gdal_calc.py -A input.tif --outfile=result.tif --calc="A*(A>0)" --NoDataValue=0
        # # L = (g*T^2/2/pi)*sqrt(tanh(4*pi^2.*d/T^2/g));
    def exec_(self):
        super(raster_calculator, self).exec_()

def writeRasterLayer(layer, filename) :
    progress = QProgressDialog("Writing mesh size layer...", "Abort", 0, layer.width())
    progress.setMinimumDuration(0)
    progress.setWindowModality(Qt.WindowModal)
    progress.setValue(0)
    f = open(filename, "wb")
    ext = layer.extent()
    f.write(struct.pack("3d", ext.xMinimum(), ext.yMinimum(), 0))
    f.write(struct.pack("3d", ext.width() / layer.width(), ext.height() / layer.height(), 1))
    f.write(struct.pack("3i", layer.width(), layer.height(), 1))
    block = layer.dataProvider().block(1, layer.extent(), layer.width(), layer.height())
    for j in range(layer.width()) : 
        progress.setValue(j)
        if progress.wasCanceled():
            return False
        v = list([block.value(i, j) for i in range(layer.height() -1, -1, -1)])
        f.write(struct.pack("{}d".format(len(v)), *v))
    f.close()
    return True

class TitleLayout(QVBoxLayout) :

    def __init__(self, title, widget, parent) :
        super(TitleLayout, self).__init__()
        parent.addLayout(self)
        self.label = QLabel("<b>" + title + "</b>")
        self.addWidget(self.label)
        self.addWidget(widget)


class FileSelectorLayout(QVBoxLayout) :

    def __init__(self, title, mainWindow, action, ext, parent, txt = None) :
        super(FileSelectorLayout, self).__init__()
        self.action = action
        self.ext = ext
        self.title = title
        parent.addLayout(self)
        self.addWidget(QLabel("<b>" + title + "</b>"))
        self.mainWindow = mainWindow
        layout = QHBoxLayout()
        self.addLayout(layout)
        fileButton = QPushButton("");
        fileButton.setIcon(QIcon.fromTheme("document-open"))
        fileButton.clicked.connect(self.browseFile)
        self.fileWidget = QLineEdit()
        self.fileWidget.setText(txt)
        layout.addWidget(self.fileWidget)
        layout.addWidget(fileButton)

    def browseFile(self) :
        if self.action == "save" :
            filename,_ = QFileDialog.getSaveFileName(self.mainWindow,
                self.title, filter=self.ext)
        elif self.action == "opendir" :
            filename = QFileDialog.getExistingDirectory(self.mainWindow,
                self.title)
        else :
            filename,_ = QFileDialog.getOpenFileName(self.mainWindow,
                self.title, filter=self.ext)
        if filename :
            self.setFile(filename)

    def setFile(self, filename):
        self.fileWidget.setText(filename)

    def getFile(self) :
        return self.fileWidget.text()


class CancelRunLayout(QHBoxLayout) :

    def __init__(self, dialog, runTitle, runCallback, parent) :
        super(CancelRunLayout, self).__init__()
        parent.addLayout(self)
        self.addStretch(1)
        cancelButton = QPushButton("Cancel")
        cancelButton.clicked.connect(dialog.close)
        self.addWidget(cancelButton, 0)
        self.runButton = QPushButton(runTitle)
        self.runButton.clicked.connect(runCallback)
        self.addWidget(self.runButton, 0)

    def setFocus(self) :
        self.runButton.setFocus()



if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = raster_calculator(['1','2'],'wavelength')
    sys.exit(ex.exec_())