# author  : Jonathan Lambrechts jonathan.lambrechts@uclouvain.be
# licence : GPLv2 (see LICENSE.md)

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QApplication,QVBoxLayout,QHBoxLayout,QPushButton,QDialog,QLabel,QLineEdit,QFileDialog,QProgressDialog,QInputDialog,QWidget
from PyQt5.QtGui import QIcon,QColor
from qgis.gui import QgsProjectionSelectionTreeWidget
from qgis.core import *
import sys,os
import numpy as np
from PyQt5 import QtWidgets
import struct
from datetime import datetime
from qgis.utils import iface

def get_layer(layer_name):
    proj = QgsProject.instance()
    raster=[]
    for child in proj.layerTreeRoot().findLayers():        
        layer = proj.mapLayer(child.layerId())
        if layer.name()==layer_name: #and layer.type()==QgsMapLayer.RasterLayer:
            return layer

def update_field(layer,fieldName,value):
    layer.startEditing()
    features=layer.getFeatures()
    layer_provider=layer.dataProvider()
    idx = layer.fields().indexFromName(fieldName)

    for ie,f in enumerate(features):
        id=f.id()
        attr_value={idx:float('%9.3f' % value[ie])}
        layer_provider.changeAttributeValues({id:attr_value})

    layer.commitChanges()

def assign_bathy(layer):
    classes=10
    renderer = layer.renderer()
    provider = layer.dataProvider()
    idx = layer.fields().indexFromName('Depth')
    max_val=layer.maximumValue(idx)
    min_val=layer.minimumValue(idx)
    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    colDic = {'green':'#40ff00','blue':'0000ff'}

    colorRamp = QgsGradientColorRamp.create(colDic)
    renderer = QgsGraduatedSymbolRenderer.createRenderer( layer, 'Depth', classes, QgsGraduatedSymbolRenderer.Pretty, symbol, colorRamp )
    layer.setRenderer( renderer )


    layer.triggerRepaint()

    return layer

def assign_values(layer,min_val,warn_val):

    renderer = layer.renderer()
    provider = layer.dataProvider()



    colDic = {'red':'#ff0000', 'yellow':'#ffe600','green':'#40ff00'}


    myRangeList = []
    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(colDic['red']))
    myRange = QgsRendererRange(0, min_val, symbol, 'Bad CFL')
    myRangeList.append(myRange)

    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(colDic['yellow']))
    myRange = QgsRendererRange( min_val+0.0001,warn_val, symbol, 'Warning CFL')
    myRangeList.append(myRange)

    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(colDic['green']))
    myRange = QgsRendererRange( warn_val+0.0001,np.inf, symbol, 'Good CFL')
    myRangeList.append(myRange)


    myRenderer = QgsGraduatedSymbolRenderer('CFL', myRangeList)
    myRenderer.setMode(QgsGraduatedSymbolRenderer.Custom)

                                                                  

    layer.setRenderer(myRenderer)

    #layer.triggerRepaint()

    return layer


def assign_colorbar(layer):

    renderer = layer.renderer()
    provider = layer.dataProvider()

    ver = provider.hasStatistics(1, QgsRasterBandStats.All)

    stats = provider.bandStatistics(1, QgsRasterBandStats.All,layer.extent(), 0)


    if (stats.minimumValue < 0):
       min = 0  

    else: 
        min= stats.minimumValue

    max = stats.maximumValue
    range = max - min
    add = range//2
    interval = min + add

    colDic = {'red':'#ff0000', 'yellow':'#ffff00','blue':'#0000ff'}

    valueList =[min, interval, max]

    lst = [QgsColorRampShader.ColorRampItem(valueList[0], QColor(colDic['red'])),\
           QgsColorRampShader.ColorRampItem(valueList[1], QColor(colDic['yellow'])), \
           QgsColorRampShader.ColorRampItem(valueList[2], QColor(colDic['blue']))]

    myRasterShader = QgsRasterShader()
    myColorRamp = QgsColorRampShader()

    myColorRamp.setColorRampItemList(lst)
    myColorRamp.setColorRampType(QgsColorRampShader.Interpolated)
    myRasterShader.setRasterShaderFunction(myColorRamp)

    myPseudoRenderer = QgsSingleBandPseudoColorRenderer(layer.dataProvider(), 
                                                        layer.type(),
                                                        myRasterShader)                                                                    

    layer.setRenderer(myPseudoRenderer)

    layer.triggerRepaint()

def add_raster(fname,name):
    proj = QgsProject.instance()
    vlayer = QgsRasterLayer(fname, name)
    assign_colorbar(vlayer)
    proj.addMapLayer(vlayer)

def write_raster(shapesSHPFilename,rasterisedShapesFilename,xy,res):
    #raster bound
    xiMin = xy[0]
    xiMax = xy[1]
    etaMin = xy[2]
    etaMax = xy[3]

    # raster resolution
    numb_xiPoints = res[0]
    numb_etaPoints = res[1]

    # Calculate output resolution
    delta_xi = abs(xiMax - xiMin)/(numb_xiPoints-1)
    delta_eta = abs(etaMax - etaMin)/(numb_etaPoints-1)
    # The distance function calculation is here carried out using gdal_proximity.
    # In order for that utility to give accurate distances the pixels must be
    # square: delta_xi == delta_eta. Below we check for that condition, 
    # and if not the case we change the resolution and/or enlarge the raster
    # extents, to make the pixels square.
    if delta_xi != delta_eta:
        if delta_xi < delta_eta:
            delta_eta = delta_xi
            numb_etaPoints = (abs(etaMax - etaMin)/delta_eta) + 1
            numb_etaPoints = int(np.ceil(numb_etaPoints))
            etaMax = delta_eta*(numb_etaPoints - 1) + etaMin
        elif delta_eta < delta_xi:
            delta_xi = delta_eta
            numb_xiPoints = (abs(xiMax - xiMin)/delta_xi) + 1
            numb_xiPoints = int(np.ceil(numb_xiPoints))
            xiMax = delta_xi*(numb_xiPoints - 1) + xiMin


    gdal_rasterize_cmd = 'gdal_rasterize -q -burn 1 -a_nodata 0 -init 0 -at -tr %f %f -te %f %f %f %f -of netCDF %s %s' \
    % (delta_xi,delta_eta,xiMin,etaMin,xiMax,etaMax,shapesSHPFilename,rasterisedShapesFilename)

    os.system(gdal_rasterize_cmd)



def writeRasterLayer(layer, filename) :
    progress = QProgressDialog("Writing mesh size layer...", "Abort", 0, layer.width())
    progress.setMinimumDuration(0)
    progress.setWindowModality(Qt.WindowModal)
    progress.setValue(0)
    # f = open(filename, "w")
    # ext = layer.extent()
    # f.write('%f\t%f\t%f\n' % (ext.xMinimum(), ext.yMinimum(), 0))
    # f.write('%f\t%f\t%f\n' % (ext.width() / layer.width(), ext.height() / layer.height(), 1))
    # f.write('%f\t%f\t%f\n' % (layer.width(), layer.height(), 1))
    # block = layer.dataProvider().block(1, layer.extent(), layer.width(), layer.height())
    # for j in range(layer.width()) : 
    #     progress.setValue(j)
    #     if progress.wasCanceled():
    #         return False
    #     v = list([block.value(i, j) for i in range(layer.height() -1, -1, -1)])
    #     f.write('\t'.join(str(x) for x in v)+'\n')
    # f.close()
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
