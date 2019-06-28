# author  : Jonathan Lambrechts jonathan.lambrechts@uclouvain.be
# licence : GPLv2 (see LICENSE.md)

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QApplication,QVBoxLayout,QHBoxLayout,QPushButton,QDialog,QLabel,QLineEdit,QFileDialog,QProgressDialog,QInputDialog,QWidget
from PyQt5.QtGui import QIcon
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

def add_raster(fname,name):
    proj = QgsProject.instance()
    vlayer = QgsRasterLayer(fname, name)
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


    gdal_rasterize_cmd = ['gdal_rasterize',
          '-q','-burn','1','-a_nodata','0','-init','0','-at',
          '-tr',str(delta_xi),str(delta_eta),
          '-te',str(self.xiMin),str(self.etaMin),str(self.xiMax),str(self.etaMax),
          '-of','netCDF',shapesSHPFilename,rasterisedShapesFilename]

class raster_calculator(QtWidgets.QDialog):

    def __init__(self,rasters,funct_name):
        super(raster_calculator,self).__init__()


        layout = QtWidgets.QVBoxLayout()
        self.rasterSelector = QtWidgets.QListWidget()
        self.rasterSelector.addItems(rasters)
        if funct_name=='distance':
            TitleLayout("Shapes", self.rasterSelector, layout)
        else:
            TitleLayout("Raster", self.rasterSelector, layout)

        if funct_name=='wavelength':
            self.title = 'Wave length'
            self.setWindowTitle(self.title)
            self.initWL(layout)
        if funct_name=='scale':
            self.title = 'Scale'
            self.setWindowTitle(self.title)
            self.initScale(layout)

        if funct_name=='distance':
            self.title = 'Distance'
            self.setWindowTitle(self.title)
            self.initDist(layout)

    def initDist(self,layout):
       
        self.gradationDistance=QtWidgets.QLineEdit()
        self.gradationDistance.setText("1")
        TitleLayout("gradationDistance", self.gradationDistance, layout)

        self.gradationStartDistance=QtWidgets.QLineEdit()
        self.gradationStartDistance.setText("0.01")
        TitleLayout("gradationStartDistance", self.gradationStartDistance, layout)

        layer = iface.activeLayer() # load the layer as you want
        ext = layer.extent()
        xmin = ext.xMinimum()
        xmax = ext.xMaximum()
        ymin = ext.yMinimum()
        ymax = ext.yMaximum()
        coords = "%f,%f,%f,%f" %(xmin, xmax, ymin, ymax)
        self.extent=QtWidgets.QLineEdit()
        self.extent.setText(coords)
        TitleLayout("Raster extent", self.extent, layout)

        self.res=QtWidgets.QLineEdit()
        self.res.setText('10,10')
        TitleLayout("Raster resolution", self.res, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.dist_calc, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

    def initScale(self,layout):
       
        self.minimum_value=QtWidgets.QLineEdit()
        self.minimum_value.setText("50")
        TitleLayout("minimum value", self.minimum_value, layout)

        self.maximum_value=QtWidgets.QLineEdit()
        self.maximum_value.setText("300")
        TitleLayout("maximum value", self.maximum_value, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.scale_calc, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

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

        x=(g*t**2/2/np.pi)
        minL = x*np.sqrt(np.tanh(4*np.pi**2.*d0/t**2/g))

        shapein=get_layer(raster).dataProvider().dataSourceUri()

        root,f=os.path.split(shapein)
        shapeout=os.path.join(root,'wavelength.tif')

        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="((%f*sqrt(tanh(4*pi**2.*A/%f**2/%f)))*(A>%f)+ (%f)*(A<=%f))" --NoDataValue=0' % (shapein,shapeout,x,t,g,d0,minL,d0))
        add_raster(shapeout,'wavelength')
        self.close()

        return
        # # L = (g*T^2/2/pi)*sqrt(tanh(4*pi^2.*d/T^2/g));

    def dist_calc(self):
        """ Set the gradation parameters. """
        #(5.0,15000.0,1.0,0.01)
        gradationDistance=float(self.gradationDistance.text())
        gradationStartDistance=float(self.gradationStartDistance.text())
        shapein=self.rasterSelector.currentItem().text()
        shapein=get_layer(shapein).dataProvider().dataSourceUri()
        extent=[float(x) for x in self.extent.text().split(',')]
        res=[float(x) for x in self.res.text().split(',')]
        time = datetime.now()
        shapesSHPFilename = '/tmp/shapes'+time.isoformat()+'.shp'
        rasterisedShapesFilename = '/tmp/rasterisedShapes'+time.isoformat()+'.nc'
        write_raster(shapesSHPFilename,rasterisedShapesFilename,extent,res)

        """ Compute the linear gradation. """

        # Construct temporary file-names.

        # Calculate and read-in distance-function.
        # self.writeDistanceNetCDF(rasterisedDistanceFile)
        # logLevel = LOG.level
        # LOG.setLevel('WARNING')
        # self.fromFile(rasterisedDistanceFile)
        # LOG.setLevel(logLevel)
        # # Map distance to mesh-size-metric
        # meshSizeMetric = self.variableData*((float(self.metricAwayFromShapes) - float(self.metricAtShapes))/float(self.gradationDistance)) + \
        #       float(self.metricAwayFromShapes) - (float(self.gradationDistance) + float(self.gradationStartDistance))*\
        #       ((float(self.metricAwayFromShapes) - float(self.metricAtShapes))/float(self.gradationDistance))
        # meshSizeMetric = np.maximum(meshSizeMetric, np.ones_like(meshSizeMetric)*float(self.metricAtShapes))
        # meshSizeMetric = np.minimum(meshSizeMetric, np.ones_like(meshSizeMetric)*float(self.metricAwayFromShapes))
        # self.variableData = meshSizeMetric
        # Clean-up temporary files.
        self.close()
        return


    def scale_calc(self):
        Vmin=float(self.minimum_value.text())
        Vmax=float(self.maximum_value.text())

        raster=self.rasterSelector.currentItem().text()
        
        layer=get_layer(raster)
        shapein=layer.dataProvider().dataSourceUri()
        provider = layer.dataProvider()

        stats = provider.bandStatistics(1, QgsRasterBandStats.All, layer.extent(), 0)
        min_raster = stats.minimumValue
        max_raster = stats.maximumValue
        
        root,f=os.path.split(shapein)
        shapeout=os.path.join(root,'scaled.tif')

        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="(((%f-%f)*(A-%f))/(%f-%f))+%f" --NoDataValue=%f' % (shapein,shapeout,Vmax,Vmin,min_raster,max_raster,min_raster,Vmin,Vmin))

        add_raster(shapeout,'scaled')
        self.close()
        return


    def exec_(self):
        super(raster_calculator, self).exec_()

def writeRasterLayer(layer, filename) :
    progress = QProgressDialog("Writing mesh size layer...", "Abort", 0, layer.width())
    progress.setMinimumDuration(0)
    progress.setWindowModality(Qt.WindowModal)
    progress.setValue(0)
    f = open(filename, "w")
    ext = layer.extent()
    f.write('%f\t%f\t%f\n' % (ext.xMinimum(), ext.yMinimum(), 0))
    f.write('%f\t%f\t%f\n' % (ext.width() / layer.width(), ext.height() / layer.height(), 1))
    f.write('%f\t%f\t%f\n' % (layer.width(), layer.height(), 1))
    block = layer.dataProvider().block(1, layer.extent(), layer.width(), layer.height())
    for j in range(layer.width()) : 
        progress.setValue(j)
        if progress.wasCanceled():
            return False
        v = list([block.value(i, j) for i in range(layer.height() -1, -1, -1)])
        f.write('\t'.join(str(x) for x in v)+'\n')
    f.close()
    # f = open(filename, "wb")
    # ext = layer.extent()
    # f.write(struct.pack("3d", ext.xMinimum(), ext.yMinimum(), 0))
    # f.write(struct.pack("3d", ext.width() / layer.width(), ext.height() / layer.height(), 1))
    # f.write(struct.pack("3i", layer.width(), layer.height(), 1))
    # block = layer.dataProvider().block(1, layer.extent(), layer.width(), layer.height())
    # for j in range(layer.width()) : 
    #     progress.setValue(j)
    #     if progress.wasCanceled():
    #         return False
    #     v = list([block.value(i, j) for i in range(layer.height() -1, -1, -1)])
    #     f.write(struct.pack("{}d".format(len(v)), *v))
    # f.close()
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
