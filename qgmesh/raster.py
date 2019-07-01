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
from .tools import *

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



        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

    def initDist(self,layout):
       
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
        self.res.setText('100,100')
        TitleLayout("Raster resolution", self.res, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.dist_calc, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)



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
        shapein=self.rasterSelector.currentItem().text()
        shapesSHPFilename=get_layer(shapein).dataProvider().dataSourceUri().split('|')[0]
        extent=[float(x) for x in self.extent.text().split(',')]
        res=[float(x) for x in self.res.text().split(',')]
        time = datetime.now()


        rasterisedShapesFilename='/tmp/raster_dist.nc'
        outputRasterFilename='/tmp/distance.tif'

        if os.path.isfile(rasterisedShapesFilename):
            os.system('rm -f %s' % rasterisedShapesFilename)

        if os.path.isfile(outputRasterFilename):
            os.system('rm -f %s' % outputRasterFilename)

        write_raster(shapesSHPFilename,rasterisedShapesFilename,extent,res)

        gdal_proximity_cmd = 'gdal_proximity.py -q %s %s -of GTiff -distunits GEO ' % (rasterisedShapesFilename,outputRasterFilename)

        os.system(gdal_proximity_cmd)
        add_raster(outputRasterFilename,'distance')

        if os.path.isfile(rasterisedShapesFilename):
            os.system('rm -f %s' % rasterisedShapesFilename)

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