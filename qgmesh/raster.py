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
import uuid

class raster_calculator(QtWidgets.QDialog):

    def __init__(self,rasters,funct_name,tmp='/tmp'):
        super(raster_calculator,self).__init__()


        layout = QtWidgets.QVBoxLayout()
        self.rasterSelector = QtWidgets.QListWidget()
        self.rasterSelector.addItems(rasters)
        self.tempdir=tmp
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


        if funct_name=='choose':
            self.title = 'Choose a raster'
            self.setWindowTitle(self.title)
            self.initChoose(layout)


        if funct_name=='mask':
            self.title = 'Apply a mask'
            self.setWindowTitle(self.title)
            self.initMask(layout)            

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

    def getval(self):
        raster=self.rasterSelector.currentItem().text()
        layer=get_layer(raster)
        provider = layer.dataProvider()
        stats = provider.bandStatistics(1, QgsRasterBandStats.All, layer.extent(), 0)
        min_raster = stats.minimumValue
        max_raster = stats.maximumValue
        self.origin_min.setText(str(min_raster))
        self.origin_max.setText(str(max_raster))

    def initChoose(self,layout):
        self.runLayout = CancelRunLayout(self,"Choose", self.choose_raster, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)


    def initMask(self,layout):
       
        layer = iface.activeLayer() # load the layer as you want
        ext=iface.mapCanvas().extent()
        #ext = layer.extent()
        xmin = ext.xMinimum()
        xmax = ext.xMaximum()
        ymin = ext.yMinimum()
        ymax = ext.yMaximum()
        coords = "%f,%f,%f,%f" %(xmin, xmax, ymin, ymax)
        self.extent=QtWidgets.QLineEdit()
        self.extent.setText(coords)
        TitleLayout("Raster extent", self.extent, layout)

        self.res=QtWidgets.QLineEdit()
        xres=(xmax-xmin)/100
        yres=(ymax-ymin)/100
        self.res.setText(str(xres)+','+str(yres))
        TitleLayout("Raster resolution X,Y (m)",self.res, layout)


        self.valin=QtWidgets.QLineEdit()
        self.valin.setText(str(50))
        TitleLayout("Resolution inside in m",self.valin, layout)

        self.valout=QtWidgets.QLineEdit()
        self.valout.setText(str(10000))
        TitleLayout("Resolution outside in m",self.valout, layout)


        self.runLayout = CancelRunLayout(self,"calculate", self.apply_mask, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)

    def initDist(self,layout):
       
        layer = iface.activeLayer() # load the layer as you want
        ext=iface.mapCanvas().extent()
        #ext = layer.extent()
        xmin = ext.xMinimum()
        xmax = ext.xMaximum()
        ymin = ext.yMinimum()
        ymax = ext.yMaximum()
        coords = "%f,%f,%f,%f" %(xmin, xmax, ymin, ymax)
        self.extent=QtWidgets.QLineEdit()
        self.extent.setText(coords)
        TitleLayout("Raster extent", self.extent, layout)

        self.res=QtWidgets.QLineEdit()
        xres=(xmax-xmin)/100
        yres=(ymax-ymin)/100
        self.res.setText(str(xres)+','+str(yres))
        TitleLayout("Raster resolution X,Y (m)",self.res, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.dist_calc, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)



    def initScale(self,layout):
 
        self.origin_min=QtWidgets.QLineEdit()
        self.origin_min.setText("50")
        TitleLayout("Original minimum value", self.origin_min, layout)

        self.origin_max=QtWidgets.QLineEdit()
        self.origin_max.setText("300")
        TitleLayout("Original maximum value", self.origin_max, layout)


        self.minimum_value=QtWidgets.QLineEdit()
        self.minimum_value.setText("50")
        TitleLayout("minimum value", self.minimum_value, layout)

        self.maximum_value=QtWidgets.QLineEdit()
        self.maximum_value.setText("300")
        TitleLayout("maximum value", self.maximum_value, layout)

        self.rasterSelector.itemClicked.connect(self.getval)

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
        shapeout=os.path.join(root,'wavelength'+str(uuid.uuid4())+'.tif')

        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="%f*(A<=%f) + A*(A>%f)" --NoDataValue=-99'% (shapein,shapeout,d0,d0,d0))
        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="%f*sqrt(tanh(4*pi**2.*A/%f**2/%f))" --NoDataValue=-99' % (shapeout,shapeout,x,t,g))
        add_raster(shapeout,'wavelength')
        self.close()

        return
        # # L = (g*T^2/2/pi)*sqrt(tanh(4*pi^2.*d/T^2/g));

    def dist_calc(self):
       
        """ Set the gradation parameters. """
        shapein=self.rasterSelector.currentItem().text()
        shapesSHPFilename=get_layer(shapein).dataProvider().dataSourceUri().split('|')[0]
        root,f=os.path.split(shapesSHPFilename)
        extent=[float(x) for x in self.extent.text().split(',')]
        resM=[float(x) for x in self.res.text().split(',')]
        res=[(extent[1]-extent[0])/resM[0],(extent[3]-extent[2])/resM[1]]
        time = datetime.now()


        rasterisedShapesFilename=os.path.join( self.tempdir,'raster_dist.nc')
        outputRasterFilename=os.path.join( root,'distance'+str(uuid.uuid4())+'.tif')

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


    def apply_mask(self):
        """ Set the gradation parameters. """
        shapein=self.rasterSelector.currentItem().text()
        shapesSHPFilename=get_layer(shapein).dataProvider().dataSourceUri().split('|')[0]
        root,f=os.path.split(shapesSHPFilename)
        extent=[float(x) for x in self.extent.text().split(',')]
        resM=[float(x) for x in self.res.text().split(',')]
        res=[(extent[1]-extent[0])/resM[0],(extent[3]-extent[2])/resM[1]]
        time = datetime.now()
        valin=float(self.valin.text())
        valout=float(self.valout.text())


        outputRasterFilename=os.path.join( root,'mask'+str(uuid.uuid4())+'.tif')


        if os.path.isfile(outputRasterFilename):
            os.system('rm -f %s' % outputRasterFilename)



        #raster bound
        xiMin = extent[0]
        xiMax = extent[1]
        etaMin = extent[2]
        etaMax = extent[3]

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


        gdal_rasterize_cmd = 'gdal_rasterize -q -burn %f -a_nodata 0 -init %f -at -tr %f %f -te %f %f %f %f %s %s' \
        % (valin,valout,delta_xi,delta_eta,xiMin,etaMin,xiMax,etaMax,shapesSHPFilename,outputRasterFilename)


        os.system(gdal_rasterize_cmd)
        add_raster(outputRasterFilename,'mask')


        self.close()
        return



    def scale_calc(self):
        Vmin=float(self.minimum_value.text())
        Vmax=float(self.maximum_value.text())

        raster=self.rasterSelector.currentItem().text()
        
        layer=get_layer(raster)
        shapein=layer.dataProvider().dataSourceUri()
        #provider = layer.dataProvider()

        #stats = provider.bandStatistics(1, QgsRasterBandStats.All, layer.extent(), 0)
        min_raster = float(self.origin_min.text())
        max_raster = float(self.origin_max.text())
        
        root,f=os.path.split(shapein)
        shapeout=os.path.join(root,'scaled'+str(uuid.uuid4())+'.tif')

        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="((((%f-%f)*(A-%f))/(%f-%f))+%f)" --NoDataValue=-99' % (shapein,os.path.join( self.tempdir,'tmp1.tif'),Vmax,Vmin,min_raster,max_raster,min_raster,Vmin))
        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="maximum(A,%f)" --NoDataValue=-99' % (os.path.join( self.tempdir,'tmp1.tif'),os.path.join( self.tempdir,'tmp2.tif'),Vmin))
        os.system('gdal_calc.py -A %s --overwrite --outfile=%s --calc="minimum(A,%f)" --NoDataValue=-99' % (os.path.join( self.tempdir,'tmp2.tif'),shapeout,Vmax))

        add_raster(shapeout,'scaled')
        self.close()
        return


    def choose_raster(self):
        raster=self.rasterSelector.currentItem().text()
        
        layer=get_layer(raster)
        shapein=layer.dataProvider().dataSourceUri()
        self.close()

    def exec_(self):

        super(raster_calculator, self).exec_()
        raster=self.rasterSelector.currentItem().text()
        
        layer=get_layer(raster)
        shapein=layer.dataProvider().dataSourceUri()
        return shapein 