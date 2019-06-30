import sys,os
sys.path.append(os.path.join(os.path.dirname(__file__),'..','qgmesh')) 
from tools import write_raster



xy=[167.708857,167.804516,-46.793372,-46.654917]
shapesSHPFilename='../../../islan.shp'
rasterisedShapesFilename='/home/remy/Software/QGmesh/test/test2.nc'
outputRasterFilename='/home/remy/Software/QGmesh/test/prox2.tif'
res=[100,100]
os.system('rm -f %s' % rasterisedShapesFilename)
write_raster(shapesSHPFilename,rasterisedShapesFilename,xy,res)
os.system('rm -f %s' % outputRasterFilename)
gdal_proximity_cmd = 'gdal_proximity.py -q %s %s -of GTiff -distunits GEO' % (rasterisedShapesFilename,outputRasterFilename)
os.system(gdal_proximity_cmd)