# -*- coding: utf-8 -*-

import sys,os
sys.path.append(os.path.join(os.path.dirname(__file__),'..','qgmesh')) 
from qgis.core import (
     QgsApplication, 
     QgsProcessingFeedback, 
     QgsVectorLayer,
     QgsProject,
     QgsCoordinateTransform
)

from exportGeometry import GeoFile
from utils import get_crs

# See https://gis.stackexchange.com/a/155852/4972 for details about the prefix 
QgsApplication.setPrefixPath('/usr', True)
qgs = QgsApplication([], False)
qgs.initQgis()

# Append the path where processing plugin can be found
sys.path.append('/home/remy/.local/share/QGIS/QGIS3/profiles/default/processing/')

#import processing
#from processing.core.Processing import Processing
#Processing.initialize()


filename=os.path.join('/home/remy/Software/QGmesh/test','simple_test2.qgz')
print(filename)
proj = QgsProject.instance()
proj.read(filename)

ignoredLayers = set(proj.readEntry("qgmsh", "ignored_boundary_layers", "")[0].split("%%"))
tetraLayers = set(proj.readEntry("qgmsh", "tetralayers", "")[0].split("%%"))
quadLayers = set(proj.readEntry("qgmsh", "quadlayers", "")[0].split("%%"))
meshSizeLayerId = proj.readEntry("qgmsh", "mesh_size_layer", "None")[0]
projid = proj.readEntry("qgmsh", "projection", "")[0]
crs=get_crs(projid)


    
geo=GeoFile()
for child in proj.layerTreeRoot().findGroups():      
    import pdb;pdb.set_trace()  
    if child.name() in ['Boundaries','Islands','Channels']:
        for sub_subChild in child.children():
            layer = proj.mapLayer(sub_subChild.layerId())
            xform = QgsCoordinateTransform(layer.crs(), crs,proj)
            self.geo.add_layer(layer,xform)
            added.append(child.name())



geo.writeSurface()


with open('test.geo', 'w') as f:
    f.write(geo.geo.get_code() + '\n')

f.close()