import sys,os
sys.path.append(os.path.join(os.path.dirname(__file__),'..','qgmesh')) 
from qgis.core import (
     QgsApplication, 
     QgsProcessingFeedback, 
     QgsVectorLayer,
     QgsProject,
     QgsCoordinateTransform,
     QgsMeshLayer,
)

from exportGeometry import GeoFile
from utils import get_crs
import runGmsh
from mesh import Mesh
import meshio
import pygmsh

# See https://gis.stackexchange.com/a/155852/4972 for details about the prefix 
QgsApplication.setPrefixPath('/usr', True)
qgs = QgsApplication([], False)
qgs.initQgis()

# Append the path where processing plugin can be found
sys.path.append('/home/remy/.local/share/QGIS/QGIS3/profiles/default/processing/')

#import processing
#from processing.core.Processing import Processing
#Processing.initialize()



filename=os.path.join('/home/remy/Software/QGmesh/test','quad.qgz')
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

    if child.name() in ['Boundaries','Islands','Channels']:
       for sub_subChild in child.children():
            layer = proj.mapLayer(sub_subChild.layerId())
            xform = QgsCoordinateTransform(layer.crs(), crs,proj)
            geo.add_layer(layer,xform,child.name(),grid_type)




geo.writeSurface()

for child in proj.layerTreeRoot().findGroups():      
    if child.name() in ['Sizing']:
    	for sub_subChild in child.children():
    		layer = proj.mapLayer(sub_subChild.layerId())
    		xform = QgsCoordinateTransform(layer.crs(), crs,proj)
    		geo.add_sizing(layer,xform,child.name())






# main=runGmsh.RunGmshDialog(geo)
# main.show()

# myStream=runGmsh.EmittingStream()
# myStream.textWritten.connect(main.normalOutputWritten)

# sys.stdout = myStream

# msh=main.exec_()
msh=pygmsh.generate_mesh(geo.geo)

mesh=meshio.Mesh(points=msh.points,cells=msh.cells,point_data=msh.point_data,cell_data=msh.cell_data,field_data=msh.field_data)

mesh=Mesh(mesh)

stri=mesh._build_string()
#mesh.writeUnstructuredGridSMS('temp.sms')



outLayer = QgsMeshLayer( mesh.stri, 'shape_name',"mesh_memory")

proj.addMapLayer(outLayer)



