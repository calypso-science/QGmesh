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
from tools import get_format_from_gmsh,Mesh2Shapefile

from qgis.core import QgsCoordinateReferenceSystem

#See https://gis.stackexchange.com/a/155852/4972 for details about the prefix 
QgsApplication.setPrefixPath('/usr', True)
qgs = QgsApplication([], False)
qgs.initQgis()

#Append the path where processing plugin can be found
sys.path.append('/home/remy/.local/share/QGIS/QGIS3/profiles/default/processing/')

# geo = pygmsh.built_in.Geometry()

# with open('/home/remy/Documents/test/file.geo', 'r') as fin:
#     geo._GMSH_CODE.append(fin.read())

# msh=pygmsh.generate_mesh(geo,extra_gmsh_arguments=['-2','-algo','front2d','-epslc1d','1e-3'])

# mesh=meshio.Mesh(points=msh.points,cells=msh.cells,point_data=msh.point_data,cell_data=msh.cell_data,field_data=msh.field_data)

# triangles,edges,physicalID=get_format_from_gmsh(mesh)

# me=Mesh(mesh.points[:,0],mesh.points[:,1],mesh.points[:,2],triangles,\
#     edges=edges,\
#     physical=mesh.field_data,\
#     physicalID=physicalID)


# Mesh2Shapefile(me)



# sys.exit(-1)
#import processing
#from processing.core.Processing import Processing
#Processing.initialize()



filename=os.path.join('/oldroot/home/remy/Buisness/0448_Hokianga/grids/','hokianga.qgz')
print(filename)
proj = QgsProject.instance()
proj.read(filename)




ignoredLayers = set(proj.readEntry("qgmsh", "ignored_boundary_layers", "")[0].split("%%"))
tetraLayers = set(proj.readEntry("qgmsh", "tetralayers", "")[0].split("%%"))
quadLayers = set(proj.readEntry("qgmsh", "quadlayers", "")[0].split("%%"))
meshSizeLayerId = proj.readEntry("qgmsh", "mesh_size_layer", "None")[0]
projid = proj.readEntry("qgmsh", "projection", "")[0]

crs=QgsCoordinateReferenceSystem("EPSG:2193")


    
geo=GeoFile()
for child in proj.layerTreeRoot().findGroups():      

    if child.name() in ['Boundaries','QuadPatch','Channels']:
        grid_type='tetra'#child.customProperty('grid type')
        print(child.name())
        for sub_subChild in child.children():
            layer = proj.mapLayer(sub_subChild.layerId())
            xform = QgsCoordinateTransform(layer.crs(), crs,proj)
            geo.add_layer(layer,xform,child.name(),grid_type)




geo.writeSurface(grid_type)

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

#stri=mesh._build_string()
#mesh.writeUnstructuredGridSMS('temp.sms')



# outLayer = QgsMeshLayer( mesh.stri, 'shape_name',"mesh_memory")

# proj.addMapLayer(outLayer)



