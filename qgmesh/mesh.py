import numpy as np
from qgis.core import QgsWkbTypes,QgsVectorFileWriter,QgsMeshLayer,QgsProject,QgsField,QgsFields,QgsLayerTreeGroup,QgsCoordinateReferenceSystem,QgsPointXY,QgsFeature,QgsGeometry

from PyQt5.QtCore import QThread
import gdal

def bilinear(px, py,gt,band_array, no_data=np.nan):
    '''Bilinear interpolated point at (px, py) on band_array
    example: bilinear(2790501.920, 6338905.159)'''
    ny, nx = band_array.shape
    # Half raster cell widths
    hx = gt[1]/2.0
    hy = gt[5]/2.0
    # Calculate raster lower bound indices from point
    fx = (px - (gt[0] + hx))/gt[1]
    fy = (py - (gt[3] + hy))/gt[5]
    ix1 = int(np.floor(fx))
    iy1 = int(np.floor(fy))
    # Special case where point is on upper bounds
    if fx == float(nx - 1):
        ix1 -= 1
    if fy == float(ny - 1):
        iy1 -= 1
    # Upper bound indices on raster
    ix2 = ix1 + 1
    iy2 = iy1 + 1
    # Test array bounds to ensure point is within raster midpoints
    if (ix1 < 0) or (iy1 < 0) or (ix2 > nx - 1) or (iy2 > ny - 1):
        return no_data
    # Calculate differences from point to bounding raster midpoints
    dx1 = px - (gt[0] + ix1*gt[1] + hx)
    dy1 = py - (gt[3] + iy1*gt[5] + hy)
    dx2 = (gt[0] + ix2*gt[1] + hx) - px
    dy2 = (gt[3] + iy2*gt[5] + hy) - py
    # Use the differences to weigh the four raster values
    div = gt[1]*gt[5]
    return (band_array[iy1,ix1]*dx2*dy2/div +
            band_array[iy1,ix2]*dx1*dy2/div +
            band_array[iy2,ix1]*dx2*dy1/div +
            band_array[iy2,ix2]*dx1*dy1/div)

class Mesh(object) :

    def __init__(self,mesh) :
        self.mesh=mesh
        
        self.types=None
        self.x=mesh.points[:,0]
        self.y=mesh.points[:,1]
        self.z=mesh.points[:,2]
        self.nodes=np.arange(1,len(self.x)+1)
        self.triangles=mesh.cells
        proj = QgsProject.instance()
        self.crs=proj.crs()

        tot=0
        tri_len=0
        quad_len=0
        if 'triangle' in mesh.cells:
            tri_len=len(mesh.cells['triangle'])
            tot+=tri_len

        if 'quad' in mesh.cells:
            quad_len=len(mesh.cells['quad'])
            tot+=quad_len

        self.triangles=np.ones((tot,4),int)*-1

        self.edges=mesh.cells['line']

        if 'triangle' in mesh.cells:
            self.triangles[0:tri_len,0:3]=mesh.cells['triangle']

        if 'quad' in mesh.cells:
            self.triangles[tri_len:tot,:]=mesh.cells['quad']

        self.triangles=self.triangles

    def _build_string(self):
        stri=''
        for pt in range(0,len(self.nodes)):
            stri+='%.5f, %.5f \n' % (self.x[pt],self.y[pt])
        stri+="---\n"
        for face in range(0,len(self.triangles)):
            if self.triangles[face,-1]<0:
                stri+='%.f, %.f, %.f \n' % (self.triangles[face,0],self.triangles[face,1],self.triangles[face,2])
            else:
                stri+='%.f, %.f, %.f, %.f \n' % (self.triangles[face,0],self.triangles[face,1],self.triangles[face,2],self.triangles[face,3])


        self.stri=stri
    def to_Gridshapefile(self,shape_name):


        group='Mesh'
        proj = QgsProject.instance()

        childs=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup):
                childs.append(child.name())


        if group not in childs:
            G=proj.layerTreeRoot().addGroup(group)
        else:
            G=proj.layerTreeRoot().findGroup(group)


        QThread.sleep(1)





        self._build_string()
        outLayer = QgsMeshLayer( self.stri, \
            shape_name, \
            "mesh_memory")

        QgsProject.instance().addMapLayer(outLayer,False)
        G.addLayer(outLayer)

        # root = QgsProject.instance().layerTreeRoot()
        # layer = root.findLayer(outLayer.id())
        # clone = layer.clone()
        # G.insertChildNode(0, clone)
        # root.removeChildNode(layer)
    def writeShapefile(self, filename,shape_type):

        if filename[-4:]!='.shp':
              filename += '.shp'

        proj = QgsProject.instance()
        crs=proj.crs()
        #Create new shapefile object, loop trough triangle edges and add each
        # edge as a line.


        fileWriter = QgsVectorFileWriter(filename,
                   "system", QgsFields(), QgsWkbTypes.Point, crs,
                   "ESRI Shapefile")
        if fileWriter.hasError() != QgsVectorFileWriter.NoError:
            raise Exception('Error when creating shapefile '+filename+
                        ' : '+str(fileWriter.hasError()))

        if shape_type == 'points':
            for node in range(0,len(self.x)):
                point = QgsPointXY()
                point.setX(self.x[node])
                point.setY(self.y[node])
                newFeature = QgsFeature()
                newFeature.setGeometry(QgsGeometry.fromPointXY(point))
                fileWriter.addFeature(newFeature)

            # #Write new features to shapefile.
            # for edge in self.edges:
            #     pointsList = []
            #     for node in edge:
            #             point = QgsPoint()
            #             point.setX(self.x[node])
            #             point.setY(self.y[node])
            #             pointsList.append(point)
            #     pointsList.append(pointsList[0])

            #     newFeature = QgsFeature()
            #     newFeature.setGeometry(QgsGeometry.fromPolyline(pointsList))
            #     fileWriter.addFeature(newFeature)

        del fileWriter
    def AddBathy(self, raster):
        raster=gdal.Open(raster)
        bathy=raster.GetRasterBand(1).ReadAsArray()
        gt = raster.GetGeoTransform()
        for pt in range(0,len(self.x)):
            self.z[pt]=bilinear(self.x[pt],self.y[pt],gt,bathy, no_data=np.nan)



    def writeUnstructuredGridSMS(self, mesh):
        """
        Takes appropriate triangle, node, boundary type and coordinate data and
        writes out an SMS formatted grid file (mesh). The footer is largely static,
        but the elements, nodes and node strings are parsed from the input data.

       """
        triangles=self.triangles+1
        x=self.x
        y=self.y
        z=self.z


        fileWrite = open(mesh, 'w')
        # Add a header
        fileWrite.write('hgrid create with QGmesh\n')
        fileWrite.write('%i\t%i\n' % (len(self.triangles),len(self.x)))

        # Write out the connectivity table (triangles)
        for node in range(0,len(self.x)):
            fileWrite.write('%i\t%f\t%f\t%f\t\n'% (node+1,self.x[node],self.y[node],self.z[node]))

        for face in range(0,len(self.triangles)):
            if self.triangles[face,-1]<0:
                fileWrite.write('%i\t3\t%i\t%i\t%i\t0\n' % (face ,triangles[face,0],triangles[face,1],triangles[face,2]))
            else:
                fileWrite.write('%i\t4\t%i\t%i\t%i\t%i\n' % (face ,triangles[face,0],triangles[face,1],triangles[face,2],triangles[face,3]))




        fileWrite.close()
