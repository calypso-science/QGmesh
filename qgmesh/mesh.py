import numpy as np
from qgis.core import QgsPoint,QgsWkbTypes,QgsVectorFileWriter,QgsMeshLayer,QgsProject,QgsField,QgsFields,QgsLayerTreeGroup,QgsCoordinateReferenceSystem,QgsPointXY,QgsFeature,QgsGeometry
from PyQt5 import QtWidgets
from PyQt5.QtCore import QThread,QVariant,Qt
import gdal
from collections import OrderedDict
import numpy as np


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

def cal_tri_area(a):
    return np.absolute((a[0]*(a[3]-a[5])+a[2]*(a[5]-a[1])+a[4]*(a[1]-a[3]))/2.0)

def tri_area(x,y):
    # area from x, y which are (n,3)
    # tested in matlab version compared to polyarea.m same to to 10^-9m for  1500m^2 or larger triangles 
    # get lengths of 3 sides for herons formula
    # ross vennel May 2018
    a=np.sqrt((x[:, 1]-x[:,0])**2+(y[:, 1]-y[:,0])**2)
    b=np.sqrt((x[:, 2]-x[:,1])**2+(y[:, 2]-y[:,1])**2)
    c=np.sqrt((x[:, 0]-x[:,2])**2+(y[:, 0]-y[:,2])**2)

    s=(a+b+c)/2.0
    A=np.sqrt(s*(s-a)*(s-b)*(s-c))
    return A

def calculate_q(A,a,b,c):
    l1=calculate_distances(a,b)
    l2=calculate_distances(b,c)
    l3=calculate_distances(c,a)
    return 4 * np.sqrt(3) * A / (l1**2+l2**2+l3**2)   

def calculate_CFL(depth,A,dt):
    CFL=np.sqrt(np.pi*9.8*depth/(4*A))*dt
    return CFL

def calculate_distances(a,b):
    L=np.sqrt(abs(a[0]-b[0])**2+abs(a[1]-b[1])**2)
    return L
def _angleBetween(ab, ac):
    v1_u = ab / np.linalg.norm(ab)
    v2_u = ac / np.linalg.norm(ac)
    angle = np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))
    return np.degrees(angle)

def _calculate_angles(a,b,c):
    ab = b - a
    ac = c - a
    bc = c - b
    alpha = _angleBetween(ab, ac)
    beta = _angleBetween(-ab, bc)
    gamma = _angleBetween(-ac, -bc)

    try:
        assert np.allclose(gamma, 180.0 - alpha - beta)
    except:
        import pdb;pdb.set_trace()

    return alpha, beta, gamma

class Mesh(object) :

    def __init__(self,x,y,z,faces,edges=None,physical=None,physicalID=None) :

        self.types=None
        self.x=x
        self.y=y
        self.z=z
        
        
        self.physical=physical
        self.physicalID=physicalID
        self.edges=edges


        self.nodes=np.arange(1,len(self.x)+1)
        
        if len(x)>0:
            self.faces=faces.astype('int')
            self._calculate_face_nodes()
            #self._remove_hanging_nodes()
            self.calc_parameters()



    def calc_parameters(self):
        self.min_angle=np.ones(len(self.faces))*0.
        self.nsr=np.ones(len(self.faces))*0.
        self.eta=np.ones(len(self.faces))*0.
        self.res=np.ones(len(self.faces))*0.
        
        self.areas=self._calculate_areas2()
        self.eta=self._calculate_eta2()

        # for face in range(0,len(self.faces)):
        #     if self.faces[face,-1]<0:
        #         ty=3
        #         self.nsr[face]=self._calculate_nsr(face)

        #     self.min_angle[face]=self._minimumAngle(face,ty)
            
        self._calculate_res()
        self._calculate_connection()
        

    def _calculate_connection(self):
        conn1=np.ones(len(self.x))*0
        conn2=np.ones(len(self.x))*0
        conn3=np.ones(len(self.x))*0
        conn4=np.ones(len(self.x))*0
        f0=np.unique(self.faces[:,0],return_counts=True)
        conn1[f0[0]]=f0[1]
        f1=np.unique(self.faces[:,1],return_counts=True)
        conn2[f1[0]]=f1[1]
        f2=np.unique(self.faces[:,2],return_counts=True)
        conn3[f2[0]]=f2[1]
        f3=np.unique(self.faces[:,3],return_counts=True)
        gd=f3[0]>=0
        if np.any(gd):
            conn4[f3[0][gd]]=f3[1][gd]

        self.conn=conn1+conn2+conn3+conn4

    def _minimumAngle(self,face,ty):
        if ty==3:      
            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            c=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            return np.min(_calculate_angles(a,b,c)) / 60.
        if ty==4:
            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            c=np.array([self.x[self.faces[face,3]],self.y[self.faces[face,3]]])
            tri1=np.min(_calculate_angles(a,b,c)) / 60.
            a=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            b=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            c=np.array([self.x[self.faces[face,3]],self.y[self.faces[face,3]]])
            tri2=np.min(_calculate_angles(a,b,c)) / 60.
            return np.min([tri1,tri2])            
    
    def _calculate_nsr(self,face):
        a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
        b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
        c=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
        l1=calculate_distances(a,b)
        l2=calculate_distances(b,c)
        l3=calculate_distances(c,a)
        r = 2 * self.areas[face] / (l1 + l2 + l3)  # inradius
        R = 0.25 * l1 * l2 * l3 / self.areas[face]  # circumradius
        return 2 * r / R


     
    def _calculate_eta2(self):

        tri=self.faces[:,3]<0
        quad=self.faces[:,3]>=0
        eta=np.ones(len(self.faces))*0.

        if np.any(tri):

            a=np.array([self.x[self.faces[tri,0]],self.y[self.faces[tri,0]]])
            b=np.array([self.x[self.faces[tri,1]],self.y[self.faces[tri,1]]])
            c=np.array([self.x[self.faces[tri,2]],self.y[self.faces[tri,2]]])
            eta[tri]=calculate_q(self.areas,a,b,c)

        if np.any(quad):
            # tri A,B,C
            a=np.array([self.x[self.faces[quad,0]],self.y[self.faces[quad,0]]])
            b=np.array([self.x[self.faces[quad,1]],self.y[self.faces[quad,1]]])
            c=np.array([self.x[self.faces[quad,2]],self.y[self.faces[quad,2]]])
            A=np.array([self.x[self.faces[quad,0]],self.x[self.faces[quad,1]],self.x[self.faces[quad,2]]])
            B=np.array([self.y[self.faces[quad,0]],self.y[self.faces[quad,1]],self.y[self.faces[quad,2]]])
            A1=tri_area(A,B)
            q1=calculate_q(A1,a,b,c) #A,B,C

            # tri A,C,D
            a=np.array([self.x[self.faces[quad,0]],self.y[self.faces[quad,0]]])
            b=np.array([self.x[self.faces[quad,2]],self.y[self.faces[quad,2]]])
            c=np.array([self.x[self.faces[quad,3]],self.y[self.faces[quad,3]]])
            A=np.array([self.x[self.faces[quad,0]],self.x[self.faces[quad,2]],self.x[self.faces[quad,3]]])
            B=np.array([self.y[self.faces[quad,0]],self.y[self.faces[quad,2]],self.y[self.faces[quad,3]]])
            A2=tri_area(A,B)
            q2=calculate_q(A2,a,b,c) #A,B,C

            # tri A,B,D
            a=np.array([self.x[self.faces[quad,0]],self.y[self.faces[quad,0]]])
            b=np.array([self.x[self.faces[quad,1]],self.y[self.faces[quad,1]]])
            c=np.array([self.x[self.faces[quad,3]],self.y[self.faces[quad,3]]])
            A=np.array([self.x[self.faces[quad,0]],self.x[self.faces[quad,1]],self.x[self.faces[quad,3]]])
            B=np.array([self.y[self.faces[quad,0]],self.y[self.faces[quad,1]],self.y[self.faces[quad,3]]])
            A3=tri_area(A,B)
            q3=calculate_q(A3,a,b,c) #A,B,C

            # tri B,C,D
            a=np.array([self.x[self.faces[quad,1]],self.y[self.faces[quad,1]]])
            b=np.array([self.x[self.faces[quad,2]],self.y[self.faces[quad,2]]])
            c=np.array([self.x[self.faces[quad,3]],self.y[self.faces[quad,3]]])
            A=np.array([self.x[self.faces[quad,1]],self.x[self.faces[quad,2]],self.x[self.faces[quad,3]]])
            B=np.array([self.y[self.faces[quad,1]],self.y[self.faces[quad,2]],self.y[self.faces[quad,3]]])
            A3=tri_area(A,B)
            q3=calculate_q(A3,a,b,c) #B,C.D

            eta[quad]=(0.86602540-np.abs((q1+q2+q3+q4)/4 - 0.86602540))/0.86602540

        return eta
    def _calculate_eta(self,face,ty):
        

        if ty==3:
            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            c=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            eta=calculate_q(self.areas[face],a,b,c)

        elif ty==4:
            tri1 = np.zeros((1,3,3))
            tri2 = np.zeros((1,3,3))
            tri3 = np.zeros((1,3,3))
            tri4 = np.zeros((1,3,3))
            x=self.x
            y=self.y
            ref=np.zeros((len(x),1))
            nodes_coor = np.hstack((np.vstack([x,y]).T,ref))

            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            c=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            for iv,ivert in enumerate([0,1,2]):
                tri1[0,iv,:] = nodes_coor[self.faces[face,ivert]]
            A=cal_tri_area(tri1[0,:,0:2].reshape(1,6).transpose())
            q1=calculate_q(A,a,b,c) #A,B,C

            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            c=np.array([self.x[self.faces[face,3]],self.y[self.faces[face,3]]])
            for iv,ivert in enumerate([0,2,3]):
                tri2[0,iv,:] = nodes_coor[self.faces[face,ivert]]
            A=cal_tri_area(tri2[0,:,0:2].reshape(1,6).transpose())
            q2=calculate_q(A,a,b,c) #A,B,C

            
            a=np.array([self.x[self.faces[face,0]],self.y[self.faces[face,0]]])
            b=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            c=np.array([self.x[self.faces[face,3]],self.y[self.faces[face,3]]])
            for iv,ivert in enumerate([0,1,3]):
                tri3[0,iv,:] = nodes_coor[self.faces[face,ivert]]
            A=cal_tri_area(tri3[0,:,0:2].reshape(1,6).transpose())
            q3=calculate_q(A,a,b,c) #A,B,C

            a=np.array([self.x[self.faces[face,1]],self.y[self.faces[face,1]]])
            b=np.array([self.x[self.faces[face,2]],self.y[self.faces[face,2]]])
            c=np.array([self.x[self.faces[face,3]],self.y[self.faces[face,3]]])
            for iv,ivert in enumerate([1,2,3]):
                tri4[0,iv,:] = nodes_coor[self.faces[face,ivert]]
            A=cal_tri_area(tri4[0,:,0:2].reshape(1,6).transpose())
            q4=calculate_q(A,a,b,c) #A,B,C

            eta=(0.86602540-np.abs((q1+q2+q3+q4)/4 - 0.86602540))/0.86602540

        return eta

    def _calculate_res(self):
        self.res=2*np.sqrt(self.areas/np.pi)

    def _calculate_areas2(self):
        tri=self.faces[:,3]<0
        quad=self.faces[:,3]>=0
        areas=np.ones(len(self.faces))*0.

        if np.any(tri):
            x0=self.faces[tri,0]
            x1=self.faces[tri,1]
            x2=self.faces[tri,2]
            x=np.zeros((len(x0),3))
            x[:,0]=self.x[x0]
            x[:,1]=self.x[x1]
            x[:,2]=self.x[x2]

            y=np.zeros((len(x0),3))
            y[:,0]=self.y[x0]
            y[:,1]=self.y[x1]
            y[:,2]=self.y[x2]

            areas[tri]=tri_area(x,y)

        if np.any(quad):
            x0=self.faces[quad,0]
            x1=self.faces[quad,1]
            x2=self.faces[quad,2]
            x3=self.faces[quad,3]

            x1=np.zeros((len(x0),3))
            x1[:,0]=self.x[x0]
            x1[:,1]=self.x[x1]
            x1[:,2]=self.x[x3]

            y1=np.zeros((len(x0),3))
            y1[:,0]=self.y[x0]
            y1[:,1]=self.y[x1]
            y1[:,2]=self.y[x3]

            x2=np.zeros((len(x0),3))
            x2[:,0]=self.x[x1]
            x2[:,1]=self.x[x2]
            x2[:,2]=self.x[x3]

            y2=np.zeros((len(x0),3))
            y2[:,0]=self.y[x1]
            y2[:,1]=self.y[x2]
            y2[:,2]=self.y[x3]

            areas[quad]=tri_area(x1,y1)+tri_area(x2,y2)
        
        return areas

    # def _calculate_areas(self,face,ty):
        
    #     x=self.x
    #     y=self.y
    #     ref=np.zeros((len(x),1))
    #     nodes_coor = np.hstack((np.vstack([x,y]).T,ref))

    #     if ty==3:
    #         tri = np.zeros((1,3,3))
    #         for ivert in range(3):
    #             tri[0,ivert,:]=nodes_coor[self.faces[face,ivert]] 
    #         areas = cal_tri_area(tri[0,:,0:2].reshape(1,6).transpose())

    #     elif ty==4:
    #         tri1 = np.zeros((1,3,3))
    #         tri2 = np.zeros((1,3,3))
    #         for iv,ivert in enumerate([0,1,3]):
    #             tri1[0,iv,:] = nodes_coor[self.faces[face,ivert]] 
    #         for iv,ivert in enumerate([1,2,3]):
    #             tri2[0,iv,:] = nodes_coor[self.faces[face,ivert]]

    #         a1=cal_tri_area(tri1[0,:,0:2].reshape(1,6).transpose())
    #         a2=cal_tri_area(tri2[0,:,0:2].reshape(1,6).transpose())

    #         areas[face] = a1+a2

    #     return areas


    def _calculate_face_nodes(self):
        self.xctr=np.ones(len(self.faces))*-1.
        self.yctr=np.ones(len(self.faces))*-1.

        for face in range(0,len(self.faces)):
            if self.faces[face,-1]<0:
                self.xctr[face]=np.sum(self.x[self.faces[face,0:3]])/3
                self.yctr[face]=np.sum(self.y[self.faces[face,0:3]])/3

            else:
                self.xctr[face]=np.sum(self.x[self.faces[face,0:4]])/4
                self.yctr[face]=np.sum(self.y[self.faces[face,0:4]])/4


    def _build_string(self):
        stri=''
        for pt in range(0,len(self.nodes)):
            stri+='%.5f, %.5f \n' % (self.x[pt],self.y[pt])
        stri+="---\n"
        for face in range(0,len(self.faces)):
            if self.faces[face,-1]<0:
                stri+='%.f, %.f, %.f \n' % (self.faces[face,0],self.faces[face,1],self.faces[face,2])
            else:
                stri+='%.f, %.f, %.f, %.f \n' % (self.faces[face,0],self.faces[face,1],self.faces[face,2],self.faces[face,3])


        self.stri=stri

    def _remove_hanging_nodes(self):

        unique_nodes=np.unique(self.faces+1)
        unique_nodes=unique_nodes[unique_nodes!=0]

        hanging_nodes = (set(self.nodes) | set(unique_nodes)) - (set(self.nodes) & set(unique_nodes))
        hanging_nodes=sorted(hanging_nodes)
        X=self.x.tolist()
        Y=self.y.tolist()
        Z=self.z
        Z[np.isnan(Z)]=0
        Z=Z.tolist()
        triangles=self.faces.astype('float')
        edges=self.edges.astype('float')
        triangles[triangles==-1]=np.nan
        for hanging_node in hanging_nodes:
            edges[edges==hanging_node]=np.nan




        
        if len(hanging_nodes)>0:
            node_removed=1
            for hanging_node in hanging_nodes:
                X[hanging_node-1]=np.nan
                Y[hanging_node-1]=np.nan
                Z[hanging_node-1]=np.nan

                triangles[triangles>hanging_node-node_removed]=triangles[triangles>hanging_node-node_removed]-1

                edges[edges>hanging_node-node_removed]=edges[edges>hanging_node-node_removed]-1
                node_removed+=1


            triangles[np.isnan(triangles)]=-1
            self.faces=triangles.astype('int64')
            
            self.physicalID=np.asarray([self.physicalID[x] for x in range(0,len(edges)) if str(edges[x]) != 'nan'])
            self.edges=np.asarray([x for x in edges if str(x) != 'nan'])
            self.edges=self.edges.astype('int64')
            self.x=np.asarray([x for x in X if str(x) != 'nan'])
            self.y=np.asarray([x for x in Y if str(x) != 'nan'])
            self.z=np.asarray([x for x in Z if str(x) != 'nan'])





    def writeShapefile(self, filename,shape_type):

        if filename[-4:]!='.shp':
              filename += '.shp'

        proj = QgsProject.instance()
        crs=proj.crs()
        #Create new shapefile object, loop trough triangle edges and add each
        # edge as a line.
        if shape_type == 'points':
            shape=QgsWkbTypes.Point
            fields=  QgsFields()      
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("Depth", QVariant.Double,'double', 9, 3))
            fields.append(QgsField("Connect", QVariant.Int,'integer', 2, 0))

        elif shape_type == 'edges':
            shape=QgsWkbTypes.Point
            fields = QgsFields()
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("Flag", QVariant.Int,'integer', 2, 0))
            fields.append(QgsField("Name", QVariant.String,'string', 10, 0))

        elif shape_type == 'faces':
            shape=QgsWkbTypes.Polygon
            fields=  QgsFields()      
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("Node1", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node2", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node3", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node4", QVariant.Int,'integer',11,0))
            fields.append(QgsField("type", QVariant.Int,'integer', 1, 0))
            fields.append(QgsField("area", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("resolution", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("ETA", QVariant.Double,'double', 3, 2))
            fields.append(QgsField("NSR", QVariant.Double,'double', 3, 2))
            fields.append(QgsField("Angle", QVariant.Int,'integer', 3, 0))



        fileWriter = QgsVectorFileWriter(filename,
                   "system", fields,shape , crs,
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
                newFeature.setFields(fields)
                newFeature.setAttributes([int(node+1),float('%9.3f' % self.z[node]),int(self.conn[node])])
                fileWriter.addFeature(newFeature)

        if shape_type=='edges':
            inv_map = {v[0]: k for k, v in self.physical.items()}
            for ie,edge in enumerate(self.edges):
                point = QgsPointXY()
                if edge>0:
                    point.setX(self.x[edge])
                    point.setY(self.y[edge])
                    newFeature = QgsFeature()
                    newFeature.setGeometry(QgsGeometry.fromPointXY(point))
                    newFeature.setFields(fields)
                    newFeature.setAttributes([int(edge+1),int(self.physicalID[ie]),inv_map[self.physicalID[ie]]])
                    fileWriter.addFeature(newFeature)

        if shape_type=='faces':
            for ie in range(0,len(self.faces)):
                if self.faces[ie,-1]<0:
                    nface=3
                else:
                    nface=4
                
                points = []
                for fc in range(0,nface):
                    i=self.faces[ie,fc]
                    points.append(QgsPointXY(self.x[i],self.y[i]))

                i=self.faces[ie,0]
                points.append(QgsPointXY(self.x[i],self.y[i]))
                newFeature = QgsFeature()
                newFeature.setGeometry(QgsGeometry.fromPolygonXY([points]))
                newFeature.setFields(fields)
                newFeature.setAttributes([int(ie+1),\
                    int(self.faces[ie,0]+1),int(self.faces[ie,1]+1),int(self.faces[ie,2]+1),int(self.faces[ie,3]+1),\
                    int(nface),\
                    float('%9.f' % self.areas[ie]),float('%9.f' % self.res[ie]),\
                    float('%3.2f' % self.eta[ie]),float('%3.2f' % self.nsr[ie]),int(self.min_angle[ie])])
                fileWriter.addFeature(newFeature)


        del fileWriter

    def AddBathy(self, raster):
        raster=gdal.Open(raster)
        bathy=raster.GetRasterBand(1).ReadAsArray()
        gt = raster.GetGeoTransform()
        for pt in range(0,len(self.x)):
            self.z[pt]=bilinear(self.x[pt],self.y[pt],gt,bathy, no_data=np.nan)


    


    def map_np2ne(self,D):
        elem=self.faces
        idx3    = elem[:,-1]<0
        idx4    = quad=~idx3

        De = np.zeros((elem.shape[0],))
        De[idx3] = (D[elem[idx3,0]]+D[elem[idx3,1]]+D[elem[idx3,2]])/3
        De[idx4] = (D[elem[idx4,0]]+D[elem[idx4,1]]+D[elem[idx4,2]]+D[elem[idx4,3]])/4

        return De
    

    def get_CFL(self,dt):
        X=self.x
        Y=self.y
        D=self.z
        A=self.areas
        ele = self.faces
        De=self.map_np2ne(D)
        De[np.where(De<0.1)]=0.1
        CFL=calculate_CFL(De,A,dt)

        return CFL

