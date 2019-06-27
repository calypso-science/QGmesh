# -*- coding: utf-8 -*-
from qgis.core import *
from PyQt5 import QtWidgets
from PyQt5.QtCore import Qt,pyqtRemoveInputHook,QFileInfo
import pygmsh
import os
from .tools import writeRasterLayer

def get_corner(surface,geofile):
    import pdb;pdb.set_trace()
    return p1,p2,p3,p4

def samepoint(a, b) :
    return ((a[0] - b[0])**2 + (a[1] - b[1])**2)**0.5 < 1e-8

def get_points(pts,xform):

    if xform :
        pts = [xform.transform(x) for x in pts]

    return pts
class lineloop :
    
    def __init__(self, x0, x1, id0, id1, lids) :
        self.id = [id0, id1]
        self.x = [x0, x1]
        self.lines = [(lid, True) for lid in lids]
    
    def reverse(self) :
        self.lines = [(id, not flag) for id, flag in self.lines[::-1]]
        self.id.reverse()
        self.x.reverse()

    def merge(self, o) :
        if self.id[1] == o.id[1] or self.id[0] == o.id[0]:
            self.reverse()
        if self.id[1] == o.id[0] :
            self.id[1] = o.id[1]
            self.x[1] = o.x[1]
            self.lines = self.lines + o.lines
            return True
        if self.id[0] == o.id[1] :
            self.id[0] = o.id[0]
            self.x[0] = o.x[0]
            self.lines = o.lines + self.lines
            return True
        return False
    
    def closed(self) :
        return self.id[0] == self.id[1]


class GeoFile():
    def __init__(self,filename='test.geo'):
        self.filename=filename
        self.geo= pygmsh.built_in.Geometry()
        self.ip = 0
        self.p=[]
        self.il = 0
        self.l=[]
        self.ill = 0
        self.physicals = {}
        self.lineloops = []
        self.ll = []
        self.lineInSurface = []
        self.pointInSurface = []
        self.surf=[]
        self.physicals_ll={}
        self.channel_border=[]
        self.Field=[]

    def writePoint(self, pt, lc) :
        self.p.append(self.geo.add_point([pt[0],pt[1], 0.],lcar=lc))
        self.ip += 1

        return self.ip - 1

    def writePointCheckLineLoops(self, pt, lc) :
        for ll in self.lineloops :
            if samepoint(ll.x[0], pt) :
                return ll.id[0]
            if samepoint(ll.x[1], pt) :
                return ll.id[1]
        return self.writePoint(pt, lc)

    def writeLine(self, pts) :

        p=[]
        for pt in pts:
            p.append(self.p[pt])
  
        self.l.append(self.geo.add_spline(p))
        self.il += 1
        return self.il - 1

    def writeLineLoop(self, ll) :
        strid = [self.l[i] if o else -self.l[i] for i, o in ll.lines]

        self.ll.append(self.geo.add_line_loop(strid))
        self.ill += 1
        return self.ill - 1

    def writeSurface(self,grid_type) :

        holes=[]
        domain=[]
        channel=[]

        for physical in self.physicals_ll:

            if physical == 'Islands' or physical == 'Channels':
                for x in set(self.physicals_ll[physical]):
                    holes.append(self.ll[x])
            else:
                domain=self.ll[self.physicals_ll[physical][0]]


            if physical=='Channels' and grid_type is not 'quad':
                for x in set(self.physicals_ll[physical]):
                    channel.append(self.ll[x])
                    sf=self.geo.add_plane_surface(self.ll[x])
                    self.geo. set_transfinite_surface(sf)
                    self.geo.add_raw_code('Recombine Surface {%s};' % sf.id)


        sf=self.geo.add_plane_surface(domain,holes=holes)
        if grid_type=='quad':
            # try:
            #     self.geo.set_transfinite_surface(sf)
            # except:
            p1,p2,p3,p4=get_corner(sf,self.geo)
            self.geo.add_raw_code('Transfinite Surface{%s} = {%s,%s,%s,%s};' %(sf.id,p1.id,p2.id,p3.id,p4.id))

            self.geo.add_raw_code('Recombine Surface {%s};' % sf.id)


    def addLineFromCoords(self, pts, xform, lc, physical,group_name,trans=None,bump=None,progression=None,grid_type='tetra') :
        if trans ==0:
            trans=None
        if bump==0:
            bump=None
        if progression==0:
            progression=None

        if xform :
            pts = [xform.transform(x) for x in pts]
        firstp = self.ip
        id0 = self.writePointCheckLineLoops(pts[0], lc)
        if samepoint(pts[0], pts[-1]) :
            id1 = id0
        else :
            id1 = self.writePointCheckLineLoops(pts[-1], lc)


        ids = [id0] + [self.writePoint(x, lc) for x in pts[1:-1]] + [id1]

        if group_name == 'Channels' or grid_type=='quad':
            lids = [self.writeLine(ids)] 

            self.geo.set_transfinite_lines([self.l[-1]], trans, progression=progression, bump=bump)

        else:
            lids = [self.writeLine((ids[i],ids[i+1])) for i in range(len(ids)-1)]


        if physical :
            for lid in lids :
                if physical in self.physicals :
                    self.physicals[physical].append(lid)
                else :
                    self.physicals[physical] = [lid]

   


        ll = lineloop(pts[0], pts[-1], id0, id1, lids)
        self.lineloops = [o for o in self.lineloops if not ll.merge(o)]


        if group_name in self.physicals_ll :
            self.physicals_ll[group_name].append(len(self.ll))
        else :
            self.physicals_ll[group_name] = [len(self.ll)]


            

        if ll.closed():
            
            self.writeLineLoop(ll)
            
        else:
            self.lineloops.append(ll)



    def add_layer(self,layer,xform,group_name,grid_type):

        name = layer.name()
        fields = layer.fields()

        mesh_size_idx = fields.indexFromName("mesh_size")
        physical_idx = fields.indexFromName("physical")
        trans_idx = fields.indexFromName("trans")
        prog_idx = fields.indexFromName("prog")
        bump_idx = fields.indexFromName("bump")
        
        lc = None
        physical = None
        trans=None
        prog=None
        bump=None
        L=[]
        for ie,feature in enumerate(layer.getFeatures()):
            geom = feature.geometry()
            if geom is None :
                continue

            if mesh_size_idx >= 0 :
                lc = feature[mesh_size_idx]

            if physical_idx >= 0 :
                physical = feature[physical_idx]

            if trans_idx >= 0 :
                trans = feature[trans_idx]

            if prog_idx >= 0 :
                prog = feature[prog_idx]

            if bump_idx >= 0 :
                bump = feature[bump_idx]

            if geom.type() == QgsWkbTypes.PolygonGeometry :

                for loop in geom.asMultiPolygon() :
                    for line in loop:
                        self.addLineFromCoords(line, xform, lc, physical,group_name,trans,bump,prog,grid_type)

            elif geom.type() == QgsWkbTypes.LineGeometry :
                lines = geom.asMultiPolyline()
                if not lines :
                    print ('asPolyline()')
                else :
                    for line in lines :

                        self.addLineFromCoords(line, xform, lc, physical ,group_name,trans,bump,prog,grid_type)


            elif geom.type() == QgsWkbTypes.PointGeometry :
                 print ('asPoint()')

    def addBall(self,pt,xform,
         radius=None,
         vin=None,
         vout=None,
         **kwargs):
         
        # Don't use [] as default argument, cf.
        # <https://stackoverflow.com/a/113198/353337>

        if xform :
            pt = xform.transform(pt)

        self.geo._FIELD_ID += 1
        name = "field{}".format(self.geo._FIELD_ID)

        self.geo._GMSH_CODE.append("{} = newf;".format(name))

        self.geo._GMSH_CODE.append("Field[{}] = Ball;".format(name))

        self.geo._GMSH_CODE.append("Field[{}].XCenter= {!r};".format(name, pt[0]))
        self.geo._GMSH_CODE.append("Field[{}].YCenter= {!r};".format(name, pt[1]))
        self.geo._GMSH_CODE.append("Field[{}].ZCenter= {!r};".format(name, 0))
        self.geo._GMSH_CODE.append("Field[{}].Radius= {!r};".format(name, radius))

        if vin:
            self.geo._GMSH_CODE.append("Field[{}].VIn= {!r};".format(name, vin))
        if vout:
            self.geo._GMSH_CODE.append("Field[{}].VOut= {!r};".format(name, vout))

        return name

    def addBox(self,xmin,xmax,ymin,ymax,
         vin=None,
         vout=None,
         **kwargs):
         
        # Don't use [] as default argument, cf.
        # <https://stackoverflow.com/a/113198/353337>


        self.geo._FIELD_ID += 1
        name = "field{}".format(self.geo._FIELD_ID)

        self.geo._GMSH_CODE.append("{} = newf;".format(name))

        self.geo._GMSH_CODE.append("Field[{}] = Box;".format(name))

        self.geo._GMSH_CODE.append("Field[{}].XMin= {!r};".format(name, xmin))
        self.geo._GMSH_CODE.append("Field[{}].XMax= {!r};".format(name, xmax))
        self.geo._GMSH_CODE.append("Field[{}].YMin= {!r};".format(name, ymin))
        self.geo._GMSH_CODE.append("Field[{}].YMax= {!r};".format(name, ymax))

        if vin:
            self.geo._GMSH_CODE.append("Field[{}].VIn= {!r};".format(name, vin))
        if vout:
            self.geo._GMSH_CODE.append("Field[{}].VOut= {!r};".format(name, vout))

        return name

    def addStructured(self,fname,
         vout=None,
         **kwargs):
         
        # Don't use [] as default argument, cf.
        # <https://stackoverflow.com/a/113198/353337>


        self.geo._FIELD_ID += 1
        name = "field{}".format(self.geo._FIELD_ID)

        self.geo._GMSH_CODE.append("{} = newf;".format(name))

        self.geo._GMSH_CODE.append("Field[{}] = Structured;".format(name))

        self.geo._GMSH_CODE.append("Field[{}].FileName= {!r};".format(name, fname))

        if vout:
            self.geo._GMSH_CODE.append("Field[{}].VOut= {!r};".format(name, vout))

        #self.geo._GMSH_CODE.append("Mesh.CharacteristicLengthFromPoints = 0;")
        #self.geo._GMSH_CODE.append("Mesh.CharacteristicLengthFromCurvature = 0;")
        #self.geo._GMSH_CODE.append("Mesh.CharacteristicLengthExtendFromBoundary = 0;")




        return name

    def add_sizing(self,layer,xform,group_name):

        name = layer.name()
        

        if layer.type() == QgsMapLayer.RasterLayer :
            root,f=os.path.split(layer.dataProvider().dataSourceUri())
            fname=os.path.join(root,name+'.dat')

            writeRasterLayer(layer,fname)
            options={}
            self.Field.append(self.addStructured( fname,**options))

        else:
            fields = layer.fields()
            for ie,feature in enumerate(layer.getFeatures()):
                geom = feature.geometry()
                if geom is None :
                    continue
                options={}
                for opt in fields.names():
                    idx=fields.indexFromName(opt)
                    options[opt.lower()]=feature[idx]


                if name.lower() == 'ball' :
                    point = geom.asPoint()
                    self.Field.append(self.addBall(point, xform,**options))


                if name.lower() == 'box' :
                    xmin=float('inf')
                    xmax=float('inf')*-1
                    ymin=float('inf')
                    ymax=float('inf')*-1
                    lines = geom.asMultiPolyline()
                    for line in lines:
                        if xform :
                            line = [xform.transform(x) for x in line]
                        for pt in line:
                            xmin=min(xmin,pt[0])
                            xmax=max(xmax,pt[0])
                            ymin=min(xmin,pt[1])
                            ymax=max(ymax,pt[1])

                    self.Field.append(self.addBox(xmin,xmax,ymin,ymax,**options))



        

        