# -*- coding: utf-8 -*-
from qgis.core import *
from PyQt5 import QtWidgets
from PyQt5.QtCore import Qt,pyqtRemoveInputHook,QFileInfo
import pygmsh
import os


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

    def writePoint(self, pt, lc) :
        if lc is not None :
            self.p.append(self.geo.add_point([pt[0],pt[1], lc]))
        else :
            self.p.append(self.geo.add_point([pt[0],pt[1], 0.]))
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

    def writeSurface(self) :

        holes=[]
        domain=[]
        channel=[]
        print(self.physicals_ll)
        for physical in self.physicals_ll:

            if physical.lower() in 'island' or physical.lower() in 'channel':
                for x in set(self.physicals_ll[physical]):
                    holes.append(self.ll[x])
            else:
                domain=self.ll[self.physicals_ll[physical][0]]


            if physical.lower() in 'channel':
                for x in set(self.physicals_ll[physical]):
                    channel.append(self.ll[x])
                    sf=self.geo.add_plane_surface(self.ll[x])
                    self.geo. set_transfinite_surface(sf)
                    self.geo.add_raw_code('Recombine Surface {%s};' % sf.id)


        self.geo.add_plane_surface(domain,holes=holes)


    def addLineFromCoords(self, pts, xform, lc, physical,trans=None,progression=1) :
        
        if xform :
            pts = [xform.transform(x) for x in pts]
        firstp = self.ip
        id0 = self.writePointCheckLineLoops(pts[0], lc)
        if samepoint(pts[0], pts[-1]) :
            id1 = id0
        else :
            id1 = self.writePointCheckLineLoops(pts[-1], lc)


        ids = [id0] + [self.writePoint(x, lc) for x in pts[1:-1]] + [id1]

        if physical == 'channel' :
            lids = [self.writeLine(ids)] 
            if trans:
                self.geo.set_transfinite_lines([self.l[-1]], trans, progression=progression)

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

        if physical :
                if physical in self.physicals_ll :
                    self.physicals_ll[physical].append(len(self.ll))
                else :
                    self.physicals_ll[physical] = [len(self.ll)]

        if ll.closed():
            
            self.writeLineLoop(ll)
            
        else:
            self.lineloops.append(ll)



    def add_layer(self,layer,xform):

        name = layer.name()
        fields = layer.fields()

        mesh_size_idx = fields.indexFromName("mesh_size")
        physical_idx = fields.indexFromName("physical")
        trans_idx = fields.indexFromName("trans")
        
        lc = None
        physical = None
        trans=None
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

            if geom.type() == QgsWkbTypes.PolygonGeometry :

                for loop in geom.asMultiPolygon() :
                    for line in loop:
                        self.addLineFromCoords(line, xform, lc, physical,trans)

            elif geom.type() == QgsWkbTypes.LineGeometry :
                lines = geom.asMultiPolyline()
                if not lines :
                    print ('asPolyline()')
                else :
                    for line in lines :

                        self.addLineFromCoords(line, xform, lc, physical ,trans)


            elif geom.type() == QgsWkbTypes.PointGeometry :
                 print ('asPoint()')





        