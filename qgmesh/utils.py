# -*- coding: utf-8 -*-


from qgis.core import QgsCoordinateReferenceSystem

def get_crs(projid=''):
    
    crs=None
    if projid :
        crs = QgsCoordinateReferenceSystem(projid)
    if crs is None or not crs.isValid():
        crs = QgsCoordinateReferenceSystem("EPSG:4326")

    return crs
