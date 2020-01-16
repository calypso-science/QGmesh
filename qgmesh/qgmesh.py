# -*- coding: utf-8 -*-
"""
/***************************************************************************
 qgmesh
                                 A QGIS plugin
 This plugin create 2D regular/irregular grid mesh 
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2019-05-23
        git sha              : $Format:%H$
        copyright            : (C) 2019 by Remy Zyngfogel
        email                : r.zyngfogel@metocean.co.nz
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
from PyQt5.QtCore import QSettings, QTranslator, qVersion, QCoreApplication,QVariant,QThread
from PyQt5.QtGui import QIcon
from qgis.PyQt.QtWidgets import QProgressBar
from PyQt5.QtWidgets import QAction,QFileDialog,QMenu,QApplication

from qgis.core import QgsMapLayer,QgsProject,QgsLayerTreeGroup,Qgis,QgsCoordinateTransform,QgsWkbTypes,QgsVectorLayer,QgsField

# Initialize Qt resources from file resources.py
from .resources import *
# Import the code for the dialog
import os.path

from .exportGeometry import GeoFile
from .runGmsh import *
import meshio

from .mesh import Mesh
from .raster import raster_calculator
from .cfl_inputs import CFL_calculator

from qgis.gui import QgsMapLayerComboBox#, QgsFieldProxyModel#QgsMapLayerProxyModel

from .tools import *
import uuid

from .runNICEGRID2 import *
from .meshselector import *

import platform

class qgmesh:
    """QGIS Plugin Implementation."""

    def __init__(self, iface):
        """Constructor.

        :param iface: An interface instance that will be passed to this class
            which provides the hook by which you can manipulate the QGIS
            application at run time.
        :type iface: QgsInterface
        """
        # Save reference to the QGIS interface
        self.iface = iface
        # initialize plugin directory
        self.plugin_dir = os.path.dirname(__file__)
        # initialize locale
        locale = QSettings().value('locale/userLocale')[0:2]
        locale_path = os.path.join(
            self.plugin_dir,
            'i18n',
            'qgmesh_{}.qm'.format(locale))

        if os.path.exists(locale_path):
            self.translator = QTranslator()
            self.translator.load(locale_path)

            if qVersion() > '4.3.3':
                QCoreApplication.installTranslator(self.translator)

        # Declare instance attributes
        self.actions = []
        self.menu = self.tr(u'&qgmesh')

        # Check if plugin was started the first time in current QGIS session
        # Must be set in initGui() to survive plugin reloads
        self.first_start = None

    # noinspection PyMethodMayBeStatic
    def tr(self, message):
        """Get the translation for a string using Qt translation API.

        We implement this ourselves since we do not inherit QObject.

        :param message: String for translation.
        :type message: str, QString

        :returns: Translated version of message.
        :rtype: QString
        """
        # noinspection PyTypeChecker,PyArgumentList,PyCallByClass
        return QCoreApplication.translate('qgmesh', message)


    def add_action(
        self,
        icon_path,
        text,
        callback,
        menu=None,
        enabled_flag=True,
        add_to_menu=True,
        add_to_toolbar=True,
        status_tip=None,
        whats_this=None,
        parent=None):


        """Add a toolbar icon to the toolbar.

        :param icon_path: Path to the icon for this action. Can be a resource
            path (e.g. ':/plugins/foo/bar.png') or a normal file system path.
        :type icon_path: str

        :param text: Text that should be shown in menu items for this action.
        :type text: str

        :param callback: Function to be called when the action is triggered.
        :type callback: function

        :param enabled_flag: A flag indicating if the action should be enabled
            by default. Defaults to True.
        :type enabled_flag: bool

        :param add_to_menu: Flag indicating whether the action should also
            be added to the menu. Defaults to True.
        :type add_to_menu: bool

        :param add_to_toolbar: Flag indicating whether the action should also
            be added to the toolbar. Defaults to True.
        :type add_to_toolbar: bool

        :param status_tip: Optional text to show in a popup when mouse pointer
            hovers over the action.
        :type status_tip: str

        :param parent: Parent widget for the new action. Defaults None.
        :type parent: QWidget

        :param whats_this: Optional text to show in the status bar when the
            mouse pointer hovers over the action.

        :returns: The action that was created. Note that the action is also
            added to self.actions list.
        :rtype: QAction
        """
        if menu is None:
            menu=self.menu

        icon = QIcon(icon_path)
        action = QAction(icon, text, parent)
        if callback is not None:
            action.triggered.connect(callback)

        action.setEnabled(enabled_flag)

        if status_tip is not None:
            action.setStatusTip(status_tip)

        if whats_this is not None:
            action.setWhatsThis(whats_this)

        if add_to_toolbar:
            # Adds plugin icon to Plugins toolbar
            self.iface.addToolBarIcon(action)

        if add_to_menu:
            self.iface.addPluginToMenu(
                menu,
                action)

        self.actions.append(action)

        return action

    def initGui(self):
        root=os.path.dirname(__file__)
        meshit_icon=os.path.join(root,'icons','MeshIt.ico')
        refresh_icon=os.path.join(root,'icons','refresh.png')
        wavelength_icon=os.path.join(root,'icons','wavelength.png')
        scale_icon=os.path.join(root,'icons','Downsize.png')
        tape_icon=os.path.join(root,'icons','tape.png')

        icon_path = ''

        """Create the menu entries and toolbar icons inside the QGIS GUI."""
        self.menu = QMenu(self.iface.mainWindow())
        self.menu.setObjectName("QGmesh")
        self.menu.setTitle("QGmesh")

        ## Initialize
        self.menu_init = QMenu(self.iface.mainWindow())
        self.menu_init.setObjectName("Initializing")
        self.menu_init.setTitle("Initializing")


     


        init_fold_tetra=self.add_action(
            icon_path,
            text=self.tr(u'Finite'),
            callback=self.initialize_folders_tetra,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Generate each folder needed by QGmsh.",
            whats_this="Generate folders: \n Boundaries, Channel. Islands  \n which are needed to run QGmsh.")

        self.menu_init.addAction(init_fold_tetra)

        init_fold_quad=self.add_action(
            icon_path,
            text=self.tr(u'Quad'),
            callback=self.initialize_folders_quad,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Generate each folder needed by QGmsh.",
            whats_this="Generate folders: \n Boundaries, Channel. Islands  \n which are needed to run QGmsh.")

        self.menu_init.addAction(init_fold_quad)

        init_fold_trans=self.add_action(
            icon_path,
            text=self.tr(u'Transfinite'),
            callback=self.initialize_folders_transfinite,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Generate each folder needed by QGmsh.",
            whats_this="Generate folders: \n Boundaries, Channel. Islands  \n which are needed to run QGmsh.")

        self.menu_init.addAction(init_fold_trans)

        self.menu.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.menu_init)

        ## Geometry

        self.menu_geo = QMenu(self.iface.mainWindow())
        self.menu_geo.setObjectName("Geometry")
        self.menu_geo.setTitle("Geometry")

        update_geo=self.add_action(
            icon_path,
            text=self.tr(u'Update geometry'),
            callback=self.update_geofile,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Update the Geometry",
            whats_this="This will update th geometry needed by GMSH.")

        self.menu_geo.addAction(update_geo)
        
        export_geo=self.add_action(
            icon_path,
            text=self.tr(u'Export geometry'),
            callback=self.export_geofile,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Export the Geometry",
            whats_this="This will export the geometry file for manual editing.")

        self.menu_geo.addAction(export_geo)


        self.menu.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.menu_geo)

        ## Mesh file

        self.menu_mesh = QMenu(self.iface.mainWindow())
        self.menu_mesh.setObjectName("Meshing")
        self.menu_mesh.setTitle("Meshing")

        mesh_it=self.add_action(
            meshit_icon,
            text=self.tr(u'Mesh it'),
            callback=self.mesh_geofile,
            parent=self.iface.mainWindow(),
            add_to_toolbar=True,
            add_to_menu=False,
            status_tip="Create the mesh",
            whats_this="This will launch GMSH and create the mesh.")

        self.menu_mesh.addAction(mesh_it)


        export_msh=self.add_action(
            icon_path,
            text=self.tr(u'Export Mesh'),
            callback=self.export_mesh,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Export the Mesh",
            whats_this="This will export the Mesh file for manual editing.")

        self.menu_mesh.addAction(export_msh)

        import_msh=self.add_action(
            icon_path,
            text=self.tr(u'Import Mesh'),
            callback=self.import_mesh,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Import the Mesh",
            whats_this="This will import the Mesh file for manual editing.")

        self.menu_mesh.addAction(import_msh)

        refresh_msh=self.add_action(
            refresh_icon,
            text=self.tr(u'Refresh Mesh'),
            callback=self.refresh_mesh,
            parent=self.iface.mainWindow(),
            add_to_toolbar=True,
            add_to_menu=False,
            status_tip="Refresh the Mesh",
            whats_this="This will resfresh the Mesh with latest values.")

        self.menu_mesh.addAction(refresh_msh)

        get_CFL=self.add_action(
            icon_path,
            text=self.tr(u'Display CFL'),
            callback=self.get_CFL,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Calculate CFL",
            whats_this="This will calculate and display the CFL number.")

        self.menu_mesh.addAction(get_CFL)

        nicegrid=self.add_action(
            icon_path,
            text=self.tr(u'Clean FEM mesh'),
            callback=self.nicegrid,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Clean a mesh",
            whats_this="This will clean the mesh")

        self.menu_mesh.addAction(nicegrid)

        self.menu.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.menu_mesh)

        ## Bathymery
        self.menu_bathy= QMenu(self.iface.mainWindow())
        self.menu_bathy.setObjectName("Bathymetry")
        self.menu_bathy.setTitle("Bathymetry")

        add_bathy=self.add_action(
            icon_path,
            text=self.tr(u'Add bathy'),
            callback=self.add_bathy,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Add bathy to the mesh",
            whats_this="This will add bathy to the mesh.")

        self.menu_bathy.addAction(add_bathy)

        bnd_correction=self.add_action(
            icon_path,
            text=self.tr(u'Bnd correction'),
            callback=self.bnd_correction,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip="Correct the bathymetry near the open boundaries",
            whats_this="This will make th bathymetry deeper near the open boundaries")

        self.menu_bathy.addAction(bnd_correction)


        self.menu.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.menu_bathy)

        ## Sizing
        self.menu_size= QMenu(self.iface.mainWindow())
        self.menu_size.setObjectName("Sizing")
        self.menu_size.setTitle("Sizing")


        ball_field=self.add_action(
            icon_path,
            text=self.tr(u'add ball'),
            callback=self.add_sizing_ball,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip=".",
            whats_this=".")

        self.menu_size.addAction(ball_field)

        box_field=self.add_action(
            icon_path,
            text=self.tr(u'add box'),
            callback=self.add_sizing_box,
            parent=self.iface.mainWindow(),
            add_to_toolbar=False,
            add_to_menu=False,
            status_tip=".",
            whats_this=".")

        self.menu_size.addAction(box_field)

        self.raster_calc= QMenu(self.menu_size)
        self.raster_calc.setObjectName("Raster calculator")
        self.raster_calc.setTitle("Raster calculator")

        to_wavelength=self.add_action(
            wavelength_icon,
            text=self.tr(u'convert to wavelength'),
            callback=self.to_wavelength,
            parent=self.menu_size,
            add_to_toolbar=True,
            add_to_menu=False,
            status_tip=".",
            whats_this=".")

        self.raster_calc.addAction(to_wavelength)

        to_scale=self.add_action(
            scale_icon,
            text=self.tr(u'Scale value to range'),
            callback=self.to_scale,
            parent=self.menu_size,
            add_to_toolbar=True,
            add_to_menu=False,
            status_tip=".",
            whats_this=".")

        self.raster_calc.addAction(to_scale)

        to_dist=self.add_action(
            tape_icon,
            text=self.tr(u'Calculate distances'),
            callback=self.to_dist,
            parent=self.menu_size,
            add_to_toolbar=True,
            add_to_menu=False,
            status_tip=".",
            whats_this=".")

        self.raster_calc.addAction(to_dist)


        self.menu_size.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.raster_calc)

        self.menu.insertMenu(self.iface.firstRightStandardMenu().menuAction(),self.menu_size)
        
        menuBar = self.iface.mainWindow().menuBar()
        menuBar.insertMenu(self.iface.firstRightStandardMenu().menuAction(), self.menu)

        # will be set False in run()
        self.first_start = True


        plat=platform.system()
        if plat in 'Linux':
            self.tempdir = "/tmp"
        elif plat in 'Windows':
            self.tempdir ="C:\TEMP"
        elif plat in 'Darwin':
            self.tempdir='/tmp'

    def unload(self):
        """Removes the plugin menu item and icon from QGIS GUI."""
        for action in self.actions:
            self.iface.removePluginMenu(
                self.tr(u'&qgmesh'),
                action)
            self.iface.removeToolBarIcon(action)

        
    def add_sizing_ball(self):
        inLayerGeometryType = ['Point','Line','Polygon'][QgsWkbTypes.PointGeometry]
        proj = QgsProject.instance()
        crs=proj.crs()
        outLayer = QgsVectorLayer( inLayerGeometryType+ '?crs='+crs.authid(), \
            'Ball', \
            'memory')

        outLayer.startEditing()
        outLayer.dataProvider().addAttributes([\
            QgsField("vin", QVariant.Double,"double",10,4),\
            QgsField("vout", QVariant.Double,"double",10,4),\
            QgsField("radius", QVariant.Double,"double",10,4)])

        outLayer.commitChanges()

        proj.addMapLayer(outLayer)

    def add_sizing_box(self):
        path_absolute = QgsProject.instance().readPath('./')
        filename=os.path.join(path_absolute,'sizing_box_'+str(uuid.uuid4())+'.shp')
        inLayerGeometryType =QgsWkbTypes.Polygon #['Point','Line','Polygon'][QgsWkbTypes.PolygonGeometry]
        proj = QgsProject.instance()
        crs=proj.crs()


        fields=  QgsFields()      
        fields.append(QgsField("vin", QVariant.Double,'double', 10,4))
        fields.append(QgsField("vout", QVariant.Double,'double',10,4))

        fileWriter = QgsVectorFileWriter(filename,
           "system", fields,inLayerGeometryType , crs,
           "ESRI Shapefile")

        del fileWriter

        outLayer = QgsVectorLayer(filename, "Box", "ogr")

        proj.addMapLayer(outLayer)




    def to_wavelength(self):
        proj = QgsProject.instance()
        raster=[]
        for child in proj.layerTreeRoot().findLayers():   
            layer = proj.mapLayer(child.layerId())     
            if layer.type()==QgsMapLayer.RasterLayer:
                raster.append(layer.name())

        ex = raster_calculator(raster,'wavelength',tmp= self.tempdir)
        ex.show()
        ex.exec_()

    def to_scale(self):
        proj = QgsProject.instance()
        raster=[]
        for child in proj.layerTreeRoot().findLayers():   
            layer = proj.mapLayer(child.layerId())     
            if layer.type()==QgsMapLayer.RasterLayer:
                raster.append(layer.name())

        ex = raster_calculator(raster,'scale',tmp= self.tempdir)
        ex.show()
        ex.exec_()

    def to_dist(self):
        proj = QgsProject.instance()
        raster=[]
        for child in proj.layerTreeRoot().findLayers():   
            layer = proj.mapLayer(child.layerId())     
            if layer.type()!=QgsMapLayer.RasterLayer:
                raster.append(layer.name())

        ex = raster_calculator(raster,'distance',tmp= self.tempdir)
        ex.show()
        ex.exec_() 

   

    def get_CFL(self):
        ex = CFL_calculator()
        ex.show()
        dt,min_CFL,warn_CFL=ex.exec_()
        
        CFL=self.mesh.get_CFL(dt)
        self.iface.messageBar().pushMessage("Info", "%s CFL10 " % str(CFL[10]), level=Qgis.Info)

        proj = QgsProject.instance()
        crs=proj.crs()
        #Create new shapefile object, loop trough triangle edges and add each
        # edge as a line.
        shape=QgsWkbTypes.Point
        path_absolute = QgsProject.instance().readPath('./')
        
        filename='new_grid_CFL_dt%ss.shp' % str(dt)
        filename=os.path.join(path_absolute,filename)

        fields = QgsFields()
        fields.append(QgsField("CFL", QVariant.Double,'double', 4, 2))

        fileWriter = QgsVectorFileWriter(filename,
                   "system", fields,shape , crs,
                   "ESRI Shapefile")

        if fileWriter.hasError() != QgsVectorFileWriter.NoError:
            raise Exception('Error when creating shapefile '+filename+
                        ' : '+str(fileWriter.hasError()))

        
        
        for ele in range(0,len(self.mesh.xctr)):
            point = QgsPointXY()
            point.setX(self.mesh.xctr[ele])
            point.setY(self.mesh.yctr[ele])
            newFeature = QgsFeature()
            newFeature.setGeometry(QgsGeometry.fromPointXY(point))
            newFeature.setFields(fields)
            newFeature.setAttributes([float('%4.2f' % CFL[ele])])
            fileWriter.addFeature(newFeature)

        del fileWriter
        vlayer = QgsVectorLayer(filename, "CFL", "ogr")
        vlayer=assign_values(vlayer,min_CFL,warn_CFL)
        QgsProject.instance().addMapLayer(vlayer,False)
        proj.layerTreeRoot().addLayer(vlayer)




    def export_mesh(self):
        qfd = QFileDialog()
        path = ""

        export_function=load_IO()
        fil=''
        for model in export_function:
            fil+=export_function[model]['extension']()+';'

       
        fname = QFileDialog.getSaveFileName(qfd, 'exprt Mesh file', path, fil)
        fname=fname[0]

        if fname=='':
            return

        filename, file_extension = os.path.splitext(fname)
        if file_extension=='':
            return



        proj = QgsProject.instance()
        Mesh=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup) and child.name()[:4]=='Mesh':
                Mesh.append(child.name())

        msh=mesh_selector(Mesh)
        msh_name=msh.exec_()
        value=int(msh_name.replace('Mesh_',''))
        self.mesh=self.rebuild_mesh(value=value)

        for model in export_function:
            ext=export_function[model]['extension']()
            if file_extension in ext :
                export_function[model]['export'](self.mesh,fname)  

        self.iface.messageBar().pushMessage("Info", "%s exported " % fname, level=Qgis.Info)

    def import_mesh(self):
        qfd = QFileDialog()
        path = ""

        import_function=load_IO()
        fil=''
        for model in import_function:
            fil+=import_function[model]['extension']()+';'


        fname = QFileDialog.getOpenFileName(qfd, 'Import Mesh file', path, fil)
        fname=fname[0]
        if fname=='':
            return

        filename, file_extension = os.path.splitext(fname)
        if file_extension=='':
            return

        progressMessageBar = self.iface.messageBar().createMessage("Load Mesh")
        progress = QProgressBar()
        progress.setMaximum(10)
        progress.setAlignment(Qt.AlignLeft|Qt.AlignVCenter)
        progressMessageBar.layout().addWidget(progress)
        self.iface.messageBar().pushWidget(progressMessageBar, Qgis.Info)

        self.mesh=Mesh([],[],[],[])
        for model in import_function:
            ext=import_function[model]['extension']()
            if file_extension in ext :
                progress.setValue(0)
                self.mesh=import_function[model]['import'](self.mesh,fname)   
                progress.setValue(3)
                self.mesh._calculate_face_nodes()
                progress.setValue(6)
#                self.mesh._remove_hanging_nodes()
                self.mesh.calc_parameters()
                progress.setValue(9)
                Mesh2Shapefile(self.mesh)
                

        self.iface.messageBar().clearWidgets()
    def nicegrid(self):

        proj = QgsProject.instance()
        shapefile=['None']
        meshfiles=[]
        
        for child in proj.layerTreeRoot().findLayers():   
            layer = proj.mapLayer(child.layerId())     
            if layer.type()==QgsMapLayer.VectorLayer:
                shapefile.append(layer.name())


        run_cleaner = RunSCHISMDialog(self.mesh,shapefile,tmp= self.tempdir)
        root=os.path.dirname(__file__)
        nicegrid=os.path.join(root,'nicegrid2')
        if not os.path.isfile(nicegrid):
            os.system('gfortran -o %s %s' %(nicegrid,os.path.join(root,'nicegrid2.f90')))
            self.iface.messageBar().pushMessage("Info", "NCIEGRID2 not found and compiled. ", level=Qgis.Info)
            

        run_cleaner.exec_()

        import_function=load_IO()
        self.mesh=import_function['schismIO']['import'](self.mesh,os.path.join( self.tempdir,'gridout.gr3'))   
        self.mesh._calculate_face_nodes()
#                self.mesh._remove_hanging_nodes()
        self.mesh.calc_parameters()
        Mesh2Shapefile(self.mesh)

    def rebuild_mesh(self,value=None):
        
        proj = QgsProject.instance()
        if value is None:
            value=-1
            for child in proj.layerTreeRoot().children():
                if isinstance(child, QgsLayerTreeGroup) and child.name()[:4]=='Mesh':
                    value+=1


        self.mesh=Mesh([],[],[],[])

        self.mesh.physical={}
        self.mesh.edges=[]
        self.mesh.physicalID=[]
            
        
        for child in proj.layerTreeRoot().findGroups():        
            if child.name() == 'Mesh_%i'%value:
                for sub_subChild in child.children():
                    layer = proj.mapLayer(sub_subChild.layerId())

                    if layer.name()=='Nodes':
                        idx = layer.fields().indexFromName('Depth')
                        self.mesh.x=np.ones(layer.featureCount())*-1.
                        self.mesh.y=np.ones(layer.featureCount())*-1.
                        self.mesh.z=np.ones(layer.featureCount())*-1.

                        for node in layer.getFeatures():
                            nodeID = node.attribute(0)-1
                            geom = node.geometry().asPoint()
                            dep = node.attribute(idx)

                            self.mesh.z[nodeID]=dep
                            self.mesh.x[nodeID]=geom[0]
                            self.mesh.y[nodeID]=geom[1]
                        assign_bathy(layer)

                    if layer.name()=='Faces':
                        self.mesh.faces=np.ones((layer.featureCount(),4),'int32')*-1
                        idx0 = layer.fields().indexFromName('Node1')
                        idx1 = layer.fields().indexFromName('Node2')
                        idx2 = layer.fields().indexFromName('Node3')
                        idx3 = layer.fields().indexFromName('Node4')
                        for ie,face in enumerate(layer.getFeatures()):
                            self.mesh.faces[ie,0]= face.attribute(idx0)-1
                            self.mesh.faces[ie,1]= face.attribute(idx1)-1
                            self.mesh.faces[ie,2]= face.attribute(idx2)-1
                            if face.attribute(idx3)>=0:
                                self.mesh.faces[ie,3]= face.attribute(idx3)-1

                        self.mesh.faces=self.mesh.faces.astype('int')
                        self.mesh._calculate_face_nodes()
                        self.mesh.calc_parameters()


                    if layer.name() =='Edges':
                        idx0 = layer.fields().indexFromName('Id')
                        flag = layer.fields().indexFromName('Flag')
                        name = layer.fields().indexFromName('Name')

                        for ie,bnd in enumerate(layer.getFeatures()):
                            if bnd.attribute(name) not in self.mesh.physical.keys():
                                self.mesh.physical[bnd.attribute(name)]=np.array([bnd.attribute(flag),1])                              

                            nodes = bnd.geometry().asMultiPolyline()[0]
                            for node in nodes:
                                ids=np.where(((self.mesh.x==node[0]) & (self.mesh.y==node[1])))[0]
                                if len(ids)>0:
                                    self.mesh.edges.append(ids[0])
                                    self.mesh.physicalID.append(bnd.attribute(flag))


                            self.mesh.edges.append(-1)
                            self.mesh.physicalID.append(-1) 

        self.mesh.edges=np.array(self.mesh.edges).astype('int32')
        self.mesh.physicalID=np.array(self.mesh.physicalID).astype('int32')


        # if face_change:
        #     self.mesh.writeShapefile(os.path.join(path_absolute,'new_grid_faces'),'faces')
        #     QThread.sleep(1)
        #     vlayer = QgsVectorLayer(os.path.join(path_absolute,'new_grid_faces.shp'), "Faces", "ogr")
        #     QgsProject.instance().addMapLayer(vlayer,False)
        #     G.addLayer(vlayer)


        # self.mesh.writeShapefile(os.path.join(path_absolute,'new_grid_bnd'),'edges')
        # vlayer = QgsVectorLayer(os.path.join(path_absolute,'new_grid_bnd.shp'), 'Edges', "ogr")
        # assign_bnd(vlayer,self.mesh.physical)
        # QgsProject.instance().addMapLayer(vlayer,False)
        # G.addLayer(vlayer)


        self.iface.messageBar().pushMessage("Info", "Mesh updated with Mesh#%i" %value, level=Qgis.Info)
        
        return self.mesh

    def refresh_mesh(self):
        
        proj = QgsProject.instance()
        Mesh=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup) and child.name()[:4]=='Mesh':
                Mesh.append(child.name())

        msh=mesh_selector(Mesh)
        msh_name=msh.exec_()
        value=int(msh_name.replace('Mesh_',''))
        self.mesh=self.rebuild_mesh(value=value)


    def bnd_correction(self):
        proj = QgsProject.instance()

        Mesh=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup) and child.name()[:4]=='Mesh':
                Mesh.append(child.name())

        msh=mesh_selector(Mesh)
        msh_name=msh.exec_()
        value=int(msh_name.replace('Mesh_',''))
        self.mesh=self.rebuild_mesh(value=value)


    def add_bathy(self):
        proj = QgsProject.instance()
        
        Mesh=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup) and child.name()[:4]=='Mesh':
                Mesh.append(child.name())

        msh=mesh_selector(Mesh)
        msh_name=msh.exec_()
        value=int(msh_name.replace('Mesh_',''))
        self.mesh=self.rebuild_mesh(value=value)

        raster=[]
        for child in proj.layerTreeRoot().findLayers():   
            layer = proj.mapLayer(child.layerId())     
            if layer.type()==QgsMapLayer.RasterLayer:
                raster.append(layer.name())

        if len(raster)==0:
            self.iface.messageBar().pushMessage("Error", "You need at Raster to add bathy", level=Qgis.Warning)
            return

        ex = raster_calculator(raster,'choose',tmp= self.tempdir)
        ex.show()
        layer=ex.exec_()
        
        self.mesh.AddBathy(layer)

        for child in proj.layerTreeRoot().findGroups():      
            if child.name() in ['Mesh_'+str(value)]:
                for sub_subChild in child.children():
                    if sub_subChild.name()=='Nodes':
                        layer = proj.mapLayer(sub_subChild.layerId())     
                        update_field(layer,'Depth',self.mesh.z,fmt='%9.3f')
                        assign_bathy(layer)
                    if sub_subChild.name()=='Faces':
                        update_field(layer,'depth',self.mesh.zctr,fmt='%9.2f')
                        update_field(layer,'volume',self.mesh.areas*self.mesh.zctr,fmt='%9.f')




        self.iface.messageBar().pushMessage("Info", "Bathy interpolated to the mesh " , level=Qgis.Info)

    def mesh_geofile(self):
        path_absolute = QgsProject.instance().readPath('./')


        self.update_geofile()
        main=RunGmshDialog(self.geo)
        main.show()

        myStream=EmittingStream()
        myStream.textWritten.connect(main.normalOutputWritten)

        sys.stdout = myStream

        msh=main.exec_()
        if not hasattr(msh,'points'):
            return

        msh=meshio.Mesh(points=msh.points,cells=msh.cells,point_data=msh.point_data,cell_data=msh.cell_data,field_data=msh.field_data)
        
        unique_name=str(uuid.uuid4())
        mesh_out=os.path.join(path_absolute,unique_name+'.msh')
        #meshio.write(mesh_out,msh)

        proj = QgsProject.instance()
        proj.writeEntry("QGmsh", "mesh_file", mesh_out)

        triangles,edges,physicalID=get_format_from_gmsh(msh)

        self.mesh=Mesh(msh.points[:,0],msh.points[:,1],msh.points[:,2],triangles,\
            edges=edges,\
            physical=msh.field_data,\
            physicalID=physicalID)


        Mesh2Shapefile(self.mesh)

       
    def update_geofile(self):
        proj = QgsProject.instance()
        crs=proj.crs()
        self.geo=GeoFile()
        added=[]

        for child in proj.layerTreeRoot().findGroups():        
            if child.name() in ['Boundaries','Islands','Channels','Corners','Inclusion','QuadPatch']:
                if child.name()=='Boundaries':
                    self.geo.extra_gmsh_arguments=child.customProperty('wmsAbstract').split(',')

                grid_type=child.customProperty('grid type')
                for sub_subChild in child.children():
                    layer = proj.mapLayer(sub_subChild.layerId())
                    xform = QgsCoordinateTransform(layer.crs(), crs,proj)
                    self.geo.add_layer(layer,xform,child.name(),grid_type)
                    added.append(child.name())
        
        if len(added)>0:
            self.iface.messageBar().pushMessage("Info", "layer(s): %s added to Geometry" % ','.join(added), level=Qgis.Info)

        self.geo.writeSurface(grid_type)

        self.geo.write_physical()

        for child in proj.layerTreeRoot().findGroups():      
            if child.name() in ['Sizing']:
                for sub_subChild in child.children():
                    layer = proj.mapLayer(sub_subChild.layerId())
                    xform = QgsCoordinateTransform(layer.crs(), crs,proj)
                    self.geo.add_sizing(layer,xform,child.name())

        if len(self.geo.Field)>0:
            self.geo.geo.add_background_field(self.geo.Field,aggregation_type='Min')
            #import pdb;pdb.set_trace()

        for i,li in enumerate(self.geo.geo._GMSH_CODE):
            if 'Spline' in li:
                self.geo.geo._GMSH_CODE[i]=li.replace('Spline','Line')

    def export_geofile(self):
        self.update_geofile()
        qfd = QFileDialog()
        path = ""
        fil = "geo(*.geo)"
        fname = QFileDialog.getSaveFileName(qfd, 'exprt Geo file', path, fil)
        fname=fname[0]
        if fname=='':
            return

        if not fname.endswith('.geo'):
            fname=fname+'.geo'

        with open(fname, 'w') as f:
            f.write(self.geo.geo.get_code() + '\n')

        f.close()
        self.iface.messageBar().pushMessage("Info", "%s exported " % fname, level=Qgis.Info)

       


    def initialize_folders_tetra(self):
        """Run method that performs all the real work"""
        # See if OK was pressed
        groups=['Boundaries','Channels','Islands','Sizing','Inclusion','QuadPatch']
        proj = QgsProject.instance()




        childs=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup):
                childs.append(child.name())

        exists_already=[]
        for group in groups:
            if group not in childs:
                
                gr=proj.layerTreeRoot().addGroup(group)
                gr.setCustomProperty('grid type','tetra')
                gr.setCustomProperty('wmsAbstract','-2,-algo,front2d,-epslc1d,1e-3')
            else:
                exists_already.append(group)

        #if len(exists_already)>1:
        #    self.iface.messageBar().pushMessage("Error", "layer(s): %s already exists" % ','.join(exists_already), level=Qgis.Warning)
    def initialize_folders_quad(self):
        """Run method that performs all the real work"""
        # See if OK was pressed
        groups=['Boundaries','Islands','Sizing','Channels']
        proj = QgsProject.instance()



        childs=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup):
                childs.append(child.name())

        exists_already=[]
        for group in groups:
            if group not in childs:
                gr=proj.layerTreeRoot().addGroup(group)
                gr.setCustomProperty('grid type','quad')
                gr.setCustomProperty('wmsAbstract','-2,-algo,front2d,-epslc1d,1e-3')

            else:
                exists_already.append(group)

    def initialize_folders_transfinite(self):
        """Run method that performs all the real work"""
        # See if OK was pressed
        groups=['Boundaries','Corners']
        proj = QgsProject.instance()




        childs=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup):
                childs.append(child.name())

        exists_already=[]
        for group in groups:
            if group not in childs:
                gr=proj.layerTreeRoot().addGroup(group)
                gr.setCustomProperty('grid type','transfinite')
                gr.setCustomProperty('wmsAbstract','-2,-algo,front2d,-epslc1d,1e-3')

            else:
                exists_already.append(group)

