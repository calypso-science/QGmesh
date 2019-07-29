from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QApplication,QVBoxLayout,QHBoxLayout,QPushButton,QDialog,QLabel,QLineEdit,QFileDialog,QProgressDialog,QInputDialog,QWidget
from PyQt5.QtGui import QIcon,QColor
from qgis.gui import QgsProjectionSelectionTreeWidget
from qgis.core import *
import sys,os
import numpy as np
from PyQt5 import QtWidgets
import struct
from datetime import datetime
from qgis.utils import iface
from .tools import *

class mesh_selector(QtWidgets.QDialog):

    def __init__(self,meshes):
        super(mesh_selector,self).__init__()


        layout = QtWidgets.QVBoxLayout()
        self.meshselector = QtWidgets.QListWidget()
        self.meshselector.addItems(meshes)
        TitleLayout("Mesh", self.meshselector, layout)

        self.title = 'Choose a mesh'
        self.setWindowTitle(self.title)
        self.initChoose(layout)

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

    def initChoose(self,layout):
        self.runLayout = CancelRunLayout(self,"Choose", self.choose_mesh, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)
  

    def choose_mesh(self):
        mesh=self.meshselector.currentItem().text()
        self.close()

    def exec_(self):

        super(mesh_selector, self).exec_()
        mesh=self.meshselector.currentItem().text()
        return mesh 