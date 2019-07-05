from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QApplication,QVBoxLayout,QHBoxLayout,QPushButton,QDialog,QLabel,QLineEdit,QFileDialog,QProgressDialog,QInputDialog,QWidget
from PyQt5.QtGui import QIcon,QColor
from qgis.gui import QgsProjectionSelectionTreeWidget
from qgis.core import *
import sys,os
from PyQt5 import QtWidgets
from .tools import *

class CFL_calculator(QtWidgets.QDialog):

    def __init__(self):
        super(CFL_calculator,self).__init__()


        layout = QtWidgets.QVBoxLayout()
        self.dt=QtWidgets.QLineEdit()
        self.dt.setText("100")
        TitleLayout("dt (s)", self.dt, layout)

        self.min_CFL=QtWidgets.QLineEdit()
        self.min_CFL.setText('0.4')
        TitleLayout("Minimum CFL", self.min_CFL, layout)

        self.warn_CFL=QtWidgets.QLineEdit()
        self.warn_CFL.setText('0.8')
        TitleLayout("Warning CFL", self.warn_CFL, layout)

        self.runLayout = CancelRunLayout(self,"calculate", self.finsih, layout)
        self.runLayout.runButton.setEnabled(True)
        self.setLayout(layout)

        self.setMaximumHeight(10)
        self.resize(max(400, self.width()), self.height())
        self.show()

    def finsih(self):
        self.close()

 
    def exec_(self):

        super(CFL_calculator, self).exec_()
        dt=float(self.dt.text())
        min_CFL=float(self.min_CFL.text())
        warn_CFL=float(self.warn_CFL.text())
        
        return dt,min_CFL,warn_CFL