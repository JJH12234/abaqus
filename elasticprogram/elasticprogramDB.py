# -*- coding: utf-8 -*-
from abaqusConstants import *
from abaqusGui import sendCommand
from abaqusGui import *
from kernelAccess import mdb, session
import os
import re
import sys
from abaqusGui import FXWindow
from abaqusGui import AFXMessageDialog
from abaqusGui import sendCommand 
from abaqusGui import getAFXApp
thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)

def get_current_viewport():
    return session.viewports[session.currentViewportName]

def get_current_odbdp():
    return get_current_viewport().odbDisplay

def get_current_odb():
    return session.odbs[get_current_odbdp().name]

def get_current_odbdata():
    return session.odbData[get_current_odbdp().name]

###########################################################################
# Class definition
###########################################################################

class SoftwareprogramDB(AFXDataDialog):

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ID_FIRST1 = AFXDataDialog.ID_LAST
    ID_ANALYSE_TYPE_CHANGED = ID_FIRST1 + 1
    ID_CFICriterion_CHANGED = ID_FIRST1 + 2
    ID_PATH_SETTINGS_CHANGED = ID_FIRST1 + 3
    ID_EXTRAPOLATE_TYPE_CHANGED = ID_FIRST1 + 4
    ID_TABLE_2_CHANGED = ID_FIRST1 + 5
    ID_TABLE1_CHANGED = ID_FIRST1 + 6
    ID_TABLE2_CHANGED = ID_FIRST1 + 7
    ID_TINKER = ID_FIRST1 + 8
    # ID_APPLY = ID_FIRST1 + 7

    # AFXDataDialog.ID_LAST = ID_TABLE2_CHANGED + 7


    def __init__(self, form):

        # Construct the base class.
        self.form = form
        AFXDataDialog.__init__(self, form, u'非弹性分析工具'.encode('GB18030'),
            self.OK|self.APPLY|self.CANCEL, DIALOG_ACTIONS_SEPARATOR)
            
        FXMAPFUNC(self, SEL_COMMAND, self.ID_ANALYSE_TYPE_CHANGED, SoftwareprogramDB.onAnalyseTypeChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CFICriterion_CHANGED, SoftwareprogramDB.onCFICriterionChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_PATH_SETTINGS_CHANGED, SoftwareprogramDB.onPathTypeChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_EXTRAPOLATE_TYPE_CHANGED, SoftwareprogramDB.onExtrapolateTypeChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TABLE_2_CHANGED, SoftwareprogramDB.updatetable2kw)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TABLE1_CHANGED,
          SoftwareprogramDB.onAnyTableChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TABLE2_CHANGED,
                SoftwareprogramDB.onAnyTableChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TINKER,
          SoftwareprogramDB.onTinker)
        # FXMAPFUNC(self, SEL_COMMAND, self.ID_OK, SoftwareprogramDB.onOk)
        # okBtn = self.getActionButton(self.ID_CLICKED_OK)
        # okBtn.setTarget(self)
        # okBtn.setSelector(self.ID_OK)
        # okBtn.setText('OK')
        # applyBtn = self.getActionButton(self.ID_CLICKED_APPLY)
        # applyBtn.setTarget(self)
        # applyBtn.setSelector(self.ID_APPLY)
        # applyBtn.setText('Apply')
        # FXMAPFUNC(self, SEL_COMMAND, self.ID_APPLY, SoftwareprogramDB.onApply)
        self.tinkerBtn = self.appendActionButton('Tinker', self, self.ID_TINKER)
        # self.appendActionButton('Tinker', self, self.ID_TINKER)
        GroupBox_3 = FXGroupBox(p=self, text=u'设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X)
        self.ComboBox_2 = AFXComboBox(p=GroupBox_3, ncols=0, nvis=1, text=u'分析类型'.encode('GB18030'), tgt=form.analysetypeKw, sel=0)
        self.ComboBox_2.setMaxVisible(10)
        self.ComboBox_2.appendItem(text=u'非弹性应变'.encode('GB18030'))
        self.ComboBox_2.appendItem(text=u'非弹性损伤'.encode('GB18030'))
        self.ComboBox_2.appendItem(text=u'防脆断分析'.encode('GB18030'))
        form.analysetypeKw.setTarget(self)
        form.analysetypeKw.setSelector(self.ID_ANALYSE_TYPE_CHANGED)
        HFrame_8 = FXHorizontalFrame(p=GroupBox_3, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_1 = FXVerticalFrame(p=HFrame_8, opts=LAYOUT_FILL_X|LAYOUT_FILL_Y, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.GroupBox_5 = FXGroupBox(p=VFrame_1, text=u'损伤评价设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        HFrame_9 = FXHorizontalFrame(p=self.GroupBox_5, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VAligner_1 = AFXVerticalAligner(p=HFrame_9, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.ComboBox_6 = AFXComboBox(p=VAligner_1, ncols=0, nvis=1, text=u'蠕变损伤场号'.encode('GB18030'), tgt=form.CreepDamageFieldKw, sel=0)
        self.ComboBox_6.setMaxVisible(10)
        self.ComboBox_6.appendItem(text='UVARM')
        self.ComboBox_6.appendItem(text='SDV')
        self.ComboBox_7 = AFXComboBox(p=VAligner_1, ncols=0, nvis=1, text=u'疲劳损伤场号'.encode('GB18030'), tgt=form.FatigueDamageFieldKw, sel=0)
        self.ComboBox_7.setMaxVisible(10)
        self.ComboBox_7.appendItem(text='UVARM')
        self.ComboBox_7.appendItem(text='SDV')
        self.Textfield_CF = AFXTextField(p=VAligner_1, ncols=9, labelText=u'蠕变疲劳交互判据'.encode('GB18030'), tgt=form.CFICriterionKw, sel=0)
        form.CFICriterionKw.setTarget(self)
        form.CFICriterionKw.setSelector(self.ID_CFICriterion_CHANGED)
        self.VAligner_3 = AFXVerticalAligner(p=HFrame_9, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.spinner_creep = AFXSpinner(self.VAligner_3, 3, ':', form.CreepDamageFieldnumKw, 0)
        self.spinner_creep.setRange(1, 10000)
        self.spinner_creep.setIncrement(1)
        self.spinner_fatigue = AFXSpinner(self.VAligner_3, 3, ':', form.FatigueDamageFieldnumKw, 0)
        self.spinner_fatigue.setRange(1, 10000)
        self.spinner_fatigue.setIncrement(1)
        self.GroupBox_7 = FXGroupBox(p=VFrame_1, text=u'路径设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        self.HFrame_6 = FXHorizontalFrame(p=self.GroupBox_7, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.ComboBox_4 = AFXComboBox(p=self.HFrame_6, ncols=0, nvis=1, text=u'取点类型:'.encode('GB18030'), tgt=form.pathStyleKw, sel=0)
        self.ComboBox_4.setMaxVisible(10)
        self.ComboBox_4.appendItem(text='UNIFORM_SPACING')
        self.ComboBox_4.appendItem(text='PATH_POINTS')
        form.pathStyleKw.setTarget(self)
        form.pathStyleKw.setSelector(self.ID_PATH_SETTINGS_CHANGED)
        self.spinner_intervals = AFXSpinner(self.HFrame_6, 4, 'Intervals:', form.numIntervalsKw, 0)
        self.spinner_intervals.setRange(0, 10000)
        self.spinner_intervals.setIncrement(1)
        self.HFrame_7 = FXHorizontalFrame(p=self.GroupBox_7, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=self.HFrame_7, text=u'变形:'.encode('GB18030'), opts=JUSTIFY_LEFT)
        self.undeformed_button = FXRadioButton(p=self.HFrame_7, text='UNDEFORMED', tgt=form.shapeKw1, sel=29)
        self.deformed_button = FXRadioButton(p=self.HFrame_7, text='DEFORMED', tgt=form.shapeKw1, sel=30)
        VFrame_2 = FXVerticalFrame(p=HFrame_8, opts=LAYOUT_FILL_X|LAYOUT_FILL_Y, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        GroupBox_6 = FXGroupBox(p=VFrame_2, text=u'分析步与循环设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X)
        VAligner_2 = AFXVerticalAligner(p=GroupBox_6, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.spinner_precondition = AFXSpinner(VAligner_2, 4, u'不计入步数'.encode('GB18030'), form.stepIDFs1Kw, 0)
        self.spinner_precondition.setRange(0, 10000)
        self.spinner_precondition.setIncrement(1)
        self.spinner_cycle = AFXSpinner(VAligner_2, 4, u'循环节步数'.encode('GB18030'), form.stepIDFs2Kw, 0)
        self.spinner_cycle.setRange(1, 10000)
        self.spinner_cycle.setIncrement(1)
        self.spinner_superfluous = AFXSpinner(VAligner_2, 4, u'循环外步数'.encode('GB18030'), form.stepIDFs3Kw, 0)
        self.spinner_superfluous.setRange(0, 10000)
        self.spinner_superfluous.setIncrement(1)
        self.GroupBox_9 = FXGroupBox(p=VFrame_2, text=u'防脆断设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        self.ComboBox_8 = AFXComboBox(p=self.GroupBox_9, ncols=0, nvis=1, text=u'应力类型'.encode('GB18030'), tgt=form.BrittleStressKw, sel=0)
        self.ComboBox_8.setMaxVisible(10)
        self.ComboBox_8.appendItem(text='Mises')
        self.ComboBox_8.appendItem(text='Max. Principal')
        self.ComboBox_8.appendItem(text='Mid. Principal')
        self.ComboBox_8.appendItem(text='Min. Principal')
        self.ComboBox_8.appendItem(text='Tresca')
        self.ComboBox_8.appendItem(text='Pressure')
        self.ComboBox_8.appendItem(text='S11')
        self.ComboBox_8.appendItem(text='S22')
        self.ComboBox_8.appendItem(text='S33')
        self.ComboBox_8.appendItem(text='S12')
        self.ComboBox_8.appendItem(text='S13')
        self.ComboBox_8.appendItem(text='S23')
        GroupBox_8 = FXGroupBox(p=GroupBox_3, text=u'外推设置'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_5 = FXHorizontalFrame(p=GroupBox_8, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.ComboBox_3 = AFXComboBox(p=HFrame_5, ncols=0, nvis=1, text=u'外推类型'.encode('GB18030'), tgt=form.extrapolateTypeKw, sel=0)
        self.ComboBox_3.setMaxVisible(10)
        self.ComboBox_3.appendItem(text='Direct')
        self.ComboBox_3.appendItem(text='Add')
        self.ComboBox_3.appendItem(text='None')
        form.extrapolateTypeKw.setTarget(self)
        form.extrapolateTypeKw.setSelector(self.ID_EXTRAPOLATE_TYPE_CHANGED)
        self.spinner_extrao = AFXSpinner(HFrame_5, 6, u'外推周次'.encode('GB18030'), form.extrapolateTimesKw, 0)
        self.spinner_extrao.setRange(1, 100000)
        self.spinner_extrao.setIncrement(1)
        self.Textfield_step = AFXTextField(p=GroupBox_8, ncols=50, labelText=u'外推补充分析步名(逗号分隔):'.encode('GB18030'), tgt=form.addTypeStepNamesKw, sel=0)
        GroupBox_1 = FXGroupBox(p=self, text=u'鼠标选点'.encode('GB18030'), opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_1 = FXHorizontalFrame(p=GroupBox_1, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        pickHf = FXHorizontalFrame(p=HFrame_1, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        pickHf.setSelector(99)
        label = FXLabel(p=pickHf, text=u'选取焊缝节点 (None)'.encode('GB18030'), ic=None, opts=LAYOUT_CENTER_Y|JUSTIFY_LEFT)
        self.pickHandler_points_weld = SoftwareprogramDBPickHandler(form, form.picks1Kw, u'选取焊缝节点'.encode('GB18030'), NODES, MANY, label)
        icon = afxGetIcon('select', AFX_ICON_SMALL )
        self.button_points_weld = FXButton(p=pickHf, text='\tPick Items in Viewport', ic=icon, tgt=self.pickHandler_points_weld, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=1, pb=1)
        pickHf = FXHorizontalFrame(p=HFrame_1, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        pickHf.setSelector(99)
        label = FXLabel(p=pickHf, text=u'选取通常节点 (None)'.encode('GB18030'), ic=None, opts=LAYOUT_CENTER_Y|JUSTIFY_LEFT)
        self.pickHandler_points = SoftwareprogramDBPickHandler(form, form.picks2Kw, 'Pick an entity', NODES, MANY, label)
        icon = afxGetIcon('select', AFX_ICON_SMALL )
        self.button_points = FXButton(p=pickHf, text='\tPick Items in Viewport', ic=icon, tgt=self.pickHandler_points, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=1, pb=1)
        vf = FXVerticalFrame(GroupBox_1, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # vf.setSelector(99)
        try:
            odbdata =  get_current_odbdata()
        except AttributeError:
            mw=getAFXApp().getMainWindow()
            mw.writeToMessageArea(u'请先打开odb文件'.encode('GB18030'))
            raise AttributeError(u'请先打开odb文件'.encode('GB18030'))
        instance_list = odbdata.instances.keys()

        # Create the main frame to hold the ComboBox and Table
        frame_0 = FXVerticalFrame(p=self, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
        frame_1 = FXHorizontalFrame(p=frame_0, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)

        # Step 2: Create the table for Node Labels and isWeld
        table_group = FXGroupBox(p=frame_1, text=u"结点".encode('GB18030'), opts=FRAME_GROOVE | LAYOUT_FILL_X)

        # Create the table with 3 columns (Instance, Node Labels, isWeld)
        self.table_points = AFXTable(table_group, 6, 3, 6, 3, form.tabledata1Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        form.tabledata1Kw.setTarget(self)
        form.tabledata1Kw.setSelector(self.ID_TABLE1_CHANGED)
        # Set the popup options for the table
        self.table_points.setPopupOptions(AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE |
                                        AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS |
                                        AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        self.table_points.setLeadingRows(1)
        self.table_points.setLeadingRowLabels(u"实例名称\t结点标签 (e.g. 5,10,62:04)\t焊缝".encode('GB18030'))
        # Set column properties for Node Labels (Text input)
        self.table_points.setColumnWidth(1, 200)  # Node Labels (Text input)
        self.table_points.setColumnType(1, AFXTable.TEXT)

        # Set column properties for isWeld (Checkbox)
        self.table_points.setColumnWidth(2, 100)   # isWeld (Checkbox)
        self.table_points.setColumnType(2, AFXTable.BOOL)
        self.table_points.setColumnJustify(2, AFXTable.CENTER)
        form.tabledata2Kw.setTarget(self)
        form.tabledata2Kw.setSelector(self.ID_TABLE2_CHANGED)
        # Get the instance list dynamically from odbdata
        odbdata = get_current_odbdata()
        odbdata = session.odbData[session.viewports[session.currentViewportName].odbDisplay.name]
        instance_list = odbdata.instances.keys()

        # Create the list for ComboBox from the instance_list
        listId = self.table_points.addList()
        for instance in instance_list:
            self.table_points.appendListItem(listId, instance)

        # Set the first column (Instance) to be a combo box with the list
        self.table_points.setColumnWidth(0, 200)
        self.table_points.setColumnType(0, AFXTable.LIST)
        self.table_points.setColumnListId(0, listId)
        # Show the grid lines for the table
        self.table_points.showHorizontalGrid(True)
        self.table_points.showVerticalGrid(True)

        frame_2 = FXHorizontalFrame(p=frame_0, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
        self.GroupBox_2 = FXGroupBox(p=frame_2, text=u'路径'.encode('GB18030'), opts=FRAME_GROOVE | LAYOUT_FILL_X)
        HFrame_2 = FXHorizontalFrame(p=self.GroupBox_2, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0, pl=0, pr=0, pt=0, pb=0)
        # pickHf = FXHorizontalFrame(p=HFrame_2, opts=0, x=0, y=0, w=0, h=0, pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # pickHf.setSelector(99)
        vf_paths = FXVerticalFrame(self.GroupBox_2, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X, 0, 0, 0, 0, 0, 0, 0, 0)

        # Create the table with 4 columns: Path Name, Instance Name, Node Labels, isWeld
        self.table_paths = AFXTable(vf_paths, 6, 4, 6, 4, form.tabledata2Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        form.tabledata2Kw.setTarget(self)
        form.tabledata2Kw.setSelector(self.ID_TABLE_2_CHANGED)
        # Set the popup options for the table
        self.table_paths.setPopupOptions(AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE |
                                        AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS |
                                        AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)

        # Set leading rows and columns
        self.table_paths.setLeadingRows(1)
        self.table_paths.setLeadingRowLabels(u'路径名称\t实例名称\t结点标签 (e.g. 5,10,6:20:4)\t焊缝'.encode('GB18030'))

        # Set column widths and types
        self.table_paths.setColumnWidth(0, 100)  # Path Name
        self.table_paths.setColumnType(0, AFXTable.TEXT)  # ComboBox for Path Name

        # self.table_paths.setColumnWidth(1, 100)  # Instance Name
        # self.table_paths.setColumnType(1, AFXTable.TEXT)  # ComboBox for Instance Name

        self.table_paths.setColumnWidth(2, 200)  # Node Labels
        self.table_paths.setColumnType(2, AFXTable.TEXT)  # Text input for Node Labels

        self.table_paths.setColumnWidth(3, 80)   # isWeld
        self.table_paths.setColumnType(3, AFXTable.BOOL)  # Checkbox for isWeld
        self.table_paths.setColumnJustify(3, AFXTable.CENTER)

        # Set the stretchable column to the last column
        self.table_paths.setStretchableColumn(self.table_paths.getNumColumns() - 1)

        # Show grid lines
        self.table_paths.showHorizontalGrid(True)
        self.table_paths.showVerticalGrid(True)
        listId_pts = self.table_points.addList()
        for inst in instance_list:
            self.table_points.appendListItem(listId_pts, inst)

        self.table_points.setColumnType(0, AFXTable.LIST)
        self.table_points.setColumnListId(0, listId_pts)
        

        # -----------------------  Paths 表：新增下拉框 ------------------------

        # 1) 创建下拉框列表并填充
        listId_paths = self.table_paths.addList()
        for inst in instance_list:
            self.table_paths.appendListItem(listId_paths, inst)

        # 2) 把第 1 行 (索引 1) 改为下拉框并绑定
        self.table_paths.setColumnType(1, AFXTable.LIST)
        self.table_paths.setColumnListId(1, listId_paths)
        self.table_paths.setColumnWidth(1, 200)
        # Populate the table with data
        data = []
        for pathname in session.paths.keys():
            p = session.paths[pathname]
            if p.type == NODE_LIST:  # If NODE_LIST is a constant in Abaqus
                for row in p.expression:
                    data.append((p.name, row[0], str(row[1]).replace('(', '').replace(')', ''), False))

        # Populate the combo boxes for both columns (Path Name and Instance Name)
        for row_index, row in enumerate(data):  # Use row_index to represent the row number
            path_name, instance_name, node_labels, is_weld = row
            # Append items to the "Path Name" combo box
            self.table_paths.setItemValue(row_index+1, 0, path_name)
            # Append items to the "Instance Name" combo box
            self.table_paths.setItemValue(row_index+1, 1, instance_name)
            self.table_paths.setItemText(row_index+1, 2, node_labels)

        # Update the UI
        self.getOwner().recalc()
        self.getOwner().repaint()

        # Update widgets based on analysis type
        self.updateWidgetsByAnalyseType()

    # ==== 4) 事件回调（加到类的其他回调之后即可） ====
    def get_current_viewport():
        return session.viewports[session.currentViewportName]

    def get_current_odbdp():
        return get_current_viewport().odbDisplay

    def get_current_odb():
        return session.odbs[get_current_odbdp().name]

    def get_current_odbdata():
        return session.odbData[get_current_odbdp().name]
    # ---------------------------------------------
    def onTinker(self, sender, sel, ptr):
        """通过 sendCommand 运行 brittle_assess.py，并把异常打印到 CLI"""
        script = r"D:/SIMULIA/EstProducts/2023/win_b64/code/python2.7/lib/abaqus_plugins/elasticprogram/brittle_assess.py"
        cmd = (
            "import sys, runpy, traceback\n"
            "sys.modules.pop('brittle_assess', None)\n"
            "print '>>> running brittle_assess.py'\n"
            "try:\n"
            "    runpy.run_path(r'%s', run_name='__main__')\n"
            "except Exception:\n"
            "    traceback.print_exc()\n"
        ) % script.replace('\\', '\\\\')      # 双反斜杠防止转义

        sendCommand(cmd)                      
        getAFXApp().getAFXMainWindow().writeToMessageArea(
            "【Tinker】脚本命令已发松，请在 Kernel Command 视图查看运行日志\n"
        )
        return 1

    # ---------------------------------------------

    # ====================  dialog  ====================
    # ------------------------------------------------------------
    def _wipe_table_and_kw(self, table, tableKw):
        """AFXTable Keyword修改"""
        rows, cols = table.getNumRows(), table.getNumColumns()
        for r in range(rows):
            for c in range(cols):
                tableKw.setValue(r, c, '')        

        if rows > 1:
            table.deleteRows(1, rows - 1)

        table.update()
    def onAnyTableChanged(self, sender, sel, ptr):
        self.processTables()
        return 1


    # ===== 转换编码=====
    def _is_int(s):

        return s.lstrip('+-').isdigit()



    def _to_ascii(msg):
        if isinstance(msg, unicode):
            return msg.encode('ascii', 'replace')
        try:
            return msg.decode('utf-8').encode('ascii', 'replace')
        except Exception:
            return unicode(msg, errors='replace').encode('ascii', 'replace')


    def processTables(self):
        def _to_ascii(msg):
            if isinstance(msg, unicode):
                return msg.encode('ascii', 'replace')
            try:
                return msg.decode('utf-8').encode('ascii', 'replace')
            except Exception:
                return unicode(msg, errors='replace').encode('ascii', 'replace')
        wr = lambda t: getAFXApp().getAFXMainWindow().writeToMessageArea(_to_ascii(t))

        try:
            # ------------------------------------------------ Points  ------------------------------------------------
            pts_rows = []
            for r in xrange(1, self.table_points.getNumRows()):
                inst = self.table_points.getItemText(r, 0).strip()
                node = self.table_points.getItemText(r, 1).strip()
                weld = 1 if str(self.table_points.getItemValue(r, 2)).lower() in ('1', 'true') else 0
                if inst and node:
                    pts_rows.append((inst, node, weld))
                elif weld:  # ֻ是否 weld 判断
                    wr(u"[警告] 路径表 行{} 跳过: \n".format(unicode(r)).encode('GB18030'))


            kw1 = self.form.tabledata1Kw
            # kw1.setRowSize(len(pts_rows))
            # kw1.setColumnSize(3)

            for i, (inst, node, weld) in enumerate(pts_rows):
                kw1.setValue(i, 0, inst)          # STRING
                kw1.setValue(i, 1, node)          # STRING
                kw1.setValue(i, 2, 'True' if weld else 'False')

            # ------------------------------------------------ Paths �� -------------------------------------------------
            pth_rows = []
            for r in xrange(1, self.table_paths.getNumRows()):
                path = self.table_paths.getItemText(r, 0).strip()
                inst = self.table_paths.getItemText(r, 1).strip()
                node = self.table_paths.getItemText(r, 2).strip()
                weld = 1 if str(self.table_paths.getItemValue(r, 3)).lower() in ('1', 'true') else 0
                if path and inst and node:
                    pth_rows.append((path, inst, node, weld))
                elif weld:
                    wr(u"[警告] 路径 行{} 已跳过: \n".format(unicode(r)).encode('GB18030'))

            kw2 = self.form.tabledata2Kw
            # kw2.setRowSize(len(pth_rows))
            # kw2.setColumnSize(4)

            for i, (path, inst, node, weld) in enumerate(pth_rows):
                kw2.setValue(i, 0, path)
                kw2.setValue(i, 1, inst)
                kw2.setValue(i, 2, node)
                kw2.setValue(i, 3, 'True' if weld else 'False')

            wr(u"完成: 总路径={} 行, 当前路径={} 行\n".format(len(pts_rows), len(pth_rows)).encode('GB18030'))

        except Exception as e:
            wr(u"表格处理错误:\n{}\n".format(unicode(e)).encode('GB18030'))
            AFXMessageDialog("Table-processing error  see Message Area.").showModal()


    def show(self):

        AFXDataDialog.show(self)               
        self.updateWidgetsByAnalyseType()

        data = []
        for pathname in session.paths.keys():
            p = session.paths[pathname]
            if p.type == NODE_LIST:  # If NODE_LIST is a constant in Abaqus
                for row in p.expression:
                    data.append((p.name, row[0], str(row[1]).replace('(', '').replace(')', ''), False))

        # Populate the combo boxes for both columns (Path Name and Instance Name)
        for row_index, row in enumerate(data):  # Use row_index to represent the row number
            path_name, instance_name, node_labels, is_weld = row
            # Append items to the "Path Name" combo box
            self.table_paths.setItemValue(row_index+1, 0, path_name)
            # Append items to the "Instance Name" combo box
            self.table_paths.setItemValue(row_index+1, 1, instance_name)
            self.table_paths.setItemText(row_index+1, 2, node_labels)
        # 强制刷新
        self.getOwner().recalc()
        self.getOwner().repaint()
        

    def onAnalyseTypeChanged(self, sender, sel, ptr):
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea("--- onAnalyseTypeChanged\n")
        
        # 分析类型改变
        if sender is not None:
            msg = u"触发事件控件: {}".format(unicode(sender.getName()))
            main_window.writeToMessageArea(msg.encode('GB18030') + "\n")
        else:
            main_window.writeToMessageArea(u"触发事件控件: 未知".encode('GB18030') + "\n")
        
        current_index = self.ComboBox_2.getCurrentItem()
        current_text = self.ComboBox_2.getItemText(current_index)
        main_window.writeToMessageArea(u"当前选项文本: {}\n".format(current_text.decode('GB18030')).encode('GB18030'))
        
        # 更新控件
        self.updateWidgetsByAnalyseType()
        return 1

    def updateWidgetsByAnalyseType(self):
        current_index = self.ComboBox_2.getCurrentItem()
        current_text = self.ComboBox_2.getItemText(current_index)
        
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea("--- updateWidgetsByAnalyseType  ---\n")
        main_window.writeToMessageArea(u"当前文本是 :{}\n".format(current_text.decode('GB18030')).encode('GB18030'))
        all_widgets = [
            self.ComboBox_2, self.ComboBox_8, self.GroupBox_9, self.ComboBox_6,
            self.ComboBox_7, self.Textfield_CF, self.VAligner_3, self.spinner_creep,
            self.spinner_fatigue, self.GroupBox_7, self.ComboBox_4, self.spinner_intervals,
            self.HFrame_7, self.undeformed_button, self.deformed_button, self.GroupBox_2,
            self.table_paths, self.spinner_precondition, self.spinner_cycle, self.spinner_superfluous,
            self.ComboBox_3, self.spinner_extrao, self.Textfield_step, self.button_points_weld,
            self.button_points, self.table_points]
        for widget in all_widgets:
            widget.enable()
        # 非弹性应变
        if current_text == u'非弹性应变'.encode('GB18030'):
            main_window.writeToMessageArea(u" 防脆断分析...\n".encode('GB18030'))
            self.ComboBox_8.disable()
            self.GroupBox_9.disable()
            self.ComboBox_6.disable()
            self.ComboBox_7.disable()
            self.Textfield_CF.disable()
            self.VAligner_3.disable()
            self.spinner_creep.disable()
            self.spinner_fatigue.disable()
            self.tinkerBtn.disable()
        elif current_text == u'非弹性损伤'.encode('GB18030'):
            main_window.writeToMessageArea(" 3, 5, 9 \n")
            self.GroupBox_7.disable()
            self.ComboBox_4.disable()
            self.spinner_intervals.disable()
            self.HFrame_7.disable()
            self.undeformed_button.disable()
            self.deformed_button.disable()
            self.ComboBox_8.disable()
            self.GroupBox_9.disable()
            self.GroupBox_2.disable()
            self.table_paths.disable()
            self.tinkerBtn.disable()
        elif current_text == u'防脆断分析'.encode('GB18030'):
            main_window.writeToMessageArea(" 2, 4, 6, 7, 8 \n")
            self.ComboBox_6.disable()
            self.ComboBox_7.disable()
            self.Textfield_CF.disable()
            self.VAligner_3.disable()
            self.spinner_creep.disable()
            self.spinner_fatigue.disable()
            self.spinner_precondition.disable()
            self.spinner_cycle.disable()
            self.spinner_superfluous.disable()
            self.ComboBox_3.disable()
            self.spinner_extrao.disable()
            self.Textfield_step.disable()
            self.button_points_weld.disable()
            self.button_points.disable()
            self.table_points.disable()
            self.tinkerBtn.enable()

        self.getOwner().recalc()
        self.getOwner().repaint()
        
    def onCFICriterionChanged(self, sender, sel, ptr):

        main_window = getAFXApp().getAFXMainWindow()
        input_value = self.Textfield_CF.getText()
        main_window.writeToMessageArea(u"输入值是 {}\n".format(unicode(input_value)).encode('GB18030'))
        # 'float,float' 
        if not self.isValidCFICriterion(input_value):
            self.showErrorMessage(" C-F Interaction Criterion ( 0.3, 0.3)!")
        return 1
    def isValidCFICriterion(self, input_value):
        """
        'float,float' 
        """
        pattern = r"^[-+]?\d*\.\d+,\s*[-+]?\d*\.\d+$"  #  float,float 
        return bool(re.match(pattern, input_value))
    def showErrorMessage(self, title, message):
        """
        输出信息到终端
        """
        main_window = getAFXApp().getAFXMainWindow()
        showAFXErrorDialog(main_window, 'An invalid value was supplied.')

    def onPathTypeChanged(self, sender, sel, ptr):
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea("--- onPathTypeChanged ---\n")
        
        # 类型选择改变了
        if sender is not None:
            main_window.writeToMessageArea("Path type changed: %s\n" % sender.getName())
        else:
            main_window.writeToMessageArea(
            u"错误: 未知控件\n".encode('GB18030')
        )
        
        current_index = self.ComboBox_4.getCurrentItem()
        current_text = self.ComboBox_4.getItemText(current_index)
        main_window.writeToMessageArea(u"现在的类型是: {}\n".format(current_text.decode('GB18030')).encode('GB18030'))    
        # 更新控件状态
        self.updateWidgetsByPathType()
        return 1
    def updateWidgetsByPathType(self):
        current_index = self.ComboBox_4.getCurrentItem()
        current_text = self.ComboBox_4.getItemText(current_index)
        
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea("--- updateWidgetsByPathType---\n")
        main_window.writeToMessageArea(u"现在的类型是: {}\n".format(current_text.decode('GB18030')).encode('GB18030'))
        self.spinner_intervals.enable()
        # 插值方式改变
        if current_text == 'PATH_POINTS':
            main_window.writeToMessageArea(" intervals...\n")
            self.spinner_intervals.disable()
        # 强制刷新
        self.getOwner().recalc()
        self.getOwner().repaint()

    def onExtrapolateTypeChanged(self, sender, sel, ptr):
            main_window = getAFXApp().getAFXMainWindow()
            main_window.writeToMessageArea("--- onExtrapolateTypeChanged  ---\n")
            
            # 外推类型改变
            if sender is not None:
                main_window.writeToMessageArea(
            u"外推类型改变: {}\n".format(unicode(sender.getName())).encode('GB18030')
        )
            else:
                main_window.writeToMessageArea(
            u"出现错误: 未知控件\n".encode('GB18030')
        )
            
            # 获取当前类型
            current_index = self.ComboBox_3.getCurrentItem()
            current_text = self.ComboBox_3.getItemText(current_index)
            main_window.writeToMessageArea(
        u"当前类型是: {}\n".format(current_text.decode('GB18030')).encode('GB18030')
    )       
            self.updateWidgetsByExtrapolateType()
            return 1
    def updateWidgetsByExtrapolateType(self):
        current_index = self.ComboBox_3.getCurrentItem()
        current_text = self.ComboBox_3.getItemText(current_index)
        
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea(
        "--- updateWidgetsByExtrapolateType ---\n"
    )
        main_window.writeToMessageArea(
        u"当前外推类型: {}\n".format(current_text.decode('GB18030')).encode('GB18030')
    )
        self.spinner_extrao.enable()
        self.Textfield_step.enable()
        if current_text == 'None':
            main_window.writeToMessageArea(
            u"选择 None → 关闭所有外推控件\n".encode('GB18030')
        )
            self.spinner_extrao.disable()
            self.Textfield_step.disable()
        if current_text == 'Direct':
            self.Textfield_step.disable()
        if current_text == 'Add':
            self.spinner_extrao.enable()
            self.Textfield_step.enable()
        self.getOwner().recalc()
        self.getOwner().repaint()
    
    def updatetable2kw(self, sender, sel, ptr):
        main_window = getAFXApp().getAFXMainWindow()
        main_window.writeToMessageArea(
        u"--- updatetable2kw 开始检查 ---\n".encode('GB18030')
    )

        table = self.table_paths
        table_kw = self.form.tabledata2Kw
        rows = table.getNumRows()
        cols = table.getNumColumns()

        type_map = {
            AFXTABLE_TYPE_STRING: 'STRING',
            AFXTABLE_TYPE_INT: 'INT',
            AFXTABLE_TYPE_FLOAT: 'FLOAT',
            AFXTABLE_TYPE_BOOL: 'BOOL',
        }

        for i in range(1, rows):  
            main_window.writeToMessageArea(
            u"第 {} 行:\n".format(i).encode('GB18030')
        )
            for j in range(cols):
                val = table.getItemText(i, j)
                col_type = table_kw.getColumnType(j)
                typename = type_map.get(col_type, 'UNKNOWN')
                main_window.writeToMessageArea(
                u"  列 {}: 值=[{}], 类型={}\n".format(j, val, typename).encode('GB18030')
            )
        return 1

###########################################################################
# Class definition
###########################################################################

class SoftwareprogramDBPickHandler(AFXProcedure):

        count = 0

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        def __init__(self, form, keyword, prompt, entitiesToPick, numberToPick, label):

                self.form = form
                self.keyword = keyword
                self.prompt = prompt
                self.entitiesToPick = entitiesToPick # Enum value
                self.numberToPick = numberToPick # Enum value
                self.label = label
                self.labelText = label.getText()

                AFXProcedure.__init__(self, form.getOwner())

                SoftwareprogramDBPickHandler.count += 1
                self.setModeName('SoftwareprogramDBPickHandler%d' % (SoftwareprogramDBPickHandler.count) )

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        def getFirstStep(self):

                return  AFXPickStep(self, self.keyword, self.prompt, 
                    self.entitiesToPick, self.numberToPick, sequenceStyle=TUPLE)

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        def getNextStep(self, previousStep):

                self.label.setText( self.labelText.replace('None', 'Picked') )
                return None

        def deactivate(self):

            AFXProcedure.deactivate(self)
            if  self.numberToPick == ONE and self.keyword.getValue() and self.keyword.getValue()[0]!='<':
                sendCommand(self.keyword.getSetupCommands() + '\nhighlight(%s)' % self.keyword.getValue() )

