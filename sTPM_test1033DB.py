# -*- coding: utf-8 -*-
from abaqusConstants import *
from abaqusGui import *
from kernelAccess import mdb, session
import os
import json
import xlrd
from collections import OrderedDict, Counter, defaultdict
import re

thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)


# 定义工作路径变量（可以手动修改）
WORK_DIR = r"E:\桌面\project\Abaqus".decode('utf-8')  # 使用 decode 处理中文路径
os.chdir(WORK_DIR)  # 设置工作目录

###########################################################################
# Class definition
###########################################################################

class STPM_test1033DB(AFXDataDialog):
    [
        ID_CLICKED_import1,
        ID_MODEL_MATERIAL_COMBO_CHANGED,
        ID_CLICKED_NEW,
        ID_CLICKED,
        ID_COMBO_CHANGED_SHEET,
        ID_COMBO_CHANGED_TREE,
        ID_FILE_CHANGED,
        ID_FILE_NAME_CHANGED,
        ID_FILE_NAME_CHANGED_1,
        ID_CLICKED_LIST,
        ID_TEXT_CHANGED,
        ID_CYCLE_LIST_CHANGED,
        ID_TAB_CHANGED
    ] = range(AFXForm.ID_LAST+1, AFXForm.ID_LAST + 14)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form):

        # Construct the base class.

        AFXDataDialog.__init__(self, form, 'STPM_GB_Before',)
                               # self.OK | self.CANCEL, )
        self.materials_data = {}
        self.form = form
        self.temp_json = {}  # 添加临时JSON存储
        # okBtn = self.getActionButton(self.ID_CLICKED_OK)
        # okBtn.setText('OK')

        TabBook_1 = FXTabBook(p=self, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL | LAYOUT_FILL_X | LAYOUT_FILL_Y,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)

        # try:
        #     excel_path = u"D:/桌面/工程开发项目/ParaModelingData.xls"
        #     workbook = xlrd.open_workbook(excel_path)
        #     sheet = workbook.sheet_by_index(0)
        #
        #     self.excel_data = []
        #     for row in range(sheet.nrows):
        #         row_data = []
        #         for col in range(sheet.ncols):
        #             value = sheet.cell_value(row, col)
        #             row_data.append(value)
        #         self.excel_data.append(row_data)
        # except Exception as e:
        #     mw = getAFXApp().getAFXMainWindow()
        #     mw.writeToMessageArea("Error reading Excel file:" + str(e))
        #     self.excel_data = None

        tabItem = FXTabItem(p=TabBook_1, text='Geometry', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_22 = FXVerticalFrame(p=TabBook_1,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = XslFileHandler(form, self, 'fileName', '(*.xls)')
        fileTextHf = FXHorizontalFrame(p=TabItem_22, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=60, labelText='File name:', tgt=form.fileNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        self.ComboBox_14 = AFXComboBox(p=TabItem_22, ncols=0, nvis=1, text='Model:', tgt=form.keyword95Kw, sel=0)
        self.ComboBox_14.setMaxVisible(10)
        self.ComboBox_14.appendItem(text='111')
        self.ComboBox_14.appendItem(text='222')
        self.ComboBox_14.appendItem(text='...')
        self.ComboBox_14.setTarget(self)
        self.ComboBox_14.setSelector(self.ID_COMBO_CHANGED_SHEET)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED_SHEET, self.onSheetChanged)

        vf = FXVerticalFrame(TabItem_22, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        self.table = AFXTable(vf, 20, 5, 200, 6, form.keyword97Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        self.table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        self.table.setLeadingRows(1)
        self.table.setLeadingColumns(1)
        self.table.setColumnWidth(1, 200)
        self.table.setColumnType(1, AFXTable.TEXT)
        self.table.setColumnWidth(2, 200)
        self.table.setColumnType(2, AFXTable.TEXT)
        self.table.setColumnWidth(3, 200)
        self.table.setColumnType(3, AFXTable.TEXT)
        self.table.setColumnWidth(4, 200)
        self.table.setColumnType(4, AFXTable.TEXT)
        self.table.setColumnWidth(5, 200)
        self.table.setColumnType(5, AFXTable.TEXT)
        self.table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')
        self.table.setStretchableColumn(self.table.getNumColumns() - 1)
        self.table.showHorizontalGrid(True)
        self.table.showVerticalGrid(True)

        # if self.excel_data:
        #     try:
        #
        #         for i in range(0, len(self.excel_data)):
        #             for j in range(0, len(self.excel_data[i])):
        #                 table.setItemText(i + 1, j + 1, str(self.excel_data[i][j]))
        #     except Exception as e:
        #         print("Error filling table:", str(e))

        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        button = FXButton(p=TabItem_22, text='para change', opts=BUTTON_NORMAL | JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Materials', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_16 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = TesttreeDBFileHandler(form, self,'JSONName', 'All files (*)')
        fileTextHf = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=50, labelText='JSON:', tgt=form.JSONNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        HFrame_26 = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_24 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.ComboBox_12 = AFXComboBox(p=VFrame_24, ncols=0, nvis=1, text='Material', tgt=form.keyword88Kw, sel=0)
        self.ComboBox_12.setMaxVisible(10)
        self.ComboBox_12.appendItem(text='2.25Cr1Mo')
        self.ComboBox_12.appendItem(text='SA336')
        self.ComboBox_12.appendItem(text='Na')
        self.ComboBox_12.setTarget(self)
        self.ComboBox_12.setSelector(self.ID_COMBO_CHANGED_TREE)
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED, STPM_test1033DB.onItemDoubleClicked)

        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())


        self.listVf = FXVerticalFrame(p=VFrame_24, opts=FRAME_SUNKEN|FRAME_THICK, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.listVf.setSelector(99)
        self.tree = FXTreeList(self.listVf, 20, tgt=self, sel=self.ID_CLICKED,
                               opts=LAYOUT_FILL_X | LAYOUT_FILL_Y |
                                    TREELIST_SHOWS_BOXES | TREELIST_ROOT_BOXES |
                                    TREELIST_SHOWS_LINES | LAYOUT_FIX_WIDTH |TREELIST_CHECK_BOXES|HSCROLLER_ALWAYS|TREELIST_PROPAGATE_CHECKS,
                               x=0, y=0, w=200, h=0)
        self.updateTree(selected_material)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED_TREE, STPM_test1033DB.onMaterialChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.onTreeCheckChanged)  # 新增事件绑定
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.get_checked_data)
        # List_1 = AFXList(p=listVf, nvis=20, tgt=form.keyword47Kw, sel=0, opts=HSCROLLING_OFF|LIST_SINGLESELECT)
        # List_1.appendItem(text='  -Prime property1')
        # List_1.appendItem(text='    -Secondary propetry1')
        # List_1.appendItem(text='       Data1(ASME)')
        # List_1.appendItem(text='       Data2(Test)')
        # List_1.appendItem(text='    +Secondary propetry2')
        # List_1.appendItem(text='\xa1\xa1')
        # List_1.appendItem(text='  +Prime property2')
        # List_1.appendItem(text='  +Prime property3')
        if isinstance(HFrame_26, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_14 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=100, pb=0)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        HFrame_27 = FXHorizontalFrame(p=VFrame_14, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_25 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_25, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_25, text='>>', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_27, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_16 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        HFrame_38 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        m = self.get_current_model()
        form.keyword99Kw.setValue(m.name)
        AFXTextField(p=HFrame_38, ncols=12, labelText="ModelName:", tgt=form.keyword99Kw, sel=0)
        self.ComboBox_15 = AFXComboBox(p=HFrame_38, ncols=0, nvis=1, text='Material :', tgt=form.keyword98Kw, sel=self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        self.ComboBox_15.setTarget(self)
        self.ComboBox_15.setSelector(self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_MODEL_MATERIAL_COMBO_CHANGED, STPM_test1033DB.model_meterial_changed)
        self.ComboBox_15.setMaxVisible(10)
        for material in m.materials.keys():
            self.ComboBox_15.appendItem(text=material)
        self.ComboBox_15.appendItem(text='New')
        self.newMaterialText = AFXTextField(p=HFrame_38, ncols=12, labelText='New:', 
            tgt=form.keyword100Kw, sel=0, opts=0)
        self.newMaterialText.hide()
        # AFXTextField(p=HFrame_38, ncols=12, labelText='New:', tgt=form.keyword100Kw, sel=0)
        HFrame_31 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VAligner_9 = AFXVerticalAligner(p=HFrame_31, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        spinner1 = AFXSpinner(VAligner_9, 6, 'UVARM vars', form.keyword61Kw, 0)
        spinner1.setRange(1, 10)
        spinner1.setIncrement(1)
        spinner2 = AFXSpinner(VAligner_9, 6, 'SDV vars', form.keyword62Kw, 0)
        spinner2.setRange(1, 10)
        spinner2.setIncrement(1)
        import1 = FXButton(p=VFrame_16, text='import', ic=None, tgt=self,
                                     sel=self.ID_CLICKED_import1,
                                     opts=BUTTON_NORMAL |LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        #l = FXLabel(p=VFrame_16, text='Button: Run', opts=JUSTIFY_LEFT)
        # 为import1按钮添加事件处理
        import1.setTarget(self)
        import1.setSelector(self.ID_CLICKED_import1)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_import1, STPM_test1033DB.onImport1Clicked)
        tabItem = FXTabItem(p=TabBook_1, text='Amplitudes', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_6 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_5 = FXGroupBox(p=TabItem_6, text='Data Format', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        HFrame_8 = FXHorizontalFrame(p=GroupBox_5, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        GroupBox_2 = FXGroupBox(p=HFrame_8, text='Separated XY*', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        HFrame_6 = FXHorizontalFrame(p=GroupBox_2, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_6, text='Separated', tgt=form.DataXYTypeKw1, sel=53)
        FXRadioButton(p=HFrame_6, text='Unified X', tgt=form.DataXYTypeKw1, sel=54)
        GroupBox_3 = FXGroupBox(p=HFrame_8, text='Based On...', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        HFrame_7 = FXHorizontalFrame(p=GroupBox_3, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_7, text='Step Time', tgt=form.DataTimeTypeKw1, sel=55)
        if isinstance(HFrame_7, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        FXRadioButton(p=HFrame_7, text='Time(h)', tgt=form.TimeUnitKw1, sel=56)
        FXRadioButton(p=HFrame_7, text='Time(s)', tgt=form.TimeUnitKw1, sel=57)
        fileHandler = InputFileHandler(form, self, 'InputDataName', '(*.xls)')
        fileTextHf = FXHorizontalFrame(p=TabItem_6, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=70, labelText='File name:', tgt=form.InputDataNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        if isinstance(TabItem_6, FXHorizontalFrame):
            FXVerticalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        run2 = FXButton(p=TabItem_6, text='run2', ic=None, tgt=fileHandler,
                        sel=AFXMode.ID_ACTIVATE + 1,
                        opts=BUTTON_NORMAL, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # l = FXLabel(p=TabItem_6, text='Button: Run', opts=JUSTIFY_LEFT)
        l.setFont(getAFXFont(FONT_BOLD))
        HFrame_10 = FXHorizontalFrame(p=TabItem_6, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        GroupBox_7 = FXGroupBox(p=HFrame_10, text='*Separated Example', opts=FRAME_GROOVE)
        HFrame_9 = FXHorizontalFrame(p=GroupBox_7, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        VFrame_1 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_1, text='Time1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_1, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_1, text='1', opts=JUSTIFY_LEFT)
        VFrame_2 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_2, text='Amp1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_2, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_2, text='100', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_9, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_3 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_3, text='Time2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='0.5', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='1', opts=JUSTIFY_LEFT)
        VFrame_4 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_4, text='Amp2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='100', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='500', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_10, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        GroupBox_8 = FXGroupBox(p=HFrame_10, text='*Unified Example', opts=FRAME_GROOVE)
        HFrame_11 = FXHorizontalFrame(p=GroupBox_8, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        VFrame_7 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_7, text='Time', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='0.5', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='1', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_11, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_6 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_6, text='Amp1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='50', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='100', opts=JUSTIFY_LEFT)
        VFrame_5 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_5, text='Amp2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='100', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='500', opts=JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Cycle', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_5 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        HFrame_32 = FXHorizontalFrame(p=TabItem_5, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        listVf = FXVerticalFrame(p=HFrame_32, opts=FRAME_SUNKEN | FRAME_THICK, x=0, y=0, w=0, h=0,
                                 pl=0, pr=0, pt=0, pb=0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        listVf.setSelector(99)
        self.List_3 = AFXList(p=listVf, nvis=10, tgt=form.keyword64Kw, sel=0, opts=HSCROLLING_OFF | LIST_SINGLESELECT)
        self.List_3.appendItem(text='1')
        self.List_3.appendItem(text='2')
        self.List_3.appendItem(text='3')
        self.List_3.appendItem(text='4')
        self.List_3.appendItem(text='5')

        self.List_3.setTarget(self)
        self.List_3.setSelector(self.ID_CLICKED_LIST)
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED_LIST, self.onListItemDoubleClicked)

        VFrame_17 = FXVerticalFrame(p=HFrame_32, opts=0, x=0, y=0, w=0, h=0,
                                    pl=0, pr=0, pt=0, pb=0)
        VAligner_10 = AFXVerticalAligner(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
                                         pl=0, pr=0, pt=0, pb=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText='     before:', tgt=form.keyword68Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText='>>Cycle List:', tgt=form.keyword65Kw, sel=0)
        form.keyword65Kw.setValue("")
        self.form.keyword65Kw.setTarget(self)
        self.form.keyword65Kw.setSelector(self.ID_CYCLE_LIST_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CYCLE_LIST_CHANGED, self.onCycleListChanged)

        AFXTextField(p=VAligner_10, ncols=50, labelText='     after:', tgt=form.keyword69Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText='     Cycle times:', tgt=form.keyword77Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText='     HOLDING time:', tgt=form.keyword94Kw, sel=0)
        HFrame_33 = FXHorizontalFrame(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
                                      pl=20, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_33, text='NEW', tgt=form.HFrame33Kw1, sel=58)
        FXRadioButton(p=HFrame_33, text='REPLACE', tgt=form.HFrame33Kw1, sel=59)
        ComboBox_13 = AFXComboBox(p=HFrame_33, ncols=0, nvis=1, text='       StepType:', tgt=form.keyword90Kw, sel=0)
        ComboBox_13.setMaxVisible(10)
        ComboBox_13.appendItem(text='AUTO')
        ComboBox_13.appendItem(text='StaticStep')
        ComboBox_13.appendItem(text='ViscoStep')
        ComboBox_13.appendItem(text='HeatTransferStep')
        createstep = FXButton(p=VFrame_17, text='creat step', ic=None, tgt=fileHandler,
                              sel=AFXMode.ID_ACTIVATE + 1,
                              opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        l = FXLabel(p=VFrame_17, text='Button: creat step', opts=JUSTIFY_LEFT)
        if isinstance(TabItem_5, FXHorizontalFrame):
            FXVerticalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        GroupBox_23 = FXGroupBox(p=TabItem_5, text='Quickly setting tools', opts=FRAME_GROOVE)
        HFrame_34 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_34, text='\xa1\xa1', tgt=form.GroupBox23Kw1, sel=60)
        AFXTextField(p=HFrame_34, ncols=12, labelText='for step name contains:', tgt=form.keyword72Kw, sel=0)
        FXRadioButton(p=HFrame_34, text='or', tgt=form.GroupBox23Kw1, sel=61)
        AFXTextField(p=HFrame_34, ncols=12, labelText='for N*n+i step; n,i=', tgt=form.keyword73Kw, sel=0)
        HFrame_35 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        ComboBox_11 = AFXComboBox(p=HFrame_35, ncols=0, nvis=1, text='method:', tgt=form.keyword70Kw, sel=0)
        ComboBox_11.setMaxVisible(10)
        ComboBox_11.appendItem(text='set Time period')
        ComboBox_11.appendItem(text='set Initial increment size')
        ComboBox_11.appendItem(text='set Max increment size')
        ComboBox_11.appendItem(text='set Ramp linearly over step')
        ComboBox_11.appendItem(text='set Instantaneous')
        ComboBox_11.appendItem(text='change Static to Visco')
        ComboBox_11.appendItem(text='change Visco to Static')
        ComboBox_11.appendItem(text='enable Restart')
        AFXTextField(p=HFrame_35, ncols=12, labelText=' Value:', tgt=form.keyword74Kw, sel=0)
        l = FXLabel(p=GroupBox_23, text='Button: modify step', opts=JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Loads&&HTC', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_7 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_26 = FXGroupBox(p=TabItem_7, text='Step List (%OP%)', opts=FRAME_GROOVE)
        VAligner_11 = AFXVerticalAligner(p=GroupBox_26, opts=0, x=0, y=0, w=0, h=0,
                                         pl=0, pr=0, pt=0, pb=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText='before:', tgt=form.keyword92Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=50, labelText='Composition of cycle:', tgt=form.keyword80Kw, sel=0)

        self.form.keyword80Kw.setTarget(self)
        self.form.keyword80Kw.setSelector(self.ID_TEXT_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TEXT_CHANGED, self.onTextChanged)

        AFXTextField(p=VAligner_11, ncols=12, labelText='after:', tgt=form.keyword93Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText='Cycle times:', tgt=form.keyword81Kw, sel=0)
        TabBook_5 = FXTabBook(p=TabItem_7, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        try:
            # currentModelName = getCurrentContext().get('modelName', '')
            # x = m.interactions.keys()
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(str(x))
            self.HTCList = []
            for i in m.interactions.keys():
                # mw.writeToMessageArea(str(m.interactions[i].__name__))
                if m.interactions[i].__name__ == 'FilmCondition':
                    self.HTCList.append(i)

        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("no model")
            self.HTCList = ['HTC1', 'HTC2']
        tabItem = FXTabItem(p=TabBook_5, text='Heat', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_20 = FXVerticalFrame(p=TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_20, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 4, 6, 4, form.keyword78Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 200)
        table.setColumnType(1, AFXTable.TEXT)
        table.setColumnWidth(2, 200)
        table.setColumnType(2, AFXTable.TEXT)
        table.setColumnWidth(3, 200)
        table.setColumnType(3, AFXTable.TEXT)
        table.setLeadingRowLabels('HTC (%NM%)\tAmpName\tTempName')
        table.setStretchableColumn(table.getNumColumns() - 1)
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)

        if self.HTCList:
            try:
                for i in range(0, len(self.HTCList)):
                    table.setItemText(i + 1, 1, str(self.HTCList[i]))
                    table.setItemText(i + 1, 2, '%OP%_%NM%_HTC')
                    table.setItemText(i + 1, 3, '%OP%_%NM%_TEMP')
            except Exception as e:
                print("Error filling table:", str(e))

        try:
            self.STRESSList = []
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Debug: Attempting to populate STRESSList for model: " + str(m.name))
            
            model_loads = m.loads 
            if not model_loads:
                mw.writeToMessageArea("Debug: m.loads is None or empty.")
                # self.STRESSList will remain empty
            else:
                model_loads_keys = model_loads.keys()
                mw.writeToMessageArea("Debug: m.loads.keys() content: " + str(model_loads_keys))
                if not model_loads_keys:
                    mw.writeToMessageArea("Debug: m.loads.keys() is empty.")
                for i in model_loads_keys:
                    self.STRESSList.append(i)
            mw.writeToMessageArea("Debug: self.STRESSList after try block: " + str(self.STRESSList))

        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("no model2")
            self.STRESSList = ['error1', 'error2']

        tabItem = FXTabItem(p=TabBook_5, text='Stress', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_21 = FXVerticalFrame(p=TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 3, 6, 3, form.keyword82Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 200)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 100)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setLeadingRowLabels('Load (%NM%)\tAmp')
        table.setStretchableColumn(table.getNumColumns() - 1)
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        if self.STRESSList:
            try:
                for i in range(0, len(self.STRESSList)):
                    table.setItemText(i + 1, 1, str(self.STRESSList[i]))
                    table.setItemText(i + 1, 2, '%OP%_%NM%')
            except Exception as e:
                print("Error filling table:", str(e))

        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        self.table1 = AFXTable(vf, 12, 6, 12, 6, form.keyword83Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        self.table1.setLeadingRows(1)
        self.table1.setLeadingColumns(1)
        self.table1.setColumnWidth(1, 200)
        self.table1.setColumnType(1, AFXTable.FLOAT)
        self.table1.setColumnWidth(2, 100)
        self.table1.setColumnType(2, AFXTable.FLOAT)
        self.table1.setColumnWidth(3, 100)
        self.table1.setColumnType(3, AFXTable.FLOAT)
        self.table1.setColumnWidth(4, 100)
        self.table1.setColumnType(4, AFXTable.FLOAT)
        self.table1.setColumnWidth(5, 100)
        self.table1.setColumnType(5, AFXTable.FLOAT)
        self.table1.setLeadingRowLabels('Temperature field at OP\tstart Step\tstart Inc\tend Step\tend Inc')
        self.table1.setStretchableColumn(table.getNumColumns() - 1)
        self.table1.showHorizontalGrid(True)
        self.table1.showVerticalGrid(True)
        tabItem = FXTabItem(p=TabBook_1, text='Fortran Subroutine', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_9 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = STPM_test1033DBFileHandler(form, 'SubroutineName', '*.for')
        fileTextHf = FXHorizontalFrame(p=TabItem_9, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=70, labelText='SubroutineName:', tgt=form.SubroutineNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        GroupBox_14 = FXGroupBox(p=TabItem_9, text='Redefine Creep', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        ComboBox_9 = AFXComboBox(p=GroupBox_14, ncols=0, nvis=1, text='Creep Consti:', tgt=form.keyword43Kw, sel=0)
        ComboBox_9.setMaxVisible(10)
        ComboBox_9.appendItem(text='RCC')
        ComboBox_9.appendItem(text='NB')
        vf = FXVerticalFrame(GroupBox_14, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 8, 6, 8, form.keyword16Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 100)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 100)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 100)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 100)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setColumnWidth(5, 100)
        table.setColumnType(5, AFXTable.FLOAT)
        table.setColumnWidth(6, 100)
        table.setColumnType(6, AFXTable.FLOAT)
        table.setColumnWidth(7, 100)
        table.setColumnType(7, AFXTable.FLOAT)
        table.setLeadingRowLabels('Temp\tp1\tp2\tp3\tp4\tp5\tp6')
        table.setStretchableColumn(table.getNumColumns() - 1)
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        HFrame_25 = FXHorizontalFrame(p=TabItem_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        GroupBox_12 = FXGroupBox(p=HFrame_25, text='Sr', opts=FRAME_GROOVE | LAYOUT_FILL_Y)
        TabBook_3 = FXTabBook(p=GroupBox_12, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        tabItem = FXTabItem(p=TabBook_3, text='Name', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_14 = FXVerticalFrame(p=TabBook_3,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_14, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 2, 5, 2, 5, form.keyword35Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 50)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 50)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 50)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 50)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setLeadingRowLabels('a\tb\tc\tC')
        table.setStretchableColumn(table.getNumColumns() - 1)
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        GroupBox_22 = FXGroupBox(p=HFrame_25, text='SN', opts=FRAME_GROOVE | LAYOUT_FILL_X | LAYOUT_FILL_Y)
        TabBook_4 = FXTabBook(p=GroupBox_22, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        tabItem = FXTabItem(p=TabBook_4, text='Name', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_18 = FXVerticalFrame(p=TabBook_4,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_18, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 5, 6, 5, form.keyword41Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 50)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 50)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 50)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 50)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setLeadingRowLabels('Life\tcol1\tcol2\tcol3')
        table.setStretchableColumn(table.getNumColumns() - 1)
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        GroupBox_13 = FXGroupBox(p=TabItem_9, text='Redefine KeyType', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        HFrame_12 = FXHorizontalFrame(p=GroupBox_13, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        spinner = AFXSpinner(HFrame_12, 6, 'SkipSteps:', form.keyword08Kw, 0)
        spinner.setRange(1, 999)
        spinner.setIncrement(1)
        spinner = AFXSpinner(HFrame_12, 6, 'Every n Step is a Cycle', form.keyword07Kw, 0)
        spinner.setRange(1, 999)
        spinner.setIncrement(1)
        AFXTextField(p=HFrame_12, ncols=12, labelText='DownStepLength:', tgt=form.keyword30Kw, sel=0)

        # 在TabBook_5创建后添加事件监听
        TabBook_5.setTarget(self)
        TabBook_5.setSelector(self.ID_TAB_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TAB_CHANGED, STPM_test1033DB.onTabChanged)

    # 主界面DB初始化结束，开始定义函数
    def get_current_model(self):
        """获取当前模型并更新界面显示
        
        Returns:
            Model: 当前模型对象，如果没有则返回 None
        """
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            model = mdb.models[currentModelName]
            # 更新ModelName显示
            self.form.keyword99Kw.setValue(currentModelName)
            return model
        return None
    def model_meterial_changed(self,form,sel, ptr):
        selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
        # 根据选择显示/隐藏输入框
        if selected_material == "New":
            self.newMaterialText.show()
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
        else:
            self.newMaterialText.hide()
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
    #1034版本代码
    # def model_meterial_changed(self, form, sel, ptr):
    #     selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
    #     if selected_material == "New":
    #         self.New_model_meterial_DetailDialog()

    # def New_model_meterial_DetailDialog(self):
    #     detail_dialog = AFXDialog(self, "ADD NEW ", opts=DIALOG_NORMAL, w=0,
    #                               h=0)
    #     vframe = FXVerticalFrame(detail_dialog, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
    #     AFXTextField(p=vframe, ncols=12, labelText='New:', sel=0)
    #     addnew = FXButton(p=vframe, text='add', ic=None, tgt=self,
    #                       sel=self.ID_CLICKED_NEW,
    #                       opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
    #     detail_dialog.create()
    #     detail_dialog.show()

    def updateTree(self, material):
        self.tree.clearItems()
        material_data = self.materials_data.get(material, {})

        def add_subtree(parent, node_name, node_data):
            node_name = str(node_name)
            if isinstance(node_data, dict):
                sub_parent = self.tree.addItemLast(parent, node_name)
                for key, value in node_data.items():
                    add_subtree(sub_parent, key, value)
            else:
                self.tree.addItemLast(parent, node_name)

            self.tree.expandTree(parent)

        root = self.tree.addItemLast(None, material)
        for key, value in material_data.items():
            add_subtree(root, key, value)
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea(str(material_data))
        self.tree.update()

            
    # 新增复选框状态变化处理函数
    def onTreeCheckChanged(self, sender, sel, ptr):
        item = self.tree.getCurrentItem()
        # 添加显式状态变量和参数校验
        if item and self.tree.getItemCheck(item) == 1:  
            selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(selected_material)
            # mw.writeToMessageArea(self.tree.getItemText(item))

            if str(self.tree.getItemText(item)) == selected_material:
                # 如果是根节点被选中
                def select_first_leaf(node):
                    # 如果是叶子节点，返回True
                    if not node.getFirst():
                        return True
                    
                    # 获取第一个子节点
                    child = node.getFirst()
                    first_leaf_found = False
                    
                    while child:
                        # 如果当前子节点是叶子节点
                        if select_first_leaf(child):
                            if not first_leaf_found:
                                # 选中第一个叶子节点
                                self.tree.setItemCheck(child, True, notify=False)
                                first_leaf_found = True
                            else:
                                # 取消选中其他叶子节点
                                self.tree.setItemCheck(child, False, notify=False)
                        child = child.getNext()
                    
                    return False

                # 从根节点开始递归处理
                select_first_leaf(item)
                
                # 更新树的显示
                self.tree.update()
            else:
                if self.tree.isItemLeaf(item):
                    parent = item.getParent()
                    if parent:
                        sibling = parent.getFirst()
                        while sibling:
                            if sibling != item and self.tree.isItemLeaf(sibling):
                                self.tree.setItemCheck(sibling, False, notify=False)  #
                            sibling = sibling.getNext()
                    self.tree.setItemCheck(item, True, notify=False)
                    self.tree.update()
                else:
                    while not self.tree.isItemLeaf(item):
                        child = item.getFirst()
                        self.tree.setItemCheck(child, True, notify=False)
                        sibling = child.getNext()
                        while sibling:
                            self.tree.setItemCheck(sibling, False, notify=False)  #
                            sibling = sibling.getNext()
                        item=child
                    self.tree.setItemCheck(item, True, notify=False)
                    self.tree.update()
                    
    def get_checked_data(self):
        """获取所有被勾选的叶子节点数据"""
        jsondata = {}
        full_data = self.materials_data
        mw = getAFXApp().getAFXMainWindow()
        
        def traverse_tree(node, current_dict, full_data):
            """递归遍历树节点"""
            if not node:
                return
                
            # 跳过根节点的处理
            if not node.getParent():  # 如果是根节点
                # 直接处理其子节点
                child = node.getFirst()
                while child:
                    traverse_tree(child, current_dict, full_data[self.tree.getItemText(node)])
                    child = child.getNext()
                return
                
            # 获取节点文本和检查状态
            node_text = self.tree.getItemText(node)
            is_checked = self.tree.getItemCheck(node)
            
            # 如果节点未被勾选，直接返回
            if not is_checked:
                return
                
            # 如果是叶子节点
            if not node.getFirst():
                # 构建路径
                path = []
                temp_node = node
                while temp_node and temp_node.getParent():  # 不包含根节点
                    path.insert(0, self.tree.getItemText(temp_node))
                    temp_node = temp_node.getParent()
                
                # 从完整数据中获取数据
                temp_data = full_data
                for key in path[:-1]:  # 不包含最后一个节点名称
                    temp_data = temp_data[key]
                
                # 构建数据结构
                temp_dict = current_dict
                for i, key in enumerate(path[:-1]):
                    if key not in temp_dict:
                        temp_dict[key] = {}
                    temp_dict = temp_dict[key]
                
                # 添加叶子节点数据
                temp_dict[path[-1]] = temp_data[path[-1]]
                
            else:
                # 如果不是叶子节点，继续遍历子节点
                if node_text not in current_dict:
                    current_dict[node_text] = {}
                child = node.getFirst()
                while child:
                    traverse_tree(child, current_dict[node_text], full_data)
                    child = child.getNext()
        
        # 获取根节点
        root = self.tree.getFirstItem()
        if not root:
            return jsondata
            
        # 开始遍历，从根节点的子节点开始
        traverse_tree(root, jsondata, full_data)
        
        return jsondata
    def onImport1Clicked(self, sender, sel, ptr):
        """处理import1按钮点击事件"""
        try:
            mw = getAFXApp().getAFXMainWindow()
            
            # 获取材料名称并验证
            aimMaterialName = self.newMaterialText.getText()
            if not aimMaterialName or not aimMaterialName.strip():
                mw.writeToMessageArea(u"错误: 材料名称不能为空".encode('utf-8'))
                return
                
            # 验证材料名称是否合法（只允许字母、数字、下划线和中文）
            import re
            if not re.match(r'^[a-zA-Z0-9_\u4e00-\u9fa5]+$', aimMaterialName):
                mw.writeToMessageArea(u"错误: 材料名称只能包含字母、数字、下划线和中文字符".encode('utf-8'))
                return
            
            mw.writeToMessageArea("Material name type: " + str(type(aimMaterialName)))
            mw.writeToMessageArea("Material name value: " + str(aimMaterialName))
            
            # 获取UVARM和SDV数量
            UVARMnum = self.form.keyword61Kw.getValue() 
            SDVnum = self.form.keyword62Kw.getValue()
            mw.writeToMessageArea("UVARM type: " + str(type(UVARMnum)))
            mw.writeToMessageArea("SDV type: " + str(type(SDVnum)))
            
            # 获取选中的材料数据
            jsondata = self.get_checked_data()
            mw.writeToMessageArea("JSON data type: " + str(type(jsondata)))
            mw.writeToMessageArea("JSON data content: " + str(jsondata))  # 添加这行来查看具体内容
            
            # 检查jsondata的内容
            if not isinstance(jsondata, dict):
                mw.writeToMessageArea("Error: JSON data must be a dictionary")
                return
                
            # 检查是否有选中的数据
            if not jsondata:
                mw.writeToMessageArea("Error: No material data selected")
                return
                
            # 调用pre_materialImport导入材料
            try:
                fortran_data = self.pre_materialImport(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum))
                # 导入成功后显示消息
                mw.writeToMessageArea(u"材料 '{}' 导入成功".format(aimMaterialName).encode('utf-8'))
                # 更新材料下拉框
                self.updateComboBox_15Materials()
            except Exception as e:
                mw.writeToMessageArea(u"错误: 材料导入失败 - ".encode('utf-8') + str(e))
                return
            
        except Exception as e:
            # 导入失败时显示错误信息
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Error importing material: " + str(e))
            # 打印更详细的错误信息
            import traceback
            mw.writeToMessageArea("Detailed error: " + str(traceback.format_exc()))


    
    def onMaterialChanged(self, sender, sel, ptr):
        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
        self.updateTree(selected_material)

    def updateMaterialComboBox(self, materials):
        self.ComboBox_12.clearItems()
        for material in materials:
            self.ComboBox_12.appendItem(text=material)

    def onItemDoubleClicked(self, sender, sel, ptr):
        def get_item_data(item):
            path = []
            while item is not None:
                path.append(self.tree.getItemText(item))
                item = item.getParent()
            path.reverse()
            data = self.materials_data
            for level in path:
                data = data.get(level)
                if data is None:
                    return None
            return data
        
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea('TreeItemDoubleClicked1')
        selected_item = self.tree.getCurrentItem()
        if selected_item:
            node_name = self.tree.getItemText(selected_item)
            selected_item_data = get_item_data(selected_item)
            if isinstance(selected_item_data, list):

                self.showDetailDialog(node_name, selected_item_data)
            # showAFXInformationDialog(mw, str(get_item_path(selected_item)))

    # def showDetailDialog(self, node_name, material_data):
    #     detail_dialog = AFXDialog(self, "Details for " + node_name, opts=DIALOG_NORMAL, w=0,
    #                               h=0)
    #     vframe = FXVerticalFrame(detail_dialog, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
    #     # for key, value in material_data.items():
    #     #     label = FXLabel(vframe, "{}: {}".format(key, str(value)))
    #     label = FXLabel(vframe, "{}: {}".format(node_name, str(material_data)))
    #     detail_dialog.create()
    #     detail_dialog.show()
    def showDetailDialog(self, node_name, material_data):
        # 创建对话框并传入数据副本
        dialog = MaterialDataDialog(self, "Editing: {}".format(node_name), material_data)
        dialog.create()
        dialog.showModal()  # 模态显示
        
        # 对话框关闭后处理
        if dialog.getModifiedData() is not None:
            # 更新临时JSON变量（示例变量名为temp_json）
            self.temp_json[node_name] = dialog.getModifiedData()
            
            # 可选：立即更新树形结构显示
            self.updateTree(self.ComboBox_12.getItemText(
                self.ComboBox_12.getCurrentItem()))
    ###########################################################################
    
    
    def show(self):
        """显示对话框时被调用"""
        AFXDataDialog.show(self)

        # 注册会话变化监听
        session.registerQuery(self.onSessionChange, False)
        
        # 获取并设置当前模型名称
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            self.regModel = mdb.models[currentModelName]
            # 注册材料变化监听
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False)
            # 设置当前模型名称
            self.form.keyword99Kw.setValue(currentModelName)
        
        # 更新材料下拉框
        self.updateComboBox_15Materials()

    def hide(self):
        """隐藏对话框时被调用"""
        AFXDataDialog.hide(self)
        # 注销会话变化监听
        session.unregisterQuery(self.onSessionChange)
        # 注销材料变化监听
        try:
            if self.regModel:
                self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
        except:
            pass

    def updateComboBox_15Materials(self):
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            # 更新ComboBox内容
            self.ComboBox_15.clearItems()
            model = mdb.models[currentModelName]
            names = model.materials.keys()
            if names:
                names.sort()
                for name in names:
                    self.ComboBox_15.appendItem(name)
                self.ComboBox_15.appendItem("New")
            # 更新关键字值
                self.form.keyword98Kw.setValue(names[0] if self.form.keyword98Kw.getValue() not in names else self.form.keyword98Kw.getValue())
            else:
                self.form.keyword98Kw.setValue('')
            self.resize(self.getDefaultWidth(), self.getDefaultHeight())
            return 1
        else:
            return 0

    def onSessionChange(self):
        """当会话改变时（包括模型切换）被调用"""
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            if self.regModel and getattr(self.regModel,'name',None) != currentModelName: #当模型切换过
                #取消注册旧模型材料
                try:
                    self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
                except:
                    pass
            #注册新模型材料
            self.regModel = mdb.models[currentModelName]
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False)
            
            # 更新ModelName显示
            self.form.keyword99Kw.setValue(currentModelName)
            
            # 更新材料下拉框
            self.updateComboBox_15Materials()
            return 1
        else:
            return 0

    def pre_materialImport(self, jsondata, aimMaterialName, UVARMnum, SDVnum):
        """导入材料数据到当前模型
        
        Args:
            jsondata (dict): 材料属性数据字典
            aimMaterialName (str): 目标材料名称
            UVARMnum (int): UVARM变量数量
            SDVnum (int): SDV变量数量
            
        Returns:
            dict: Fortran相关数据
        """
        # 验证参数
        if not isinstance(jsondata, dict):
            raise ValueError(u"JSON数据必须是字典格式".encode('utf-8'))
        
        if not isinstance(aimMaterialName, str):
            raise ValueError(u"材料名称必须是字符串类型".encode('utf-8'))
            
        if not aimMaterialName.strip():
            raise ValueError(u"材料名称不能为空".encode('utf-8'))
            
        # 获取当前模型
        m = self.get_current_model()
        if not m:
            raise RuntimeError(u"无法获取当前模型".encode('utf-8'))
            
        # 创建或获取材料
        try:
            if aimMaterialName in m.materials:
                mm = m.materials[aimMaterialName]
            else:
                mm = m.Material(name=aimMaterialName)
        except Exception as e:
            raise RuntimeError(u"创建材料失败: ".encode('utf-8') + str(e))
            
        # 分离用户自定义数据和标准材料数据
        filtered_data = {}
        fortran_data = {}
        for k, v in jsondata.items():
            if k.startswith("user_"):
                fortran_data[k] = v
            else:
                filtered_data[k] = v
                
        # 处理标准材料数据
        try:
            data = self.process_dict(filtered_data)
            for properrow in data:
                self.addproperty(mm, properrow)
        except Exception as e:
            raise RuntimeError(u"处理材料属性失败: ".encode('utf-8') + str(e))
            
        # 处理UVARM
        try:
            if UVARMnum > 0:
                mm.UserOutputVariables(n=UVARMnum)
            else:
                if hasattr(mm, 'userOutputVariables'):
                    del mm.userOutputVariables
        except Exception as e:
            raise RuntimeError(u"设置UVARM失败: ".encode('utf-8') + str(e))
            
        # 处理SDV
        try:
            if SDVnum > 0:
                mm.UserDefinedField()
                mm.Depvar(n=SDVnum)
            else:
                if hasattr(mm, 'depvar'):
                    del mm.depvar
                if hasattr(mm, 'userDefinedField'):
                    del mm.userDefinedField
        except Exception as e:
            raise RuntimeError(u"设置SDV失败: ".encode('utf-8') + str(e))
            
        return fortran_data

    def process_dict(self, d, path=None):
        if path is None:
            path = []
        result = []
        for key, value in d.items():
            if isinstance(value, dict):
                result.extend(self.process_dict(value, path + [key]))
            else:
                result.append(path + [key, value])
        return result

    def tDepCheck(self, property_name, property_type, property_data):
        # 定义属性与期望列数的映射
        property_columns = {
            'Density': {'Uniform': 1},
            'Elastic': {'Isotropic': 2},
            'Conductivity': {'Isotropic': 1},
            'Specific Heat': {'*': 1},
            'Expansion': {'Isotropic': 1},
            'Plastic': {'Isotropic': 2, 'User': 1},
            'Creep': {
                'Strain-Harding': 3,
                'Time-Harding': 3,
                'Power': 4,
                'Time_Power': 4,
                'User': 0
            },
        }
        # 获取传入数据的列数
        if not property_data:
            return OFF  # 无数据时默认关闭
        # 获取实际数据列数
        num_columns = len(property_data[0])
        
        # 获取期望列数
        columns_spec = property_columns.get(property_name)
        if columns_spec is None:
            return ON  # 属性不在映射中，默认开启
        
        # 解析期望列数
        if isinstance(columns_spec, dict):
            # 先检查具体类型
            expected_num = columns_spec.get(property_type)
            # 如果具体类型不存在，检查通配符
            if expected_num is None and '*' in columns_spec:
                expected_num = columns_spec['*']
        else:
            # 直接数字类型规格
            expected_num = columns_spec
        
        # 如果未找到具体类型或通配符，默认开启
        if expected_num is None:
            return ON
        
        # 判断列数是否匹配（实际列数 = 期望列数 + 1）
        return ON if num_columns == expected_num + 1 else OFF

    def addproperty(self, mm, datarow):
        property_name = datarow[0]
        property_type = datarow[1]
        table_data = datarow[-1]
        tDCflag = self.tDepCheck(property_name, property_type, table_data)
        try:
            tryarg=globals()[property_type.upper().replace(' ', '')]
        except (KeyError , AttributeError):
            tryarg=NONE
            print("{pt} is not a abaqusConstants".format(pt=str(property_type).upper().replace(' ', '')))
            print("the type of {pn} is set as default".format(pn=property_name))
        handler_map = {
            # Density处理
            ('Density', 'Uniform'): {
                'method': 'Density',
                'args': {
                    'distributionType': UNIFORM,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Density', '*'): {
                'method': 'Density',
                'args': {
                    'distributionType': DISCRETE_FIELD,
                    'fieldName': property_type,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # Creep处理
            ('Creep', 'User_defined'): {
                'method': 'Creep',
                'args': {'law': USER, 'table': ()}
            },
            ('Creep', 'Power'): {
                'method': 'Creep',
                'args': {
                    'law': POWER_LAW,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Creep', 'Time_Power'): {
                'method': 'Creep',
                'args': {
                    'law': TIME_POWER_LAW,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Creep', 'Time-Harding'): {
                'method': 'Creep',
                'args': {
                    'law': TIME,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Creep', 'Strain-Harding'): {
                'method': 'Creep',
                'args': {
                    'law': STRAIN,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Creep', '*'): {
                'method': 'Creep',
                'args': {
                    'law': tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # Plastic处理
            ('Plastic', '*'): {
                'method': 'Plastic',
                'args': {
                    'hardening': tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # Specific Heat处理
            ('Specific Heat', '*'): {
                'method': 'SpecificHeat',
                'args': {
                    'law': tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # 通用类型处理（Elastic, Expansion, Conductivity）
            ('Elastic', '*'): {
                'method': property_name,
                'args': {
                    'type':  tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Expansion', '*'): {
                'method': property_name,
                'args': {
                    'type':  tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('Conductivity', '*'): {
                'method': property_name,
                'args': {
                    'type':  tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            ('*', '*'): {
                'method': property_name.replace(' ', ''),
                'args': OrderedDict([
                    ('type',  tryarg),
                    ('table', table_data),
                    ('temperatureDependency', tDCflag)
                ])
            }
        }
        
        # 查找处理程序
        handler = None
        specific_key = (property_name, property_type)
        wildcard_key = (property_name, '*')
        any_key = ('*', '*')
        
        if specific_key in handler_map:
            handler = handler_map[specific_key]
        elif wildcard_key in handler_map:
            handler = handler_map[wildcard_key]
        
        # 如果未找到处理程序，抛出异常
        if not handler:
            #raise ValueError(f"No handler for {property_name}/{property_type}")
            print("No handler for {property_name}/{property_type}".format(property_name=property_name,property_type=property_type))
            handler = handler_map[any_key]
            method = getattr(mm, handler['method'])
            args_dict = handler['args']
            if tryarg==NONE:
                del method_args['type']
                method(**args_dict)
            else:
                if args_dict:
                    # 提取第一个参数的值和剩余参数
                    keys = list(args_dict.keys())
                    first_value = args_dict[keys[0]]
                    remaining_args = {k: args_dict[k] for k in keys[1:]}
                    # 调用时第一个参数作为第一个位置的参数传递
                    method(first_value, **remaining_args)
            return
        # 获取方法并调用
        method_name = handler['method']
        method_args = handler['args']
        # 检查 mm 对象是否有对应方法
        if not hasattr(mm, method_name):
            #raise AttributeError(f"Method '{method_name}' not found in mm object")
            print("Method '{method_name}' not found in mm object".format(method_name=method_name))
        if tryarg==NONE:
            del method_args['type']
        method = getattr(mm, method_name)
        method(**method_args)

    def onSheetChanged(self, sender, sel, ptr, *args):
        # 获取当前选中的 Sheet 索引
        selected_sheet_index = self.ComboBox_14.getCurrentItem()

        # 获取文件路径
        selected_file_path = self.fileNameKw.getValue()

        if selected_file_path:
            try:
                # 打开 Excel 文件
                workbook = xlrd.open_workbook(selected_file_path)

                # 获取选中的 Sheet
                sheet = workbook.sheet_by_index(selected_sheet_index)

                # 填充表格数据
                self.fillTableWithSheetData(sheet)
            except Exception as e:
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Error reading selected sheet: " + str(e))

    def fillTableWithSheetData(self, sheet):
        # 获取表格控件
        table = self.getTable()

        # 清空表格内容
        for row in range(table.getNumRows()):
            for col in range(table.getNumColumns()):
                table.setItemText(row, col, "")  # 将每个单元格内容设置为空字符串

        # 设置表头
        table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

        # 填充数据
        for row in range(sheet.nrows):
            for col in range(sheet.ncols):
                value = sheet.cell_value(row, col)
                table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

        # 更新表格显示
        table.update()

    def onListItemDoubleClicked(self, sender, sel, ptr, *args):
        # 获取当前选中的 item
        try:
            selected_item = self.List_3.getCurrentItem()
            if selected_item!="":
                # 获取 item 的文本内容
                item_text = self.List_3.getItemText(selected_item)
    
                # 将 item 的文本内容写入 AFXTextField
                old_text=self.form.keyword65Kw.getValue()
                if old_text=='':
                    self.form.keyword65Kw.setValue(str(item_text))
                else:
                    self.form.keyword65Kw.setValue(self.form.keyword65Kw.getValue()+','+str(item_text))

                # 显式调用 onCycleListChanged 来更新 keyword80Kw
                self.onCycleListChanged(self.form.keyword65Kw, 0, None)
                
                # 如果需要，可以在消息区域显示选中的内容
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Selected item: " + str(item_text))
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def rename_duplicates(self, lst):
        # 统计每个元素的总出现次数
        freq = Counter(lst)
        # 用于记录每个重复元素目前出现的次数
        occurrence = defaultdict(int)
        result = []
        for item in lst:
            # 如果元素出现多次，则添加后缀
            if freq[item] > 1:
                occurrence[item] += 1
                result.append("{}-{}".format(item, occurrence[item]))
            else:
                result.append(item)
        return result

    def onTextChanged(self, sender, sel, ptr, *args):
        # 获取 AFXTextField 的内容
        text = self.form.keyword80Kw.getValue()
        # 将内容解析为列表
        items = text.split(',')
        # 使用 rename_duplicates 函数修饰列表
        renamed_items = self.rename_duplicates(items)
        # 获取表格控件
        table = self.getTable1()
        # 清空表格内容
        for row in range(1, table.getNumRows()):  # 从第2行开始
            for col in range(1, table.getNumColumns()):  # 从第2列开始
                table.setItemText(row, col, "")
        # 填充表格
        for i, item in enumerate(renamed_items):
            # 第一列
            table.setItemText(i + 1, 1, item)

            # 第二列
            if item == 'HOLDING':
                table.setItemText(i + 1, 2, 'Propagated')
            else:
                table.setItemText(i + 1, 2, str(i + 1))
            # 第三列
            table.setItemText(i + 1, 3, '0')
            # 第四列
            table.setItemText(i + 1, 4, item)
            # 第五列
            table.setItemText(i + 1, 5, '-1')
        # 更新表格显示
        table.update()

    def onCycleListChanged(self, sender, sel, ptr, *args):
        # 获取控件1的内容
        cycle_list_text = self.form.keyword65Kw.getValue()
        # 将内容设置到控件2
        self.form.keyword80Kw.setValue(cycle_list_text)

    def getTable(self):
        return self.table

    def getComboBox14(self):
        return self.ComboBox_14

    def getList3(self):
        return self.List_3

    def getTable1(self):
        return self.table1

    def onTabChanged(self, sender, sel, ptr):
        try:
            # 获取当前选中的标签页索引
            current_tab = sender.getCurrent()
            
            # 如果是Stress标签页（索引为1）
            if current_tab == 1:
                # 获取文本框内容
                text = self.form.keyword80Kw.getValue()
                if text:
                    # 将内容解析为列表
                    items = text.split(',')
                    # 使用 rename_duplicates 函数修饰列表
                    renamed_items = self.rename_duplicates(items)
                    # 获取表格控件
                    table = self.getTable1()
                    # 清空表格内容
                    for row in range(1, table.getNumRows()):  # 从第2行开始
                        for col in range(1, table.getNumColumns()):  # 从第2列开始
                            table.setItemText(row, col, "")
                    # 填充表格
                    for i, item in enumerate(renamed_items):
                        # 第一列
                        table.setItemText(i + 1, 1, item)
                        # 第二列
                        if item == 'HOLDING':
                            table.setItemText(i + 1, 2, 'Propagated')
                        else:
                            table.setItemText(i + 1, 2, str(i + 1))
                        # 第三列
                        table.setItemText(i + 1, 3, '0')
                        # 第四列
                        table.setItemText(i + 1, 4, item)
                        # 第五列
                        table.setItemText(i + 1, 5, '-1')
                    # 更新表格显示
                    table.update()
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Error in onTabChanged: " + str(e))



# Class definition
###########################################################################

class STPM_test1033DBFileHandler(FXObject):

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form, keyword, patterns='*'):
        self.form = form
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.fileNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, STPM_test1033DBFileHandler.activate)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def activate(self, sender, sel, ptr):
        if not self.fileNameKw.getValue():
            self.fileNameKw.setValue(WORK_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
            self.fileNameKw, self.readOnlyKw,
            AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.create()
        fileDb.showModal()


class TesttreeDBFileHandler(FXObject):
    # 定义一个整数常量来代表 fileNameKw 改变事件

    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.JSONNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, TesttreeDBFileHandler.activate)
        # 监听 self.JSONNameKw 的改变事件
        self.JSONNameKw.setTarget(self)
        # 使用整数常量作为选择器
        self.JSONNameKw.setSelector(db.ID_FILE_NAME_CHANGED)
        # 映射事件到处理方法
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED, TesttreeDBFileHandler.onFileNameChanged)

    def activate(self, sender, sel, ptr):
        if not self.JSONNameKw.getValue():
            self.JSONNameKw.setValue(os.path.join(WORK_DIR, 'MaterialData.json'))
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.JSONNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        # 当 self.JSONNameKw 改变时，调用 outputFilePath 方法
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.JSONNameKw.getValue()
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))
        with open(selectedFilePath, 'r') as f:
            self.db.materials_data = json.load(f, object_pairs_hook=OrderedDict)
            # materials = ["Material 1", "Material 2", "Material 3"]
        materials = list(self.db.materials_data.keys())
        materials = [str(material) for material in materials]
        self.db.updateMaterialComboBox(materials)
        self.db.onMaterialChanged(sender, sel, ptr)


class XslFileHandler(FXObject):
    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.fileNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, XslFileHandler.activate)
        self.fileNameKw.setTarget(self)
        self.fileNameKw.setSelector(db.ID_FILE_NAME_CHANGED_1)
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED_1, XslFileHandler.onFileNameChanged)
        self.db.fileNameKw = self.fileNameKw

    def activate(self, sender, sel, ptr):
        if not self.fileNameKw.getValue():
            self.fileNameKw.setValue(os.path.join(WORK_DIR, 'ParaModelingData.xls'))
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.fileNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.fileNameKw.getValue()

        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))

        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()
            if len(sheet_names) > 1:
                mw.writeToMessageArea("yes")

            # 更新 ComboBox_14 的下拉框内容
            combo_box = self.db.getComboBox14()
            combo_box.clearItems()  # 清空现有内容
            for sheet_name in sheet_names:
                combo_box.appendItem(text=str(sheet_name))  # 添加 Sheet 名称到下拉框
            sheet = workbook.sheet_by_index(0)  # 假设读取第一个工作表
            sheet1 = workbook.sheet_by_index(1)
            mw.writeToMessageArea(str(sheet1))

            # 获取表格控件
            table = self.db.getTable()

            # 清空表格内容
            for row in range(table.getNumRows()):
                for col in range(table.getNumColumns()):
                    table.setItemText(row, col, "")  # 将每个单元格内容设置为空字符串

            # 设置表头
            table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

            # 填充数据
            for row in range(sheet.nrows):
                for col in range(sheet.ncols):
                    value = sheet.cell_value(row, col)
                    table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

            # 更新表格显示
            table.update()

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))


class InputFileHandler(FXObject):

    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.InputDataName = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, InputFileHandler.activate)
        # 监听 self.InputDataName 的改变事件
        self.InputDataName.setTarget(self)
        # 使用整数常量作为选择器
        self.InputDataName.setSelector(db.ID_FILE_NAME_CHANGED)
        # 映射事件到处理方法
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED, InputFileHandler.onFileNameChanged)

    def activate(self, sender, sel, ptr):
        if not self.InputDataName.getValue():
            self.InputDataName.setValue(WORK_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.InputDataName, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        # 当 self.InputDataName 改变时，调用 outputFilePath 方法
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.InputDataName.getValue()
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))
        list = self.db.getList3()
        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()

            # 清空 List_3 的内容
            if list:
                list.clearItems()

            for sheet_name in sheet_names:
                list.appendItem(text=str(sheet_name))
            list.appendItem(text='HOLDING')

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))

class MaterialDataDialog(AFXDialog):
    def __init__(self, owner, title, data):
        AFXDialog.__init__(self, owner, title, 
                          self.OK|self.CANCEL, 
                          opts=DIALOG_NORMAL, w=400, h=300)
        self.temp_data = data  # 存储原始数据的副本
        self.modified_data = None  # 存储修改后的数据
        
        # 创建垂直布局框架
        vframe = FXVerticalFrame(self, opts=LAYOUT_FILL_X|LAYOUT_FILL_Y)
        
        # 创建可编辑表格
        self.table = AFXTable(vframe, 20, 2, 200, 6, None, 0, 
                            AFXTABLE_EDITABLE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        self.table.setPopupOptions(AFXTable.POPUP_ALL)
        
        # 根据数据结构初始化表格
        self._initialize_table(data)
        
        # 绑定确定按钮事件
        # self.acceptCommand = self.onAccept
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_OK, self.onAccept)

    def _initialize_table(self, data):
        """根据空值/一维列表/二维列表初始化表格"""
        # 清空表格
        self.table.setTableSize(numRows=1, numColumns=1)
        try:
            if not data:  # 空值处理
                # 默认1行1列占位
                self.table.setTableSize(numRows=2, numColumns=1)  # 行数=2（表头+1行），列数=1
                # self.table.setLeadingRowLabels("Value")
                return
            
            if isinstance(data[0], (list, tuple)):  # 二维列表
                # 确定最大列数
                max_columns = max(len(row) for row in data)
                # 设置表格尺寸：行数=数据行数+1（表头），列数=最大列数
                self.table.setTableSize(numRows=len(data), numColumns=max_columns)
                # 生成表头（Column 1, Column 2...）
                # header = "".join(["Column {}\t".format(i+1) for i in range(max_columns)])
                # self.table.setLeadingRowLabels(header)
                # 填充数据（行和列从1开始）
                for row_idx, row in enumerate(data):
                    for col_idx, value in enumerate(row):
                        if col_idx + 1 > max_columns:  # 防止越界
                            break
                        self.table.setItemText(row_idx , col_idx , str(value))
            else:  # 一维列表
                # 设置表格尺寸：行数=数据长度+1（表头），列数=1
                self.table.setTableSize(numRows=len(data), numColumns=1)
                # self.table.setLeadingRowLabels("Value")
                # 填充数据（列索引固定为1）
                for row_idx, value in enumerate(data):
                    self.table.setItemText(row_idx , 0, str(value))
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def onAccept(self, sender, sel, ptr,*args):
        """保存表格数据（严格检查索引范围）"""
        try:
            num_rows = self.table.getNumRows()
            num_cols = self.table.getNumColumns()
            new_data = []
    
            if num_cols == 1:  # 一维列表
                for row in range(0, num_rows):  
                    value = self.table.getItemText(row, 0)
                    if value.strip():
                        new_data.append(self._convert_value(value))
            else:  # 二维列表
                for row in range(0, num_rows):  
                    row_data = []
                    for col in range(0, num_cols):  # 列从1到num_cols
                        value = self.table.getItemText(row, col)
                        if value.strip():
                            row_data.append(self._convert_value(value))
                    if row_data:
                        new_data.append(row_data)
            
            self.modified_data = new_data if new_data else None
            self.hide()
        except Exception as e:
            showAFXErrorDialog(getAFXApp().getAFXMainWindow(), "保存失败: {}".format(str(e)))

    def _convert_value(self, value_str):
        """尝试转换数据类型"""
        try:
            return json.loads(value_str)
        except:
            try:
                return float(value_str)
            except:
                if value_str.lower() == "true":
                    return True
                elif value_str.lower() == "false":
                    return False
                return str(value_str)

    def getModifiedData(self):
        """获取修改后的数据"""
        return self.modified_data

