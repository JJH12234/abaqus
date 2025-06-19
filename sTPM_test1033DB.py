# -*- coding: utf-8 -*-
from abaqusConstants import *
from abaqusGui import *
from kernelAccess import mdb, session
import os
import json
import xlrd
from collections import OrderedDict, Counter, defaultdict
PLUGIN_DIR = os.path.dirname(os.path.abspath(__file__))
# os.chdir(PLUGIN_DIR)
import re
thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)


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
        ID_TAB_CHANGED,
        ID_CLICKED_importfuzhi,
        ID_CLICKED_createstep,
        ID_CLICKED_modifystep,
        ID_CLICKED_updateModel,
        ID_CLICKED_AmpPair
    ] = range(AFXForm.ID_LAST+1, AFXForm.ID_LAST + 19)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form):

        # Construct the base class.
        mw = getAFXApp().getAFXMainWindow()
        AFXDataDialog.__init__(self, form, 'STPM_GB_Before',)
                               # self.OK | self.CANCEL, )
        self.materials_data = {}
        self.steptimepair = {}
        self.form = form
        self.temp_json = {}  # 添加临时JSON存储
        self.tab34flag=False
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

        tabItem = FXTabItem(p=TabBook_1, text=u'几何模型'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
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
        self.table.setColumnWidth(1, 120)
        self.table.setColumnType(1, AFXTable.TEXT)
        self.table.setColumnWidth(2, 80)
        self.table.setColumnType(2, AFXTable.TEXT)
        self.table.setColumnWidth(3, 150)
        self.table.setColumnType(3, AFXTable.TEXT)
        self.table.setColumnWidth(4, 100)
        self.table.setColumnType(4, AFXTable.TEXT)
        self.table.setColumnWidth(5, 80)
        self.table.setColumnType(5, AFXTable.TEXT)
        self.table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))
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
        updateBtn = FXButton(p=TabItem_22,
                     text=u'更新模型尺寸'.encode('GB18030'),
                     ic=None,
                     tgt=self,
                     sel=self.ID_CLICKED_updateModel,
                     opts=BUTTON_NORMAL | JUSTIFY_LEFT)
        updateBtn.setTarget(self)
        updateBtn.setSelector(self.ID_CLICKED_updateModel)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_updateModel, STPM_test1033DB.onUpdateModelClicked)
        # button = FXButton(p=TabItem_22, text=u'更新模型尺寸'.encode('GB18030'), opts=BUTTON_NORMAL | JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text=u'材料'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_16 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = TesttreeDBFileHandler(form, self,'JSONName', 'Json files (*.json)')
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
        ModelNameTxt=AFXTextField(p=HFrame_38, ncols=12, labelText=u'模型:'.encode('GB18030'), tgt=form.keyword99Kw, sel=0)
        ModelNameTxt.hide()
        self.ComboBox_15 = AFXComboBox(p=HFrame_38, ncols=0, nvis=1, text=u'目标材料:'.encode('GB18030'), tgt=form.keyword98Kw, sel=self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        self.ComboBox_15.setTarget(self)
        self.ComboBox_15.setSelector(self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_MODEL_MATERIAL_COMBO_CHANGED, STPM_test1033DB.model_meterial_changed)
        self.ComboBox_15.setMaxVisible(10)
        for material in m.materials.keys():
            self.ComboBox_15.appendItem(text=material)
        self.ComboBox_15.appendItem(text='New')
        self.newMaterialText = AFXTextField(p=HFrame_38, ncols=12, labelText='New:', 
            tgt=form.keyword100Kw, sel=0, opts=0)
        if self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())!='New': #2025年6月4日 lgp
            self.newMaterialText.hide()
        # AFXTextField(p=HFrame_38, ncols=12, labelText='New:', tgt=form.keyword100Kw, sel=0)
        HFrame_31 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VAligner_9 = AFXVerticalAligner(p=HFrame_31, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        spinner1 = AFXSpinner(VAligner_9, 6, u'UVARM个数'.encode('GB18030'), form.keyword61Kw, 0)
        spinner1.setRange(0, 100)
        spinner1.setIncrement(1)
        spinner2 = AFXSpinner(VAligner_9, 6, u'SDV个数'.encode('GB18030'), form.keyword62Kw, 0)
        spinner2.setRange(0, 9999)
        spinner2.setIncrement(1)
        import1 = FXButton(p=VFrame_16, text=u'导入该材料'.encode('GB18030'), ic=None, tgt=self,
                                     sel=self.ID_CLICKED_import1,
                                     opts=BUTTON_NORMAL |LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        #l = FXLabel(p=VFrame_16, text='Button: Run', opts=JUSTIFY_LEFT)
        # 为import1按钮添加事件处理
        import1.setTarget(self)
        import1.setSelector(self.ID_CLICKED_import1)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_import1, STPM_test1033DB.onImport1Clicked)
        tabItem = FXTabItem(p=TabBook_1, text=u'幅值表'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_6 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_5 = FXGroupBox(p=TabItem_6, text=u'数据格式'.encode('GB18030'), opts=FRAME_GROOVE | LAYOUT_FILL_X)
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
        run2 = FXButton(p=TabItem_6, text=u'导入幅值表'.encode('GB18030'), ic=None, tgt=fileHandler,
                        sel=AFXMode.ID_ACTIVATE + 1,
                        opts=BUTTON_NORMAL, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # l = FXLabel(p=TabItem_6, text='Button: Run', opts=JUSTIFY_LEFT)
        run2.setTarget(self)
        run2.setSelector(self.ID_CLICKED_importfuzhi)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_importfuzhi, STPM_test1033DB.Clicked_amplitude)
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
        tabItem = FXTabItem(p=TabBook_1, text=u'分析步'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_5 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        HFrame_32 = FXHorizontalFrame(p=TabItem_5, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        listVf = FXVerticalFrame(p=HFrame_32, opts=FRAME_SUNKEN | FRAME_THICK, x=0, y=0, w=0, h=0,
                                 pl=0, pr=0, pt=0, pb=0)
        TabBook_1.setTarget(self)                 # 监听顶层标签切换
        TabBook_1.setSelector(self.ID_TAB_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TAB_CHANGED,
                STPM_test1033DB.onMainTabChanged)
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
        AFXTextField(p=VAligner_10, ncols=50, labelText=u'     循环前分析步:'.encode('GB18030'), tgt=form.keyword68Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText=u'>>循环分析步构成:'.encode('GB18030'), tgt=form.keyword65Kw, sel=0)
        form.keyword65Kw.setValue("")
        self.form.keyword65Kw.setTarget(self)
        self.form.keyword65Kw.setSelector(self.ID_CYCLE_LIST_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CYCLE_LIST_CHANGED, self.onCycleListChanged)

        AFXTextField(p=VAligner_10, ncols=50, labelText=u'     循环后分析步:'.encode('GB18030'), tgt=form.keyword69Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText=u'     循环次数:'.encode('GB18030'), tgt=form.keyword77Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText=u'     HOLDING步步长:'.encode('GB18030'), tgt=form.keyword94Kw, sel=0)
        HFrame_33 = FXHorizontalFrame(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
                                      pl=20, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_33, text='NEW', tgt=form.HFrame33Kw1, sel=58)
        FXRadioButton(p=HFrame_33, text='REPLACE', tgt=form.HFrame33Kw1, sel=59)
        ComboBox_13 = AFXComboBox(p=HFrame_33, ncols=0, nvis=1, text=u'           分析步类型:'.encode('GB18030'), tgt=form.keyword90Kw, sel=0)
        ComboBox_13.setMaxVisible(10)
        ComboBox_13.appendItem(text='AUTO')
        ComboBox_13.appendItem(text='StaticStep')
        ComboBox_13.appendItem(text='ViscoStep')
        ComboBox_13.appendItem(text='HeatTransferStep')
        createstep = FXButton(p=VFrame_17, text=u'创建分析步'.encode('GB18030'), ic=None, tgt=fileHandler,
                              sel=AFXMode.ID_ACTIVATE + 1,
                              opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # l = FXLabel(p=VFrame_17, text='Button: creat step', opts=JUSTIFY_LEFT)
        createstep.setTarget(self)
        createstep.setSelector(self.ID_CLICKED_createstep)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_createstep, STPM_test1033DB.Createstep)
        if isinstance(TabItem_5, FXHorizontalFrame):
            FXVerticalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        GroupBox_23 = FXGroupBox(p=TabItem_5, text=u'批量设置工具'.encode('GB18030'), opts=FRAME_GROOVE)
        HFrame_34 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_34, text='\xa1\xa1', tgt=form.GroupBox23Kw1, sel=60)
        AFXTextField(p=HFrame_34, ncols=12, labelText=u'对名字中含有...的分析步:'.encode('GB18030'), tgt=form.keyword72Kw, sel=0)
        FXRadioButton(p=HFrame_34, text=u'或'.encode('GB18030'), tgt=form.GroupBox23Kw1, sel=61)
        AFXTextField(p=HFrame_34, ncols=12, labelText=u'对于从i开始每n的分析步(n,i):'.encode('GB18030'), tgt=form.keyword73Kw, sel=0)
        HFrame_35 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        ComboBox_11 = AFXComboBox(p=HFrame_35, ncols=0, nvis=1, text=u'方法:'.encode('GB18030'), tgt=form.keyword70Kw, sel=0)
        ComboBox_11.setMaxVisible(10)
        ComboBox_11.appendItem(text='set Time period')
        ComboBox_11.appendItem(text='set Initial increment size')
        ComboBox_11.appendItem(text='set Max increment size')
        ComboBox_11.appendItem(text='set Ramp linearly over step')
        ComboBox_11.appendItem(text='set Instantaneous')
        ComboBox_11.appendItem(text='change Static to Visco')
        ComboBox_11.appendItem(text='change Visco to Static')
        ComboBox_11.appendItem(text='enable Restart')
        AFXTextField(p=HFrame_35, ncols=12, labelText=u' 值:'.encode('GB18030'), tgt=form.keyword74Kw, sel=0)
        # l = FXLabel(p=GroupBox_23, text='Button: modify step', opts=JUSTIFY_LEFT)
        editstep = FXButton(p=GroupBox_23, text=u'修改分析步'.encode('GB18030'), ic=None, tgt=self,
                      sel=self.ID_CLICKED_modifystep,
                      opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        editstep.setTarget(self)
        editstep.setSelector(self.ID_CLICKED_modifystep)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_modifystep,STPM_test1033DB.Modifystep)
        tabItem = FXTabItem(p=TabBook_1, text=u'载荷/换热'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_7 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_26 = FXGroupBox(p=TabItem_7, text=u'分析步(%OP%)'.encode('GB18030'), opts=FRAME_GROOVE)
        VAligner_11 = AFXVerticalAligner(p=GroupBox_26, opts=0, x=0, y=0, w=0, h=0,
                                         pl=0, pr=0, pt=0, pb=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环前分析步:'.encode('GB18030'), tgt=form.keyword92Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=50, labelText=u'循环分析步构成:'.encode('GB18030'), tgt=form.keyword80Kw, sel=0)

        self.form.keyword80Kw.setTarget(self)
        self.form.keyword80Kw.setSelector(self.ID_TEXT_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TEXT_CHANGED, self.onTextChanged)

        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环后分析步:'.encode('GB18030'), tgt=form.keyword93Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环次数:'.encode('GB18030'), tgt=form.keyword81Kw, sel=0)
        self.importOdbName=AFXTextField(p=TabItem_7, ncols=50, labelText=u'温度场结果:'.encode('GB18030'), tgt=form.temperatureFieldKw, sel=0)
        pairamp = FXButton(p=TabItem_7, text=u'匹配幅值表(当前标签页)'.encode('GB18030'), ic=None, tgt=self,
                      sel=self.ID_CLICKED_AmpPair,
                      opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        pairamp.setTarget(self)
        pairamp.setSelector(self.ID_CLICKED_AmpPair)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_AmpPair, STPM_test1033DB.onCombineCommand)
        self.TabBook_5 = FXTabBook(p=TabItem_7, tgt=None, sel=0,
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
        tabItem = FXTabItem(p=self.TabBook_5, text=u'换热'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_20 = FXVerticalFrame(p=self.TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_20, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        self.tableH = AFXTable(vf, 6, 4, 6, 4, form.keyword78Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        self.tableH.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        self.tableH.setLeadingRows(1)
        self.tableH.setLeadingColumns(1)
        self.tableH.setColumnWidth(1, 200)
        self.tableH.setColumnType(1, AFXTable.TEXT)
        self.tableH.setColumnWidth(2, 200)
        self.tableH.setColumnType(2, AFXTable.TEXT)
        self.tableH.setColumnWidth(3, 200)
        self.tableH.setColumnType(3, AFXTable.TEXT)
        self.tableH.setLeadingRowLabels(u'膜条件(%NM%)\t换热幅值表\t温度幅值表'.encode('GB18030'))
        self.tableH.setStretchableColumn(self.tableH.getNumColumns() - 1)
        self.tableH.showHorizontalGrid(True)
        self.tableH.showVerticalGrid(True)

        if self.HTCList:
            try:
                for i in range(0, len(self.HTCList)):
                    self.tableH.setItemText(i + 1, 1, str(self.HTCList[i]))
                    self.tableH.setItemText(i + 1, 2, '%OP%_%NM%_HTC')
                    self.tableH.setItemText(i + 1, 3, '%OP%_%NM%_TEMP')
            except Exception as e:
                print("Error filling table:", str(e))

        try:
            m = self.get_current_model()
            self.STRESSList = []
            model_loads = m.loads.keys() 
            if not model_loads:
                mw.writeToMessageArea(u'Debug: 载荷列表为空'.encode('GB18030'))
            else:
                for i in model_loads:
                    self.STRESSList.append(i)
            mw.writeToMessageArea("Debug: self.STRESSList after try block: " + str(self.STRESSList))

        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

        tabItem = FXTabItem(p=self.TabBook_5, text=u'载荷'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_21 = FXVerticalFrame(p=self.TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        self.tableL = AFXTable(vf, 6, 3, 6, 3, form.keyword82Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        self.tableL.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        self.tableL.setLeadingRows(1)
        self.tableL.setLeadingColumns(1)
        self.tableL.setColumnWidth(1, 200)
        self.tableL.setColumnType(1, AFXTable.FLOAT)
        self.tableL.setColumnWidth(2, 100)
        self.tableL.setColumnType(2, AFXTable.FLOAT)
        self.tableL.setLeadingRowLabels(u'载荷(%NM%)\t幅值表'.encode('GB18030'))
        self.tableL.setStretchableColumn(self.tableL.getNumColumns() - 1)
        self.tableL.showHorizontalGrid(True)
        self.tableL.showVerticalGrid(True)
        if self.STRESSList:
            try:
                for i in range(len(self.STRESSList)):
                    if i >= self.tableL.getNumRows() - 1: # 如果需要更多行
                        self.tableL.insertRows(self.tableL.getNumRows(), 1) # 在末尾插入新行
                    self.tableL.setItemText(i + 1, 1, str(self.STRESSList[i]))
                    self.tableL.setItemText(i + 1, 2, '%OP%_%NM%')
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
        self.table1.setLeadingRowLabels(u'预定义温度场（工况）\t开始分析步\t开始增量步\t结束分析步\t结束增量步\t步长\t最大增量步'.encode('GB18030'))
        self.table1.setStretchableColumn(self.table1.getNumColumns() - 1)
        self.table1.showHorizontalGrid(True)
        self.table1.showVerticalGrid(True)
        ###子程序页面
        tabItem = FXTabItem(p=TabBook_1, text=u'子程序'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        tabItem.hide()
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
        self.TabBook_5.setTarget(self)
        self.TabBook_5.setSelector(self.ID_TAB_CHANGED)
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
        
        # 递归获取所有被勾选的节点
        def collect_checked_nodes(node, path=None):
            if path is None:
                path = []
            
            if not node:
                return []
                
            result = []
            node_text = self.tree.getItemText(node)
            is_checked = self.tree.getItemCheck(node) == 1
            current_path = path + [node_text]
            
            # 如果是叶子节点且被勾选
            if not node.getFirst() and is_checked:
                result.append(current_path)
            
            # 处理子节点
            child = node.getFirst()
            while child:
                result.extend(collect_checked_nodes(child, current_path))
                child = child.getNext()
                
            return result
        
        # 构建结果数据
        def build_data_from_paths(paths):
            result = {}
            
            for path in paths:
                # 跳过根节点
                if len(path) <= 1:
                    continue
                    
                # 提取除根节点外的路径
                node_path = path[1:]
                
                # 获取数据源
                material_name = path[0]
                if material_name not in self.materials_data:
                    continue
                    
                data_source = self.materials_data[material_name]
                
                # 遍历路径获取数据
                temp_data = data_source
                for i, key in enumerate(node_path[:-1]):
                    if key not in temp_data:
                        break
                    temp_data = temp_data[key]
                
                # 检查最后一个键是否存在
                last_key = node_path[-1]
                if last_key not in temp_data:
                    continue
                
                # 构建结果数据结构
                temp_dict = result
                for i, key in enumerate(node_path[:-1]):
                    if key not in temp_dict:
                        temp_dict[key] = {}
                    temp_dict = temp_dict[key]
                
                # 添加叶子节点数据
                temp_dict[last_key] = temp_data[last_key]
            
            return result
        
        # 获取根节点
        root = self.tree.getFirstItem()
        if not root:
            return jsondata
        
        # 收集所有勾选的路径
        checked_paths = collect_checked_nodes(root)
        mw.writeToMessageArea("Checked paths: " + str(checked_paths))
        
        # 从路径构建数据
        jsondata = build_data_from_paths(checked_paths)
        
        # 添加调试输出
        try:
            # 将字典转换为字符串并安全输出
            json_str = str(jsondata)
            mw.writeToMessageArea("Selected data: " + json_str)
        except Exception as e:
            mw.writeToMessageArea("Error displaying data: " + str(e))
        
        return jsondata

    def onUpdateModelClicked(self, sender, sel, ptr):
        mw = getAFXApp().getAFXMainWindow()

        excel_path = self.fileNameKw.getValue().strip()
        if not excel_path:
            showAFXErrorDialog(mw, u"请先指定 Excel 文件路径")
            return
        if not os.path.exists(excel_path):
            showAFXErrorDialog(mw, u"文件不存在：\n" + excel_path)
            return

        # 若插件目录不在 sys.path，可插入；若已在，可省略
        plugin_dir = os.path.dirname(__file__).replace('\\', '\\\\')

        cmd = (
            "import sys, os\n"
            "sys.path.insert(0, r'{plugin_dir}')\n"
            "import Parametric_modeling as pm\n"
            "pm.pre_paraModeling_main(r'{xls}')\n"
        ).format(plugin_dir=plugin_dir, xls=excel_path.replace('\\', '\\\\'))

        sendCommand(cmd)

        mw.writeToMessageArea(
            (u"模型尺寸已根据 {} 更新完毕\n"
            .format(os.path.basename(excel_path))).encode('GB18030'))


    def onImport1Clicked(self, sender, sel, ptr):
        """处理import1按钮点击事件"""
        try:
            mw = getAFXApp().getAFXMainWindow()
            
            # 获取材料名称并验证
            aimMaterialName = self.newMaterialText.getText()
            if not aimMaterialName or not aimMaterialName.strip():
                mw.writeToMessageArea("Error: Material name cannot be empty")
                return
                
            # 验证材料名称是否合法（只允许字母、数字、下划线和中文）
            import re
            if not re.match(r'^[a-zA-Z0-9_\u4e00-\u9fa5]+$', aimMaterialName):
                mw.writeToMessageArea("Error: Material name can only contain letters, numbers, underscores and Chinese characters")
                return
            
            mw.writeToMessageArea("Material name: " + str(aimMaterialName))
            cur_item = self.ComboBox_15.getCurrentItem()
            combo_value = self.ComboBox_15.getItemText(cur_item)
            if combo_value == "New":
            # 选择了“New”才用文本框
                aimMaterialName = self.newMaterialText.getText().strip()
                if not aimMaterialName:
                    showAFXErrorDialog(mw, "Please input a new material name.")
                    return
            else:
                # 否则用下拉框的已有材料名
                aimMaterialName = combo_value
            # 获取UVARM和SDV数量
            UVARMnum = self.form.keyword61Kw.getValue() 
            SDVnum = self.form.keyword62Kw.getValue()
            mw.writeToMessageArea("UVARM count: " + str(UVARMnum) + ", SDV count: " + str(SDVnum))
            
            # 获取选中的材料数据
            mw.writeToMessageArea("Getting selected material data...")
            jsondata = self.get_checked_data()
            
            # 检查jsondata的内容
            if not isinstance(jsondata, dict):
                mw.writeToMessageArea("Error: JSON data must be a dictionary")
                return
                
            # 检查是否有选中的数据
            if not jsondata:
                mw.writeToMessageArea("Error: No material data selected")
                return
            
            # 输出选中的数据结构，便于调试
            try:
                keys_str = str(list(jsondata.keys()))
                mw.writeToMessageArea("Selected material data structure: " + keys_str)
            except Exception as e:
                mw.writeToMessageArea("Error displaying keys: " + str(e))
            
            # 检查数据结构的深度
            def check_data_depth(data, path=""):
                if isinstance(data, dict):
                    for key, value in data.items():
                        try:
                            new_path = path + " > " + str(key) if path else str(key)
                            mw.writeToMessageArea("Data path: " + new_path)
                            check_data_depth(value, new_path)
                        except Exception as e:
                            mw.writeToMessageArea("Error processing path: " + str(e))
            
            # 输出数据结构的深度
            mw.writeToMessageArea("Data structure details:")
            check_data_depth(jsondata)
            
            # 调用pre_materialImport导入材料
            # try:
            mw.writeToMessageArea("Starting material import...")
            cmds=("import updatematerial\n"
                    "updatematerial.pre_materialImport_main({},'{}',{},{})\n".format(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum)))
            sendCommand(cmds)
            #fortran_data = self.pre_materialImport(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum))
            # 导入成功后显示消息
            mw.writeToMessageArea(u"Material {} imported successfully".format(aimMaterialName).encode('GB18030'))
            # 更新材料下拉框
            self.updateComboBox_15Materials()
            # except Exception as e:
            #     mw.writeToMessageArea("Error: Material import failed - " + str(e))
            #     return
            
        except Exception as e:
            # 导入失败时显示错误信息
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(u"错误: 材料导入失败 - {}".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            # 打印更详细的错误信息
            import traceback
            error_trace = str(traceback.format_exc())
            mw.writeToMessageArea(u"详细错误信息: {}".format(unicode(error_trace, 'utf-8', errors='replace')).encode('GB18030'))

    def Clicked_amplitude(self, sender, sel, ptr):
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea(u"正在导入幅值表...".encode('GB18030'))
        # 获取文件路径
        file_path = self.form.InputDataNameKw.getValue()
        
        # 检查文件路径是否存在
        if not file_path:
            mw.writeToMessageArea(u"错误: 未选择文件".encode('GB18030'))
            return
        
        # 构建命令并执行
        try:
            cmds=("import fuzhijiemian\n"
                  "fuzhijiemian.pre_ampTableChange('{}','%OP%_%TT%')\n".format(file_path))
            sendCommand(cmds)
            # 导入成功后显示消息
            mw.writeToMessageArea(u"幅值表导入成功，请查看消息区域获取详细信息".encode('GB18030'))
        except Exception as e:
            mw.writeToMessageArea(u"导入幅值表时出错: {}".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
    
    def Createstep(self, sender, sel, ptr):
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea(u"开始创建分析步...".encode('GB18030'))
        
        try:
            # 获取界面中的参数
            bstep_text = self.form.keyword68Kw.getValue()
            csteplist_text = self.form.keyword65Kw.getValue()
            astep_text = self.form.keyword69Kw.getValue()
            cyctimes = int(self.form.keyword77Kw.getValue())
            modeltype = str(self.form.keyword90Kw.getValue())
            
            # 解析步骤列表
            bstep = [x.strip() for x in bstep_text.split(',')] if bstep_text.strip() else []
            csteplist = [x.strip() for x in csteplist_text.split(',')] if csteplist_text.strip() else []
            astep = [x.strip() for x in astep_text.split(',')] if astep_text.strip() else []
            
            # 检查必要参数
            if not csteplist:
                mw.writeToMessageArea(u"错误: 循环分析步不能为空".encode('GB18030'))
                return
                
            # 创建步长字典
            steptimepair = self.steptimepair
            steptimepair['HOLDING'] = self.form.keyword94Kw.getValue()
            mw.writeToMessageArea(u'步长字典: {}'.format(steptimepair).encode('GB18030'))
            # {
            #     'G5':107600.0,
            #     'Steady':480000.0,
            #     'G6':3000.0,
            #     'G13':4500.0,
            #     'G9':18000.0,
            #     'HOLDING':20.0*365*24*3600,
            #     '*': 1.0#缺省值
            #     }  # 默认值
            
            # # 添加HOLDING步长（如果有指定）
            # holding_time = self.form.keyword94Kw.getValue()
            # if holding_time and holding_time.strip():
            #     try:
            #         steptimepair['HOLDING'] = float(holding_time)
            #     except ValueError:
            #         mw.writeToMessageArea(u"警告: HOLDING步长格式不正确，将使用默认值".encode('GB18030'))
            #         steptimepair['HOLDING'] = 20.0*365*24*3600  # 默认20年
            # else:
            #     steptimepair['HOLDING'] = 20.0*365*24*3600  # 默认20年
                
            # # 为csteplist中的每个步骤设置默认步长
            # for step in csteplist:
            #     if step not in steptimepair:
            #         steptimepair[step] = 1.0
                    
            # # 为bstep和astep中的每个步骤设置默认步长
            # for step in bstep + astep:
            #     if step not in steptimepair:
            #         steptimepair[step] = 1.0
            
            # 获取创建标志
            creatFlag = 'REPLACE' if self.form.HFrame33Kw1.getValue() == 59 else 'NEW'
            
            # 调用pre_stepBuild函数
            cmds=("import fenxibu\n"
                  "fenxibu.pre_stepBuild({},{},{},{},{},'{}','{}')\n".format(bstep, csteplist, steptimepair, astep, cyctimes, modeltype, creatFlag))
            sendCommand(cmds)
            
            mw.writeToMessageArea(u"分析步创建成功!".encode('GB18030'))

        except Exception as e:
            mw.writeToMessageArea(u"创建分析步时出错: '{}'".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            import traceback
            error_trace = traceback.format_exc()
            mw.writeToMessageArea(error_trace)

    def Modifystep(self, sender, sel, ptr):
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea(u"开始修改分析步...".encode('GB18030'))
        
        try:
            # 获取当前模型
            m = self.get_current_model()
            if not m:
                mw.writeToMessageArea(u"错误: 未找到当前模型".encode('GB18030'))
                return
                
            # 获取所有分析步
            all_steps = list(m.steps.keys())
            if not all_steps:
                mw.writeToMessageArea(u"错误: 当前模型没有分析步".encode('GB18030'))
                return
                
            # 获取单选框选择
            radio_selection = self.form.GroupBox23Kw1.getValue()
            
            # 获取界面中的其他参数
            edittype = self.form.keyword70Kw.getValue()  # 方法
            value = self.form.keyword74Kw.getValue()     # 值
            
            # 根据单选框选择筛选分析步
            filtered_steps = []
            
            if radio_selection == 60:  # 第一个单选框 - 对名字中含有...的分析步
                filter_text = self.form.keyword72Kw.getValue()
                if not filter_text:
                    mw.writeToMessageArea(u"警告: 未指定筛选文本，将修改所有分析步".encode('GB18030'))
                    filtered_steps = all_steps
                else:
                    # 筛选包含指定文本的分析步
                    filtered_steps = [step for step in all_steps if filter_text in step]
                    mw.writeToMessageArea(u"根据文本 '{}' 筛选出 {} 个分析步".format(
                        filter_text, len(filtered_steps)).encode('GB18030'))
            
            elif radio_selection == 61:  # 第二个单选框 - 对于从i开始每n的分析步(n,i)
                pattern_text = self.form.keyword73Kw.getValue()
                if not pattern_text:
                    mw.writeToMessageArea(u"错误: 未指定 n,i 格式".encode('GB18030'))
                    return
                    
                try:
                    parts = pattern_text.split(',')
                    if len(parts) < 2:
                        mw.writeToMessageArea(u"错误: 格式应为 'n,i'".encode('GB18030'))
                        return
                        
                    n = int(parts[0].strip())  # 每n个
                    i = int(parts[1].strip())  # 从i开始
                    
                    if i < 1:
                        i = 1  # 确保i至少为1
                        
                    # 按照从i开始每n个的方式筛选
                    filtered_steps = []
                    for idx, step in enumerate(all_steps, 1):  # 从1开始计数
                        if (idx - i) % n == 0 and idx >= i:
                            filtered_steps.append(step)
                    
                    mw.writeToMessageArea(u"从第{}个开始每{}个分析步，筛选出{}个分析步".format(
                        i, n, len(filtered_steps)).encode('GB18030'))
                        
                except ValueError:
                    mw.writeToMessageArea(u"错误: n,i 必须是整数".encode('GB18030'))
                    return
            else:
                # 未选择任何筛选方式，使用所有分析步
                filtered_steps = all_steps
                mw.writeToMessageArea(u"未指定筛选方式，将修改所有分析步".encode('GB18030'))
            
            # 检查筛选结果
            if not filtered_steps:
                mw.writeToMessageArea(u"警告: 筛选后没有分析步符合条件".encode('GB18030'))
                return
                
            # 检查编辑类型和值
            if not edittype:
                mw.writeToMessageArea(u"错误: 请选择修改方法".encode('GB18030'))
                return
                
            # 构建步骤名称列表字符串
            step_names_str = "[" + ",".join(["'{}'".format(step) for step in filtered_steps]) + "]"
            
            # 确保值是正确格式
            if value.isdigit() or (value and value[0] == '-' and value[1:].isdigit()):
                value_str = value  # 数字值不加引号
            else:
                try:
                    float_val = float(value)
                    value_str = str(float_val)  # 浮点数值不加引号
                except ValueError:
                    value_str = "'{}'".format(value)  # 字符串值加引号
            
            # 构建命令
            cmds = ("import fenxibu\n"
                   "fenxibu.pre_stepModify({}, '{}', {})\n".format(
                       step_names_str, edittype, value_str))
            
            # 调试输出命令
            mw.writeToMessageArea(u"执行命令: {}".format(cmds).encode('GB18030'))
            
            # 执行命令
            sendCommand(cmds)
            
            mw.writeToMessageArea(u"分析步修改成功!".encode('GB18030'))
            
        except Exception as e:
            mw.writeToMessageArea(u"修改分析步时出错: '{}'".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            import traceback
            error_trace = traceback.format_exc()
            mw.writeToMessageArea(error_trace)

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
            
            # self.materials_data【path1】【path2】【path3】=
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

                self.showDetailDialog(node_name, selected_item)
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
    def showDetailDialog(self, node_name, material_item):
        # 创建对话框并传入数据副本
        dialog = MaterialDataDialog(self, "Editing: {}".format(node_name), material_item)
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
            self.ComboBox_15.clearItems()

            model  = mdb.models[currentModelName]
            names  = sorted(model.materials.keys())   
            for name in names:
                self.ComboBox_15.appendItem(name)

            self.ComboBox_15.appendItem("New")

            if names:
                default = names[0]
            else:
                default = "New"
            self.form.keyword98Kw.setValue(default)

            self.resize(self.getDefaultWidth(), self.getDefaultHeight())
            return 1
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
        table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))

        # 填充数据
        for row in range(sheet.nrows):
            for col in range(sheet.ncols):
                value = sheet.cell_value(row, col)
                table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

        # 更新表格显示
        table.update()

    # ─── 仍然在 STPM_test1033DB 类体内，放在其他方法后面 ───
    def onMainTabChanged(self, sender, sel, ptr):
        try:
            if sender is None or not isinstance(sender, FXTabBook):
                return
            if sender.getCurrent() == 4:      # 4 对应 Loads&HTC
                # 从 Cycle 关键字 → Loads&HTC 关键字
                self.form.keyword92Kw.setValue(self.form.keyword68Kw.getValue())   # before
                self.form.keyword80Kw.setValue(self.form.keyword65Kw.getValue())   # composition
                self.form.keyword93Kw.setValue(self.form.keyword69Kw.getValue())   # after
                self.form.keyword81Kw.setValue(self.form.keyword77Kw.getValue())   # cycle times

                self.onTextChanged(None, None, None)
                self.tab34flag=True
            elif sender.getCurrent() == 3 and self.tab34flag:
                self.tab34flag=False
                self.form.keyword68Kw.setValue(self.form.keyword92Kw.getValue())   # before
                self.form.keyword65Kw.setValue(self.form.keyword80Kw.getValue())   # composition
                self.form.keyword69Kw.setValue(self.form.keyword93Kw.getValue())   # after
                self.form.keyword77Kw.setValue(self.form.keyword81Kw.getValue())   # cycle times
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(u'同步 Cycle.Loads&HTC 失败: {}'.format(e).encode('gb18030'))

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
    
                # 如果需要，可以在消息区域显示选中的内容
                # mw = getAFXApp().getAFXMainWindow()
                # mw.writeToMessageArea("Selected item: " + str(item_text))
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
            table.setItemText(i + 1, 4, str(i + 1))
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
                        table.setItemText(i + 1, 4, str(i + 1))
                        # 第五列
                        table.setItemText(i + 1, 5, '-1')
                    # 更新表格显示
                    table.update()
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Error in onTabChanged: " + str(e))
    def onCombineCommand(self, sender, sel, ptr):
        def getTableData(table):
            num_rows = table.getNumRows()
            num_cols = table.getNumColumns()
            tabledata = []
            for row in range(1, num_rows):  
                row_data = []
                for col in range(1, num_cols):  # 列从1到num_cols
                    value = table.getItemText(row, col)
                    if value.strip():
                        row_data.append(value)
                if row_data:
                    tabledata.append(tuple(row_data))
            return tabledata
        if self.TabBook_5.getCurrent()==0:
            table=self.tableH
            importedODBName=''
            tableFdata=((),)
        elif self.TabBook_5.getCurrent()==1:
            table=self.tableL
            tableF=self.table1
            tableFdata=getTableData(tableF)
            temp_odb = self.importOdbName.getText().strip()
            if temp_odb:
                if not temp_odb.endswith('.odb'):
                    temp_odb += '.odb'
                importedODBName = temp_odb
            else:
                importedODBName=''
        tabledata=getTableData(table)
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea(str(tuple(tabledata)))
        self.callkernelCombine(
            tabledata=tuple(tabledata),
            bstep=self.form.keyword92Kw.getValue().split(',') if self.form.keyword92Kw.getValue() else [],
            csteplist=self.form.keyword80Kw.getValue().split(',') if self.form.keyword80Kw.getValue() else [],
            astep=self.form.keyword93Kw.getValue().split(',') if self.form.keyword93Kw.getValue() else [],
            cyctimes=self.form.keyword81Kw.getValue(),
            fieldData=tuple(tableFdata),
            importedODBName=importedODBName,)
        return 1
    def callkernelCombine(self, tabledata, bstep, csteplist, cyctimes, astep,
      fieldData,importedODBName='',lossDefaultStrategy='Propagated',preFieldname='ImportedTemperature'):
        cmds=("""
import AmpPair
AmpPair.kernelCombine(
    tabledata={},
    bstep={},
    csteplist={},
    cyctimes={},
    astep={},
    lossDefaultStrategy='{}',
    fieldData={},
    preFieldname='{}',
    importedODBName='{}',
    )""".format(tabledata,bstep,csteplist,cyctimes,astep,lossDefaultStrategy,fieldData,preFieldname,importedODBName))
        sendCommand(cmds)
        pass


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
            self.fileNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
            self.fileNameKw, self.readOnlyKw,
            AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
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
            self.JSONNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.JSONNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
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
            self.fileNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.fileNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
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
            table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))

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
            self.InputDataName.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.InputDataName, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setDirectory(PLUGIN_DIR)
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
        self.db.steptimepair={}
        steptimepair = self.db.steptimepair
        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()

            # 清空 List_3 的内容
            if list:
                list.clearItems()
            
            # 遍历每个sheet，获取第一列最后一行的数值
            for sheet_name in sheet_names:
                sheet = workbook.sheet_by_name(sheet_name)
                if sheet.nrows > 0:  # 确保sheet不为空
                    # 获取第一列最后一行的值
                    last_row_value = sheet.cell_value(sheet.nrows-1, 0)
                    # 尝试将值转换为浮点数
                    try:
                        last_row_value = float(last_row_value)
                    except (ValueError, TypeError):
                        last_row_value = 1.0  # 如果转换失败，设置默认值
                    
                    # 将sheet名和值添加到steptimepair字典
                    steptimepair[sheet_name] = last_row_value
                    mw.writeToMessageArea(u'添加步长: {} = {}'.format(sheet_name, last_row_value).encode('GB18030'))
                
                # 添加到列表显示
                list.appendItem(text=str(sheet_name))
            
            # 添加HOLDING项
            list.appendItem(text='HOLDING')
            steptimepair.setdefault('HOLDING', 20.0*365*24*3600)  # 默认20年    
            steptimepair.setdefault('*', 1.0)  # 添加默认值
            
            # 输出完整的steptimepair字典
            # mw.writeToMessageArea(u'步长字典: {}'.format(steptimepair).encode('GB18030'))

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))

class MaterialDataDialog(AFXDialog):
    def __init__(self, owner, title, item):
        AFXDialog.__init__(self, owner, title, 
                          self.OK|self.CANCEL, 
                          opts=DIALOG_NORMAL, w=400, h=300)
        data = self.get_item_data(owner, item)
        self.temp_data = data  # 存储原始数据的副本
        self.modified_data = None  # 存储修改后的数据
        self.owner = owner
        self.item = item    
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
                        new_data.append((self._convert_value(value),))
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
            self.update_item_data(self.owner,self.item,self.modified_data)
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(u"保存成功{}".format(self.modified_data).encode('GB18030'))
            # mw.writeToMessageArea("保存成功{}".format(self.owner.materials_data))
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
    
    
    def update_item_data(self,owner,item,new_data):
        path = []
        while item is not None:
            path.append(owner.tree.getItemText(item))
            item = item.getParent()
        path.reverse()
        data = owner.materials_data
        #owner.materials_data['2.25Cr1Mo']['Density']['Uniform']['ASME']=new_data
        for level in path[:-1]:
            data = data[level]
            if data is None:
                return None
        data[path[-1]] = new_data
        return data
    
    # 获取item的data
    def get_item_data(self, owner, item):
        path = []
        while item is not None:
            path.append(owner.tree.getItemText(item))
            item = item.getParent()
        path.reverse()
        data = owner.materials_data
        
        # owner.materials_data【path1】【path2】【path3】=
        for level in path:
            data = data.get(level)
            if data is None:
                return None
        return data
