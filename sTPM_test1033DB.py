# -*- coding: GB18030 -*-
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
        ID_TAB_CHANGED
    ] = range(AFXForm.ID_LAST+1, AFXForm.ID_LAST + 14)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form):

        # Construct the base class.

        AFXDataDialog.__init__(self, form, 'STPM_GB_Before',)
                               # self.OK | self.CANCEL, )
        self.materials_data = {}
        self.form = form
        self.temp_json = {}  # 1¤71¤71¤71¤71¤71¤70ľ2JSON1¤7Ľ
        # okBtn = self.getActionButton(self.ID_CLICKED_OK)
        # okBtn.setText('OK')

        TabBook_1 = FXTabBook(p=self, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL | LAYOUT_FILL_X | LAYOUT_FILL_Y,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)

        # try:
        #     excel_path = u"D:/1¤71¤71¤71¤7/1¤71¤71¤70Â91¤71¤71¤71¤71¤707/ParaModelingData.xls"
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
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.onTreeCheckChanged)  # 1¤71¤71¤71¤71¤7041¤71¤71¤7
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
        ModelNameTxt=AFXTextField(p=HFrame_38, ncols=12, labelText="ModelName:", tgt=form.keyword99Kw, sel=0)
        ModelNameTxt.hide()
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
        if self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())!='New': #20251¤71¤761¤71¤741¤71¤7 lgp
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
        # 0Ë2import11¤71¤7021¤71¤71¤71¤71¤7041¤71¤71¤71¤71¤7
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
        TabBook_1.setTarget(self)                 # 1¤71¤71¤71¤71¤71¤71¤71¤71¤70Ą51¤7§Ý1¤7
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
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(str(m.loads.keys()))
            for i in m.loads.keys():
                self.STRESSList.append(i)
            self.STRESSList = ['none1', 'none2']  # test

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

        # 1¤71¤7TabBook_51¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7041¤71¤71¤71¤71¤7
        TabBook_5.setTarget(self)
        TabBook_5.setSelector(self.ID_TAB_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TAB_CHANGED, STPM_test1033DB.onTabChanged)

    # 1¤71¤71¤71¤71¤71¤7DB1¤71¤70ś31¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70ś31¤71¤71¤7ď1¤71¤7
    def get_current_model(self):
        """1¤71¤70§01¤71¤70˘2001¤70Č01¤71¤71¤71¤7051¤71¤71¤71¤71¤70ś5
        
        Returns:
            Model: 1¤71¤70˘2001¤70Č41¤71¤71¤71¤71¤71¤7041¤71¤71¤7Ć5Ď31¤7 None
        """
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            model = mdb.models[currentModelName]
            # 1¤71¤71¤71¤7ModelName1¤71¤70ś5
            self.form.keyword99Kw.setValue(currentModelName)
            return model
        return None
    def model_meterial_changed(self,form,sel, ptr):
        selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
        # 1¤71¤71¤71¤70Ô51¤71¤71¤71¤70ś5/1¤71¤71¤71¤71¤71¤71¤71¤71¤7
        if selected_material == "New":
            self.newMaterialText.show()
            self.newMaterialText.getParent().recalc()  # 1¤71¤71¤70ă61¤71¤71¤71¤71¤71¤71¤71¤7051¤71¤71¤7
        else:
            self.newMaterialText.hide()
            self.newMaterialText.getParent().recalc()  # 1¤71¤71¤70ă61¤71¤71¤71¤71¤71¤71¤71¤7051¤71¤71¤7
    #10341¤7ˇÚ1¤71¤71¤71¤7
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

            
    # 1¤71¤71¤71¤71¤71¤70Ô51¤71¤70ü80Á01¤7Ł1¤71¤71¤71¤71¤71¤71¤71¤7
    def onTreeCheckChanged(self, sender, sel, ptr):
        item = self.tree.getCurrentItem()
        # 1¤71¤71¤71¤71¤71¤70ś40ü80Á01¤71¤71¤71¤71¤70Č01¤71¤71¤7§ľ1¤71¤7
        if item and self.tree.getItemCheck(item) == 1:  
            selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(selected_material)
            # mw.writeToMessageArea(self.tree.getItemText(item))

            if str(self.tree.getItemText(item)) == selected_material:
                # 1¤71¤71¤71¤70Ł01¤71¤7111Ę70Ô51¤71¤7
                def select_first_leaf(node):
                    # 1¤71¤71¤71¤71¤70Ý01¤70ä1110î41¤71¤71¤71¤7True
                    if not node.getFirst():
                        return True
                    
                    # 1¤71¤70§01¤71¤70Ý51¤71¤71¤70ä1111¤7
                    child = node.getFirst()
                    first_leaf_found = False
                    
                    while child:
                        # 1¤71¤71¤71¤71¤70˘21¤70ä1111¤71¤71¤70Ý01¤70ä1111¤7
                        if select_first_leaf(child):
                            if not first_leaf_found:
                                # 0Ô51¤7§Ö1¤70Ý51¤71¤70Ý01¤70ä1111¤7
                                self.tree.setItemCheck(child, True, notify=False)
                                first_leaf_found = True
                            else:
                                # 0§01¤71¤70Ô51¤71¤71¤71¤71¤71¤70Ý01¤70ä1111¤7
                                self.tree.setItemCheck(child, False, notify=False)
                        child = child.getNext()
                    
                    return False

                # 1¤70ă61¤71¤7112¤50ś31¤71Ľ7ř1¤71¤7
                select_first_leaf(item)
                
                # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70ś5
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
        """1¤71¤70§01¤71¤71¤7§Ň1¤71¤71¤70Ô51¤71¤70Ý01¤70ä1111¤71¤71¤71¤71¤7"""
        jsondata = {}
        full_data = self.materials_data
        mw = getAFXApp().getAFXMainWindow()
        
        # 1¤71Ľ71¤71¤70§01¤71¤71¤7§Ň1¤71¤71¤70Ô51¤705111¤7
        def collect_checked_nodes(node, path=None):
            if path is None:
                path = []
            
            if not node:
                return []
                
            result = []
            node_text = self.tree.getItemText(node)
            is_checked = self.tree.getItemCheck(node) == 1
            current_path = path + [node_text]
            
            # 1¤71¤71¤71¤71¤70Ý01¤70ä1111¤71¤70Ü51¤71¤71¤70Ô5
            if not node.getFirst() and is_checked:
                result.append(current_path)
            
            # 1¤71¤71¤71¤71¤70ä1111¤7
            child = node.getFirst()
            while child:
                result.extend(collect_checked_nodes(child, current_path))
                child = child.getNext()
                
            return result
        
        # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
        def build_data_from_paths(paths):
            result = {}
            
            for path in paths:
                # 1¤71¤71¤71¤71¤71¤71¤7111¤7
                if len(path) <= 1:
                    continue
                    
                # 1¤71¤70§01¤71¤71¤71¤71¤7111¤71¤71¤71¤7Ą¤1¤71¤7
                node_path = path[1:]
                
                # 1¤71¤70§01¤71¤71¤71¤70é6
                material_name = path[0]
                if material_name not in self.materials_data:
                    continue
                    
                data_source = self.materials_data[material_name]
                
                # 1¤71¤71¤71¤7Ą¤1¤71¤71¤71¤70§01¤71¤71¤71¤7
                temp_data = data_source
                for i, key in enumerate(node_path[:-1]):
                    if key not in temp_data:
                        break
                    temp_data = temp_data[key]
                
                # 1¤71¤71¤71¤71¤71¤70Ý51¤71¤71¤71¤71¤70˘91¤71¤71¤71¤7
                last_key = node_path[-1]
                if last_key not in temp_data:
                    continue
                
                # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71Ś15ú5
                temp_dict = result
                for i, key in enumerate(node_path[:-1]):
                    if key not in temp_dict:
                        temp_dict[key] = {}
                    temp_dict = temp_dict[key]
                
                # 1¤71¤71¤71¤70Ý01¤70ä1111¤71¤71¤71¤71¤7
                temp_dict[last_key] = temp_data[last_key]
            
            return result
        
        # 1¤71¤70§01¤71¤71¤7111¤7
        root = self.tree.getFirstItem()
        if not root:
            return jsondata
        
        # 1¤70đ81¤71¤71¤71¤7§Ű1¤70Ô51¤71¤7Ą¤1¤71¤7
        checked_paths = collect_checked_nodes(root)
        mw.writeToMessageArea("Checked paths: " + str(checked_paths))
        
        # 1¤71¤7Ą¤1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
        jsondata = build_data_from_paths(checked_paths)
        
        # 1¤71¤71¤70ă31¤71¤71¤71¤71¤71¤7
        try:
            # 1¤71¤71¤70ö51¤70ű81¤71¤70Ë21¤70ö71¤71¤71¤71¤71¤71¤71¤70¨01¤71¤71¤7
            json_str = str(jsondata)
            mw.writeToMessageArea("Selected data: " + json_str)
        except Exception as e:
            mw.writeToMessageArea("Error displaying data: " + str(e))
        
        return jsondata

    def onImport1Clicked(self, sender, sel, ptr):
        """1¤71¤71¤71¤7import11¤71¤7021¤71¤71¤71¤7041¤7"""
        try:
            mw = getAFXApp().getAFXMainWindow()
            
            # 1¤71¤70§01¤71¤71¤71¤71¤71¤71¤7081¤71¤71¤70ô8
            aimMaterialName = self.newMaterialText.getText()
            if not aimMaterialName or not aimMaterialName.strip():
                mw.writeToMessageArea("Error: Material name cannot be empty")
                return
                
            # 1¤71¤70ô81¤71¤71¤71¤71¤71¤71¤71¤71¤70˘91¤70Đ51¤71¤71¤70÷11¤71¤71¤71¤71¤71¤7001¤71¤71¤71¤71¤70ô51¤71¤7031¤71¤71˛61¤71¤71¤71¤7001¤7
            import re
            if not re.match(r'^[a-zA-Z0-9_\u4e00-\u9fa5]+$', aimMaterialName):
                mw.writeToMessageArea("Error: Material name can only contain letters, numbers, underscores and Chinese characters")
                return
            
            mw.writeToMessageArea("Material name: " + str(aimMaterialName))
            cur_item = self.ComboBox_15.getCurrentItem()
            combo_value = self.ComboBox_15.getItemText(cur_item)
            if combo_value == "New":
            # 0Ô51¤71¤71¤70š51¤7New1¤71¤71¤71¤71¤71¤71¤7031¤71¤71¤7
                aimMaterialName = self.newMaterialText.getText().strip()
                if not aimMaterialName:
                    showAFXErrorDialog(mw, "Please input a new material name.")
                    return
            else:
                # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7§Ó1¤71¤71¤71¤71¤7
                aimMaterialName = combo_value
            # 1¤71¤70§0UVARM1¤71¤7SDV1¤71¤71¤71¤7
            UVARMnum = self.form.keyword61Kw.getValue() 
            SDVnum = self.form.keyword62Kw.getValue()
            mw.writeToMessageArea("UVARM count: " + str(UVARMnum) + ", SDV count: " + str(SDVnum))
            
            # 1¤71¤70§00Ô51¤7§Ö041¤71¤71¤71¤71¤71¤71¤7
            mw.writeToMessageArea("Getting selected material data...")
            jsondata = self.get_checked_data()
            
            # 1¤71¤71¤7jsondata1¤71¤71¤71¤71¤71¤7
            if not isinstance(jsondata, dict):
                mw.writeToMessageArea("Error: JSON data must be a dictionary")
                return
                
            # 1¤71¤71¤71¤70˘91¤71¤71¤70Ô51¤7§Ö1¤71¤71¤71¤71¤7
            if not jsondata:
                mw.writeToMessageArea("Error: No material data selected")
                return
            
            # 1¤71¤71¤70Ô51¤7§Ö1¤71¤71¤71¤71Ś15ú51¤71¤71¤71¤71¤7111¤71¤71¤7
            try:
                keys_str = str(list(jsondata.keys()))
                mw.writeToMessageArea("Selected material data structure: " + keys_str)
            except Exception as e:
                mw.writeToMessageArea("Error displaying keys: " + str(e))
            
            # 1¤71¤71¤71¤71¤71¤71Ś15ú51¤71¤71¤71¤71¤7
            def check_data_depth(data, path=""):
                if isinstance(data, dict):
                    for key, value in data.items():
                        try:
                            new_path = path + " > " + str(key) if path else str(key)
                            mw.writeToMessageArea("Data path: " + new_path)
                            check_data_depth(value, new_path)
                        except Exception as e:
                            mw.writeToMessageArea("Error processing path: " + str(e))
            
            # 1¤71¤71¤71¤71¤71¤71Ś15ú51¤71¤71¤71¤71¤7
            mw.writeToMessageArea("Data structure details:")
            check_data_depth(jsondata)
            
            # 1¤71¤71¤71¤7pre_materialImport1¤71¤71¤71¤71¤71¤71¤7
            # try:
            mw.writeToMessageArea("Starting material import...")
            cmds=("import updatematerial\n"
                    "updatematerial.pre_materialImport_main({},'{}',{},{})\n".format(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum)))
            sendCommand(cmds)
            #fortran_data = self.pre_materialImport(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum))
            # 1¤71¤71¤71¤70Ż61¤71¤71¤71¤71¤70ś51¤71¤70Î4
            mw.writeToMessageArea("Material '" + str(aimMaterialName) + "' imported successfully")
            # 1¤71¤71¤7051¤71¤71¤71¤71¤71¤71¤71¤71¤7
            self.updateComboBox_15Materials()
            # except Exception as e:
            #     mw.writeToMessageArea("Error: Material import failed - " + str(e))
            #     return
            
        except Exception as e:
            # 1¤71¤71¤71¤70´21¤71¤70ľ21¤71¤70ś51¤71¤71¤71¤71¤71¤70Î4
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Error: Exception occurred during material import - " + str(e))
            # 1¤71¤70á31¤71¤71¤71¤70Đ61¤7061¤71¤71¤71¤71¤70Î4
            import traceback
            error_trace = str(traceback.format_exc())
            mw.writeToMessageArea("Detailed error information: " + error_trace)


    
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
            
            # self.materials_data1¤71¤7path11¤71¤71¤71¤7path21¤71¤71¤71¤7path31¤71¤7=
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
        # 1¤71¤71¤71¤71¤70ę31¤71¤7Ä8ł01¤71¤71¤71¤71¤71¤71Ľ61¤71¤71¤7
        dialog = MaterialDataDialog(self, "Editing: {}".format(node_name), material_item)
        dialog.create()
        dialog.showModal()  # 000Á01¤71¤70ś5
        
        # 1¤70ę31¤71¤71¤7190đ61¤71¤71¤7
        if dialog.getModifiedData() is not None:
            # 1¤71¤71¤71¤71¤71¤70ľ2JSON1¤71¤71¤71¤71¤71¤70ś51¤71¤71¤71¤71¤71¤71¤71¤70Ë2temp_json1¤71¤7
            self.temp_json[node_name] = dialog.getModifiedData()
            
            # 1¤71¤70Ô51¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7ŚÍ5ú51¤71¤70ś5
            self.updateTree(self.ComboBox_12.getItemText(
                self.ComboBox_12.getCurrentItem()))
    ###########################################################################
    
    
    def show(self):
        """1¤71¤70ś51¤70ę31¤71¤71¤70ľ21¤71¤71¤71¤71¤71¤7"""
        AFXDataDialog.show(self)

        # 0ű01¤71¤7641¤7Ł1¤71¤71¤71¤7
        session.registerQuery(self.onSessionChange, False)
        
        # 1¤71¤70§01¤71¤71¤71¤71¤7011¤70˘2001¤71¤71¤71¤71¤71¤7
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            self.regModel = mdb.models[currentModelName]
            # 0ű01¤71¤71¤71¤70Ď9Ł1¤71¤71¤71¤7
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False)
            # 1¤71¤71¤7011¤70˘2001¤71¤71¤71¤71¤71¤7
            self.form.keyword99Kw.setValue(currentModelName)
        
        # 1¤71¤71¤7051¤71¤71¤71¤71¤71¤71¤71¤71¤7
        self.updateComboBox_15Materials()

    def hide(self):
        """1¤71¤71¤7140ę31¤71¤71¤70ľ21¤71¤71¤71¤71¤71¤7"""
        AFXDataDialog.hide(self)
        # 0ű01¤71¤71¤7641¤7Ł1¤71¤71¤71¤7
        session.unregisterQuery(self.onSessionChange)
        # 0ű01¤71¤71¤71¤71¤70Ď9Ł1¤71¤71¤71¤7
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
        """1¤71¤71¤7641¤7031¤70ľ21¤71¤71¤71¤71¤71¤7001¤71¤71¤7§Ý1¤71¤71¤71¤71¤71¤71¤71¤71¤7"""
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            if self.regModel and getattr(self.regModel,'name',None) != currentModelName: #1¤71¤7001¤71¤71¤7§Ý1¤71¤71¤7
                #0§01¤71¤70ű01¤71¤71¤7001¤70Č01¤71¤71¤7
                try:
                    self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
                except:
                    pass
            #0ű01¤71¤71¤71¤7001¤70Č01¤71¤71¤7
            self.regModel = mdb.models[currentModelName]
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False)
            
            # 1¤71¤71¤71¤7ModelName1¤71¤70ś5
            self.form.keyword99Kw.setValue(currentModelName)
            
            # 1¤71¤71¤7051¤71¤71¤71¤71¤71¤71¤71¤71¤7
            self.updateComboBox_15Materials()
            return 1
        else:
            return 0

    def pre_materialImport(self, jsondata, aimMaterialName, UVARMnum, SDVnum):
        """1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71Ľ31¤71¤71¤70˘2001¤71¤7
        
        Args:
            jsondata (dict): 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70ö51¤7
            aimMaterialName (str): 071¤71¤71¤71¤71¤71¤71¤71¤71¤7
            UVARMnum (int): UVARM1¤71¤71¤71¤71¤71¤71¤71¤7
            SDVnum (int): SDV1¤71¤71¤71¤71¤71¤71¤71¤7
            
        Returns:
            dict: Fortran1¤71¤71¤71¤71¤71¤71¤7
        """
        # 1¤71¤70ô81¤71¤71¤71¤7
        if not isinstance(jsondata, dict):
            raise ValueError(u"JSON1¤71¤71¤71¤91¤71¤71¤71¤71¤71¤70ö51¤71¤70ś4")
        
        if not isinstance(aimMaterialName, str):
            raise ValueError(u"1¤71¤71¤71¤71¤71¤71¤7071¤71¤71¤71¤71¤71¤70ö71¤71¤71¤71¤71¤71¤71¤7")
            
        if not aimMaterialName.strip():
            raise ValueError(u"1¤71¤71¤71¤71¤71¤71¤7081¤71¤71¤70Ë21¤71¤7")
            
        # 1¤71¤70§01¤71¤70˘2001¤71¤7
        m = self.get_current_model()
        if not m:
            raise RuntimeError(u"1¤71Ť91¤71¤71¤70§01¤71¤70˘2001¤71¤7")
            
        # 1¤71¤71¤71¤71¤71¤71¤70§01¤71¤71¤71¤7
        try:
            if aimMaterialName in m.materials:
                mm = m.materials[aimMaterialName]
            else:
                mm = m.Material(name=aimMaterialName)
        except Exception as e:
            raise RuntimeError(u"1¤71¤71¤71¤71¤71¤71¤71¤70´21¤71¤7: " + str(e))
            
        # 1¤71¤71¤71¤71¤7041¤71¤70é81¤71¤71¤71¤71¤71¤71Ľ80Ç91¤70ý61¤71¤71¤71¤71¤71¤71¤71¤7
        filtered_data = {}
        fortran_data = {}
        for k, v in jsondata.items():
            if k.startswith("user_"):
                fortran_data[k] = v
            else:
                filtered_data[k] = v
                
        # 1¤71¤71¤71¤71¤71¤70ý61¤71¤71¤71¤71¤71¤71¤71¤7
        try:
            data = self.process_dict(filtered_data)
            for properrow in data:
                self.addproperty(mm, properrow)
        except Exception as e:
            raise RuntimeError(u"1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70´21¤71¤7: " + str(e))
            
        # 1¤71¤71¤71¤7UVARM
        try:
            if UVARMnum > 0:
                mm.UserOutputVariables(n=UVARMnum)
            else:
                if hasattr(mm, 'userOutputVariables'):
                    del mm.userOutputVariables
        except Exception as e:
            raise RuntimeError(u"1¤71¤71¤71¤7UVARM0´21¤71¤7: " + str(e))
            
        # 1¤71¤71¤71¤7SDV
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
            raise RuntimeError(u"1¤71¤71¤71¤7SDV0´21¤71¤7: " + str(e))
            
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
        # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70ă11¤71¤7
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
        # 1¤71¤70§01¤71¤71¤71¤71¤71¤71¤71Ľ31¤71¤71¤71¤71¤7
        if not property_data:
            return OFF  # 1¤71¤71¤71¤71¤71¤70ľ2081¤70Đ7191¤7
        # 1¤71¤70§00ľ61¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
        num_columns = len(property_data[0])
        
        # 1¤71¤70§01¤71¤71¤71¤71¤71¤71¤71¤7
        columns_spec = property_columns.get(property_name)
        if columns_spec is None:
            return ON  # 1¤71¤71¤70é41¤71¤71¤70ă11¤71¤71¤7§ľ1¤7081¤70Ń31¤71¤71¤7
        
        # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
        if isinstance(columns_spec, dict):
            # 1¤70Š71¤71¤71¤71¤71¤71¤71¤71¤71¤7
            expected_num = columns_spec.get(property_type)
            # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Č01¤71¤71¤71¤7131¤71¤71¤71¤70Ç01¤71¤71¤7
            if expected_num is None and '*' in columns_spec:
                expected_num = columns_spec['*']
        else:
            # 0ö11¤71¤71¤71¤71¤71¤71¤71¤71¤70Č71¤71¤7
            expected_num = columns_spec
        
        # 1¤71¤71¤7ŚÄ1¤70Ü91¤71¤71¤71¤71¤71¤71¤71¤70Č91¤70Ç01¤71¤71¤71¤71¤7081¤70Ń31¤71¤71¤7
        if expected_num is None:
            return ON
        
        # 1¤7§Ř1¤71¤71¤71¤71¤71¤70˘91¤7051¤7440ľ61¤71¤71¤71¤71¤71¤7 = 1¤71¤71¤71¤71¤71¤71¤71¤7 + 11¤71¤7
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
            # Density1¤71¤71¤71¤7
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
            # Creep1¤71¤71¤71¤7
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
            # Plastic1¤71¤71¤71¤7
            ('Plastic', '*'): {
                'method': 'Plastic',
                'args': {
                    'hardening': tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # Specific Heat1¤71¤71¤71¤7
            ('Specific Heat', '*'): {
                'method': 'SpecificHeat',
                'args': {
                    'law': tryarg,
                    'table': table_data,
                    'temperatureDependency': tDCflag
                }
            },
            # 0Ç01¤71¤71¤71¤71¤70Č21¤71¤71¤71¤71¤7Elastic, Expansion, Conductivity1¤71¤7
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
        
        # 1¤71¤71¤70Ü81¤71¤71¤71¤71¤71¤71¤7
        handler = None
        specific_key = (property_name, property_type)
        wildcard_key = (property_name, '*')
        any_key = ('*', '*')
        
        if specific_key in handler_map:
            handler = handler_map[specific_key]
        elif wildcard_key in handler_map:
            handler = handler_map[wildcard_key]
        
        # 1¤71¤71¤7ŚÄ1¤70Ü91¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70ü71¤71¤74§4
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
                    # 1¤71¤70§01¤71¤70Ý51¤71¤71¤71¤71¤71¤71¤71¤70ö51¤71¤70ł81¤71¤71¤71¤71¤7
                    keys = list(args_dict.keys())
                    first_value = args_dict[keys[0]]
                    remaining_args = {k: args_dict[k] for k in keys[1:]}
                    # 1¤71¤71¤71¤70ľ21¤71¤70Ý51¤71¤71¤71¤71¤71¤71¤71¤70Ë21¤71¤70Ý51¤71¤7ŚË1¤701041¤71¤71¤71¤71¤71¤71¤7
                    method(first_value, **remaining_args)
            return
        # 1¤71¤70§01¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
        method_name = handler['method']
        method_args = handler['args']
        # 1¤71¤71¤7 mm 1¤71¤71¤71¤71¤70˘91¤71¤7§Ř1¤70á81¤71¤71¤71¤7
        if not hasattr(mm, method_name):
            #raise AttributeError(f"Method '{method_name}' not found in mm object")
            print("Method '{method_name}' not found in mm object".format(method_name=method_name))
        if tryarg==NONE:
            del method_args['type']
        method = getattr(mm, method_name)
        method(**method_args)

    def onSheetChanged(self, sender, sel, ptr, *args):
        # 1¤71¤70§01¤71¤70˘20Ô51¤7§Ö1¤7 Sheet 1¤71¤71¤71¤7
        selected_sheet_index = self.ComboBox_14.getCurrentItem()

        # 1¤71¤70§01¤7041¤7Ą¤1¤71¤7
        selected_file_path = self.fileNameKw.getValue()

        if selected_file_path:
            try:
                # 1¤71¤7 Excel 1¤7041¤7
                workbook = xlrd.open_workbook(selected_file_path)

                # 1¤71¤70§00Ô51¤7§Ö1¤7 Sheet
                sheet = workbook.sheet_by_index(selected_sheet_index)

                # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
                self.fillTableWithSheetData(sheet)
            except Exception as e:
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Error reading selected sheet: " + str(e))

    def fillTableWithSheetData(self, sheet):
        # 1¤71¤70§01¤71¤71¤71¤7101¤7
        table = self.getTable()

        # 1¤71¤70ď71¤71¤71¤71¤71¤71¤71¤7
        for row in range(table.getNumRows()):
            for col in range(table.getNumColumns()):
                table.setItemText(row, col, "")  # 1¤71¤7071¤71¤71¤71¤70č61¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Ë21¤71¤71¤70ö71¤71¤71¤7

        # 1¤71¤71¤7091¤70Č5
        table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

        # 1¤71¤71¤71¤71¤71¤71¤7
        for row in range(sheet.nrows):
            for col in range(sheet.ncols):
                value = sheet.cell_value(row, col)
                table.setItemText(row + 1, col + 1, str(value))  # 1¤70ă3121¤71¤7§á1¤70ś31¤71¤71¤71¤71¤71¤71¤7

        # 1¤71¤71¤7ĄŔ1¤71¤71¤71¤71¤70ś5
        table.update()

    # 1¤71¤71¤71¤71¤71¤7 1¤71¤70Š61¤71¤7 STPM_test1033DB 1¤71¤71¤71¤71¤7131¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7 1¤71¤71¤71¤71¤71¤7
    def onMainTabChanged(self, sender, sel, ptr):
        try:
            if sender is None or not isinstance(sender, FXTabBook):
                return
            if sender.getCurrent() == 4:      # 4 1¤71¤70á8 Loads&HTC
                # 1¤71¤7 Cycle 1¤7101¤71¤71¤7 1¤71¤7 Loads&HTC 1¤7101¤71¤71¤7
                self.form.keyword92Kw.setValue(self.form.keyword68Kw.getValue())   # before
                self.form.keyword80Kw.setValue(self.form.keyword65Kw.getValue())   # composition
                self.form.keyword93Kw.setValue(self.form.keyword69Kw.getValue())   # after
                self.form.keyword81Kw.setValue(self.form.keyword77Kw.getValue())   # cycle times

                self.onTextChanged(None, None, None)
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea('0Ç41¤71¤7 Cycle.Loads&HTC 0´21¤71¤7: {}'.format(e))

    def onListItemDoubleClicked(self, sender, sel, ptr, *args):
        # 1¤71¤70§01¤71¤70˘20Ô51¤7§Ö1¤7 item
        try:
            selected_item = self.List_3.getCurrentItem()
            if selected_item!="":
                # 1¤71¤70§0 item 1¤71¤71¤7031¤71¤71¤71¤71¤7
                item_text = self.List_3.getItemText(selected_item)
    
                # 1¤71¤7 item 1¤71¤71¤7031¤71¤71¤71¤71¤7§Ő1¤71¤7 AFXTextField
                old_text=self.form.keyword65Kw.getValue()
                if old_text=='':
                    self.form.keyword65Kw.setValue(str(item_text))
                else:
                    self.form.keyword65Kw.setValue(self.form.keyword65Kw.getValue()+','+str(item_text))
    
                # 1¤71¤71¤71¤71¤70Ű81¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Î41¤71¤71¤71¤71¤71¤70ś50Ô51¤7§Ö1¤71¤71¤71¤71¤7
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Selected item: " + str(item_text))
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def rename_duplicates(self, lst):
        # 0Č11¤71¤7071¤71¤70č61¤7131¤71¤7171¤71¤70ö41¤71¤71¤7
        freq = Counter(lst)
        # 1¤71¤71¤7181¤704071¤71¤71¤7161¤70č61¤71¤7070˘21¤71¤71¤70ö5061¤71¤71¤7
        occurrence = defaultdict(int)
        result = []
        for item in lst:
            # 1¤71¤71¤70č61¤7111¤71¤70ö61¤7Ś˛1¤71¤71¤71¤71¤71¤70ă81¤70ý4
            if freq[item] > 1:
                occurrence[item] += 1
                result.append("{}-{}".format(item, occurrence[item]))
            else:
                result.append(item)
        return result

    def onTextChanged(self, sender, sel, ptr, *args):
        # 1¤71¤70§0 AFXTextField 1¤71¤71¤71¤71¤71¤7
        text = self.form.keyword80Kw.getValue()
        # 1¤71¤71¤71¤71¤71Ś11¤71¤71¤70Ë21¤7§Ň1¤7
        items = text.split(',')
        # 0ś01¤71¤7 rename_duplicates 1¤71¤71¤71¤71¤71¤71¤71¤71¤7§Ň1¤7
        renamed_items = self.rename_duplicates(items)
        # 1¤71¤70§01¤71¤71¤71¤7101¤7
        table = self.getTable1()
        # 1¤71¤70ď71¤71¤71¤71¤71¤71¤71¤7
        for row in range(1, table.getNumRows()):  # 1¤70ă31¤721¤7§á1¤70ś3
            for col in range(1, table.getNumColumns()):  # 1¤70ă31¤721¤7§á1¤70ś3
                table.setItemText(row, col, "")
        # 1¤71¤71¤71¤71¤71¤7
        for i, item in enumerate(renamed_items):
            # 1¤71¤70Ý51¤71¤7
            table.setItemText(i + 1, 1, item)

            # 1¤7121¤71¤71¤7
            if item == 'HOLDING':
                table.setItemText(i + 1, 2, 'Propagated')
            else:
                table.setItemText(i + 1, 2, str(i + 1))
            # 1¤71¤71¤71¤71¤71¤7
            table.setItemText(i + 1, 3, '0')
            # 1¤71¤71¤71¤71¤71¤7
            table.setItemText(i + 1, 4, item)
            # 1¤71¤71¤71¤71¤71¤7
            table.setItemText(i + 1, 5, '-1')
        # 1¤71¤71¤7ĄŔ1¤71¤71¤71¤71¤70ś5
        table.update()

    def onCycleListChanged(self, sender, sel, ptr, *args):
        # 1¤71¤70§01¤7101¤711¤71¤71¤71¤71¤71¤7
        cycle_list_text = self.form.keyword65Kw.getValue()
        # 1¤71¤71¤71¤71¤71¤71¤71¤71¤7011¤71¤7101¤72
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
            # 1¤71¤70§01¤71¤70˘20Ô51¤7§Ö031¤70Ą50Ü71¤71¤71¤71¤7
            current_tab = sender.getCurrent()
            
            # 1¤71¤71¤71¤71¤7Stress1¤71¤70Ą50Ü71¤71¤71¤71¤71¤71¤70Ë211¤71¤7
            if current_tab == 1:
                # 1¤71¤70§01¤7031¤71¤71¤71¤71¤71¤71¤7
                text = self.form.keyword80Kw.getValue()
                if text:
                    # 1¤71¤71¤71¤71¤71Ś11¤71¤71¤70Ë21¤7§Ň1¤7
                    items = text.split(',')
                    # 0ś01¤71¤7 rename_duplicates 1¤71¤71¤71¤71¤71¤71¤71¤71¤7§Ň1¤7
                    renamed_items = self.rename_duplicates(items)
                    # 1¤71¤70§01¤71¤71¤71¤7101¤7
                    table = self.getTable1()
                    # 1¤71¤70ď71¤71¤71¤71¤71¤71¤71¤7
                    for row in range(1, table.getNumRows()):  # 1¤70ă31¤721¤7§á1¤70ś3
                        for col in range(1, table.getNumColumns()):  # 1¤70ă31¤721¤7§á1¤70ś3
                            table.setItemText(row, col, "")
                    # 1¤71¤71¤71¤71¤71¤7
                    for i, item in enumerate(renamed_items):
                        # 1¤71¤70Ý51¤71¤7
                        table.setItemText(i + 1, 1, item)
                        # 1¤7121¤71¤71¤7
                        if item == 'HOLDING':
                            table.setItemText(i + 1, 2, 'Propagated')
                        else:
                            table.setItemText(i + 1, 2, str(i + 1))
                        # 1¤71¤71¤71¤71¤71¤7
                        table.setItemText(i + 1, 3, '0')
                        # 1¤71¤71¤71¤71¤71¤7
                        table.setItemText(i + 1, 4, item)
                        # 1¤71¤71¤71¤71¤71¤7
                        table.setItemText(i + 1, 5, '-1')
                    # 1¤71¤71¤7ĄŔ1¤71¤71¤71¤71¤70ś5
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
            self.fileNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
            self.fileNameKw, self.readOnlyKw,
            AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
        fileDb.create()
        fileDb.showModal()


class TesttreeDBFileHandler(FXObject):
    # 1¤71¤71¤71¤70Ý51¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7 fileNameKw 1¤7031¤71¤7041¤7

    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.JSONNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, TesttreeDBFileHandler.activate)
        # 1¤71¤71¤71¤7 self.JSONNameKw 1¤700031¤71¤7041¤7
        self.JSONNameKw.setTarget(self)
        # 0ś01¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Ë20Ô51¤71¤71¤71¤7
        self.JSONNameKw.setSelector(db.ID_FILE_NAME_CHANGED)
        # 0ă11¤71¤71¤7041¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
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
        # 1¤71¤7 self.JSONNameKw 1¤7031¤70ľ21¤71¤71¤71¤71¤71¤7 outputFilePath 1¤71¤71¤71¤7
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
            # 1¤71¤70§0 Excel 1¤7041¤7
            workbook = xlrd.open_workbook(selectedFilePath)
            # 1¤71¤70§01¤71¤71¤71¤7 Sheet 1¤71¤71¤71¤7
            sheet_names = workbook.sheet_names()
            if len(sheet_names) > 1:
                mw.writeToMessageArea("yes")

            # 1¤71¤71¤71¤7 ComboBox_14 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
            combo_box = self.db.getComboBox14()
            combo_box.clearItems()  # 1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
            for sheet_name in sheet_names:
                combo_box.appendItem(text=str(sheet_name))  # 1¤71¤71¤71¤7 Sheet 1¤71¤71¤7011¤71¤71¤71¤71¤71¤71¤7
            sheet = workbook.sheet_by_index(0)  # 1¤71¤71¤71¤71¤70§01¤71¤70Ý51¤71¤71¤71¤71¤71¤71¤71¤7
            sheet1 = workbook.sheet_by_index(1)
            mw.writeToMessageArea(str(sheet1))

            # 1¤71¤70§01¤71¤71¤71¤7101¤7
            table = self.db.getTable()

            # 1¤71¤70ď71¤71¤71¤71¤71¤71¤71¤7
            for row in range(table.getNumRows()):
                for col in range(table.getNumColumns()):
                    table.setItemText(row, col, "")  # 1¤71¤7071¤71¤71¤71¤70č61¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Ë21¤71¤71¤70ö71¤71¤71¤7

            # 1¤71¤71¤7091¤70Č5
            table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

            # 1¤71¤71¤71¤71¤71¤71¤7
            for row in range(sheet.nrows):
                for col in range(sheet.ncols):
                    value = sheet.cell_value(row, col)
                    table.setItemText(row + 1, col + 1, str(value))  # 1¤70ă3121¤71¤7§á1¤70ś31¤71¤71¤71¤71¤71¤71¤7

            # 1¤71¤71¤7ĄŔ1¤71¤71¤71¤71¤70ś5
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
        # 1¤71¤71¤71¤7 self.InputDataName 1¤700031¤71¤7041¤7
        self.InputDataName.setTarget(self)
        # 0ś01¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤70Ë20Ô51¤71¤71¤71¤7
        self.InputDataName.setSelector(db.ID_FILE_NAME_CHANGED)
        # 0ă11¤71¤71¤7041¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7
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
        # 1¤71¤7 self.InputDataName 1¤7031¤70ľ21¤71¤71¤71¤71¤71¤7 outputFilePath 1¤71¤71¤71¤7
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.InputDataName.getValue()
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))
        list = self.db.getList3()
        try:
            # 1¤71¤70§0 Excel 1¤7041¤7
            workbook = xlrd.open_workbook(selectedFilePath)
            # 1¤71¤70§01¤71¤71¤71¤7 Sheet 1¤71¤71¤71¤7
            sheet_names = workbook.sheet_names()

            # 1¤71¤71¤7 List_3 1¤71¤71¤71¤71¤71¤7
            if list:
                list.clearItems()

            for sheet_name in sheet_names:
                list.appendItem(text=str(sheet_name))
            list.appendItem(text='HOLDING')

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))

class MaterialDataDialog(AFXDialog):
    def __init__(self, owner, title, item):
        AFXDialog.__init__(self, owner, title, 
                          self.OK|self.CANCEL, 
                          opts=DIALOG_NORMAL, w=400, h=300)
        data = self.get_item_data(owner, item)
        self.temp_data = data  # 1¤7Ľ0č90ś31¤71¤71¤71Ľ3001¤71¤71¤7
        self.modified_data = None  # 1¤7Ľ1¤71Ź0021¤71¤71¤71¤71¤71¤7
        self.owner = owner
        self.item = item    
        # 1¤71¤71¤71¤71¤71¤70ö11¤71¤71¤70÷51¤71¤7
        vframe = FXVerticalFrame(self, opts=LAYOUT_FILL_X|LAYOUT_FILL_Y)
        
        # 1¤71¤71¤71¤71¤70Ž82ě91¤71¤71¤71¤7
        self.table = AFXTable(vframe, 20, 2, 200, 6, None, 0, 
                            AFXTABLE_EDITABLE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        self.table.setPopupOptions(AFXTable.POPUP_ALL)
        
        # 1¤71¤71¤71¤71¤71¤71¤71Ś15ú51¤71¤70ś31¤71¤71¤71¤71¤71¤7
        self._initialize_table(data)
        
        # 1¤71¤70Š21¤71¤71¤71¤7021¤7041¤7
        # self.acceptCommand = self.onAccept
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_OK, self.onAccept)

    def _initialize_table(self, data):
        """1¤71¤71¤71Ś31¤70ö5/0Ý50Ë41¤7§Ň1¤7/1¤71¤70Ë41¤7§Ň1¤71¤71¤70ś31¤71¤71¤71¤71¤71¤7"""
        # 1¤71¤70ď71¤71¤71¤7
        self.table.setTableSize(numRows=1, numColumns=1)
        try:
            if not data:  # 1¤71¤70ö51¤71¤71¤71¤7
                # 081¤71¤711¤71¤711¤71¤70đ8ŚË
                self.table.setTableSize(numRows=2, numColumns=1)  # 1¤71¤71¤71¤7=21¤71¤71¤71¤70Č5+11¤7§ľ1¤71¤71¤71¤71¤71¤71¤7=1
                # self.table.setLeadingRowLabels("Value")
                return
            
            if isinstance(data[0], (list, tuple)):  # 1¤71¤70Ë41¤7§Ň1¤7
                # 0Š21¤71¤71¤71¤71¤71¤71¤71¤71¤7
                max_columns = max(len(row) for row in data)
                # 1¤71¤71¤7091¤71¤71¤71˛0ťÇ1¤71¤71¤71¤7=1¤71¤71¤71¤71¤71¤71¤71¤7+11¤71¤71¤71¤70Č51¤71¤71¤71¤71¤71¤71¤71¤7=1¤71¤71¤71¤71¤71¤71¤7
                self.table.setTableSize(numRows=len(data), numColumns=max_columns)
                # 1¤71¤71¤70Ž81¤70Č51¤71¤7Column 1, Column 2...1¤71¤7
                # header = "".join(["Column {}\t".format(i+1) for i in range(max_columns)])
                # self.table.setLeadingRowLabels(header)
                # 1¤71¤71¤71¤71¤71¤71Ł51¤71¤7§Ü1¤71¤7§Ő1¤711¤71¤70ś31¤71¤7
                for row_idx, row in enumerate(data):
                    for col_idx, value in enumerate(row):
                        if col_idx + 1 > max_columns:  # 1¤71¤70ö90ę51¤71¤7
                            break
                        self.table.setItemText(row_idx , col_idx , str(value))
            else:  # 0Ý50Ë41¤7§Ň1¤7
                # 1¤71¤71¤7091¤71¤71¤71˛0ťÇ1¤71¤71¤71¤7=1¤71¤71¤71Ľ11¤71¤71¤7+11¤71¤71¤71¤70Č51¤71¤71¤71¤71¤71¤71¤71¤7=1
                self.table.setTableSize(numRows=len(data), numColumns=1)
                # self.table.setLeadingRowLabels("Value")
                # 1¤71¤71¤71¤71¤71¤71Ł51¤71¤71¤71¤71¤71¤71¤71¤70Â01¤70Ë211¤71¤7
                for row_idx, value in enumerate(data):
                    self.table.setItemText(row_idx , 0, str(value))
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def onAccept(self, sender, sel, ptr,*args):
        """1¤71¤71¤71¤71¤71¤71¤71¤71¤71¤71Ł51¤71¤70Đ61¤71¤71¤71¤71¤71¤71¤71¤71¤7Śś1¤71¤7"""
        try:
            num_rows = self.table.getNumRows()
            num_cols = self.table.getNumColumns()
            new_data = []
    
            if num_cols == 1:  # 0Ý50Ë41¤7§Ň1¤7
                for row in range(0, num_rows):  
                    value = self.table.getItemText(row, 0)
                    if value.strip():
                        new_data.append(self._convert_value(value))
            else:  # 1¤71¤70Ë41¤7§Ň1¤7
                for row in range(0, num_rows):  
                    row_data = []
                    for col in range(0, num_cols):  # 1¤7§Ő1¤711¤71¤7num_cols
                        value = self.table.getItemText(row, col)
                        if value.strip():
                            row_data.append(self._convert_value(value))
                    if row_data:
                        new_data.append(row_data)
            
            self.modified_data = new_data if new_data else None
            self.update_item_data(self.owner,self.item,self.modified_data)
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("1¤71¤71¤71¤70Ż61¤7{}".format(self.modified_data))
            # mw.writeToMessageArea("1¤71¤71¤71¤70Ż61¤7{}".format(self.owner.materials_data))
            self.hide()
        except Exception as e:
            showAFXErrorDialog(getAFXApp().getAFXMainWindow(), "1¤71¤71¤71¤70´21¤71¤7: {}".format(str(e)))

    def _convert_value(self, value_str):
        """1¤71¤71¤71¤70ű81¤71¤71¤71¤71¤71¤71¤71¤71¤71¤7"""
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
        """1¤71¤70§01¤71Ź0021¤71¤71¤71¤71¤71¤7"""
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
    
    # 1¤71¤70§0item1¤71¤7data
    def get_item_data(self, owner, item):
        path = []
        while item is not None:
            path.append(owner.tree.getItemText(item))
            item = item.getParent()
        path.reverse()
        data = owner.materials_data
        
        # owner.materials_data1¤71¤7path11¤71¤71¤71¤7path21¤71¤71¤71¤7path31¤71¤7=
        for level in path:
            data = data.get(level)
            if data is None:
                return None
        return data